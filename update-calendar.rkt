#lang racket/base

(require db
         google/calendar
         google/oauth
         google/simple-token-store
         gregor
         racket/cmdline
         racket/pretty
         racket/string)

(define auth-code (make-parameter ""))

(define auth-request-url? (make-parameter #f))

(define calendar-id (make-parameter ""))

(define client-secret-file (make-parameter ""))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define refresh-token? (make-parameter #f))

(define store-token? (make-parameter #f))

; Steps to follow:
; 1. Generate the client secret file.
; 2. Generate an Authentication Request URL to receive a permission code
; 3. Use the permission code to generate and store a token that can be used to interact with the calendar.
;    When the token is stored, you can use it without generating new permission codes.
; 4. After a period of time, refresh the token to continue interacting with the calendar.
; See https://developers.google.com/calendar/auth for more info

(command-line
 #:program "update-calendar.rkt"
 #:once-each
 [("-c" "--authorization-code") c
                                "Authorization code received from the Authentication Request URL. This is required to generate and store the token."
                                (auth-code c)]
 [("-i" "--calendar-id") i
                         "Calendar ID (as an email address)"
                         (calendar-id i)]
 [("-l" "--authentication-request-url") "Show the Authentication Request URL that can be used to grant permissions"
                                        (auth-request-url? #t)]
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-r" "--refresh-token") "Attempt to refresh the token previously stored with --stored-token."
                           (refresh-token? #t)]
 [("-s" "--client-secret") s
                           "Client secret file from the credentials console https://console.developers.google.com"
                           (client-secret-file s)]
 [("-t" "--store-token") "Store the token associated with the code received from the Authentication Request URL"
                         (store-token? #t)]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])

(define client-secret (file->client (client-secret-file)))

(cond
  [(auth-request-url?) (displayln "Click the following URL to allow read/write access to Calendar Events:")
                       (displayln (authentication-request-url client-secret (list "https://www.googleapis.com/auth/calendar.events")))]
  [(store-token?) (displayln "Generating and saving new token.")
                  (define token (authorization-code->token client-secret (auth-code)))
                  (store-google-token! client-secret "Renegade Way" token)]
  [(refresh-token?) (displayln "Refreshing token.")
                    (define token (lookup-google-token client-secret))
                    (define new-token (refresh-token client-secret token))
                    (replace-google-token! client-secret new-token)]
  [else (define token (lookup-google-token client-secret))
        (define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))
        (define current-positions (query-rows dbc "
with earnings_end_date as (
  select
    act_symbol,
    case when \"when\" = 'Before market open'
      then case when date_part('dow', date) = 1
        then (date - interval '3 days')::date
        else (date - interval '1 days')::date
      end
      else date
    end as end_date
  from
    zacks.earnings_calendar
  where
    date >= current_date
), expiry_end_date as (
  select
    o.account,
    o.order_id,
    min(c.expiry) as expiry
  from
    ibkr.order o
  join
    ibkr.order_leg ol
  on
    o.account = ol.account and
    o.order_id = ol.order_id
  join
    ibkr.contract c
  on
    ol.contract_id = c.contract_id
  where
    c.expiry >= current_date
  group by
    o.account,
    o.order_id
)
select
  c.symbol,
  to_char(c.expiry, 'YYYY-MM-DD') as expiry,
  c.strike,
  c.right::text,
  e.account,
  e.signed_shares,
  coalesce(to_char(case when ed.end_date is not null and ed.end_date < n.end_date
    then case when eed.expiry is not null and eed.expiry < ed.end_date
      then eed.expiry else ed.end_date end
    else case when eed.expiry is not null and eed.expiry < n.end_date
      then eed.expiry else n.end_date end
  end, 'YYYY-MM-DD'), '') as end_date
from
  (select
    max(order_id) as order_id,
    contract_id,
    account,
     sum(
        case execution.side
            when 'BOT'::text then execution.shares
            when 'SLD'::text then execution.shares * '-1'::integer::numeric
            else NULL::numeric
        end) as signed_shares
  from
    ibkr.execution
  group by
    contract_id, account) e
join
  ibkr.contract c
on
  e.contract_id = c.contract_id
left outer join
  ibkr.order_note n
on
  e.account = n.account and
  e.order_id = n.order_id
left outer join
  earnings_end_date ed
on
  c.symbol = ed.act_symbol
left outer join
  expiry_end_date eed
on
  e.account = eed.account and
  e.order_id = eed.order_id
where
  c.expiry >= current_date and
  signed_shares != 0
order by
  symbol,
  expiry,
  strike,
  \"right\";
"))
        (define expiring-positions (filter (λ (p) (date>=? (+days (today) 7)
                                                           (iso8601->date (vector-ref p 6))))
                                           current-positions))
        (define grouped-positions (foldl (λ (p acc)
                                           (if (hash-has-key? acc (vector-ref p 6))
                                               (hash-set acc (vector-ref p 6) (append (hash-ref acc (vector-ref p 6)) (list p)))
                                               (hash-set acc (vector-ref p 6) (list p))))
                                         (hash)
                                         expiring-positions))
        (if (> (hash-count grouped-positions) 0)
            (displayln "Creating calendar event(s).")
            #f)
        (hash-for-each grouped-positions
                       (λ (date-str positions)
                         (insert-events (hash 'start (hash 'dateTime (string-append date-str "T15:30:00") 'timeZone "America/New_York")
                                              'end (hash 'dateTime (string-append date-str "T16:00:00") 'timeZone "America/New_York")
                                              'summary "Close Positions"
                                              'description (string-join (map (λ (p) (format "~a" (vector->list p))) positions) "\n")
                                              'defaultReminders (list)
                                              'reminders (hash 'overrides (list (hash 'method "popup"
                                                                                      'minutes 1))
                                                               'useDefault #f
                                                               ))
                                        (calendar-id)
                                        #:token token)))])
