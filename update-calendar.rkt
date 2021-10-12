#lang racket/base

(require db
         gregor
         net/http-easy
         net/jwt
         racket/cmdline
         racket/port
         racket/pretty
         racket/string)

(define calendar-id (make-parameter ""))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

; To generate this file, take the private key from the service account JSON and do:
; $ openssl pkcs8 -in private-key.pem -outform DER -out private-key.der -nocrypt -topk8
;
; PKCS8/DER format is required by `crypto` when using "RS256". "RS256" is required by Google.
(define private-key-file (make-parameter ""))

(define service-account (make-parameter ""))

(command-line
 #:program "update-calendar.rkt"
 #:once-each
 [("-i" "--calendar-id") email-address
                         "Calendar ID (as an email address)"
                         (calendar-id email-address)]
 [("-k" "--private-key-file") pkey-file
                              "Private Key File (as PKCS8 / DER formatted)"
                              (private-key-file pkey-file)]
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-s" "--service-account") email-address
                             "Service account email address"
                             (service-account email-address)]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])

(define jwt (encode/sign "RS256"
                         (port->bytes (open-input-file (private-key-file)))
                         #:iss (service-account)
                         #:aud "https://oauth2.googleapis.com/token"
                         #:exp (seconds-between (datetime 1970) (+hours (now/utc) 1))
                         #:iat (seconds-between (datetime 1970) (now/utc))
                         #:other (hash 'scope "https://www.googleapis.com/auth/calendar.events")))

(define grant-response (post "https://oauth2.googleapis.com/token"
                             #:form (list (cons 'grant_type "urn:ietf:params:oauth:grant-type:jwt-bearer")
                                          (cons 'assertion jwt))))

(define access-token (hash-ref (response-json grant-response) 'access_token))

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
                 (post (string-append "https://www.googleapis.com/calendar/v3/calendars/"
                                      (calendar-id)
                                      "/events")
                       #:headers (hash 'Authorization (string-append "Bearer " access-token))
                       #:json (hash 'start (hash 'dateTime (string-append date-str "T15:30:00") 'timeZone "America/New_York")
                                    'end (hash 'dateTime (string-append date-str "T16:00:00") 'timeZone "America/New_York")
                                    'summary "Close Positions"
                                    'description (string-join (map (λ (p) (format "~a" (vector->list p))) positions) "\n")
                                    'defaultReminders (list)
                                    'reminders (hash 'overrides (list (hash 'method "popup"
                                                                            'minutes 1))
                                                     'useDefault #f)))))
