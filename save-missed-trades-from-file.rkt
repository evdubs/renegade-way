#lang racket/base

(require db
         gregor
         racket/cmdline
         racket/list
         racket/port
         racket/string)

(define account (make-parameter ""))

(define filename (make-parameter ""))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define db-user (make-parameter "user"))

(command-line
 #:program "racket save-missed-trades-from-file.rkt"
 #:once-each
 [("-a" "--account") acct
                     "Account for these trades. The export uses account alias and not account number."
                     (account acct)]
 [("-f" "--csv-file") csv-file
                      "CSV file name for saving."
                      (filename csv-file)]
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(call-with-input-file (filename)
  (λ (in)
    (define csv-string (port->string in))
    (define lines (map (λ (str) (string-split str "," #:trim? #f)) (string-split csv-string "\n")))
    (for-each
     (λ (line)
       (writeln line)
       (if (or (equal? "Drill Down" (first line))
               (equal? "BAG" (second line)))
           (displayln "Skipping header and BAG lines")
           (query-exec dbc "
insert into ibkr.execution (
  order_id,
  contract_id,
  execution_id,
  timestamp,
  account,
  executing_exchange,
  side,
  shares,
  price,
  perm_id,
  client_id,
  liquidation,
  cumulative_quantity,
  average_price,
  order_reference,
  model_code
) values (
  0,
  case
    when $1 = '' then
      (select
         contract_id
       from
         ibkr.contract
       where
         symbol = $14 and
         security_type = 'STK')
    else
      (select
         contract_id
       from
         ibkr.contract
       where
         symbol = $1 and
         expiry = $2::text::date and
         strike = $3::text::numeric and
         \"right\" = $4::text::ibkr.\"right\")
  end,
  $5,
  ($6 || ' ' || $7 || '+0')::timestamptz,
  $8,
  $9,
  $10,
  $11::text::numeric,
  $12::text::numeric,
  case
    when $13 = '' then null
    else $13::text::integer
  end,
  0,
  0,
  $11::text::numeric,
  $12::text::numeric,
  '',
  ''
) on conflict (execution_id) do nothing;
"
                       (list-ref line 40) ; Trading Class
                       (list-ref line 2) ; Last Trading Day
                       (list-ref line 3) ; Strike
                       (string-upcase (list-ref line 4)) ; Put/Call
                       (list-ref line 20) ; ID
                       (list-ref line 13) ; Date
                       (list-ref line 12) ; Time
                       (account)
                       (list-ref line 14) ; Exch
                       (list-ref line 6) ; Action
                       (list-ref line 8) ; Quantity
                       (list-ref line 11) ; Price
                       (list-ref line 30) ; Vol Link
                       (list-ref line 10) ; Fin Instrument
                       )))
     lines)))
