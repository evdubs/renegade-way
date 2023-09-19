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
  (select
     contract_id
   from
     ibkr.contract
   where
     symbol = $1 and
     expiry = $2::text::date and
     strike = $3::text::numeric and
     \"right\" = $4::text::ibkr.\"right\"),
  $5,
  ($6 || ' ' || $7 || '+0')::timestamptz,
  $8,
  $9,
  $10,
  $11::text::numeric,
  $12::text::numeric,
  $13::text::integer,
  0,
  0,
  $11::text::numeric,
  $12::text::numeric,
  '',
  ''
) on conflict (execution_id) do nothing;
"
                       (list-ref line 40)
                       (list-ref line 2)
                       (list-ref line 3)
                       (string-upcase (list-ref line 4))
                       (list-ref line 20)
                       (list-ref line 13)
                       (list-ref line 12)
                       (account)
                       (list-ref line 14)
                       (list-ref line 6)
                       (list-ref line 8)
                       (list-ref line 11)
                       (list-ref line 30)
                       )))
     lines)))
