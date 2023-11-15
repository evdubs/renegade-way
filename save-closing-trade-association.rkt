#lang racket/base

(require db
         gregor
         racket/cmdline
         threading)

(define db-host (make-parameter "127.0.0.1"))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(command-line
 #:program "racket save-closing-trade-association.rkt"
 #:once-each
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])

(define dbc (postgresql-connect #:server (db-host) #:user (db-user) #:database (db-name) #:password (db-pass)))

(define executions (query-rows dbc "
select
  c.symbol,
  coalesce((c.expiry + '1 day'::interval)::date, '9999-12-31'::date)::text,
  c.strike,
  coalesce(c.right::text, ''),
  e.order_id,
  e.contract_id,
  e.execution_id,
  e.timestamp::text,
  e.account,
  e.side::text,
  e.shares,
  e.price,
  c.underlying_contract_id
from
  ibkr.execution e
join
  ibkr.contract c
on
  e.contract_id = c.contract_id
order by
  timestamp,
  execution_id;
"))

(struct position
  (account
   contract-id
   underlying-contract-id
   order-id
   expiry
   num)
  #:transparent)

(define (has-position? positions account contract-id)
  (> (length (filter (λ (p) (and (equal? account (position-account p))
                                 (or (equal? contract-id (position-contract-id p))
                                     (equal? contract-id (position-underlying-contract-id p)))))
                     positions))
     0))

(define (associate-order-id order-id execution-id)
  (query-exec dbc "
insert into ibkr.closing_trade_association (
  opening_order_id,
  execution_id
) values (
  $1,
  $2
) on conflict (execution_id) do nothing;"
              order-id
              execution-id))

(define positions
  (foldl
   (λ (e ps)
     (define ref-moment (parse-moment (vector-ref e 7) "yyyy-MM-dd HH:mm:ssX"))
     (define updated-positions
       (if (has-position? ps (vector-ref e 8) (vector-ref e 5))
           (map (λ (p) (cond [(and (equal? (position-account p) (vector-ref e 8))
                                   (equal? (position-contract-id p) (vector-ref e 5)))
                              (cond [(>= 0 (vector-ref e 4))
                                     (associate-order-id (position-order-id p) (vector-ref e 6))])
                              (struct-copy position p [num (if (equal? "BOT" (vector-ref e 9))
                                                               (+ (position-num p) (vector-ref e 10))
                                                               (- (position-num p) (vector-ref e 10)))])]
                             [(and (equal? (position-account p) (vector-ref e 8))
                                   (equal? (position-underlying-contract-id p) (vector-ref e 5)))
                              (cond [(>= 0 (vector-ref e 4))
                                     (associate-order-id (position-order-id p) (vector-ref e 6))])
                              p]
                             [else p])) ps)
           (cond [(> (vector-ref e 4) 0)
                  (append ps (list (position (vector-ref e 8) (vector-ref e 5) (vector-ref e 12) (vector-ref e 4)
                                             (parse-moment (string-append (vector-ref e 1) " 23:59:00-04")
                                                           "yyyy-MM-dd HH:mm:ssX")
                                             (if (equal? "BOT" (vector-ref e 9))
                                                 (+ (vector-ref e 10))
                                                 (- (vector-ref e 10))))))]
                 [else ps])))
     ; (define nonzero-positions (filter (λ (p) (not (= 0 (position-num p)))) updated-positions))
     (filter (λ (p) (moment>? (position-expiry p) ref-moment)) updated-positions))
   (list)
   executions))

(displayln positions)
