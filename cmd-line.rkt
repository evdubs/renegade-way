#lang racket/base

(require racket/cmdline)

(provide db-user
         db-name
         db-pass
         ibkr-hostname
         ibkr-port-no)

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define ibkr-hostname (make-parameter "127.0.0.1"))

(define ibkr-port-no (make-parameter 7497))

(command-line
 #:program "racket main.rkt"
 #:once-each
 [("-a" "--ibkr-host") host
                       "Interactive Brokers TWS host address. Defaults to '127.0.0.1'"
                       (ibkr-hostname host)]
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-t" "--ibkr-port") port
                       "Interactive Brokers TWS host port. Defaults to 7497"
                       (ibkr-port-no (string->number port))]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])
