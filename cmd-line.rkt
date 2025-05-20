#lang racket/base

(require racket/cmdline
         "params.rkt")

(command-line
 #:program "racket main.rkt"
 #:once-each
 [("-a" "--ibkr-host") host
                       "Interactive Brokers TWS host address. Defaults to '127.0.0.1'"
                       (ibkr-hostname host)]
 [("-b" "--db-host") host
                     "Database host address. Defaults to '127.0.0.1'"
                     (db-host host)]
 [("-e" "--finviz-user") user
                         "FinViz username"
                         (finviz-user user)]
 [("-f" "--finviz-pass") pass
                         "FinViz password"
                         (finviz-pass pass)]
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
