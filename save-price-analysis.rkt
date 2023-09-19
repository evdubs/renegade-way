#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We want to first set up the command line args before initializing stuff in db-queries.rkt.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "params.rkt")

  (command-line
   #:program "racket save-price-analysis.rkt"
   #:once-each
   [("-d" "--end-date") end-date
                        "End date for saving. Defaults to today"
                        (save-end-date (iso8601->date end-date))]
   [("-m" "--markets") markets
                       "Markets to save. Defaults to SPY,MDY,SLY,SPSM"
                       (save-markets markets)]
   [("-n" "--db-name") name
                       "Database name. Defaults to 'local'"
                       (db-name name)]
   [("-p" "--db-pass") password
                       "Database password"
                       (db-pass password)]
   [("-u" "--db-user") user
                       "Database user name. Defaults to 'user'"
                       (db-user user)]))

(require 'cmd
         gregor
         "db-queries.rkt"
         "params.rkt"
         "price-analysis.rkt"
         "structs.rkt")

(cond [(and (or (= 0 (->wday (save-end-date)))
                (= 6 (->wday (save-end-date)))))
       (displayln (string-append "Requested date " (date->iso8601 (save-end-date)) " falls on a weekend. Terminating."))
       (exit)])

(run-price-analysis (save-markets) "" (date->iso8601 (-months (save-end-date) 5)) (date->iso8601 (save-end-date)))

(for-each (λ (msis)
            (with-handlers
              ([exn:fail? (λ (e) (displayln (string-append "Failed to process " (price-analysis-stock msis) " for date "
                                                           (date->iso8601 (save-end-date))))
                             (displayln e))])
              (insert-price-analysis (date->iso8601 (save-end-date))
                                     msis
                                     (hash-ref price-analysis-hash (price-analysis-market msis))
                                     (hash-ref price-analysis-hash (price-analysis-sector msis))
                                     (hash-ref price-analysis-hash (price-analysis-industry msis))
                                     (hash-ref price-analysis-hash (price-analysis-stock msis)))))
          price-analysis-list)
