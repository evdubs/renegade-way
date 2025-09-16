#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We need to set up the command line params to later access in our (define dbc ...) call.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "../params.rkt")

  (filename "/var/local/renegade/earnings-vibes.tsv")

  (command-line
   #:program "racket earnings-vibes-analysis-backtest.rkt"
   #:once-each
   [("-e" "--end-date") end-date
                        "End date for simulation. Defaults to today"
                        (sim-end-date (iso8601->date end-date))]
   [("-f" "--filename") file-name
                        "File name to save trades as a TSV"
                        (filename file-name)]
   [("-m" "--markets") markets
                       "Markets to save. Defaults to SPY,MDY,SLY,SPSM"
                       (save-markets markets)]
   [("-n" "--db-name") name
                       "Database name. Defaults to 'local'"
                       (db-name name)]
   [("-p" "--db-pass") password
                       "Database password"
                       (db-pass password)]
   [("-s" "--start-date") start-date
                          "Start date for simulation. Defaults to today"
                          (sim-start-date (iso8601->date start-date))]
   [("-u" "--db-user") user
                       "Database user name. Defaults to 'user'"
                       (db-user user)]))

(require 'cmd
         db
         gregor
         gregor/period
         racket/list
         threading
         "../db-queries.rkt"
         "../option-strategy.rkt"
         "../params.rkt"
         (except-in "../structs.rkt"
                    trade
                    struct:trade
                    trade?)
         "option-pricing.rkt"
         "trade.rkt")

(define dbc (postgresql-connect #:server (db-host) #:user (db-user) #:database (db-name) #:password (db-pass)))

(define trade-risk 2000)

(define trades
  (flatten
   (filter-map
    (λ (i)
      (define date (+days (sim-start-date) i))
      (define rows (get-earnings-vibes-analysis (date->iso8601 date)))
      (filter-map
       (λ (r i)
         (printf "~a ~a\n" (earnings-vibes-analysis-stock r) date)
         (with-handlers ([exn:fail? (λ (e) (displayln e) #f)])
           (define symbol (earnings-vibes-analysis-stock r))
           (define ref-price (query-value dbc "
select
  close
from
  polygon.ohlc
where
  date = (select max(date) from polygon.ohlc where date <= $1::text::date) and
  act_symbol = $2;
"
                                          (date->iso8601 date)
                                          symbol))
           (define horizontal-spread-strategies (~> (get-updated-options symbol
                                                                         (date->iso8601 date)
                                                                         ref-price
                                                                         #:compute-all-greeks #f
                                                                         #:fit-vols #f)
                                                    (suitable-options _ "EC")))
           (define horizontal-spread (hash-ref horizontal-spread-strategies "Call Horizontal Spread"))
           (define horizontal-spread-price (max 0.01 (+ (- (option-mid (first horizontal-spread)))
                                                     (option-mid (second horizontal-spread)))))
           (printf "Stock Price at Entry:\t~a\tSpread Price:\t~a\n"
                   (real->decimal-string ref-price)
                   (real->decimal-string horizontal-spread-price))
           ; can we assume earnings and splits don't overlap?
           (define end-price (query-value dbc "
select
  open
from
  polygon.ohlc
where
  date = (select min(date) from polygon.ohlc where date > $1::text::date) and
  act_symbol = $2;
"
                                          (date->iso8601 date)
                                          symbol))
           (define end-date (query-value dbc "
select
  date::text
from
  polygon.ohlc
where
  date = (select min(date) from polygon.ohlc where date > $1::text::date) and
  act_symbol = $2;
"
                                         (date->iso8601 date)
                                         symbol))
           (printf "End Date ~a Price ~a\n" end-date (real->decimal-string end-price 2))
           (define horizontal-spread-value (max 0.0001 (+ (- (price-option dbc (first horizontal-spread) end-date end-price))
                                                          (price-option dbc (second horizontal-spread) end-date end-price))))
           (printf "Stock Price after Earnings:\t~a\tSpread Value:\t~a\tSpread Return Pct:\t~a\n"
                   (real->decimal-string end-price)
                   (real->decimal-string horizontal-spread-value)
                   (real->decimal-string (* 100 (/ (- horizontal-spread-value horizontal-spread-price) horizontal-spread-price))))
           (trade symbol
                  (remove-duplicates (map (λ (l) (parse-date (option-expiration l) "yy-MM-dd")) horizontal-spread))
                  (remove-duplicates (map (λ (l) (option-strike l)) horizontal-spread))
                  (remove-duplicates (map (λ (l) (option-call-put l)) horizontal-spread))
                  date
                  horizontal-spread-price
                  (truncate (/ trade-risk (* 100 horizontal-spread-price)))
                  (* 65/100 2 (truncate (/ trade-risk (* 100 horizontal-spread-price))))
                  (iso8601->date end-date)
                  horizontal-spread-value
                  (truncate (/ trade-risk (* 100 horizontal-spread-price)))
                  (* 65/100 2 (truncate (/ trade-risk (* 100 horizontal-spread-price))))
                  (* 100 (- horizontal-spread-value horizontal-spread-price) (truncate (/ trade-risk (* 100 horizontal-spread-price))))
                  #f
                  #f
                  #f
                  #f
                  #f
                  (parse-date (regexp-replace* #rx"[AB]" (earnings-vibes-analysis-earnings-date r) "") "yy-MM-dd")
                  (earnings-vibes-analysis-option-spread r)
                  i
                  #f
                  #f
                  #f
                  #f
                  (earnings-vibes-analysis-vol-slope r)
                  (earnings-vibes-analysis-iv-hv r))))
       rows (range (length rows))))
    (range (period-ref (period-between (sim-start-date)
                                       (sim-end-date)
                                       '(days))
                       'days)))))

(call-with-output-file* (filename)
  (λ (out)
    (for-each (λ (t) (displayln (trade->tsv t) out))
              trades))
  #:exists 'replace)
