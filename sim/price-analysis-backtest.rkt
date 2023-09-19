#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We need to set up the command line params to later access in our (define dbc ...) call.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "../params.rkt")

  (command-line
   #:program "racket price-analysis-backtest.rkt"
   #:once-each
   [("-b" "--bearish") "Run analysis for bearish trades. Defaults (via omission) to bullish"
                       (bearish? #t)]
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
         racket/string
         threading
         "../option-strategy.rkt"
         "../params.rkt"
         "../pattern/ascending-triangle.rkt"
         "../pattern/bear-rally.rkt"
         "../pattern/bull-pullback.rkt"
         "../pattern/descending-triangle.rkt"
         "../pattern/high-base.rkt"
         "../pattern/low-base.rkt"
         (except-in "../structs.rkt"
                    trade
                    struct:trade
                    trade?)
         "option-pricing.rkt"
         "trade.rkt")

(define dbc (postgresql-connect #:server (db-host) #:user (db-user) #:database (db-name) #:password (db-pass)))

(define trade-risk 2000)

(define (exit-price future-prices long? entry-price stop-price target-price)
  (cond [(empty? future-prices) #f])
  (define candle (first future-prices))
  (define date (vector-ref candle 0))
  (define high (vector-ref candle 2))
  (define low (vector-ref candle 3))
  (define close (vector-ref candle 4))
  (cond [(and long? (> stop-price low)) (list date close)]
        [(and long? (> high target-price)) (list date close)]
        [(and (not long?) (> high stop-price)) (list date close)]
        [(and (not long?) (> target-price low)) (list date close)]
        [(= 1 (length future-prices)) (list date close)]
        [else (exit-price (drop future-prices 1) long? entry-price stop-price target-price)]))

(define trades
  (flatten
   (filter-map
    (λ (i)
      (define date (+days (sim-start-date) i))
      (cond [(monday? date)
             (define rows (query-rows dbc (string-append "
select
  date,
  market_act_symbol,
  market_rating,
  sector_act_symbol,
  sector_rating,
  sector_vs_market,
  industry_act_symbol,
  industry_rating,
  stock_act_symbol,
  stock_vs_sector,
  dividend_date::text,
  earnings_date::text,
  option_spread,
  patterns
from
  renegade.price_analysis
where
  date = $1::text::date and
  option_spread <= 30 and
"
                                                         (if (bearish?) "
  (patterns like '%BR%' or patterns like '%LB%' or patterns like '%DT%')
order by
  stock_vs_sector asc;" "
  (patterns like '%BP%' or patterns like '%HB%' or patterns like '%AT%')
order by
  stock_vs_sector desc;"))
                                      (date->iso8601 date)))
             (filter-map
              (λ (r i)
                (with-handlers ([exn:fail? (λ (e) (displayln e) #f)])
                  (define symbol (vector-ref r 8))
                  (define earnings-date (if (equal? sql-null (vector-ref r 11)) #f (iso8601->date (vector-ref r 11))))
                  (define end-date (if earnings-date (-days earnings-date 1) (+months date 1)))
                  (define patterns (vector-ref r 13))
                  (cond [(if (bearish?)
                             (or (string-contains? patterns "BR")
                                 (string-contains? patterns "LB")
                                 (string-contains? patterns "DT"))
                             (or (string-contains? patterns "BP")
                                 (string-contains? patterns "HB")
                                 (string-contains? patterns "AT")))
                         (define future-prices (query-rows dbc "
select
  date::text,
  open,
  high,
  low,
  close
from
  iex.split_adjusted_chart($1, $2::text::date, $3::text::date, true);
"
                                                           symbol
                                                           (date->iso8601 date)
                                                           (date->iso8601 end-date)))
                         (define past-prices (map (λ (r) (dohlc (iso8601->date (vector-ref r 0)) (vector-ref r 1)
                                                                (vector-ref r 2) (vector-ref r 3) (vector-ref r 4)))
                                                  (query-rows dbc "
select
  date::text,
  open,
  high,
  low,
  close
from
  iex.split_adjusted_chart($1, $2::text::date, $3::text::date, true);
"
                                                              symbol
                                                              (date->iso8601 (-months date 5))
                                                              (date->iso8601 (-days date 1)))))
                         (define entry (cond [(string-contains? (vector-ref r 13) "BP")
                                              (bull-pullback-entry past-prices)]
                                             [(string-contains? (vector-ref r 13) "HB")
                                              (high-base-entry past-prices)]
                                             [(string-contains? (vector-ref r 13) "AT")
                                              (ascending-triangle-entry past-prices)]
                                             [(string-contains? (vector-ref r 13) "BR")
                                              (bear-rally-entry past-prices)]
                                             [(string-contains? (vector-ref r 13) "LB")
                                              (low-base-entry past-prices)]
                                             [(string-contains? (vector-ref r 13) "DT")
                                              (descending-triangle-entry past-prices)]))
                         (define test (last (history-test entry)))
                         (define entry-price (test-entry (dv-value test)))
                         (define will-enter? (if (bearish?)
                                                 (and (> entry-price (apply min (map (λ (p) (vector-ref p 3))
                                                                                     (take future-prices 5))))
                                                      (date>=? (+days (dv-date test) 4) date))
                                                 (and (< entry-price (apply max (map (λ (p) (vector-ref p 2))
                                                                                     (take future-prices 5))))
                                                      (date>=? (+days (dv-date test) 4) date))))
                         (define stop-price (test-stop (dv-value test)))
                         (define target-price (test-target (dv-value test)))
                         (printf "~a test-date ~a entry ~a will-enter? ~a stop ~a target ~a earnings-date ~a\n"
                                 symbol
                                 (dv-date test)
                                 (real->decimal-string entry-price)
                                 will-enter?
                                 (real->decimal-string stop-price)
                                 (real->decimal-string target-price)
                                 earnings-date)
                         (define exit-date-stock-price (exit-price future-prices (not (bearish?)) entry-price stop-price target-price))
                         (cond [will-enter? (printf "~a exit-date: ~a stock price: ~a\n" symbol (first exit-date-stock-price)
                                                    (real->decimal-string (second exit-date-stock-price)))
                                            (define option (~> (get-updated-options symbol
                                                                                    (date->iso8601 date)
                                                                                    entry-price
                                                                                    #:compute-all-greeks #f)
                                                               (suitable-options _ patterns)
                                                               (hash-ref _ (if (bearish?) "Long Put" "Long Call"))
                                                               (first _)))
                                            (define option-closing-price (price-option dbc option (first exit-date-stock-price)
                                                                                       (second exit-date-stock-price)))
                                            (trade symbol
                                                   (list (parse-date (option-expiration option) "yy-MM-dd"))
                                                   (list (option-strike option))
                                                   (list (option-call-put option))
                                                   date
                                                   (option-mid option)
                                                   (truncate (/ trade-risk (* 100 (option-mid option))))
                                                   (* 65/100 (truncate (/ trade-risk (* 100 (option-mid option)))))
                                                   (iso8601->date (first exit-date-stock-price))
                                                   option-closing-price
                                                   (truncate (/ trade-risk (* 100 (option-mid option))))
                                                   (* 65/100 (truncate (/ trade-risk (* 100 (option-mid option)))))
                                                   (* 100 (- option-closing-price (option-mid option))
                                                      (truncate (/ trade-risk (* 100 (option-mid option)))))
                                                   (vector-ref r 2)
                                                   (vector-ref r 4)
                                                   (if (equal? sql-null (vector-ref r 7)) #f (vector-ref r 7))
                                                   (vector-ref r 9)
                                                   #f
                                                   earnings-date
                                                   (vector-ref r 12)
                                                   i
                                                   (vector-ref r 13)
                                                   entry-price
                                                   stop-price
                                                   target-price)]
                               [else #f])]
                        [else #f])))
              rows (range (length rows)))]
            [else #f]))
    (range (period-ref (period-between (sim-start-date)
                                       (sim-end-date)
                                       '(days))
                       'days)))))

(call-with-output-file* (filename)
  (λ (out)
    (for-each (λ (t) (displayln (trade->tsv t) out))
              trades))
  #:exists 'replace)
