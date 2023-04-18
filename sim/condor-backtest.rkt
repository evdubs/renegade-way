#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We need to set up the command line params to later access in our (define dbc ...) call.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "../params.rkt")

  (command-line
   #:program "racket condor-backtest.rkt"
   #:once-each
   [("-e" "--end-date") end-date
                        "End date for simulation. Defaults to today"
                        (sim-end-date (iso8601->date end-date))]
   [("-f" "--filename") file-name
                        "File name to save trades as a TSV"
                        (filename file-name)]
   [("-m" "--markets") markets
                       "Markets to save. Defaults to SPY,MDY,SLY"
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
      (cond [(monday? date)
             (define rows (query-rows dbc "
select
  date,
  market_act_symbol,
  market_rating,
  market_risk_reward,
  sector_act_symbol,
  sector_rating,
  sector_risk_reward,
  industry_act_symbol,
  industry_rating,
  industry_risk_reward,
  stock_act_symbol,
  stock_rating,
  stock_risk_reward,
  earnings_date::text,
  option_spread
from
  renegade.condor_analysis
where
  date = $1::text::date and
  stock_rating >= 50 and
  stock_risk_reward >= 50 and
  option_spread <= 30
order by
  coalesce(stock_rating, 0) * least(coalesce(stock_risk_reward, 0), 85.0) desc;
"
                                      (date->iso8601 date)))
             (filter-map
              (λ (r i)
                (printf "~a ~a\n" (vector-ref r 10) date)
                (with-handlers ([exn:fail? (λ (e) (displayln e) #f)])
                  (define symbol (vector-ref r 10))
                  (define ref-price (query-value dbc "
select
  close
from
  iex.chart
where
  date = (select max(date) from iex.chart where date < $1::text::date) and
  act_symbol = $2;
"
                                                 (date->iso8601 date)
                                                 symbol))
                  (define decreasing-vol-strategies (~> (get-updated-options symbol
                                                                             (date->iso8601 date)
                                                                             ref-price
                                                                             #:compute-all-greeks #f)
                                                        (suitable-options _ "DV")))
                  (define call-condor (hash-ref decreasing-vol-strategies "Call Condor"))
                  (define put-condor (hash-ref decreasing-vol-strategies "Put Condor"))
                  (define call-condor-price (- (+ (option-mid (first call-condor))
                                                  (option-mid (fourth call-condor)))
                                               (option-mid (second call-condor))
                                               (option-mid (third call-condor))))
                  (define put-condor-price (- (+ (option-mid (first put-condor))
                                                 (option-mid (fourth put-condor)))
                                              (option-mid (second put-condor))
                                              (option-mid (third put-condor))))
                  (define-values (condor condor-price) (if (>= call-condor-price put-condor-price)
                                                           (values call-condor call-condor-price)
                                                           (values put-condor put-condor-price)))
                  (printf "Strikes: ~a ~a ~a ~a\n"
                          (real->decimal-string (option-strike (first condor)))
                          (real->decimal-string (option-strike (second condor)))
                          (real->decimal-string (option-strike (third condor)))
                          (real->decimal-string (option-strike (fourth condor))))
                  (define condor-risk (max condor-price
                                           (- (+ condor-price
                                                 (option-strike (first condor))
                                                 (option-strike (fourth condor)))
                                              (option-strike (second condor))
                                              (option-strike (third condor)))))
                  (printf "Stock Price at Entry:\t~a\tCondor Price:\t~a\tCondor Risk:\t\t~a\n"
                          (real->decimal-string ref-price)
                          (real->decimal-string condor-price)
                          (real->decimal-string condor-risk))
                  (define future-prices (query-rows dbc "
select
  date::text,
  close
from
  iex.split_adjusted_chart($1, $2::text::date, to_date($3, 'YY-MM-DD'), true);
"
                                                    symbol
                                                    (date->iso8601 date)
                                                    (option-expiration (first condor))))
                  (define low-strike (min (option-strike (first condor))
                                          (option-strike (second condor))
                                          (option-strike (third condor))
                                          (option-strike (fourth condor))))
                  (define high-strike (max (option-strike (first condor))
                                           (option-strike (second condor))
                                           (option-strike (third condor))
                                           (option-strike (fourth condor))))
                  (define exit-date-price (foldl (λ (fp date-price)
                                                   (cond [(first date-price) date-price]
                                                         [else (cond [(or (> (vector-ref fp 1) high-strike)
                                                                          (< (vector-ref fp 1) low-strike))
                                                                      (list (vector-ref fp 0) (vector-ref fp 1))]
                                                                     [else date-price])]))
                                                 (list #f #f)
                                                 future-prices))
                  (printf "Exit Date ~a Price ~a\n" (first exit-date-price)
                          (if (second exit-date-price) (real->decimal-string (second exit-date-price))
                              (second exit-date-price)))
                  (define exp-price (query-value dbc "
select
  close
from
  iex.split_adjusted_chart($1, $2::text::date, to_date($3, 'YY-MM-DD'), true)
where
  date = to_date($3, 'YY-MM-DD');
"
                                                 symbol
                                                 (date->iso8601 date)
                                                 (option-expiration (first condor))))
                  (define end-date (if (first exit-date-price) (first exit-date-price)
                                       (date->iso8601 (parse-date (option-expiration (first condor)) "yy-MM-dd"))))
                  (define end-price (if (second exit-date-price) (second exit-date-price)
                                        exp-price))
                  (define condor-value (+ (price-option dbc (first condor) end-date end-price)
                                          (- (price-option dbc (second condor) end-date end-price))
                                          (- (price-option dbc (third condor) end-date end-price))
                                          (price-option dbc (fourth condor) end-date end-price)))
                  (printf "Stock Price at Exp:\t~a\tCondor Value:\t~a\tCondor Return Pct:\t~a\n"
                          (real->decimal-string exp-price)
                          (real->decimal-string condor-value)
                          (real->decimal-string (* 100 (/ (- condor-value condor-risk) condor-risk))))
                  (trade symbol
                         (remove-duplicates (map (λ (l) (parse-date (option-expiration l) "yy-MM-dd")) condor))
                         (remove-duplicates (map (λ (l) (option-strike l)) condor))
                         (remove-duplicates (map (λ (l) (option-call-put l)) condor))
                         date
                         condor-price
                         (truncate (/ trade-risk (* 100 condor-risk)))
                         (* 65/100 4 (truncate (/ trade-risk (* 100 condor-risk))))
                         (iso8601->date end-date)
                         condor-value
                         (truncate (/ trade-risk (* 100 condor-risk)))
                         (* 65/100 (truncate (/ trade-risk (* 100 condor-risk))))
                         (* 100 (- condor-value condor-price) (truncate (/ trade-risk (* 100 condor-risk))))
                         #f
                         #f
                         #f
                         (vector-ref r 11)
                         (vector-ref r 12)
                         (if (sql-null? (vector-ref r 13)) #f (iso8601->date (vector-ref r 13)))
                         (vector-ref r 14)
                         i
                         #f
                         #f
                         #f
                         #f)))
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
