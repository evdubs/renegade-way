#lang racket/base

(require gregor
         racket/cmdline
         racket/list
         racket/port
         racket/string
         threading
         "trade.rkt")

(define filename (make-parameter ""))

(command-line
   #:program "racket condor-backtest.rkt"
   #:once-each
   [("-f" "--filename") file-name
                        "TSV file to load trades from"
                        (filename file-name)])

(struct stats
  (num-profit
   total-profit
   avg-profit
   num-loss
   total-loss
   avg-loss
   num-trades
   total-pl
   avg-pl
   total-commission
   2pc-spread)
  #:transparent)

(define trades (~> (open-input-file (filename))
                   (port->string _)
                   (string-split _ "\n")
                   (map (λ (t) (tsv->trade t)) _)))

(define (compute-stats trades predicate)
  (define num-profit 0)
  (define total-profit 0)
  (define num-loss 0)
  (define total-loss 0)
  (define num-trades 0)
  (define total-pl 0)
  (define total-commission 0)
  (define 2pc-spread 0)

  (for-each (λ (t)
              (cond [(predicate t)
                     (set! num-trades (add1 num-trades))
                     (set! total-pl (+ total-pl (trade-profit-loss t)))
                     (set! total-commission (+ total-commission (trade-open-commission t) (trade-close-commission t)))
                     (set! 2pc-spread (+ 2pc-spread (* 2 (trade-open-price t) (trade-open-contracts t))))
                     (cond [(< 0 (trade-profit-loss t))
                            (set! num-profit (add1 num-profit))
                            (set! total-profit (+ total-profit (trade-profit-loss t)))]
                           [(>= 0 (trade-profit-loss t))
                            (set! num-loss (add1 num-loss))
                            (set! total-loss (+ total-loss (trade-profit-loss t)))])]))
            trades)
  (stats num-profit
         total-profit
         (if (= 0 num-profit) 0 (/ total-profit num-profit))
         num-loss
         total-loss
         (if (= 0 num-loss) 0 (/ total-loss num-loss))
         num-trades
         total-pl
         (if (= 0 num-trades) 0 (/ total-pl num-trades))
         total-commission
         2pc-spread))

(define (compute-stats-range start-date end-date)
  (compute-stats trades (λ (t) (and (date>=? (trade-open-date t) start-date)
                                    (date<? (trade-open-date t) end-date)
                                    (>= 15 (trade-num t))
                                    ))))

(displayln "PL figures shown before considering commissions and spread")
(define (displayln-stats header stats)
  (displayln header)
  (displayln "Num Profit\tTotal Profit\tAverage Profit\tNum Loss\tTotal Loss\tAverage Loss\tNum Trades\tTotal PL\tAverage PL\tEstimated Commission\t2% Spread")
  (displayln (string-append (number->string (stats-num-profit stats)) "\t"
                            (real->decimal-string (stats-total-profit stats)) "\t"
                            (real->decimal-string (stats-avg-profit stats)) "\t"
                            (number->string (stats-num-loss stats)) "\t"
                            (real->decimal-string (stats-total-loss stats)) "\t"
                            (real->decimal-string (stats-avg-loss stats)) "\t"
                            (number->string (stats-num-trades stats)) "\t"
                            (real->decimal-string (stats-total-pl stats)) "\t"
                            (real->decimal-string (stats-avg-pl stats)) "\t"
                            (real->decimal-string (stats-total-commission stats)) "\t"
                            (real->decimal-string (stats-2pc-spread stats)))))

(displayln-stats "2020 Q1" (compute-stats-range (date 2020 1 1) (date 2020 4 1)))
(displayln-stats "2020 Q2" (compute-stats-range (date 2020 4 1) (date 2020 7 1)))
(displayln-stats "2020 Q3" (compute-stats-range (date 2020 7 1) (date 2020 10 1)))
(displayln-stats "2020 Q4" (compute-stats-range (date 2020 10 1) (date 2021 1 1)))
(displayln-stats "2020 Y" (compute-stats-range (date 2020 1 1) (date 2021 1 1)))
(displayln-stats "2021 Q1" (compute-stats-range (date 2021 1 1) (date 2021 4 1)))
(displayln-stats "2021 Q2" (compute-stats-range (date 2021 4 1) (date 2021 7 1)))
(displayln-stats "2021 Q3" (compute-stats-range (date 2021 7 1) (date 2021 10 1)))
(displayln-stats "2021 Q4" (compute-stats-range (date 2021 10 1) (date 2022 1 1)))
(displayln-stats "2021 Y" (compute-stats-range (date 2021 1 1) (date 2022 1 1)))
(displayln-stats "2022 Q1" (compute-stats-range (date 2022 1 1) (date 2022 4 1)))
(displayln-stats "2022 Q2" (compute-stats-range (date 2022 4 1) (date 2022 7 1)))
(displayln-stats "2022 Q3" (compute-stats-range (date 2022 7 1) (date 2022 10 1)))
(displayln-stats "2022 Q4" (compute-stats-range (date 2022 10 1) (date 2023 1 1)))
(displayln-stats "2022 Y" (compute-stats-range (date 2022 1 1) (date 2023 1 1)))
