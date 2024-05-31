#lang racket/base

(require gregor
         gregor/period
         racket/cmdline
         racket/list
         racket/port
         racket/string
         threading
         "list-partition.rkt"
         "trade.rkt")

(define filename (make-parameter ""))

(command-line
 #:program "racket stats.rkt"
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

(define first-trade-date (argmin ->jdn (map (λ (t) (trade-open-date t)) trades)))

(define last-trade-date (argmax ->jdn (map (λ (t) (trade-close-date t)) trades)))

(define quarters-between (~> (date-period-between (-months first-trade-date 2) (+months last-trade-date 3) '(months))
                             (period-ref _ 'months)
                             (range _)
                             (map (λ (i) (+months (date (->year (-months first-trade-date 2)) (->month (-months first-trade-date 2))) i)) _)
                             (filter (λ (d) (= 1 (modulo (->month d) 3))) _)))

(define years-between (~> (date-period-between (-months first-trade-date 11) (+months last-trade-date 12) '(months))
                          (period-ref _ 'months)
                          (range _)
                          (map (λ (i) (+months (date (->year (-months first-trade-date 11)) (->month (-months first-trade-date 11))) i)) _)
                          (filter (λ (d) (= 1 (modulo (->month d) 12))) _)))

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
                                    (if (trade-market-rating t) (= 1 (trade-market-rating t)) #f)
                                    ;(if (trade-sector-rating t) (<= 50 (trade-sector-rating t)) #f)
                                    ;(trade-earnings-date t)
                                    (not (trade-earnings-date t))
                                    (>= 5 (trade-num t))
                                    ;(>= (trade-open-price t) 3)
                                    ;(<= 28 (period-ref (period-between (trade-open-date t) (first (trade-expirations t)) '(days)) 'days))
                                    ;(>= 21 (period-ref (period-between (trade-open-date t) (if (trade-earnings-date t)
                                    ;                                                           (trade-earnings-date t)
                                    ;                                                           (first (trade-expirations t))) '(days)) 'days))
                                    ;(<= 65 (trade-stock-rating t))
                                    ;(> 85 (trade-stock-rating t))
                                    ;(<= 65 (trade-stock-risk-reward t))
                                    ;(or (> 15 (->day (first (trade-expirations t))))
                                    ;    (< 21 (->day (first (trade-expirations t)))))
                                    ;(<= 15 (->day (first (trade-expirations t))))
                                    ;(>= 21 (->day (first (trade-expirations t))))
                                    ;(or (equal? "BULL PULLBACK" (trade-patterns t))
                                    ;    (equal? "HIGH BASE" (trade-patterns t))
                                    ;    (equal? "ASCENDING TRIANGLE" (trade-patterns t)))
                                    ;(or (equal? "BEAR RALLY" (trade-patterns t))
                                    ;    (equal? "LOW BASE" (trade-patterns t))
                                    ;    (equal? "DESCENDING TRIANGLE" (trade-patterns t)))
                                    ;(equal? "INCREASING VOL" (trade-patterns t))
                                    ;(equal? "DECREASING VOL" (trade-patterns t))
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

(for-each (λ (dr) (displayln-stats (string-append "Q " (date->iso8601 (first dr)))
                                   (compute-stats-range (first dr) (second dr))))
          (list-partition quarters-between 2 1))

(for-each (λ (dr) (displayln-stats (string-append "Y " (date->iso8601 (first dr)))
                                   (compute-stats-range (first dr) (second dr))))
          (list-partition years-between 2 1))
