#lang racket/base

(require gregor
         math/statistics
         racket/list
         racket/string
         racket/vector
         "db-queries.rkt"
         "structs.rkt"
         "technical-indicators.rkt"
         "pattern/ascending-triangle.rkt"
         "pattern/bull-pullback.rkt"
         "pattern/bear-rally.rkt"
         "pattern/descending-triangle.rkt"
         "pattern/high-base.rkt"
         "pattern/low-base.rkt"
         "pattern/range-rally.rkt"
         "pattern/range-pullback.rkt")

(provide analysis-hash
         price-analysis-list
         run-price-analysis)

(define (vector-first v)
  (vector-ref v 0))

(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))

;; Rating for market/sector/industry
;; Looks at price relative to moving averages
;; Scales from -3 to 3
(define (msi-rating symbol end-date-str)
  (let* ([end-date (iso8601->date end-date-str)]
         [start-date (-months end-date 15)]
         [dohlc (list->vector (get-date-ohlc symbol (date->iso8601 start-date) (date->iso8601 end-date)))]
         [sma-20 (simple-moving-average dohlc 20)]
         [sma-50 (simple-moving-average dohlc 50)]
         [sma-20-distance (* 1/2 (statistics-stddev
                                  (foldl (λ (d s r)
                                           (update-statistics r (/ (dohlc-close d) (dv-value s))))
                                         empty-statistics
                                         (vector->list (vector-drop dohlc (- (vector-length dohlc)
                                                                             (vector-length sma-20))))
                                         (vector->list sma-20))))]
         [sma-50-distance (* 1/2 (statistics-stddev
                                  (foldl (λ (d s r)
                                           (update-statistics r (/ (dohlc-close d) (dv-value s))))
                                         empty-statistics
                                         (vector->list (vector-drop dohlc (- (vector-length dohlc)
                                                                             (vector-length sma-50))))
                                         (vector->list sma-50))))])
    (+ (cond [(> (/ (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-20)))
                 (+ 1 sma-20-distance)) 1]
             [(< (/ (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-20)))
                 (- 1 sma-20-distance)) -1]
             [else 0])
       (cond [(> (/ (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-50)))
                 (+ 1 sma-50-distance)) 1]
             [(< (/ (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-50)))
                 (- 1 sma-50-distance)) -1]
             [else 0])
       (cond [(> (dv-value (vector-ref sma-20 (- (vector-length sma-20) 2)))
                 (dv-value (vector-ref sma-20 (- (vector-length sma-20) 1)))) -1/2]
             [else 1/2])
       (cond [(> (dv-value (vector-ref sma-50 (- (vector-length sma-50) 2)))
                 (dv-value (vector-ref sma-50 (- (vector-length sma-50) 1)))) -1/2]
             [else 1/2]))))

(define (stock-patterns symbol start-date end-date)
  (let* ([dohlc-list (get-date-ohlc symbol start-date end-date)])
    (if (< (length dohlc-list) 60) ""
        (string-join (filter (λ (p) (not (equal? "" p)))
                             (map (λ (p) (cond [(not (empty? (history-test ((second p) dohlc-list)))) (first p)]
                                               [else ""]))
                                  (list (list "AT" ascending-triangle-entry)
                                        (list "BP" bull-pullback-entry)
                                        (list "BR" bear-rally-entry)
                                        (list "DT" descending-triangle-entry)
                                        (list "HB" high-base-entry)
                                        (list "LB" low-base-entry)
                                        (list "RR" range-rally-entry)
                                        (list "RP" range-pullback-entry))))
                     " "))))

(define price-analysis-list (list))

(define analysis-hash (hash))

(define (run-price-analysis market sector start-date end-date)
  (let ([new-price-analysis-list (get-price-analysis market sector start-date end-date)]
        [new-analysis-hash (make-hash)])
    ; this lets us avoid calling (msi-rating) with an empty symbol
    (hash-set! new-analysis-hash "" 0)
    (for-each (λ (pa)
                (cond [(not (hash-has-key? new-analysis-hash (price-analysis-market pa)))
                       (hash-set! new-analysis-hash (price-analysis-market pa) (msi-rating (price-analysis-market pa) end-date))])
                (cond [(not (hash-has-key? new-analysis-hash (price-analysis-sector pa)))
                       (hash-set! new-analysis-hash (price-analysis-sector pa) (msi-rating (price-analysis-sector pa) end-date))])
                (cond [(not (hash-has-key? new-analysis-hash (price-analysis-industry pa)))
                       (hash-set! new-analysis-hash (price-analysis-industry pa) (msi-rating (price-analysis-industry pa) end-date))])
                (hash-set! new-analysis-hash (price-analysis-stock pa) (stock-patterns (price-analysis-stock pa) start-date end-date)))
              new-price-analysis-list)
    (set! price-analysis-list new-price-analysis-list)
    (set! analysis-hash new-analysis-hash)))
