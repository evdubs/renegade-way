#lang racket/base

(require gregor
         math/statistics
         racket/class
         racket/list
         racket/gui/base
         racket/string
         racket/vector
         "chart.rkt"
         "db-queries.rkt"
         "option-strategy.rkt"
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

(provide price-analysis-box
         price-analysis-filter
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
         [sma-20-distance (* 1/2
                             (statistics-stddev
                              (foldl (λ (d s r)
                                       (update-statistics r (- (dohlc-close d) (dv-value s))))
                                     empty-statistics
                                     (vector->list (vector-drop dohlc (- (vector-length dohlc)
                                                                         (vector-length sma-20))))
                                     (vector->list sma-20))))]
         [sma-50-distance (* 1/2
                             (statistics-stddev
                              (foldl (λ (d s r)
                                       (update-statistics r (- (dohlc-close d) (dv-value s))))
                                     empty-statistics
                                     (vector->list (vector-drop dohlc (- (vector-length dohlc)
                                                                         (vector-length sma-50))))
                                     (vector->list sma-50))))])
    (+ (cond [(> (dohlc-close (vector-last dohlc))
                 (+ (dv-value (vector-last sma-20)) sma-20-distance)) 1]
             [(< (dohlc-close (vector-last dohlc))
                 (- (dv-value (vector-last sma-20)) sma-20-distance)) -1]
             [else 0])
       (cond [(> (dohlc-close (vector-last dohlc))
                 (+ (dv-value (vector-last sma-50)) sma-50-distance)) 1]
             [(< (dohlc-close (vector-last dohlc))
                 (- (dv-value (vector-last sma-50)) sma-50-distance)) -1]
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

(define analysis-box-ref #f)

(define hide-hold (make-parameter #f))

(define hide-no-pattern (make-parameter #f))

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define (price-analysis-filter #:hide-hold hold #:hide-no-pattern no-pattern #:hide-large-spread large-spread #:hide-non-weekly non-weekly)
  (hide-hold hold)
  (hide-no-pattern no-pattern)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (update-analysis-box price-analysis-list analysis-hash))

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
    (set! analysis-hash new-analysis-hash)
    (update-analysis-box price-analysis-list analysis-hash)))

(define (update-analysis-box price-analysis-list analysis-hash)
  (let* ([filter-hold (if (hide-hold)
                          (filter (λ (m) (not (equal? "Hold" (price-analysis-zacks-rank m)))) price-analysis-list)
                          price-analysis-list)]
         [filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (not (equal? "" (hash-ref analysis-hash (price-analysis-stock m))))) filter-hold)
                             filter-hold)]
         [filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (price-analysis-option-spread m)))
                                                (> 20 (string->number (price-analysis-option-spread m))))) filter-pattern)
                            filter-pattern)]
         [filter-weekly (if (hide-non-weekly)
                            (filter (λ (m) (price-analysis-is-weekly m)) filter-spread)
                            filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (price-analysis-market m)) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-market m)))) filter-weekly)
          (map (λ (m) (price-analysis-sector m)) filter-weekly)
          (map (λ (m) (real->decimal-string (price-analysis-sector-vs-market m))) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-sector m)))) filter-weekly)
          (map (λ (m) (price-analysis-industry m)) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-industry m)))) filter-weekly)
          (map (λ (m) (price-analysis-stock m)) filter-weekly)
          (map (λ (m) (real->decimal-string (price-analysis-stock-vs-sector m))) filter-weekly)
          (map (λ (m) (price-analysis-next-div-date m)) filter-weekly)
          (map (λ (m) (price-analysis-earnings-date m)) filter-weekly)
          (map (λ (m) (price-analysis-option-spread m)) filter-weekly)
          (map (λ (m) (price-analysis-zacks-rank m)) filter-weekly)
          (map (λ (m) (hash-ref analysis-hash (price-analysis-stock m))) filter-weekly))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i (list m (hash-ref analysis-hash (price-analysis-stock m)))))
         filter-weekly (range (length filter-weekly)))))

(define analysis-box-columns (list "Market" "MktRtg" "Sector" "Sct/Mkt" "SctRtg" "Industry" "IndRtg"
                                   "Stock" "Stk/Sct" "DivDt" "ErnDt" "OptSprd" "ZckRnk" "Patterns"))

(define (price-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([market (price-analysis-market (first (send b get-data (first (send b get-selections)))))]
                           [sector (price-analysis-sector (first (send b get-data (first (send b get-selections)))))]
                           [industry (price-analysis-industry (first (send b get-data (first (send b get-selections)))))]
                           [stock (price-analysis-stock (first (send b get-data (first (send b get-selections)))))])
                       (refresh-chart market
                                      sector
                                      industry
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                (second (send b get-data (first (send b get-selections)))))))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-analysis-box price-analysis-list analysis-hash))
