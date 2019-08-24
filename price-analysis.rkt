#lang racket/base

(require racket/gui
         (only-in srfi/19 
                  add-duration
                  current-date
                  date->string
                  date->time-utc
                  make-time
                  string->date
                  subtract-duration
                  time-duration
                  time-utc->date)
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

(provide show-price-analysis)

(define (add-months d n)
  (time-utc->date (add-duration (date->time-utc d)
                                (make-time time-duration 0 (* 60 60 24 30 n)))))

(define (subtract-months d n)
  (time-utc->date (subtract-duration (date->time-utc d)
                                     (make-time time-duration 0 (* 60 60 24 30 n)))))

(define (vector-first v)
  (vector-ref v 0))

(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))

;; Rating for market/sector/industry
;; Looks at price relative to moving averages
;; Scales from -3 to 3
(define (msi-rating symbol)
  (let* ([dohlc (list->vector (get-date-ohlc symbol (send start-date-field get-value) (send end-date-field get-value)))]
         [sma-20 (simple-moving-average dohlc 20)]
         [sma-50 (simple-moving-average dohlc 50)]
         [close-above-20 (cond [(= (vector-length dohlc) 0) 0]
                               [(> (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-20))) 1]
                               [(< (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-20))) -1]
                               [else 0])]
         [close-above-50 (cond [(= (vector-length dohlc) 0) 0]
                               [(> (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-50))) 1]
                               [(< (dohlc-close (vector-last dohlc)) (dv-value (vector-last sma-50))) -1]
                               [else 0])]
         [20-above-50 (cond [(= (vector-length dohlc) 0) 0]
                            [(> (dv-value (vector-last sma-20)) (dv-value (vector-last sma-50))) 1]
                            [(< (dv-value (vector-last sma-20)) (dv-value (vector-last sma-50))) -1]
                            [else 0])])
    (+ close-above-20 close-above-50 20-above-50)))

(define (stock-patterns symbol)
  (let* ([dohlc-list (get-date-ohlc symbol (send start-date-field get-value) (send end-date-field get-value))])
    (if (< (length dohlc-list) 60) ""
        (string-trim (string-join (map (λ (p) (cond [(not (empty? (history-test ((second p) dohlc-list)))) (first p)]
                                                    [else ""]))
                                       (list (list "AT" ascending-triangle-entry)
                                             (list "BP" bull-pullback-entry)
                                             (list "BR" bear-rally-entry)
                                             (list "DT" descending-triangle-entry)
                                             (list "HB" high-base-entry)
                                             (list "LB" low-base-entry)
                                             (list "RR" range-rally-entry)
                                             (list "RP" range-pullback-entry)))
                                  " ")))))

(define analysis-frame
  (new frame% [label "Price Action Analysis"] [width 900] [height 1000]))

(define analysis-input-pane
  (new horizontal-pane%
       [parent analysis-frame]
       [stretchable-height #f]))

(define market-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Market"]
       [init-value "SPY"]))

(define sector-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Sector"]
       [init-value ""]))

(define start-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Start Date"]
       [init-value (date->string (subtract-months (current-date) 5) "~1")]))

(define end-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "End Date"]
       [init-value (date->string (current-date) "~1")]))

(define filter-input-pane
  (new horizontal-pane%
       [parent analysis-frame]
       [stretchable-height #f]))

(define hide-hold-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Hold"]
       [callback (λ (c e) (update-analysis-box msis-list analysis-hash))]))

(define hide-no-pattern-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide No Pattern"]
       [callback (λ (c e) (update-analysis-box msis-list analysis-hash))]))

(define hide-spread-20-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Large Spread"]
       [callback (λ (c e) (update-analysis-box msis-list analysis-hash))]))

(define msis-list (list))

(define analysis-hash (hash))

(define analyze-button
  (new button%
       [parent analysis-input-pane]
       [label "Analyze"]
       [callback (λ (c e)
                   (send c enable #f)
                   (let ([new-msis-list (get-msis (send market-field get-value) (send sector-field get-value)
                                                  (send start-date-field get-value) (send end-date-field get-value))]
                         [new-analysis-hash (make-hash)])
                     (for-each (λ (msis)
                                 (cond [(not (hash-has-key? new-analysis-hash (msis-market msis)))
                                        (hash-set! new-analysis-hash (msis-market msis) (msi-rating (msis-market msis)))])
                                 (cond [(not (hash-has-key? new-analysis-hash (msis-sector msis)))
                                        (hash-set! new-analysis-hash (msis-sector msis) (msi-rating (msis-sector msis)))])
                                 (cond [(not (hash-has-key? new-analysis-hash (msis-industry msis)))
                                        (hash-set! new-analysis-hash (msis-industry msis) (msi-rating (msis-industry msis)))])
                                 (hash-set! new-analysis-hash (msis-stock msis) (stock-patterns (msis-stock msis))))
                               new-msis-list)
                     (update-analysis-box new-msis-list new-analysis-hash)
                     (set! msis-list new-msis-list)
                     (set! analysis-hash new-analysis-hash))
                   (send c enable #t))]))

(define (update-analysis-box msis-list analysis-hash)
  (let* ([filter-hold (if (send hide-hold-check-box get-value)
                          (filter (λ (m) (not (equal? "Hold" (msis-zacks-rank m)))) msis-list)
                          msis-list)]
         [filter-pattern (if (send hide-no-pattern-check-box get-value)
                             (filter (λ (m) (not (equal? "" (hash-ref analysis-hash (msis-stock m))))) filter-hold)
                             filter-hold)]
         [filter-spread (if (send hide-spread-20-check-box get-value)
                            (filter (λ (m) (and (not (equal? "" (msis-option-spread m)))
                                                (> 20 (string->number (msis-option-spread m))))) filter-pattern)
                            filter-pattern)])
    (send analysis-box set
          (map (λ (m) (msis-market m)) filter-spread)
          (map (λ (m) (number->string (hash-ref analysis-hash (msis-market m)))) filter-spread)
          (map (λ (m) (msis-sector m)) filter-spread)
          (map (λ (m) (real->decimal-string (msis-sector-vs-market m))) filter-spread)
          (map (λ (m) (number->string (hash-ref analysis-hash (msis-sector m)))) filter-spread)
          (map (λ (m) (msis-industry m)) filter-spread)
          (map (λ (m) (number->string (hash-ref analysis-hash (msis-industry m)))) filter-spread)
          (map (λ (m) (msis-stock m)) filter-spread)
          (map (λ (m) (real->decimal-string (msis-stock-vs-sector m))) filter-spread)
          (map (λ (m) (msis-next-div-date m)) filter-spread)
          (map (λ (m) (msis-earnings-date m)) filter-spread)
          (map (λ (m) (msis-option-spread m)) filter-spread)
          (map (λ (m) (msis-zacks-rank m)) filter-spread)
          (map (λ (m) (hash-ref analysis-hash (msis-stock m))) filter-spread))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box set-data i (list m (hash-ref analysis-hash (msis-stock m)))))
         filter-spread (range (length filter-spread)))))

(define analysis-box-columns (list "Market" "MktRtg" "Sector" "Sct/Mkt" "SctRtg" "Industry" "IndRtg"
                                   "Stock" "Stk/Sct" "DivDt" "ErnDt" "OptSprd" "ZckRnk" "Patterns"))

(define analysis-box
  (new list-box%
       [parent analysis-frame]
       [label #f]
       [callback (λ (b e)
                   (let ([market (msis-market (first (send b get-data (first (send b get-selections)))))]
                         [sector (msis-sector (first (send b get-data (first (send b get-selections)))))]
                         [industry (msis-industry (first (send b get-data (first (send b get-selections)))))]
                         [stock (msis-stock (first (send b get-data (first (send b get-selections)))))])
                     (refresh-chart market
                                    sector
                                    industry
                                    stock
                                    (send start-date-field get-value)
                                    (send end-date-field get-value))
                     (refresh-option-strategy stock
                                              (send end-date-field get-value)
                                              (dohlc-close (first (get-date-ohlc stock (send end-date-field get-value)
                                                                                 (send end-date-field get-value))))
                                              (second (send b get-data (first (send b get-selections)))))))]
       [style (list 'single 'column-headers 'vertical-label)]
       [columns analysis-box-columns]
       [choices (list "")]))

(define (show-price-analysis)
  (send analysis-frame show #t)
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols))))
