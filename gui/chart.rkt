#lang racket/base

(require gregor
         math/matrix
         pict
         plot
         racket/class
         racket/list
         racket/gui/base
         "../db-queries.rkt"
         "../structs.rkt"
         "../technical-indicators.rkt"
         "plot-util.rkt")

(provide show-chart
         refresh-chart)

(define (refresh-chart market sector industry stock start-date end-date)
  (send chart-market-field set-value market)
  (send chart-sector-field set-value sector)
  (send chart-industry-field set-value industry)
  (send chart-stock-field set-value stock)
  (send chart-start-date-field set-value start-date)
  (send chart-end-date-field set-value end-date)
  (cond [(equal? "Price" (send chart-type-choice get-string-selection))
         (send chart-market-canvas set-snip
               (chart-price-plot chart-market-field chart-market-canvas))
         (send chart-sector-canvas set-snip
               (chart-price-plot chart-sector-field chart-sector-canvas))
         (send chart-industry-canvas set-snip
               (chart-price-plot chart-industry-field chart-industry-canvas))
         (send chart-stock-canvas set-snip
               (chart-price-plot chart-stock-field chart-stock-canvas))]
        [(equal? "Vol History" (send chart-type-choice get-string-selection))
         (send chart-market-canvas set-snip
               (chart-vol-history-plot chart-market-field chart-market-canvas))
         (send chart-sector-canvas set-snip
               (chart-vol-history-plot chart-sector-field chart-sector-canvas))
         (send chart-industry-canvas set-snip
               (chart-vol-history-plot chart-industry-field chart-industry-canvas))
         (send chart-stock-canvas set-snip
               (chart-vol-history-plot chart-stock-field chart-stock-canvas))]
        [(equal? "Vol Surface" (send chart-type-choice get-string-selection))
         (send chart-market-canvas set-snip
               (chart-vol-surface-plot chart-market-field chart-market-canvas))
         (send chart-sector-canvas set-snip
               (chart-vol-surface-plot chart-sector-field chart-sector-canvas))
         (send chart-industry-canvas set-snip
               (chart-vol-surface-plot chart-industry-field chart-industry-canvas))
         (send chart-stock-canvas set-snip
               (chart-vol-surface-plot chart-stock-field chart-stock-canvas))]))

(plot-y-tick-labels? #f)
(plot-y-far-tick-labels? #t)

(define chart-frame (new frame% [label "Market/Sector/Industry/Stock Chart"] [width 1500] [height 1000]))

(define chart-input-pane (new horizontal-pane%
                              [parent chart-frame]
                              [stretchable-height #f]))

(define chart-market-field (new text-field%
                                [parent chart-input-pane]
                                [label "Market"]
                                [init-value "SPY"]))

(define chart-sector-field (new text-field%
                                [parent chart-input-pane]
                                [label "Sector"]
                                [init-value "XLI"]))

(define chart-industry-field (new text-field%
                                  [parent chart-input-pane]
                                  [label "Industry"]
                                  [init-value "XAR"]))

(define chart-stock-field (new text-field%
                               [parent chart-input-pane]
                               [label "Stock"]
                               [init-value "BA"]))

(define chart-start-date-field (new text-field%
                                    [parent chart-input-pane]
                                    [label "Start Date"]
                                    [init-value (date->iso8601 (-months (today) 5))]))

(define chart-end-date-field (new text-field%
                                  [parent chart-input-pane]
                                  [label "End Date"]
                                  [init-value (date->iso8601 (today))]))

(define chart-type-choice (new choice%
                               [parent chart-input-pane]
                               [label "Type "]
                               [choices (list "Price" "Vol History" "Vol Surface")]))

(define chart-refresh-button (new button%
                                  [parent chart-input-pane]
                                  [label "Refresh"]
                                  [callback (λ (b e)
                                              (cond [(equal? "Price" (send chart-type-choice get-string-selection))
                                                     (send chart-market-canvas set-snip
                                                           (chart-price-plot chart-market-field chart-market-canvas))
                                                     (send chart-sector-canvas set-snip
                                                           (chart-price-plot chart-sector-field chart-sector-canvas))
                                                     (send chart-industry-canvas set-snip
                                                           (chart-price-plot chart-industry-field chart-industry-canvas))
                                                     (send chart-stock-canvas set-snip
                                                           (chart-price-plot chart-stock-field chart-stock-canvas))]
                                                    [(equal? "Vol History" (send chart-type-choice get-string-selection))
                                                     (send chart-market-canvas set-snip
                                                           (chart-vol-history-plot chart-market-field chart-market-canvas))
                                                     (send chart-sector-canvas set-snip
                                                           (chart-vol-history-plot chart-sector-field chart-sector-canvas))
                                                     (send chart-industry-canvas set-snip
                                                           (chart-vol-history-plot chart-industry-field chart-industry-canvas))
                                                     (send chart-stock-canvas set-snip
                                                           (chart-vol-history-plot chart-stock-field chart-stock-canvas))]
                                                    [(equal? "Vol Surface" (send chart-type-choice get-string-selection))
                                                     (send chart-market-canvas set-snip
                                                           (chart-vol-surface-plot chart-market-field chart-market-canvas))
                                                     (send chart-sector-canvas set-snip
                                                           (chart-vol-surface-plot chart-sector-field chart-sector-canvas))
                                                     (send chart-industry-canvas set-snip
                                                           (chart-vol-surface-plot chart-industry-field chart-industry-canvas))
                                                     (send chart-stock-canvas set-snip
                                                           (chart-vol-surface-plot chart-stock-field chart-stock-canvas))]))]))

(define chart-plot-pane (new vertical-pane%
                             [parent chart-frame]))

(define prev-time-stamp (current-milliseconds))

; taken from alex-hhh/data-frame ... least-squares-fit.rkt
(define (polynomial-fit-coefficients xs ys nitems degree)
  (define y-matrix (list->matrix nitems 1 ys))
  (define x-matrix (vandermonde-matrix xs (add1 degree)))
  (define x-matrix-transposed (matrix-transpose x-matrix))
  (define x (matrix* x-matrix-transposed x-matrix))
  (define y (matrix* x-matrix-transposed y-matrix))
  (matrix->list (matrix-solve x y)))

(define (chart-vol-surface-plot symbol-field canvas)
  (define kvs (get-vol-surface (send symbol-field get-value)
                               (send chart-end-date-field get-value)))
  (if (or (equal? (send symbol-field get-value) "")
          (= 0 (length kvs)))
      (plot-snip (lines (list #(0 0) #(1 0)))
                 #:title ""
                 #:x-label "Strike"
                 #:y-label "Vol"
                 #:width (- (send canvas get-width) 12)
                 #:height (- (send canvas get-height) 12))
      (let* ([grouped-kvs (group-by (λ (kv) (list (vector-ref kv 0) (vector-ref kv 1)))
                                    kvs)]
             [fit-kvs (map (λ (kvs) (list (string-append (vector-ref (first kvs) 0) " Fit")
                                          (polynomial-fit-coefficients (map (λ (kv) (vector-ref kv 2)) kvs)
                                                                       (map (λ (kv) (vector-ref kv 3)) kvs)
                                                                       (length kvs)
                                                                       3)))
                           (group-by (λ (kv) (list (vector-ref kv 0)))
                                     kvs))])
        (parameterize ([plot-width (- (send canvas get-width) 12)]
                       [plot-height (- (send canvas get-height) 12)])
          (plot-snip (append (list (tick-grid))
                             (map (λ (kvs i) (lines (map (λ (kv) (vector (vector-ref kv 2) (vector-ref kv 3))) kvs)
                                                    #:label (string-append (vector-ref (first kvs) 0) " " (vector-ref (first kvs) 1))
                                                    #:style 'long-dash
                                                    #:color (+ 1 i)))
                                  grouped-kvs
                                  (range 0 (length grouped-kvs)))
                             (map (λ (fits i) (function (λ (x) (+ (first (second fits)) (* x (second (second fits)))
                                                                  (* x x (third (second fits))) (* x x x (fourth (second fits)))))
                                                        #:label (first fits)
                                                        #:color (+ 1 i)))
                                  fit-kvs
                                  (range (length grouped-kvs) (+ (length grouped-kvs) (length fit-kvs)))))
                     #:title (string-append (get-security-name (send symbol-field get-value)) " ("
                                            (send symbol-field get-value) ")")
                     #:x-label "Strike"
                     #:y-label "Vol")))))

(define (chart-vol-history-plot symbol-field canvas)
  (define vol-dvs (get-date-vol-history (send symbol-field get-value)
                                        (send chart-start-date-field get-value)
                                        (send chart-end-date-field get-value)))
  (define variance-dvs (get-date-variance-history (send symbol-field get-value)
                                                  (send chart-start-date-field get-value)
                                                  (send chart-end-date-field get-value)))
  (if (or (equal? (send symbol-field get-value) "")
          (and (= 0 (length vol-dvs))
               (= 0 (length variance-dvs))))
      (plot-snip (lines (list #(0 0) #(1 0)))
                 #:title ""
                 #:x-label "Date"
                 #:y-label "Vol/Variance"
                 #:width (- (send canvas get-width) 12)
                 #:height (- (send canvas get-height) 12))
      (let* ([min-value (min (apply min (map (λ (dv) (dv-value dv)) vol-dvs))
                             (apply min (map (λ (dv) (dv-value dv)) variance-dvs)))]
             [earnings-dates-points (map (λ (d) (point-label (vector d min-value) "E" #:anchor 'bottom))
                                         (get-earnings-dates (send symbol-field get-value)
                                                             (send chart-start-date-field get-value)
                                                             (send chart-end-date-field get-value)))]
             [snip (parameterize ([plot-x-ticks (date-ticks)]
                                  [plot-width (- (send canvas get-width) 12)]
                                  [plot-height (- (send canvas get-height) 12)])
                     (plot-snip (append (list (tick-grid)
                                              (lines vol-dvs #:label "Vol" #:color 1)
                                              (lines variance-dvs #:label "Variance" #:color 2))
                                        earnings-dates-points)
                                #:title (string-append (get-security-name (send symbol-field get-value)) " ("
                                                       (send symbol-field get-value) ")")
                                #:x-label "Date"
                                #:y-label "Vol/Variance"))])
        (define item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
        (define background (make-object color% #xff #xf8 #xdc 0.8))
        (define (make-tag vol-dv variance-dv)
          (define p (if (and (empty? vol-dv) (empty? variance-dv)) (text "" item-font)
                        (vl-append
                         (hc-append
                          (text "Date: " item-font)
                          (text (~t (posix->datetime (dv-date (if (empty? vol-dv) (first variance-dv) (first vol-dv)))) "yyyy-MM-dd") item-font))
                         (hc-append
                          (text "Vol: " item-font)
                          (if (empty? vol-dv) "" (text (real->decimal-string (dv-value (first vol-dv))) item-font)))
                         (hc-append
                          (text "Variance: " item-font)
                          (if (empty? variance-dv) "" (text (real->decimal-string (dv-value (first variance-dv))) item-font))))))
          (define r (filled-rectangle
                     (+ (pict-width p) 10) (+ (pict-height p) 10)
                     #:draw-border? #f #:color background))
          (cc-superimpose r p))
        (define (get-v dv d)
          (filter (λ (e) (date=? (->date (posix->datetime d)) (->date (posix->datetime (dv-date e))))) dv))
        (define ((make-current-value-renderer vol-dvs variance-dvs) snip event x y)
          (define delta (- (current-milliseconds) prev-time-stamp))
          (cond [(< 40 delta)
                 (define overlays
                   (and x y (eq? (send event get-event-type) 'motion)
                        (let ([shift (if (< 43200 (modulo (round x) 86400)) 86400 0)])
                          (list (vrule (+ (- x (modulo (round x) 86400)) shift) #:style 'long-dash)
                                (point-pict (vector (+ (- x (modulo (round x) 86400)) shift) y)
                                            (make-tag (get-v vol-dvs (+ x 43200)) (get-v variance-dvs (+ x 43200)))
                                            #:anchor 'auto)))))
                 (send snip set-overlay-renderers overlays)
                 (set! prev-time-stamp (current-milliseconds))]))
        (send snip set-mouse-event-callback (make-current-value-renderer vol-dvs variance-dvs))
        snip)))

(define (chart-price-plot symbol-field canvas)
  (if (equal? (send symbol-field get-value) "")
      (plot-snip (lines (list #(0 0) #(1 0)))
                 #:title ""
                 #:x-label "Date"
                 #:y-label "Price"
                 #:width (- (send canvas get-width) 12)
                 #:height (- (send canvas get-height) 12))
      (let* ([dohlcs (get-date-ohlc (send symbol-field get-value)
                                    (send chart-start-date-field get-value)
                                    (send chart-end-date-field get-value))]
             [min-low (apply min (map (λ (el) (dohlc-low el)) dohlcs))]
             [earnings-dates-points (map (λ (d) (point-label (vector d min-low) "E" #:anchor 'bottom))
                                         (get-earnings-dates (send symbol-field get-value)
                                                             (send chart-start-date-field get-value)
                                                             (send chart-end-date-field get-value)))]
             [snip (parameterize ([plot-x-ticks (date-ticks)]
                                  [plot-y-ticks (currency-ticks #:kind 'USD)]
                                  [plot-width (- (send canvas get-width) 12)]
                                  [plot-height (- (send canvas get-height) 12)])
                     (plot-snip (append (list (tick-grid)
                                              (candlesticks dohlcs #:width 86400)
                                              (lines (simple-moving-average (list->vector dohlcs) 20) #:color 3 #:label "20-day SMA")
                                              (lines (simple-moving-average (list->vector dohlcs) 50) #:color 4 #:label "50-day SMA"))
                                        earnings-dates-points)
                                #:title (string-append (get-security-name (send symbol-field get-value)) " ("
                                                       (send symbol-field get-value) ")")
                                #:x-label "Date"
                                #:y-label "Price"))])
        (define item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
        (define background (make-object color% #xff #xf8 #xdc 0.8))
        (define (make-tag dohlc)
          (define p (if (empty? dohlc) (text "" item-font)
                        (vl-append
                         (hc-append
                          (text "Date: " item-font)
                          (text (~t (posix->datetime (dohlc-date (first dohlc))) "yyyy-MM-dd") item-font))
                         (hc-append
                          (text "Open: " item-font)
                          (text (real->decimal-string (dohlc-open (first dohlc))) item-font))
                         (hc-append
                          (text "High: " item-font)
                          (text (real->decimal-string (dohlc-high (first dohlc))) item-font))
                         (hc-append
                          (text "Low: " item-font)
                          (text (real->decimal-string (dohlc-low (first dohlc))) item-font))
                         (hc-append
                          (text "Close: " item-font)
                          (text (real->decimal-string (dohlc-close (first dohlc))) item-font)))))
          (define r (filled-rectangle
                     (+ (pict-width p) 10) (+ (pict-height p) 10)
                     #:draw-border? #f #:color background))
          (cc-superimpose r p))
        (define (get-ohlc dv d)
          (filter (λ (e) (date=? (->date (posix->datetime d)) (->date (posix->datetime (dohlc-date e))))) dv))
        (define ((make-current-value-renderer dv) snip event x y)
          (define delta (- (current-milliseconds) prev-time-stamp))
          (cond [(< 40 delta)
                 (define overlays
                   (and x y (eq? (send event get-event-type) 'motion)
                        (let ([shift (if (< 43200 (modulo (round x) 86400)) 86400 0)])
                          (list (vrule (+ (- x (modulo (round x) 86400)) shift) #:style 'long-dash)
                                (point-pict (vector (+ (- x (modulo (round x) 86400)) shift) y)
                                            (make-tag (get-ohlc dv (+ x 43200)))
                                            #:anchor 'auto)))))
                 (send snip set-overlay-renderers overlays)
                 (set! prev-time-stamp (current-milliseconds))]))
        (send snip set-mouse-event-callback (make-current-value-renderer dohlcs))
        snip)))

(define chart-market-sector-pane (new horizontal-pane% 
                                      [parent chart-plot-pane]))

(define chart-industry-stock-pane (new horizontal-pane% 
                                       [parent chart-plot-pane]))

(define chart-market-canvas (new settable-snip-canvas%
                                 [parent chart-market-sector-pane]))

(define chart-sector-canvas (new settable-snip-canvas%
                                 [parent chart-market-sector-pane]))

(define chart-industry-canvas (new settable-snip-canvas%
                                   [parent chart-industry-stock-pane]))

(define chart-stock-canvas (new settable-snip-canvas%
                                [parent chart-industry-stock-pane]))

(define (show-chart)
  (send chart-frame show #t))
