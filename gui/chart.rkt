#lang racket/base

(require gregor
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
               (chart-vol-history-plot chart-stock-field chart-stock-canvas))]))

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
                                    [init-value "2018-10-01"]))

(define chart-end-date-field (new text-field%
                                  [parent chart-input-pane]
                                  [label "End Date"]
                                  [init-value "2019-02-28"]))

(define chart-type-choice (new choice%
                               [parent chart-input-pane]
                               [label "Type "]
                               [choices (list "Price" "Vol History")]))

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
                                                           (chart-vol-history-plot chart-stock-field chart-stock-canvas))]))]))

(define chart-plot-pane (new vertical-pane%
                             [parent chart-frame]))

(define prev-time-stamp (current-milliseconds))

(define (chart-vol-history-plot symbol-field canvas)
  (if (equal? (send symbol-field get-value) "")
      (plot-snip (lines (list #(0 0) #(1 0)))
                 #:title ""
                 #:x-label "Date"
                 #:y-label "Vol"
                 #:width (- (send canvas get-width) 12)
                 #:height (- (send canvas get-height) 12))
      (let* ([dvs (get-date-vol-history (send symbol-field get-value)
                                        (send chart-start-date-field get-value)
                                        (send chart-end-date-field get-value))]
             [min-value (apply min (map (λ (el) (dv-value el)) dvs))]
             [earnings-dates-points (map (λ (d) (point-label (vector d min-value) "E" #:anchor 'bottom))
                                         (get-earnings-dates (send symbol-field get-value)
                                                             (send chart-start-date-field get-value)
                                                             (send chart-end-date-field get-value)))]
             [snip (parameterize ([plot-x-ticks (date-ticks)]
                                  [plot-width (- (send canvas get-width) 12)]
                                  [plot-height (- (send canvas get-height) 12)])
                     (plot-snip (append (list (tick-grid)
                                              (lines dvs))
                                        earnings-dates-points)
                                #:title (string-append (get-security-name (send symbol-field get-value)) " ("
                                                       (send symbol-field get-value) ")")
                                #:x-label "Date"
                                #:y-label "Vol"))])
        (define item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
        (define background (make-object color% #xff #xf8 #xdc 0.8))
        (define (make-tag dv)
          (define p (if (empty? dv) (text "" item-font)
                        (vl-append
                         (hc-append
                          (text "Date: " item-font)
                          (text (~t (posix->datetime (dv-date (first dv))) "yyyy-MM-dd") item-font))
                         (hc-append
                          (text "Vol: " item-font)
                          (text (real->decimal-string (dv-value (first dv))) item-font)))))
          (define r (filled-rectangle
                     (+ (pict-width p) 10) (+ (pict-height p) 10)
                     #:draw-border? #f #:color background))
          (cc-superimpose r p))
        (define (get-v dv d)
          (filter (λ (e) (date=? (->date (posix->datetime d)) (->date (posix->datetime (dv-date e))))) dv))
        (define ((make-current-value-renderer dv) snip event x y)
          (define delta (- (current-milliseconds) prev-time-stamp))
          (cond [(< 40 delta)
                 (define overlays
                   (and x y (eq? (send event get-event-type) 'motion)
                        (let ([shift (if (< 43200 (modulo (round x) 86400)) 86400 0)])
                          (list (vrule (+ (- x (modulo (round x) 86400)) shift) #:style 'long-dash)
                                (point-pict (vector (+ (- x (modulo (round x) 86400)) shift) y)
                                            (make-tag (get-v dv (+ x 43200)))
                                            #:anchor 'auto)))))
                 (send snip set-overlay-renderers overlays)
                 (set! prev-time-stamp (current-milliseconds))]))
        (send snip set-mouse-event-callback (make-current-value-renderer dvs))
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
