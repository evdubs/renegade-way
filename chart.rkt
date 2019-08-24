#lang racket/base

(require pict
         plot
         (only-in srfi/19
                  make-time
                  string->date
                  time-utc->date
                  add-duration
                  time-duration
                  date->time-utc
                  date->string)
         racket/gui
         "db-queries.rkt"
         "plot-util.rkt"
         "structs.rkt"
         "technical-indicators.rkt")

(provide show-chart
         refresh-chart)

(define (refresh-chart market sector industry stock start-date end-date)
  (send chart-market-field set-value market)
  (send chart-sector-field set-value sector)
  (send chart-industry-field set-value industry)
  (send chart-stock-field set-value stock)
  (send chart-start-date-field set-value start-date)
  (send chart-end-date-field set-value end-date)
  (send chart-market-canvas set-snip
        (chart-price-plot chart-market-field chart-market-canvas))
  (send chart-sector-canvas set-snip
        (chart-price-plot chart-sector-field chart-sector-canvas))
  (send chart-industry-canvas set-snip
        (chart-price-plot chart-industry-field chart-industry-canvas))
  (send chart-stock-canvas set-snip
        (chart-price-plot chart-stock-field chart-stock-canvas)))

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

(define chart-refresh-button (new button%
                                  [parent chart-input-pane]
                                  [label "Refresh"]
                                  [callback (λ (b e) (send chart-market-canvas set-snip
                                                           (chart-price-plot chart-market-field chart-market-canvas))
                                              (send chart-sector-canvas set-snip
                                                    (chart-price-plot chart-sector-field chart-sector-canvas))
                                              (send chart-industry-canvas set-snip
                                                    (chart-price-plot chart-industry-field chart-industry-canvas))
                                              (send chart-stock-canvas set-snip
                                                    (chart-price-plot chart-stock-field chart-stock-canvas)))]))

(define chart-plot-pane (new vertical-pane%
                             [parent chart-frame]))

(define (chart-price-plot symbol-field canvas)
  (if (equal? (send symbol-field get-value) "")
      (plot-snip (lines (list #(0 0) #(1 0)))
                 #:title ""
                 #:x-label "Date"
                 #:y-label "Price"
                 #:width (- (send canvas get-width) 12)
                 #:height (- (send canvas get-height) 12))
      (let* ([date-ohlc-vector (get-date-ohlc (send symbol-field get-value)
                                              (send chart-start-date-field get-value)
                                              (send chart-end-date-field get-value))]
             [snip (parameterize ([plot-x-ticks (date-ticks)]
                                  [plot-y-ticks (currency-ticks #:kind 'USD)]
                                  [plot-width (- (send canvas get-width) 12)]
                                  [plot-height (- (send canvas get-height) 12)])
                     (plot-snip (list (tick-grid)
                                      (candlesticks date-ohlc-vector #:width 86400)
                                      (lines (simple-moving-average (list->vector date-ohlc-vector) 20) #:color 3 #:label "20-day SMA")
                                      (lines (simple-moving-average (list->vector date-ohlc-vector) 50) #:color 4 #:label "50-day SMA"))
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
                          (text (date->string (seconds->date (dohlc-date (first dohlc))) "~1") item-font))
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
          (filter (λ (e) (and (= (date-year (seconds->date d)) (date-year (seconds->date (dohlc-date e))))
                              (= (date-month (seconds->date d)) (date-month (seconds->date (dohlc-date e))))
                              (= (date-day (seconds->date d)) (date-day (seconds->date (dohlc-date e)))))) dv))
        (define ((make-current-value-renderer dv) snip event x y)
          (define overlays
            (and x y (eq? (send event get-event-type) 'motion)
                 (list (vrule (+ (- x (modulo (round x) 86400)) 43200) #:style 'long-dash)
                       (point-pict (vector (+ (- x (modulo (round x) 86400)) 43200) y)
                                   (make-tag (get-ohlc dv (+ x 43200))) #:anchor 'auto))))
          (send snip set-overlay-renderers overlays))
        (send snip set-mouse-event-callback (make-current-value-renderer date-ohlc-vector))
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
