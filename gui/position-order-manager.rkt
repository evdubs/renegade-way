#lang racket/base

(require gregor
         gregor/period
         plot
         racket/async-channel
         racket/class
         racket/gui/base
         racket/list
         threading
         interactive-brokers-api/base-structs
         interactive-brokers-api/request-messages
         interactive-brokers-api/response-messages
         "../db-queries.rkt"
         "../ibkr.rkt"
         "../params.rkt"
         "../pricing-risk.rkt"
         "../structs.rkt"
         "../technical-indicators.rkt"
         "plot-util.rkt")

(provide set-order-data
         show-position-order-manager)

(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))

(define manager-frame
  (new frame% [label "Position/Order Manager"] [width 650] [height 600]))

(define manager-pane
  (new vertical-pane% [parent manager-frame]))

(define field-input-pane
  (new horizontal-pane%
       [parent manager-pane]
       [stretchable-height #f]))

(define trade-risk-percent-field
  (new text-field%
       [parent field-input-pane]
       [label "Trade Risk Pct"]
       [init-value "0.01"]))

(define trade-risk-field
  (new text-field%
       [parent field-input-pane]
       [label "Trade Risk"]
       [init-value "200.00"]))

(define spread-percent-field
  (new text-field%
       [parent field-input-pane]
       [label "Spread Pct"]
       [init-value "0.01"]))

(define eval-date-field
  (new text-field%
       [parent field-input-pane]
       [label "Eval Date"]
       [init-value ""]))

(define button-input-pane
  (new horizontal-pane%
       [parent manager-pane]
       [alignment '(center center)]
       [stretchable-height #f]))

(define recalc-button
  (new button%
       [parent button-input-pane]
       [label "Recalc"]
       [callback (λ (b e)
                   (set-order-data
                    (map (λ (i)
                           (define ord (send order-box get-data i))
                           (define atr-50 (~> (get-date-ohlc (order-symbol ord) (date->iso8601 (-months (today) 3)) (date->iso8601 (today)))
                                              (list->vector _)
                                              (simple-average-true-range _ 50)
                                              (vector-last _)
                                              (dv-value _)))
                           (cond [(equal? 'long-call (order-strategy ord))
                                  (struct-copy order ord
                                               [quantity (floor (/ (string->number (send trade-risk-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (- (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (+ (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'long-put (order-strategy ord))
                                  (struct-copy order ord
                                               [quantity (floor (/ (string->number (send trade-risk-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (+ (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (- (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'bull-call-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (- (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (+ (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'bull-put-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (- (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (+ (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'bear-put-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (+ (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (- (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'bear-call-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 1)) (order-strike (send order-box get-data 0)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (+ (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (- (order-stock-entry ord) (* 4 atr-50))])]
                                 [(equal? 'long-straddle (order-strategy ord))
                                  (define risk (+ (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk)))]
                                               [stock-stop (order-stock-entry ord)]
                                               [stock-target (order-stock-entry ord)])]
                                 [(equal? 'long-strangle (order-strategy ord))
                                  (define risk (+ (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk)))]
                                               [stock-stop (order-stock-entry ord)]
                                               [stock-target (order-stock-entry ord)])]
                                 [(equal? 'call-ratio-spread (order-strategy ord))
                                  (define risk (- (order-strike (send order-box get-data 1)) (order-strike (send order-box get-data 0))
                                                  (order-price (send order-box get-data 0))
                                                  (* -3 (order-price (send order-box get-data 1)))))
                                  (define base-quantity (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (= i 1) (* 3 base-quantity) (* -1 base-quantity))]
                                               [stock-stop (order-strike (send order-box get-data 1))]
                                               [stock-target (- (* 2 (order-strike (send order-box get-data 1)))
                                                                (order-strike (send order-box get-data 0)))])]
                                 [(equal? 'put-ratio-spread (order-strategy ord))
                                  (define risk (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1))
                                                  (order-price (send order-box get-data 0))
                                                  (* -3 (order-price (send order-box get-data 1)))))
                                  (define base-quantity (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (= i 1) (* 3 base-quantity) (* -1 base-quantity))]
                                               [stock-stop (order-strike (send order-box get-data 1))]
                                               [stock-target (- (* 2 (order-strike (send order-box get-data 1)))
                                                                (order-strike (send order-box get-data 0)))])]
                                 [(equal? 'call-horizontal-spread (order-strategy ord))
                                  ; ideally, the strikes would be the same, but sometimes we do not get the same strikes across expirations
                                  ; as a result, we need to take the difference between the strikes
                                  (define risk (- (- (order-price (send order-box get-data 1)) (order-price (send order-box get-data 0)))
                                                  (min 0 (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1))))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) -1 1))))]
                                               [stock-stop (- (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (order-strike (send order-box get-data 0))])]
                                 [(equal? 'put-horizontal-spread (order-strategy ord))
                                  ; see call-horizontal-spread note
                                  (define risk (- (- (order-price (send order-box get-data 1)) (order-price (send order-box get-data 0)))
                                                  (min 0 (- (order-strike (send order-box get-data 1)) (order-strike (send order-box get-data 0))))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) -1 1))))]
                                               [stock-stop (+ (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (order-strike (send order-box get-data 0))])]
                                 [(equal? 'call-diagonal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (- (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (order-strike (send order-box get-data 1))])]
                                 [(equal? 'put-diagonal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (+ (order-stock-entry ord) (* 2 atr-50))]
                                               [stock-target (order-strike (send order-box get-data 1))])]
                                 [(equal? 'call-butterfly (order-strategy ord))
                                  (define out-of-the-money-risk (+ (order-price (send order-box get-data 0))
                                                                   (* -2 (order-price (send order-box get-data 1)))
                                                                   (order-price (send order-box get-data 2))))
                                  (define risk (max (abs out-of-the-money-risk)
                                                    (abs (+ (order-strike (send order-box get-data 0))
                                                            (* -2 (order-strike (send order-box get-data 1)))
                                                            (order-strike (send order-box get-data 2))
                                                            out-of-the-money-risk))))
                                  (define contracts (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (= i 1) (* -2 contracts) contracts)]
                                               [stock-stop (order-strike (send order-box get-data 0))]
                                               [stock-target (order-strike (send order-box get-data 1))])]
                                 [(equal? 'put-butterfly (order-strategy ord))
                                  (define out-of-the-money-risk (+ (order-price (send order-box get-data 0))
                                                                   (* -2 (order-price (send order-box get-data 1)))
                                                                   (order-price (send order-box get-data 2))))
                                  (define risk (max (abs out-of-the-money-risk)
                                                    (abs (+ (* -1 (order-strike (send order-box get-data 0)))
                                                            (* 2 (order-strike (send order-box get-data 1)))
                                                            (* -1 (order-strike (send order-box get-data 2)))
                                                            out-of-the-money-risk))))
                                  (define contracts (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (= i 1) (* -2 contracts) contracts)]
                                               [stock-stop (order-strike (send order-box get-data 0))]
                                               [stock-target (order-strike (send order-box get-data 1))])]
                                 [(equal? 'call-condor (order-strategy ord))
                                  (define out-of-the-money-risk (+ (order-price (send order-box get-data 0))
                                                                   (* -1 (order-price (send order-box get-data 1)))
                                                                   (* -1 (order-price (send order-box get-data 2)))
                                                                   (order-price (send order-box get-data 3))))
                                  (define risk (max (abs out-of-the-money-risk)
                                                    (abs (+ (order-strike (send order-box get-data 0))
                                                            (* -1 (order-strike (send order-box get-data 1)))
                                                            (* -1 (order-strike (send order-box get-data 2)))
                                                            (order-strike (send order-box get-data 3))
                                                            out-of-the-money-risk))))
                                  (define contracts (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (or (= i 1) (= i 2)) (* -1 contracts) contracts)]
                                               [stock-stop (order-strike (send order-box get-data 0))]
                                               [stock-target (order-stock-entry ord)])]
                                 [(equal? 'put-condor (order-strategy ord))
                                  (define out-of-the-money-risk (+ (order-price (send order-box get-data 0))
                                                                   (* -1 (order-price (send order-box get-data 1)))
                                                                   (* -1 (order-price (send order-box get-data 2)))
                                                                   (order-price (send order-box get-data 3))))
                                  (define risk (max (abs out-of-the-money-risk)
                                                    (abs (+ (* -1 (order-strike (send order-box get-data 0)))
                                                            (order-strike (send order-box get-data 1))
                                                            (order-strike (send order-box get-data 2))
                                                            (* -1 (order-strike (send order-box get-data 3)))
                                                            out-of-the-money-risk))))
                                  (define contracts (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (or (= i 1) (= i 2)) (* -1 contracts) contracts)]
                                               [stock-stop (order-strike (send order-box get-data 0))]
                                               [stock-target (order-stock-entry ord)])]
                                 [else ord]))
                         (range (send order-box get-number)))
                    (iso8601->date (send eval-date-field get-value)))
                   (update-profit-loss-chart))]))

(define add-spread-button
  (new button%
       [parent button-input-pane]
       [label "Add Spread"]
       [callback (λ (b e)
                   (define spread-pct (string->number (send spread-percent-field get-value)))
                   (set-order-data
                    (map (λ (i)
                           (define ord (send order-box get-data i))
                           (struct-copy order ord [price (* (order-price ord) (+ 1 spread-pct))]))
                         (range (send order-box get-number)))
                    (iso8601->date (send eval-date-field get-value))))]))

(define order-box-columns (list "Symbol" "Expiry" "Strike" "CallPut" "Qty" "Price" "StkEntry" "StkStop" "StkTgt"))

(define order-box
  (new list-box%
       [parent manager-pane]
       [label ""]
       [style (list 'single 'column-headers 'vertical-label)]
       [columns order-box-columns]
       [choices (list "")]
       [min-height 125]
       [stretchable-height #f]
       [callback (λ (b e)
                   (row-editor-frame (send order-box get-selection)
                                     order-box-columns
                                     (send order-box get-data (send order-box get-selection))))]))

(define profit-loss-canvas
  (new settable-snip-canvas%
       [parent manager-pane]))

(define ratio-requirement
  (hash 'long-call ""
        'long-put ""
        'bull-call-vertical-spread "1.00"
        'bear-put-vertical-spread "1.00"
        'long-straddle ""
        'long-strangle ""
        'call-ratio-spread ""
        'put-ratio-spread ""
        'bear-call-vertical-spread "0.67"
        'bull-put-vertical-spread "0.67"
        'call-horizontal-spread "1.00"
        'put-horizontal-spread "1.00"
        'call-diagonal-spread "0.50"
        'put-diagonal-spread "0.50"
        'call-butterfly "2.00"
        'put-butterfly "2.00"
        'call-condor "0.50"
        'put-condor "0.50"))

(define (update-profit-loss-chart)
  (define ref-price (order-stock-entry (send order-box get-data 0)))
  (define stop-price (order-stock-stop (send order-box get-data 0)))
  (define target-price (order-stock-target (send order-box get-data 0)))
  (define low-price (min (* 80/100 ref-price)
                         (apply min (map (λ (i) (order-strike (send order-box get-data i)))
                                         (range (send order-box get-number))))))
  (define high-price (max (* 121/100 ref-price)
                          (apply max (map (λ (i) (order-strike (send order-box get-data i)))
                                          (range (send order-box get-number))))))
  (define prices (map (λ (i) (/ (* i ref-price) 100))
                      (range (* 100 (/ low-price ref-price)) (* 100 (/ high-price ref-price)) 0.5)))
  (define price-nearest-target (foldl (λ (p res) (if (> (abs (- res p)) (abs (- target-price p))) p res))
                                         (first prices) prices))
  (define eval-date (iso8601->date (send eval-date-field get-value)))
  (define 1-month-rate (get-1-month-rate (date->iso8601 (today))))
  (define (price-profit-loss vol-multiplier prices)
    (map (λ (p)
           (vector p (foldl (λ (i res)
                              (define order (send order-box get-data i))
                              (define yte (if (date=? eval-date (order-expiration order))
                                              1/1000000
                                              (/ (days-between eval-date (order-expiration order)) 365)))
                              (+ res (* (- (black-scholes p
                                                          yte
                                                          (order-strike order)
                                                          (order-call-put order)
                                                          1-month-rate
                                                          (* (order-vol order) vol-multiplier)
                                                          (list))
                                           (order-price order))
                                        (order-quantity order) 100)))
                            0
                            (range (send order-box get-number)))))
         prices))
  (define current-vol-profit-loss (price-profit-loss 1 prices))
  (define current-vol-profit-loss-values (map (λ (pl) (vector-ref pl 1)) current-vol-profit-loss))
  (define value-near-target (vector-ref (first (filter (λ (pl) (= price-nearest-target (vector-ref pl 0)))
                                                       current-vol-profit-loss)) 1))
  (send order-box set-label
        (string-append "Risk: " (real->decimal-string (apply min current-vol-profit-loss-values))
                       " Reward (by Tgt): " (real->decimal-string (apply max current-vol-profit-loss-values))
                       " (" (real->decimal-string value-near-target) ") "
                       " Ratio (by Tgt): " (real->decimal-string (abs (/ (apply max current-vol-profit-loss-values)
                                                                              (apply min current-vol-profit-loss-values))))
                       " (" (real->decimal-string (abs (/ value-near-target
                                                          (apply min current-vol-profit-loss-values)))) ") "
                       " Reqmnt: " (hash-ref ratio-requirement (order-strategy (send order-box get-data 0)))))
  (send profit-loss-canvas set-snip
        (plot-snip (list (tick-grid)
                         (inverse (λ (y) stop-price) #:color 4 #:label "Stop")
                         (inverse (λ (y) target-price) #:color 5 #:label "Target")
                         (lines (price-profit-loss 1.5 prices)
                                #:color 1
                                #:style 'long-dash
                                #:label "Vol * 1.5")
                         (lines (price-profit-loss 1 prices)
                                #:color 2
                                #:label "Vol")
                         (lines (price-profit-loss 0.5 prices)
                                #:color 3
                                #:style 'long-dash
                                #:label "Vol * 0.5"))
                   #:title (string-append "Order Profit/Loss at " (date->iso8601 eval-date))
                   #:x-label "Stock Price"
                   #:y-label "Profit/Loss"
                   #:width (- (send profit-loss-canvas get-width) 12)
                   #:height (- (send profit-loss-canvas get-height) 12))))

(define button-pane
  (new horizontal-pane%
       [parent manager-pane]
       [alignment '(center center)]
       [stretchable-height #f]))

(define send-button
  (new button%
       [label "Send"]
       [parent button-pane]
       [callback (λ (b e)
                   (define contract-ids
                     (map (λ (i)
                            (define item (send order-box get-data i))
                            (send ibkr send-msg (new contract-details-req%
                                                     [symbol (order-symbol item)]
                                                     [security-type 'opt]
                                                     [expiry (order-expiration item)]
                                                     [strike (order-strike item)]
                                                     [right (order-call-put item)]
                                                     [exchange "SMART"]))
                            ; go through channel to find our contract. trades may have happened, so we need
                            ; to ignore those entries in the contract channel
                            (do ([c (async-channel-get contract-channel) (async-channel-get contract-channel)])
                                ((and (equal? (order-symbol item) (contract-details-rsp-symbol c))
                                      (equal? 'opt (contract-details-rsp-security-type c))
                                      (date=? (order-expiration item) (contract-details-rsp-expiry c))
                                      (equal? (order-strike item) (contract-details-rsp-strike c))
                                      (equal? (order-call-put item) (contract-details-rsp-right c)))
                                 (contract-details-rsp-contract-id c))))
                          (range (send order-box get-number))))
                   (define first-item (send order-box get-data 0))
                   (send ibkr send-msg (new contract-details-req%
                                            [symbol (order-symbol first-item)]
                                            [security-type 'stk]
                                            [exchange "SMART"]
                                            [currency "USD"]))
                   ; go through channel to find our contract. trades may have happened, so we need
                   ; to ignore those entries in the contract channel
                   (define underlying-contract-id (do ([c (async-channel-get contract-channel) (async-channel-get contract-channel)])
                                                      ((and (equal? (order-symbol first-item) (contract-details-rsp-symbol c))
                                                            (equal? 'stk (contract-details-rsp-security-type c)))
                                                       (contract-details-rsp-contract-id c))))
                   (define quantity
                     (apply gcd (map (λ (i) (order-quantity (send order-box get-data i)))
                                     (range (send order-box get-number)))))
                   (define total-price
                     (/ (foldl (λ (i sum)
                                 (+ sum (* (order-quantity (send order-box get-data i))
                                           (order-price (send order-box get-data i)))))
                               0
                               (range (send order-box get-number)))
                        quantity))
                   (insert-order-note ibkr-account ibkr-next-order-id (send order-box get-data 0))
                   (if (= 1 (length contract-ids))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id ibkr-next-order-id]
                                                [symbol (order-symbol first-item)]
                                                [security-type 'opt]
                                                [contract-id (first contract-ids)]
                                                [order-type "LMT"]
                                                [limit-price (order-price first-item)]
                                                [time-in-force 'gtc]
                                                [oca-group (string-append (date->iso8601 (today)) "."
                                                                          (cond [(equal? 'long-call (order-strategy first-item)) "bull"]
                                                                                [(equal? 'long-put (order-strategy first-item)) "bear"]))]
                                                [oca-type 1]
                                                [action (if (< 0 (order-quantity first-item)) 'buy 'sell)]
                                                [total-quantity (abs (order-quantity first-item))]
                                                [exchange "SMART"]
                                                [currency "USD"]
                                                [conditions (list (condition 'price
                                                                             'and
                                                                             (cond [(equal? 'long-call (order-strategy first-item))
                                                                                    'greater-than]
                                                                                   [(equal? 'long-put (order-strategy first-item))
                                                                                    'less-than])
                                                                             (order-stock-entry first-item)
                                                                             underlying-contract-id
                                                                             "SMART"
                                                                             'default
                                                                             #f
                                                                             #f))]
                                                [use-price-management-algo #t]))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id ibkr-next-order-id]
                                                [symbol (order-symbol first-item)]
                                                [security-type 'bag]
                                                [order-type "LMT"]
                                                [limit-price (if (or (equal? 'bull-put-vertical-spread (order-strategy first-item))
                                                                     (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                     (equal? 'call-ratio-spread (order-strategy first-item))
                                                                     (equal? 'put-ratio-spread (order-strategy first-item)))
                                                                 total-price
                                                                 (abs total-price))]
                                                [time-in-force 'gtc]
                                                [oca-group (cond [(or (equal? 'bull-call-vertical-spread (order-strategy first-item))
                                                                      (equal? 'bull-put-vertical-spread (order-strategy first-item))
                                                                      (equal? 'call-ratio-spread (order-strategy first-item))
                                                                      (equal? 'call-diagonal-spread (order-strategy first-item))
                                                                      (equal? 'call-horizontal-spread (order-strategy first-item)))
                                                                  (string-append (date->iso8601 (today)) ".bull")]
                                                                 [(or (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                      (equal? 'bear-put-vertical-spread (order-strategy first-item))
                                                                      (equal? 'put-ratio-spread (order-strategy first-item))
                                                                      (equal? 'put-diagonal-spread (order-strategy first-item))
                                                                      (equal? 'put-horizontal-spread (order-strategy first-item)))
                                                                  (string-append (date->iso8601 (today)) ".bear")]
                                                                 [(or (equal? 'call-condor (order-strategy first-item))
                                                                      (equal? 'put-condor (order-strategy first-item))
                                                                      (equal? 'call-butterfly (order-strategy first-item))
                                                                      (equal? 'put-butterfly (order-strategy first-item))
                                                                      (equal? 'long-straddle (order-strategy first-item))
                                                                      (equal? 'long-strangle (order-strategy first-item)))
                                                                  ""])] ; no oca-group for roos
                                                [oca-type 1]
                                                [action (if (or (equal? 'bull-put-vertical-spread (order-strategy first-item))
                                                                (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                (equal? 'call-ratio-spread (order-strategy first-item))
                                                                (equal? 'put-ratio-spread (order-strategy first-item)))
                                                            'buy
                                                            (if (< 0 total-price) 'buy 'sell))]
                                                [total-quantity quantity]
                                                [exchange "SMART"]
                                                [currency "USD"]
                                                [combo-legs (map (λ (i con-id)
                                                                   (define item (send order-box get-data i))
                                                                   (combo-leg con-id
                                                                              (inexact->exact (/ (abs (order-quantity item)) quantity))
                                                                              (if (< 0 (order-quantity item)) 'buy 'sell)
                                                                              "SMART"
                                                                              'same
                                                                              0
                                                                              ""
                                                                              -1))
                                                                 (range (send order-box get-number))
                                                                 contract-ids)]
                                                [conditions (cond [(or (equal? 'bull-call-vertical-spread (order-strategy first-item))
                                                                       (equal? 'bull-put-vertical-spread (order-strategy first-item))
                                                                       (equal? 'call-ratio-spread (order-strategy first-item))
                                                                       (equal? 'call-diagonal-spread (order-strategy first-item))
                                                                       (equal? 'call-horizontal-spread (order-strategy first-item)))
                                                                   (list (condition 'price 'and 'greater-than (order-stock-entry first-item)
                                                                                    underlying-contract-id "SMART" 'default #f #f))]
                                                                  [(or (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                       (equal? 'bear-put-vertical-spread (order-strategy first-item))
                                                                       (equal? 'put-ratio-spread (order-strategy first-item))
                                                                       (equal? 'put-diagonal-spread (order-strategy first-item))
                                                                       (equal? 'put-horizontal-spread (order-strategy first-item)))
                                                                   (list (condition 'price 'and 'less-than (order-stock-entry first-item)
                                                                                    underlying-contract-id "SMART" 'default #f #f))]
                                                                  [(equal? 'call-condor (order-strategy first-item))
                                                                   (let* ([low-short-strike (order-strike (send order-box get-data 1))]
                                                                          [high-short-strike (order-strike (send order-box get-data 2))]
                                                                          [difference (- high-short-strike low-short-strike)])
                                                                     (list (condition 'price 'and 'greater-than (+ low-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)
                                                                           (condition 'price 'and 'less-than (- high-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)))]
                                                                  [(equal? 'put-condor (order-strategy first-item))
                                                                   (let* ([high-short-strike (order-strike (send order-box get-data 1))]
                                                                          [low-short-strike (order-strike (send order-box get-data 2))]
                                                                          [difference (- high-short-strike low-short-strike)])
                                                                     (list (condition 'price 'and 'greater-than (+ low-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)
                                                                           (condition 'price 'and 'less-than (- high-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)))]
                                                                  [(or (equal? 'call-butterfly (order-strategy first-item))
                                                                       (equal? 'put-butterfly (order-strategy first-item)))
                                                                   (let* ([low-short-strike (min (order-strike (send order-box get-data 0))
                                                                                                 (order-strike (send order-box get-data 2)))]
                                                                          [high-short-strike (max (order-strike (send order-box get-data 0))
                                                                                                  (order-strike (send order-box get-data 2)))]
                                                                          [difference (- high-short-strike low-short-strike)])
                                                                     (list (condition 'price 'and 'greater-than (+ low-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)
                                                                           (condition 'price 'and 'less-than (- high-short-strike (* 1/4 difference))
                                                                                      underlying-contract-id "SMART" 'default #f #f)))]
                                                                  [(or (equal? 'long-straddle (order-strategy first-item))
                                                                       (equal? 'long-strangle (order-strategy first-item)))
                                                                   (list)])]
                                                [use-price-management-algo #t])))
                   (ibkr-add1-next-order-id))]))

(define save-trades-button
  (new button%
       [label "Save Trades"]
       [parent button-pane]
       [callback (λ (b e)
                   (send ibkr send-msg (new executions-req% [timestamp (-period (now/moment) (days 7))])))]))

(define update-risk-button
  (new button%
       [label "Update Risk"]
       [parent button-pane]
       [callback (λ (b e)
                   (send ibkr send-msg (new account-data-req% [subscribe #t]))
                   (send trade-risk-field set-value
                         (real->decimal-string (* (async-channel-get net-liquidation-channel)
                                                  (string->number (send trade-risk-percent-field get-value)))))
                   (send ibkr send-msg (new account-data-req% [subscribe #f])))]))

(define (set-order-data order-data eval-date)
  (send order-box set
        (map (λ (d) (order-symbol d)) order-data)
        (map (λ (d) (~t (order-expiration d) "yy-MM-dd")) order-data)
        (map (λ (d) (real->decimal-string (order-strike d))) order-data)
        (map (λ (d) (symbol->string (order-call-put d))) order-data)
        (map (λ (d) (if (order-quantity d) (number->string (order-quantity d)) "")) order-data)
        (map (λ (d) (real->decimal-string (order-price d))) order-data)
        (map (λ (d) (real->decimal-string (order-stock-entry d))) order-data)
        (map (λ (d) (if (order-stock-stop d) (real->decimal-string (order-stock-stop d)) "")) order-data)
        (map (λ (d) (if (order-stock-stop d) (real->decimal-string (order-stock-target d)) "")) order-data))
  (for-each (λ (d i)
              (send order-box set-data i d))
            order-data (range (length order-data)))

  (send eval-date-field set-value (date->iso8601 eval-date)))

; (define order-box-columns (list "Symbol" "Expiry" "Strike" "CallPut" "Qty" "Price" "StkEntry" "StkStop" "StkTgt"))

(define (row-editor-frame index headers row)
  (define editor-frame (new frame% [label "Row Editor"] [width 300] [height 400]))
  (define editor-pane (new vertical-pane% [parent editor-frame]))
  (define editor-fields
    (list (new text-field% [parent editor-pane] [label "Pattern"] [init-value (symbol->string (order-pattern row))])
          (new text-field% [parent editor-pane] [label "Strategy"] [init-value (symbol->string (order-strategy row))])
          (new text-field% [parent editor-pane] [label "Symbol"] [init-value (order-symbol row)])
          (new text-field% [parent editor-pane] [label "Expiry"] [init-value (~t (order-expiration row) "yy-MM-dd")])
          (new text-field% [parent editor-pane] [label "Strike"] [init-value (real->decimal-string (order-strike row))])
          (new text-field% [parent editor-pane] [label "CallPut"] [init-value (symbol->string (order-call-put row))])
          (new text-field% [parent editor-pane] [label "Qty"] [init-value (if (order-quantity row) (number->string (order-quantity row)) "")])
          (new text-field% [parent editor-pane] [label "Price"] [init-value (real->decimal-string (order-price row))])
          (new text-field% [parent editor-pane] [label "Vol"] [init-value (real->decimal-string (order-vol row))])
          (new text-field% [parent editor-pane] [label "Spread"] [init-value (real->decimal-string (order-spread row))])
          (new text-field% [parent editor-pane] [label "StkEntry"] [init-value (real->decimal-string (order-stock-entry row))])
          (new text-field% [parent editor-pane] [label "StkStop"] [init-value (if (order-stock-stop row) (real->decimal-string (order-stock-stop row)) "")])
          (new text-field% [parent editor-pane] [label "StkTarget"] [init-value (if (order-stock-target row) (real->decimal-string (order-stock-target row)) "")])
          (new text-field% [parent editor-pane] [label "EndDate"] [init-value (~t (order-end-date row) "yy-MM-dd")])))
  (define save-button
    (new button%
         [label "Save"]
         [parent editor-pane]
         [callback (λ (b e)
                     (send order-box set-data index
                           (order (string->symbol (send (list-ref editor-fields 0) get-value))
                                  (string->symbol (send (list-ref editor-fields 1) get-value))
                                  (send (list-ref editor-fields 2) get-value)
                                  (parse-date (send (list-ref editor-fields 3) get-value) "yy-MM-dd")
                                  (string->number (send (list-ref editor-fields 4) get-value))
                                  (string->symbol (send (list-ref editor-fields 5) get-value))
                                  (string->number (send (list-ref editor-fields 6) get-value))
                                  (string->number (send (list-ref editor-fields 7) get-value))
                                  (string->number (send (list-ref editor-fields 8) get-value))
                                  (string->number (send (list-ref editor-fields 9) get-value))
                                  (string->number (send (list-ref editor-fields 10) get-value))
                                  (string->number (send (list-ref editor-fields 11) get-value))
                                  (string->number (send (list-ref editor-fields 12) get-value))
                                  (parse-date (send (list-ref editor-fields 13) get-value) "yy-MM-dd")))
                     (set-order-data (map (λ (i) (send order-box get-data i))
                                          (range (send order-box get-number)))
                                     (iso8601->date (send eval-date-field get-value))))]))
  (send editor-frame show #t))

(define (show-position-order-manager)
  (send manager-frame show #t)
  (let ([order-box-width (send order-box get-width)]
        [order-num-cols (length order-box-columns)])
    (for-each (λ (i) (send order-box set-column-width i 80 80 80))
              (range order-num-cols))))

(define contract-channel (make-async-channel))

(define net-liquidation-channel (make-async-channel))

(ibkr-add-handler 'account-value (λ (av) (cond [(equal? "NetLiquidation" (account-value-rsp-key av))
                                                (async-channel-put net-liquidation-channel
                                                                   (string->number (account-value-rsp-value av)))])))

(ibkr-add-handler 'contract-details (λ (cd) (async-channel-put contract-channel cd)))
