#lang racket/base

(require gregor
         plot
         racket/async-channel
         racket/class
         racket/contract
         racket/list
         racket/gui/base
         "../interactive-brokers-api/base-structs.rkt"
         "../interactive-brokers-api/main.rkt"
         "../interactive-brokers-api/request-messages.rkt"
         "../interactive-brokers-api/response-messages.rkt"
         "db-queries.rkt"
         "plot-util.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide set-order-data
         show-position-order-manager)

(define manager-frame
  (new frame% [label "Position/Order Manager"] [width 600] [height 600]))

(define manager-pane
  (new vertical-pane% [parent manager-frame]))

(define input-pane
  (new horizontal-pane%
       [parent manager-pane]
       [stretchable-height #f]))

(define trade-risk-field
  (new text-field%
       [parent input-pane]
       [label "Trade Risk"]
       [init-value "2000.00"]))

(define stop-percent-field
  (new text-field%
       [parent input-pane]
       [label "Stop Pct"]
       [init-value "0.02"]))

(define target-percent-field
  (new text-field%
       [parent input-pane]
       [label "Target Pct"]
       [init-value "0.04"]))

(define recalc-button
  (new button%
       [parent input-pane]
       [label "Recalc"]
       [callback (λ (b e)
                   (set-order-data
                    (map (λ (i)
                           (define ord (send order-box get-data i))
                           (cond [(equal? 'long-call (order-strategy ord))
                                  (struct-copy order ord
                                               [quantity (floor (/ (string->number (send trade-risk-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'long-put (order-strategy ord))
                                  (struct-copy order ord
                                               [quantity (floor (/ (string->number (send trade-risk-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bull-call-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bull-put-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bear-put-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bear-call-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 1)) (order-strike (send order-box get-data 0)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
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
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'put-ratio-spread (order-strategy ord))
                                  (define risk (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1))
                                                  (order-price (send order-box get-data 0))
                                                  (* -3 (order-price (send order-box get-data 1)))))
                                  (define base-quantity (truncate (/ (string->number (send trade-risk-field get-value)) (* 100 risk))))
                                  (struct-copy order ord
                                               [quantity (if (= i 1) (* 3 base-quantity) (* -1 base-quantity))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'call-horizontal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 1)) (order-price (send order-box get-data 0))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) -1 1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (order-strike (send order-box get-data 0))])]
                                 [(equal? 'put-horizontal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 1)) (order-price (send order-box get-data 0))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) -1 1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (order-strike (send order-box get-data 0))])]
                                 [(equal? 'call-diagonal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (order-strike (send order-box get-data 1))])]
                                 [(equal? 'put-diagonal-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-risk-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
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
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
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
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
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
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
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
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (order-stock-entry ord)])]
                                 [else ord]))
                         (range (send order-box get-number))))
                   (update-profit-loss-chart))]))

(define order-box-columns (list "Symbol" "Expiry" "Strike" "CallPut" "Qty" "Price" "StkEntry" "StkStop" "StkTgt"))

(define order-box
  (new list-box%
       [parent manager-pane]
       [label #f]
       [style (list 'single 'column-headers)]
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

(define (update-profit-loss-chart)
  (define ref-price (order-stock-entry (send order-box get-data 0)))
  (define prices (map (λ (i) (/ (* i ref-price) 100))
                      (range 90 111 0.5)))
  (define first-expiry (foldl (λ (i res) (if (date<? (order-expiration (send order-box get-data i)) res)
                                             (order-expiration (send order-box get-data i))
                                             res))
                              (order-expiration (send order-box get-data 0))
                              (range (send order-box get-number))))
  (define eval-date (if (date<? (order-end-date (send order-box get-data 0)) first-expiry)
                        (order-end-date (send order-box get-data 0))
                        first-expiry))
  (define 1-month-rate (get-1-month-rate (date->iso8601 (today))))
  (define price-profit-loss
    (map (λ (p)
           (vector p (foldl (λ (i res)
                              (define order (send order-box get-data i))
                              (define yte (if (date=? eval-date (order-expiration order))
                                              1/10000
                                              (/ (days-between eval-date (order-expiration order)) 365)))
                              (+ res (* (- (black-scholes p
                                                          yte
                                                          (order-strike order)
                                                          (order-call-put order)
                                                          1-month-rate
                                                          (order-vol order)
                                                          (list))
                                           (order-price order))
                                        (order-quantity order) 100)))
                            0
                            (range (send order-box get-number)))))
         prices))
  (send profit-loss-canvas set-snip
        (plot-snip (list (tick-grid)
                         (lines price-profit-loss))
                   #:title "Order Profit/Loss at First Expiration/Exit"
                   #:x-label "Stock Price"
                   #:y-label "Profit/Loss")))

(define button-pane
  (new horizontal-pane%
       [parent manager-pane]
       [alignment '(center center)]
       [stretchable-height #f]))

(define connect-button
  (new button%
       [label "Connect"]
       [parent button-pane]
       [callback (λ (b e)
                   (set! ibkr (new ibkr-session%
                                   [handle-contract-details-rsp (λ (cd)
                                                                  (async-channel-put contract-id-channel (contract-details-rsp-contract-id cd))
                                                                  (insert-contract cd))]
                                   [handle-execution-rsp (λ (e) (insert-execution e)
                                                            (thread (λ () (send ibkr send-msg
                                                                                (new contract-details-req% [contract-id (execution-rsp-contract-id e)])))))]
                                   [handle-open-order-rsp (λ (oo) (insert-order oo))]
                                   [handle-next-valid-id-rsp (λ (id) (set! next-order-id (next-valid-id-rsp-order-id id)))]
                                   [write-messages #t]))
                   (send ibkr connect))]))

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
                            (async-channel-get contract-id-channel))
                          (range (send order-box get-number))))
                   (define first-item (send order-box get-data 0))
                   (send ibkr send-msg (new contract-details-req%
                                            [symbol (order-symbol first-item)]
                                            [security-type 'stk]
                                            [exchange "SMART"]
                                            [currency "USD"]))
                   (define underlying-contract-id (async-channel-get contract-id-channel))
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
                   (insert-order-note next-order-id (send order-box get-data 0))
                   (if (= 1 (length contract-ids))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id next-order-id]
                                                [symbol (order-symbol first-item)]
                                                [security-type 'opt]
                                                [contract-id (first contract-ids)]
                                                [order-type "LMT"]
                                                [limit-price (order-price first-item)]
                                                [time-in-force 'gtc]
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
                                                                             'default))]))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id next-order-id]
                                                [symbol (order-symbol first-item)]
                                                [security-type 'bag]
                                                [order-type "LMT"]
                                                [limit-price (abs total-price)]
                                                [time-in-force 'gtc]
                                                [action (if (< 0 total-price) 'buy 'sell)]
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
                                                [conditions (if (or (equal? 'long-straddle (order-strategy first-item))
                                                                    (equal? 'long-strangle (order-strategy first-item))
                                                                    (equal? 'call-butterfly (order-strategy first-item))
                                                                    (equal? 'put-butterfly (order-strategy first-item))
                                                                    (equal? 'call-condor (order-strategy first-item))
                                                                    (equal? 'put-condor (order-strategy first-item))) (list)
                                                                (list (condition 'price
                                                                                 'and
                                                                                 (cond [(or (equal? 'bull-call-vertical-spread (order-strategy first-item))
                                                                                            (equal? 'bull-put-vertical-spread (order-strategy first-item))
                                                                                            (equal? 'call-ratio-spread (order-strategy first-item))
                                                                                            (equal? 'call-diagonal-spread (order-strategy first-item))
                                                                                            (equal? 'call-horizontal-spread (order-strategy first-item)))
                                                                                        'greater-than]
                                                                                       [(or (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                                            (equal? 'bear-put-vertical-spread (order-strategy first-item))
                                                                                            (equal? 'put-ratio-spread (order-strategy first-item))
                                                                                            (equal? 'put-diagonal-spread (order-strategy first-item))
                                                                                            (equal? 'put-horizontal-spread (order-strategy first-item)))
                                                                                        'less-than])
                                                                                 (order-stock-entry first-item)
                                                                                 underlying-contract-id
                                                                                 "SMART"
                                                                                 'default)))])))
                   (set! next-order-id (add1 next-order-id)))]))

(define save-trades-button
  (new button%
       [label "Save Trades"]
       [parent button-pane]
       [callback (λ (b e)
                   (send ibkr send-msg (new executions-req%)))]))

(define (set-order-data order-data)
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
            order-data (range (length order-data))))

(define (row-editor-frame index headers row)
  (define editor-frame (new frame% [label "Row Editor"] [width 300] [height 400]))
  (define editor-pane (new vertical-pane% [parent editor-frame]))
  (define editor-fields
    (map (λ (name val)
           (new text-field%
                [parent editor-pane]
                [label name]
                [init-value val])) headers row))
  (define save-button
    (new button%
         [label "Save"]
         [parent editor-pane]
         [callback (λ (b e)
                     (send order-box set-data index
                           (map (λ (f) (send f get-value)) editor-fields))
                     (set-order-data (map (λ (i) (send order-box get-data i))
                                          (range (send order-box get-number)))))]))
  (send editor-frame show #t))

(define (show-position-order-manager)
  (send manager-frame show #t)
  (let ([order-box-width (send order-box get-width)]
        [order-num-cols (length order-box-columns)])
    (for-each (λ (i) (send order-box set-column-width i 80 80 80))
              (range order-num-cols))))

(define next-order-id 0)

(define contract-id-channel (make-async-channel))

(define ibkr #f)
