#lang racket/base

(require gregor
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
         "structs.rkt")

(provide set-order-data
         show-position-order-manager)

(define manager-frame
  (new frame% [label "Position/Order Manager"] [width 600] [height 400]))

(define manager-pane
  (new vertical-pane% [parent manager-frame]))

(define input-pane
  (new horizontal-pane%
       [parent manager-pane]
       [stretchable-height #f]))

(define trade-size-field
  (new text-field%
       [parent input-pane]
       [label "Trade Size"]
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
                                               [quantity (floor (/ (string->number (send trade-size-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'long-put (order-strategy ord))
                                  (struct-copy order ord
                                               [quantity (floor (/ (string->number (send trade-size-field get-value))
                                                                   (* 100 (order-price ord))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bull-call-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-size-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bull-put-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 0)) (order-strike (send order-box get-data 1)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-size-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (* (- 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (+ 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bear-put-vertical-spread (order-strategy ord))
                                  (define risk (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-size-field get-value))
                                                                      (* 100 risk (if (= i 0) 1 -1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [(equal? 'bear-call-vertical-spread (order-strategy ord))
                                  (define risk (- (- (order-strike (send order-box get-data 1)) (order-strike (send order-box get-data 0)))
                                                  (- (order-price (send order-box get-data 0)) (order-price (send order-box get-data 1)))))
                                  (struct-copy order ord
                                               [quantity (truncate (/ (string->number (send trade-size-field get-value))
                                                                      (* 100 risk (if (= i 1) 1 -1))))]
                                               [stock-stop (* (+ 1 (string->number (send stop-percent-field get-value)))
                                                              (order-stock-entry ord))]
                                               [stock-target (* (- 1 (string->number (send target-percent-field get-value)))
                                                                (order-stock-entry ord))])]
                                 [else ord]))
                         (range (send order-box get-number)))))]))

(define order-box-columns (list "Symbol" "Expiry" "Strike" "CallPut" "Qty" "Price" "StkEntry" "StkStop" "StkTgt"))

(define order-box
  (new list-box%
       [parent manager-pane]
       [label #f]
       [style (list 'single 'column-headers)]
       [columns order-box-columns]
       [choices (list "")]
       [callback (λ (b e)
                   (row-editor-frame (send order-box get-selection)
                                     order-box-columns
                                     (send order-box get-data (send order-box get-selection))))]))

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
                                   [handle-execution-rsp (λ (e) (insert-execution e))]
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
                                                [currency "USD"]))
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
                                                [conditions (list (condition 'price
                                                                             'and
                                                                             (cond [(or (equal? 'bull-call-vertical-spread (order-strategy first-item))
                                                                                        (equal? 'bull-put-vertical-spread (order-strategy first-item)))
                                                                                    'greater-than]
                                                                                   [(or (equal? 'bear-call-vertical-spread (order-strategy first-item))
                                                                                        (equal? 'bear-put-vertical-spread (order-strategy first-item)))
                                                                                    'less-than])
                                                                             (order-stock-entry first-item)
                                                                             underlying-contract-id
                                                                             "SMART"
                                                                             'default))])))
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
