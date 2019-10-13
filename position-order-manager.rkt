#lang racket/base

(require racket/async-channel
         racket/gui
         (only-in srfi/19 string->date)
         "../interactive-brokers-api/base-structs.rkt"
         "../interactive-brokers-api/main.rkt"
         "../interactive-brokers-api/request-messages.rkt"
         "../interactive-brokers-api/response-messages.rkt"
         "db-queries.rkt")

(provide set-order-data
         show-position-order-manager)

(define manager-frame
  (new frame% [label "Position/Order Manager"] [width 600] [height 400]))

(define manager-pane
  (new vertical-pane% [parent manager-frame]))

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
                                   [handle-contract-details-rsp (λ (cd) (async-channel-put contract-id-channel (contract-details-rsp-contract-id cd)))]
                                   [handle-execution-rsp (λ (e) (insert-execution e))]
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
                                                     [symbol (list-ref item 0)]
                                                     [security-type 'opt]
                                                     [expiry (string->date (list-ref item 1) "~y-~m-~d")]
                                                     [strike (string->number (list-ref item 2))]
                                                     [right (if (equal? (list-ref item 3) "Call") 'call 'put)]
                                                     [exchange "SMART"]))
                            (async-channel-get contract-id-channel))
                          (range (send order-box get-number))))
                   (define quantity
                     (apply gcd (map (λ (i) (string->number (list-ref (send order-box get-data i) 4)))
                                     (range (send order-box get-number)))))
                   (define total-price
                     (/ (foldl (λ (i sum)
                                 (+ sum (* (string->number (list-ref (send order-box get-data i) 4))
                                           (string->number (list-ref (send order-box get-data i) 5)))))
                               0
                               (range (send order-box get-number)))
                        quantity))
                   (if (= 1 (length contract-ids))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id next-order-id]
                                                [symbol (list-ref (send order-box get-data 0) 0)]
                                                [security-type 'opt]
                                                [contract-id (first contract-ids)]
                                                [order-type "LMT"]
                                                [limit-price (string->number (list-ref (send order-box get-data 0) 5))]
                                                [action (if (< 0 (string->number (list-ref (send order-box get-data 0) 4))) 'buy 'sell)]
                                                [total-quantity (abs (string->number (list-ref (send order-box get-data 0) 4)))]
                                                [exchange "SMART"]
                                                [currency "USD"]))
                       (send ibkr send-msg (new place-order-req%
                                                [order-id next-order-id]
                                                [symbol (list-ref (send order-box get-data 0) 0)]
                                                [security-type 'bag]
                                                [order-type "LMT"]
                                                [limit-price (abs total-price)]
                                                [action (if (< 0 total-price) 'buy 'sell)]
                                                [total-quantity quantity]
                                                [exchange "SMART"]
                                                [currency "USD"]
                                                [combo-legs (map (λ (i con-id)
                                                                   (define item (send order-box get-data i))
                                                                   (combo-leg con-id
                                                                              (/ (abs (string->number (list-ref item 4))) quantity)
                                                                              (if (< 0 (string->number (list-ref item 4))) 'buy 'sell)
                                                                              "SMART"
                                                                              'same
                                                                              0
                                                                              ""
                                                                              -1))
                                                                 (range (send order-box get-number))
                                                                 contract-ids)])))
                   (set! next-order-id (add1 next-order-id)))]))

(define save-trades-button
  (new button%
       [label "Save Trades"]
       [parent button-pane]
       [callback (λ (b e)
                   (send ibkr send-msg (new executions-req%)))]))

(define (set-order-data order-data)
  (send order-box set
        (map (λ (d) (list-ref d 0)) order-data)
        (map (λ (d) (list-ref d 1)) order-data)
        (map (λ (d) (list-ref d 2)) order-data)
        (map (λ (d) (list-ref d 3)) order-data)
        (map (λ (d) (list-ref d 4)) order-data)
        (map (λ (d) (list-ref d 5)) order-data)
        (map (λ (d) (list-ref d 6)) order-data)
        (map (λ (d) (list-ref d 7)) order-data)
        (map (λ (d) (list-ref d 8)) order-data))
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
