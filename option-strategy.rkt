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
         "db-queries.rkt"
         "position-order-manager.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide show-option-strategy
         refresh-option-strategy)

(define (add-months d n)
  (time-utc->date (add-duration (date->time-utc d)
                                (make-time time-duration 0 (* 60 60 24 30 n)))))

(define strategy-frame
  (new frame% [label "Option Strategy"] [width 1000] [height 600]))

(define strategy-input-pane
  (new horizontal-pane%
       [parent strategy-frame]
       [stretchable-height #f]))

(define symbol-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Symbol"]
       [init-value ""]))

(define date-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Date"]
       [init-value (date->string (current-date) "~1")]))

(define ref-price-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Ref Price"]
       [init-value ""]))

(define patterns-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Patterns"]
       [init-value ""]))

(define refresh-button
  (new button%
       [parent strategy-input-pane]
       [label "Refresh"]
       [callback (λ (c e) (refresh-option-strategy (send symbol-field get-value)
                                                   (send date-field get-value)
                                                   (string->number (send ref-price-field get-value))
                                                   (send patterns-field get-value)))]))

(define strategy-table-pane (new vertical-pane% [parent strategy-frame]))

(define (suitable-options options patterns)
  (cond [(or (string-contains? patterns "BP")
             (string-contains? patterns "HB")
             (string-contains? patterns "AT"))
         (hash "Long Call"
               (let ([closest-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                           (abs (- 56 (option-dte res))))
                                                        o
                                                        res))
                                         (first options)
                                         options)])
                 (list (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                 (> (option-delta o) 6/10)
                                                 (equal? (option-call-put o) "Call")))
                                     options))))
               "Bull Call Vertical Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (> (option-delta o) 6/10)
                                                           (equal? (option-call-put o) "Call")))
                                               options))]
                      [short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                             (< (option-delta o) 3/10)
                                                             (equal? (option-call-put o) "Call")))
                                                 options))])
                 (list long-call short-call))
               "Bull Put Vertical Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (< (option-delta o) -7/10)
                                                           (equal? (option-call-put o) "Put")))
                                               options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (> (option-delta o) -4/10)
                                                           (equal? (option-call-put o) "Put")))
                                               options))])
                 (list short-put long-put)))]
        [(or (string-contains? patterns "BR")
             (string-contains? patterns "LB")
             (string-contains? patterns "DT"))
         (hash "Long Put"
               (let ([closest-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                           (abs (- 56 (option-dte res))))
                                                        o
                                                        res))
                                         (first options)
                                         options)])
                 (list (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                 (< (option-delta o) -6/10)
                                                 (equal? (option-call-put o) "Put")))
                                     options))))
               "Bear Put Vertical Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (< (option-delta o) -6/10)
                                                           (equal? (option-call-put o) "Put")))
                                               options))]
                      [short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (> (option-delta o) -3/10)
                                                           (equal? (option-call-put o) "Put")))
                                               options))])
                 (list long-put short-put))
               "Bear Call Vertical Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (> (option-delta o) 7/10)
                                                           (equal? (option-call-put o) "Call")))
                                               options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                             (< (option-delta o) 4/10)
                                                             (equal? (option-call-put o) "Call")))
                                                 options))])
                 (list short-call long-call)))]
        [else (hash)]))

(define option-columns (list "Symbol" "Expiry" "Strike" "Call/Put" "Date" "Bid" "Ask" "Spread" "BsPrc" "Vol" "Delta" "Gamma" "Theta" "Vega" "Rho"))

(define (refresh-option-strategy symbol date ref-price patterns)
  ; clear contents except for input pane
  (cond [(> (length (send strategy-frame get-children)) 1)
         (for-each (λ (vp) (for-each (λ (lb) (send vp delete-child lb))
                                     (send vp get-children)))
                   (drop (send strategy-frame get-children) 1))])
  (send symbol-field set-value symbol)
  (send date-field set-value date)
  (send ref-price-field set-value (real->decimal-string ref-price))
  (send patterns-field set-value patterns)
  (hash-for-each (suitable-options (map (λ (o) (option (option-symbol o)
                                                       (option-expiration o)
                                                       (option-dte o)
                                                       (option-strike o)
                                                       (option-call-put o)
                                                       (option-date o)
                                                       (option-bid o)
                                                       (black-scholes ref-price
                                                                      (/ (option-dte o) 365)
                                                                      (option-strike o)
                                                                      (string->symbol (option-call-put o))
                                                                      (get-1-month-rate date)
                                                                      (option-vol o))
                                                       (option-ask o)
                                                       (option-vol o)
                                                       (black-scholes-delta ref-price
                                                                            (/ (option-dte o) 365)
                                                                            (option-strike o)
                                                                            (string->symbol (option-call-put o))
                                                                            (get-1-month-rate date)
                                                                            (option-vol o))
                                                       (black-scholes-gamma ref-price
                                                                            (/ (option-dte o) 365)
                                                                            (option-strike o)
                                                                            (string->symbol (option-call-put o))
                                                                            (get-1-month-rate date)
                                                                            (option-vol o))
                                                       (black-scholes-theta ref-price
                                                                            (/ (option-dte o) 365)
                                                                            (option-strike o)
                                                                            (string->symbol (option-call-put o))
                                                                            (get-1-month-rate date)
                                                                            (option-vol o))
                                                       (black-scholes-vega ref-price
                                                                           (/ (option-dte o) 365)
                                                                           (option-strike o)
                                                                           (string->symbol (option-call-put o))
                                                                           (get-1-month-rate date)
                                                                           (option-vol o))
                                                       (black-scholes-rho ref-price
                                                                          (/ (option-dte o) 365)
                                                                          (option-strike o)
                                                                          (string->symbol (option-call-put o))
                                                                          (get-1-month-rate date)
                                                                          (option-vol o))))
                                        (get-options symbol date)) patterns)
                 (λ (k v)
                   (let ([table (new list-box% [parent strategy-table-pane]
                                     [label k]
                                     [style (list 'single 'column-headers 'vertical-label)]
                                     [columns option-columns]
                                     [choices (list "")]
                                     [callback (λ (b e)
                                                 (set-order-data (map (λ (o)
                                                                        (order (string->symbol (string-replace (string-downcase k) " " "-"))
                                                                               (option-symbol o)
                                                                               (string->date (option-expiration o) "~y-~m-~d")
                                                                               (option-strike o)
                                                                               (string->symbol (string-downcase (option-call-put o)))
                                                                               #f
                                                                               (option-mid o)
                                                                               (string->number (send ref-price-field get-value))
                                                                               #f
                                                                               #f
                                                                               (add-months (string->date (send date-field get-value) "~Y-~m-~d") 1)))
                                                                      v)))])])
                     (send table set
                           (map (λ (o) (option-symbol o)) v)
                           (map (λ (o) (option-expiration o)) v)
                           (map (λ (o) (real->decimal-string (option-strike o))) v)
                           (map (λ (o) (option-call-put o)) v)
                           (map (λ (o) (option-date o)) v)
                           (map (λ (o) (real->decimal-string (option-bid o))) v)
                           (map (λ (o) (real->decimal-string (option-ask o))) v)
                           (map (λ (o) (real->decimal-string (/ (- (option-ask o) (option-bid o)) (option-ask o)))) v)
                           (map (λ (o) (real->decimal-string (option-mid o))) v)
                           (map (λ (o) (real->decimal-string (option-vol o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-delta o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-gamma o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-theta o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-vega o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-rho o) 3)) v))
                     (let ([box-width (send table get-width)]
                           [num-cols (length option-columns)])
                       (for-each (λ (i) (send table set-column-width i
                                              80
                                              80
                                              80))
                                 (range num-cols)))))))

(define (show-option-strategy)
  (send strategy-frame show #t))
