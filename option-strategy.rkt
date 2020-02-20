#lang racket/base

(require gregor
         racket/class
         racket/gui/base
         racket/list
         racket/string
         "db-queries.rkt"
         "position-order-manager.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide show-option-strategy
         refresh-option-strategy)

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
       [init-value (date->iso8601 (today))]))

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
             (string-contains? patterns "AT")
             (string-contains? patterns "IR"))
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
                                                            (< (option-delta o) -6/10)
                                                            (equal? (option-call-put o) "Put")))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (> (option-delta o) -25/100)
                                                          (equal? (option-call-put o) "Put")))
                                              options))])
                 (list short-put long-put))
               "Call Horizontal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                                  (abs (- 14 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                                 (abs (- 28 (option-dte res))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [short-call (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-front-dte))
                                                             (<= (abs (- 5/10 (option-delta o)))
                                                                 (abs (- 5/10 (option-delta res))))
                                                             (equal? (option-call-put o) "Call"))
                                                        o
                                                        res))
                                         (first options)
                                         options)]
                      [long-call (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                            (<= (abs (- 5/10 (option-delta o)))
                                                                (abs (- 5/10 (option-delta res))))
                                                            (equal? (option-call-put o) "Call"))
                                                       o
                                                       res))
                                        (first options)
                                        options)])
                 (list short-call long-call))
               "Call Diagonal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                                  (abs (- 28 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                                 (abs (- 56 (option-dte res))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-back-dte))
                                                           (> (option-delta o) 55/100)
                                                           (equal? (option-call-put o) "Call")))
                                               options))]
                      [short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-front-dte))
                                                             (< (option-delta o) 4/10)
                                                             (equal? (option-call-put o) "Call")))
                                                 options))])
                 (list long-call short-call)))]
        [(or (string-contains? patterns "BR")
             (string-contains? patterns "LB")
             (string-contains? patterns "DT")
             (string-contains? patterns "DR"))
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
                                                            (> (option-delta o) 6/10)
                                                            (equal? (option-call-put o) "Call")))
                                                options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (< (option-delta o) 25/100)
                                                            (equal? (option-call-put o) "Call")))
                                                options))])
                 (list short-call long-call))
               "Put Horizontal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                                  (abs (- 14 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                                 (abs (- 28 (option-dte res))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [short-put (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-front-dte))
                                                            (<= (abs (- -5/10 (option-delta o)))
                                                                (abs (- -5/10 (option-delta res))))
                                                            (equal? (option-call-put o) "Put"))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [long-put (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                           (<= (abs (- -5/10 (option-delta o)))
                                                               (abs (- -5/10 (option-delta res))))
                                                           (equal? (option-call-put o) "Put"))
                                                      o
                                                      res))
                                       (first options)
                                       options)])
                 (list short-put long-put))
               "Put Diagonal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                                  (abs (- 28 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                                 (abs (- 56 (option-dte res))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-back-dte))
                                                           (< (option-delta o) -55/100)
                                                           (equal? (option-call-put o) "Put")))
                                               options))]
                      [short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-front-dte))
                                                           (> (option-delta o) -4/10)
                                                           (equal? (option-call-put o) "Put")))
                                               options))])
                 (list long-put short-put)))]
        [(string-contains? patterns "IV")
         (hash "Long Straddle"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [closest-strike (foldl (λ (o res) (if (< (abs (- 5/10 (option-delta o)))
                                                               (abs (- 5/10 (option-delta res))))
                                                            o
                                                            res))
                                             (first options)
                                             (filter (λ (o) (= (option-dte o) (option-dte closest-dte))) options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (= (option-strike o) (option-strike closest-strike))
                                                            (equal? (option-call-put o) "Call")))
                                                options))]
                      [long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (= (option-strike o) (option-strike closest-strike))
                                                           (equal? (option-call-put o) "Put")))
                                               options))])
                 (list long-call long-put))
               "Long Strangle"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [closest-strike (foldl (λ (o res) (if (< (abs (- 5/10 (option-delta o)))
                                                               (abs (- 5/10 (option-delta res))))
                                                            o
                                                            res))
                                             (first options)
                                             (filter (λ (o) (= (option-dte o) (option-dte closest-dte))) options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (> (option-strike o) (option-strike closest-strike))
                                                            (equal? (option-call-put o) "Call")))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (< (option-strike o) (option-strike closest-strike))
                                                          (equal? (option-call-put o) "Put")))
                                              options))])
                 (list long-put long-call))
               "Call Ratio Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (> (option-delta o) 8/10)
                                                            (equal? (option-call-put o) "Call")))
                                                options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (> (option-mid short-call) (* 3 (option-mid o))) 
                                                            (equal? (option-call-put o) "Call")))
                                                options))])
                 (list short-call long-call))
               "Put Ratio Spread"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (< (option-delta o) -8/10)
                                                            (equal? (option-call-put o) "Put")))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (> (option-mid short-put) (* 3 (option-mid o))) 
                                                          (equal? (option-call-put o) "Put")))
                                              options))])
                 (list short-put long-put)))]
        [(or (string-contains? patterns "RR")
             (string-contains? patterns "RP")
             (string-contains? patterns "DV"))
         (hash "Call Butterfly"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                            (abs (- 14 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-call (foldl (λ (o res) (if (and (< (abs (- 5/10 (option-delta o)))
                                                                (abs (- 5/10 (option-delta res))))
                                                             (= (option-dte o) (option-dte closest-dte))
                                                             (equal? (option-call-put o) "Call"))
                                                        o
                                                        res))
                                         (first options)
                                         options)]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (option-strike short-call))
                                                                  (equal? (option-call-put o) "Call")))
                                                      options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (> (option-strike o) (option-strike short-call))
                                                                   (equal? (option-call-put o) "Call")))
                                                       options))])
                 (list first-long-call short-call second-long-call))
               "Call Condor"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [closest-strike (foldl (λ (o res) (if (< (abs (- 5/10 (option-delta o)))
                                                               (abs (- 5/10 (option-delta res))))
                                                            o
                                                            res))
                                             (first options)
                                             (filter (λ (o) (= (option-dte o) (option-dte closest-dte))) options))]
                      [first-short-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (option-strike closest-strike))
                                                                  (equal? (option-call-put o) "Call")))
                                                      options))]
                      [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                    (> (option-strike o) (option-strike closest-strike))
                                                                    (equal? (option-call-put o) "Call")))
                                                        options))]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (option-strike first-short-call))
                                                                  (equal? (option-call-put o) "Call")))
                                                      options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (> (option-strike o) (option-strike second-short-call))
                                                                   (equal? (option-call-put o) "Call")))
                                                       options))])
                 (list first-long-call first-short-call second-short-call second-long-call))
               "Put Butterfly"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                            (abs (- 14 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-put (foldl (λ (o res) (if (and (< (abs (- -5/10 (option-delta o)))
                                                               (abs (- -5/10 (option-delta res))))
                                                            (= (option-dte o) (option-dte closest-dte))
                                                            (equal? (option-call-put o) "Put"))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (> (option-strike o) (option-strike short-put))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (< (option-strike o) (option-strike short-put))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))])
                 (list first-long-put short-put second-long-put))
               "Put Condor"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [closest-strike (foldl (λ (o res) (if (< (abs (- 5/10 (option-delta o)))
                                                               (abs (- 5/10 (option-delta res))))
                                                            o
                                                            res))
                                             (first options)
                                             (filter (λ (o) (= (option-dte o) (option-dte closest-dte))) options))]
                      [first-short-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (> (option-strike o) (option-strike closest-strike))
                                                                  (equal? (option-call-put o) "Put")))
                                                      options))]
                      [second-short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (option-strike closest-strike))
                                                                  (equal? (option-call-put o) "Put")))
                                                      options))]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (> (option-strike o) (option-strike first-short-put))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (< (option-strike o) (option-strike second-short-put))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))])
                 (list first-long-put first-short-put second-short-put second-long-put)))]
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
  (hash-for-each (suitable-options (map (λ (o)
                                          (define divs (map (λ (div) (vector (/ (vector-ref div 0) 365) (vector-ref div 1)))
                                                            (get-dividend-estimates symbol
                                                                                    (iso8601->date date)
                                                                                    (parse-date (option-expiration o) "yy-MM-dd"))))
                                          (define 1-month-rate (get-1-month-rate date))
                                          (option (option-symbol o)
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
                                                                 1-month-rate
                                                                 (option-vol o)
                                                                 divs)
                                                  (option-ask o)
                                                  (option-vol o)
                                                  (black-scholes-delta ref-price
                                                                       (/ (option-dte o) 365)
                                                                       (option-strike o)
                                                                       (string->symbol (option-call-put o))
                                                                       1-month-rate
                                                                       (option-vol o)
                                                                       divs)
                                                  (black-scholes-gamma ref-price
                                                                       (/ (option-dte o) 365)
                                                                       (option-strike o)
                                                                       (string->symbol (option-call-put o))
                                                                       1-month-rate
                                                                       (option-vol o)
                                                                       divs)
                                                  (black-scholes-theta ref-price
                                                                       (/ (option-dte o) 365)
                                                                       (option-strike o)
                                                                       (string->symbol (option-call-put o))
                                                                       1-month-rate
                                                                       (option-vol o)
                                                                       divs)
                                                  (black-scholes-vega ref-price
                                                                      (/ (option-dte o) 365)
                                                                      (option-strike o)
                                                                      (string->symbol (option-call-put o))
                                                                      1-month-rate
                                                                      (option-vol o)
                                                                      divs)
                                                  (black-scholes-rho ref-price
                                                                     (/ (option-dte o) 365)
                                                                     (option-strike o)
                                                                     (string->symbol (option-call-put o))
                                                                     1-month-rate
                                                                     (option-vol o)
                                                                     divs)))
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
                                                                               (parse-date (option-expiration o) "yy-MM-dd")
                                                                               (option-strike o)
                                                                               (string->symbol (string-downcase (option-call-put o)))
                                                                               #f
                                                                               (option-mid o)
                                                                               (option-vol o)
                                                                               (string->number (send ref-price-field get-value))
                                                                               #f
                                                                               #f
                                                                               (+months (iso8601->date (send date-field get-value)) 1)))
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
