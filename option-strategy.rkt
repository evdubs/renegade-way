#lang racket/base

(require gregor
         racket/list
         racket/string
         "db-queries.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide get-updated-options
         suitable-options)

(define (get-updated-options symbol date ref-price)
  (map (λ (o)
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
       (get-options symbol date)))

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
                                                                 (< (option-strike o) (- (option-strike short-call) (option-mid short-call)))
                                                                 (equal? (option-call-put o) "Call")))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (> (option-strike o) (+ (option-strike short-call) (option-mid short-call)))
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
                                                                  (< (option-strike o) (- (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) "Call")))
                                                      options))]
                      [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                    (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                    (equal? (option-call-put o) "Call")))
                                                        options))]
                      [long-short-distance (* 1/2 (- (option-strike second-short-call) (option-strike first-short-call)))]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike first-short-call) long-short-distance))
                                                                 (equal? (option-call-put o) "Call")))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (>= (option-strike o) (+ (option-strike second-short-call) long-short-distance))
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
                                                                 (> (option-strike o) (+ (option-strike short-put) (option-mid short-put)))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (< (option-strike o) (- (option-strike short-put) (option-mid short-put)))
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
                      [closest-strike (foldl (λ (o res) (if (< (abs (- -5/10 (option-delta o)))
                                                               (abs (- -5/10 (option-delta res))))
                                                            o
                                                            res))
                                             (first options)
                                             (filter (λ (o) (= (option-dte o) (option-dte closest-dte))) options))]
                      [first-short-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) "Put")))
                                                      options))]
                      [second-short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (- (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) "Put")))
                                                      options))]
                      [long-short-distance (* 1/2 (- (option-strike first-short-put) (option-strike second-short-put)))]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (>= (option-strike o) (+ (option-strike first-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike second-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) "Put")))
                                                     options))])
                 (list first-long-put first-short-put second-short-put second-long-put)))]
        [(or (string-contains? patterns "CC"))
         (hash "Call Condor"
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
                                                                  (< (option-strike o) (- (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) "Call")))
                                                      options))]
                      [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                    (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                    (equal? (option-call-put o) "Call")))
                                                        options))]
                      [long-short-distance (* 1/2 (- (option-strike second-short-call) (option-strike first-short-call)))]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike first-short-call) long-short-distance))
                                                                 (equal? (option-call-put o) "Call")))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (>= (option-strike o) (+ (option-strike second-short-call) long-short-distance))
                                                                   (equal? (option-call-put o) "Call")))
                                                       options))])
                 (list first-long-call first-short-call second-short-call second-long-call)))]
        [else (hash)]))
