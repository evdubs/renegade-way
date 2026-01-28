#lang racket/base

(require gregor
         math/matrix
         racket/list
         racket/string
         "db-queries.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide get-updated-options
         suitable-options)

; taken from alex-hhh/data-frame ... least-squares-fit.rkt
(define (polynomial-fit-coefficients xs ys nitems degree)
  (define y-matrix (list->matrix nitems 1 ys))
  (define x-matrix (vandermonde-matrix xs (add1 degree)))
  (define x-matrix-transposed (matrix-transpose x-matrix))
  (define x (matrix* x-matrix-transposed x-matrix))
  (define y (matrix* x-matrix-transposed y-matrix))
  (matrix->list (matrix-solve x y)))

(define (get-updated-options symbol date ref-price #:compute-all-greeks [compute-all-greeks? #t] #:fit-vols [fit-vols? #f])
  (define options (get-options symbol date))
  (define options-by-expiration (group-by (λ (o) (option-expiration o)) options))
  (define coeffs (if fit-vols?
                     (make-hash (map (λ (option-group)
                                       (cons (option-expiration (first option-group))
                                             (polynomial-fit-coefficients (map (λ (o) (option-strike o)) option-group)
                                                                          (map (λ (o) (option-vol o)) option-group)
                                                                          (length option-group)
                                                                          3)))
                                     options-by-expiration))
                     #f))
  (map (λ (o)
         (define days-in-this-year (days-in-year (->year (iso8601->date date))))
         (define divs (map (λ (div) (vector (/ (vector-ref div 0) days-in-this-year)
                                            (vector-ref div 1)))
                           (get-dividend-estimates symbol
                                                   (iso8601->date date)
                                                   (parse-date (option-expiration o) "yy-MM-dd"))))
         (define 1-month-rate (get-1-month-rate date))
         (define vol (if fit-vols?
                         (+ (first (hash-ref coeffs (option-expiration o)))
                            (* (option-strike o) (second (hash-ref coeffs (option-expiration o))))
                            (* (option-strike o) (option-strike o) (third (hash-ref coeffs (option-expiration o))))
                            (* (option-strike o) (option-strike o) (option-strike o) (fourth (hash-ref coeffs (option-expiration o)))))
                         (option-vol o)))
         (option (option-symbol o)
                 (option-expiration o)
                 (option-dte o)
                 (option-strike o)
                 (option-call-put o)
                 (option-date o)
                 (option-bid o)
                 (black-scholes ref-price
                                (/ (option-dte o) days-in-this-year)
                                (option-strike o)
                                (string->symbol (option-call-put o))
                                1-month-rate
                                vol
                                divs)
                 (option-ask o)
                 vol
                 (black-scholes-delta ref-price
                                      (/ (option-dte o) days-in-this-year)
                                      (option-strike o)
                                      (string->symbol (option-call-put o))
                                      1-month-rate
                                      vol
                                      divs)
                 (if compute-all-greeks?
                     (black-scholes-gamma ref-price
                                          (/ (option-dte o) days-in-this-year)
                                          (option-strike o)
                                          (string->symbol (option-call-put o))
                                          1-month-rate
                                          vol
                                          divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-theta ref-price
                                          (/ (option-dte o) days-in-this-year)
                                          (option-strike o)
                                          (string->symbol (option-call-put o))
                                          1-month-rate
                                          vol
                                          divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-vega ref-price
                                         (/ (option-dte o) days-in-this-year)
                                         (option-strike o)
                                         (string->symbol (option-call-put o))
                                         1-month-rate
                                         vol
                                         divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-rho ref-price
                                        (/ (option-dte o) days-in-this-year)
                                        (option-strike o)
                                        (string->symbol (option-call-put o))
                                        1-month-rate
                                        vol
                                        divs)
                     #f)))
       options))

(define (suitable-options options patterns underlying-price)
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
                      [closest-back-dte (foldl (λ (o res) (if (and (< (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte o)))
                                                                      (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte res))))
                                                                   (>= (option-dte o) (+ 21 (option-dte closest-front-dte))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= (option-dte closest-front-dte) (option-dte o))
                                                                                   (= (option-dte closest-back-dte) (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [long-call (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                            (index-of eligible-strikes (option-strike o))
                                                            (<= (abs (- underlying-price (option-strike o)))
                                                                (abs (- underlying-price (option-strike res))))
                                                            (equal? (option-call-put o) "Call"))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) "Call"))
                                                           o]
                                                          [else res]))
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
                                               (last options)
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
                      [closest-back-dte (foldl (λ (o res) (if (and (< (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte o)))
                                                                      (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte res))))
                                                                   (>= (option-dte o) (+ 21 (option-dte closest-front-dte))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= (option-dte closest-front-dte) (option-dte o))
                                                                                   (= (option-dte closest-back-dte) (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [long-put (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                           (index-of eligible-strikes (option-strike o))
                                                           (<= (abs (- underlying-price (option-strike o)))
                                                               (abs (- underlying-price (option-strike res))))
                                                           (equal? (option-call-put o) "Put"))
                                                      o
                                                      res))
                                       (first options)
                                       options)]
                      [short-put (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                               (= (option-strike o) (option-strike long-put))
                                                               (equal? (option-call-put o) "Put"))
                                                          o]
                                                         [else res]))
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
                                               (last options)
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
        [(or (string-contains? patterns "PC"))
         (hash "Put Condor"
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
        [(or (string-contains? patterns "EC"))
         (hash "Call Horizontal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                                  (abs (- 14 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (and (< (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte o)))
                                                                      (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte res))))
                                                                   (>= (option-dte o) (+ 21 (option-dte closest-front-dte))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= (option-dte closest-front-dte) (option-dte o))
                                                                                   (= (option-dte closest-back-dte) (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [long-call (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                            (index-of eligible-strikes (option-strike o))
                                                            (<= (abs (- underlying-price (option-strike o)))
                                                                (abs (- underlying-price (option-strike res))))
                                                            (equal? (option-call-put o) "Call"))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) "Call"))
                                                           o]
                                                          [else res]))
                                         (first options)
                                         options)])
                 (list short-call long-call))
               "Put Horizontal Spread"
               (let* ([closest-front-dte (foldl (λ (o res) (if (< (abs (- 14 (option-dte o)))
                                                                  (abs (- 14 (option-dte res))))
                                                               o
                                                               res))
                                                (first options)
                                                options)]
                      [closest-back-dte (foldl (λ (o res) (if (and (< (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte o)))
                                                                      (abs (- (+ 28 (option-dte closest-front-dte)) (option-dte res))))
                                                                   (>= (option-dte o) (+ 21 (option-dte closest-front-dte))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= (option-dte closest-front-dte) (option-dte o))
                                                                                   (= (option-dte closest-back-dte) (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [long-put (foldl (λ (o res) (if (and (= (option-dte o) (option-dte closest-back-dte))
                                                           (index-of eligible-strikes (option-strike o))
                                                           (<= (abs (- underlying-price (option-strike o)))
                                                               (abs (- underlying-price (option-strike res))))
                                                           (equal? (option-call-put o) "Put"))
                                                      o
                                                      res))
                                       (first options)
                                       options)]
                      [short-put (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                               (= (option-strike o) (option-strike long-put))
                                                               (equal? (option-call-put o) "Put"))
                                                          o]
                                                         [else res]))
                                        (first options)
                                        options)])
                 (list short-put long-put)))]
        [else (hash)]))
