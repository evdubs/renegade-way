#lang racket/base

(require gregor
         math/matrix
         math/statistics
         racket/contract
         racket/list
         racket/string
         threading
         "db-queries.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide (contract-out
          [compute-implied-vol (-> option? rational? rational?)]
          [compute-price-greeks (-> option? rational? option?)]
          [get-updated-options (->* (string? date? rational?) (#:compute-all-greeks boolean? #:fit-vols boolean?) (listof option?))]
          [suitable-options (-> (listof option?) string? rational? (hash/c string? (listof option?)))]))

; taken from alex-hhh/data-frame ... least-squares-fit.rkt
(define (polynomial-fit-coefficients xs ys nitems degree)
  (define y-matrix (list->matrix nitems 1 ys))
  (define x-matrix (vandermonde-matrix xs (add1 degree)))
  (define x-matrix-transposed (matrix-transpose x-matrix))
  (define x (matrix* x-matrix-transposed x-matrix))
  (define y (matrix* x-matrix-transposed y-matrix))
  (matrix->list (matrix-solve x y)))

(define (compute-implied-vol opt ref-price)
  (define days-in-this-year (days-in-year (->year (option-date opt))))
  (define divs (map (λ (div) (vector (/ (vector-ref div 0) days-in-this-year)
                                     (vector-ref div 1)))
                    (get-dividend-estimates (option-symbol opt)
                                            (option-date opt)
                                            (option-expiration opt))))
  (define 1-month-rate (get-1-month-rate (option-date opt)))
  (black-scholes-implied-vol ref-price
                             (/ (option-dte opt) days-in-this-year)
                             (option-strike opt)
                             (option-call-put opt)
                             1-month-rate
                             (option-mid opt)
                             divs
                             (option-vol opt)))

(define (compute-price-greeks opt ref-price)
  (define days-in-this-year (days-in-year (->year (option-date opt))))
  (define divs (map (λ (div) (vector (/ (vector-ref div 0) days-in-this-year)
                                     (vector-ref div 1)))
                    (get-dividend-estimates (option-symbol opt)
                                            (option-date opt)
                                            (option-expiration opt))))
  (define 1-month-rate (get-1-month-rate (option-date opt)))
  (struct-copy option opt
               [mid (black-scholes ref-price
                                   (/ (option-dte opt) days-in-this-year)
                                   (option-strike opt)
                                   (option-call-put opt)
                                   1-month-rate
                                   (option-vol opt)
                                   divs)]
               [delta (black-scholes-delta ref-price
                                           (/ (option-dte opt) days-in-this-year)
                                           (option-strike opt)
                                           (option-call-put opt)
                                           1-month-rate
                                           (option-vol opt)
                                           divs)]
               [gamma (black-scholes-gamma ref-price
                                           (/ (option-dte opt) days-in-this-year)
                                           (option-strike opt)
                                           (option-call-put opt)
                                           1-month-rate
                                           (option-vol opt)
                                           divs)]
               [theta (black-scholes-theta ref-price
                                           (/ (option-dte opt) days-in-this-year)
                                           (option-strike opt)
                                           (option-call-put opt)
                                           1-month-rate
                                           (option-vol opt)
                                           divs)]
               [vega (black-scholes-vega ref-price
                                         (/ (option-dte opt) days-in-this-year)
                                         (option-strike opt)
                                         (option-call-put opt)
                                         1-month-rate
                                         (option-vol opt)
                                         divs)]
               [rho (black-scholes-rho ref-price
                                       (/ (option-dte opt) days-in-this-year)
                                       (option-strike opt)
                                       (option-call-put opt)
                                       1-month-rate
                                       (option-vol opt)
                                       divs)]))

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
         (define days-in-this-year (days-in-year (->year date)))
         (define divs (map (λ (div) (vector (/ (vector-ref div 0) days-in-this-year)
                                            (vector-ref div 1)))
                           (get-dividend-estimates symbol
                                                   date
                                                   (option-expiration o))))
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
                                (option-call-put o)
                                1-month-rate
                                vol
                                divs)
                 (option-ask o)
                 vol
                 (black-scholes-delta ref-price
                                      (/ (option-dte o) days-in-this-year)
                                      (option-strike o)
                                      (option-call-put o)
                                      1-month-rate
                                      vol
                                      divs)
                 (if compute-all-greeks?
                     (black-scholes-gamma ref-price
                                          (/ (option-dte o) days-in-this-year)
                                          (option-strike o)
                                          (option-call-put o)
                                          1-month-rate
                                          vol
                                          divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-theta ref-price
                                          (/ (option-dte o) days-in-this-year)
                                          (option-strike o)
                                          (option-call-put o)
                                          1-month-rate
                                          vol
                                          divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-vega ref-price
                                         (/ (option-dte o) days-in-this-year)
                                         (option-strike o)
                                         (option-call-put o)
                                         1-month-rate
                                         vol
                                         divs)
                     #f)
                 (if compute-all-greeks?
                     (black-scholes-rho ref-price
                                        (/ (option-dte o) days-in-this-year)
                                        (option-strike o)
                                        (option-call-put o)
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
                                                 (equal? (option-call-put o) 'call)))
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
                                                           (equal? (option-call-put o) 'call)))
                                               options))]
                      [short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                             (< (option-delta o) 3/10)
                                                             (equal? (option-call-put o) 'call)))
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
                                                            (equal? (option-call-put o) 'put)))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (> (option-delta o) -25/100)
                                                          (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) 'call))
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
                                                           (equal? (option-call-put o) 'call)))
                                               options))]
                      [short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-front-dte))
                                                             (< (option-delta o) 4/10)
                                                             (equal? (option-call-put o) 'call)))
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
                                                  (equal? (option-call-put o) 'put)))
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
                                                           (equal? (option-call-put o) 'put)))
                                               options))]
                      [short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (> (option-delta o) -3/10)
                                                           (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call)))
                                                options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (< (option-delta o) 25/100)
                                                            (equal? (option-call-put o) 'call)))
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
                                                           (equal? (option-call-put o) 'put))
                                                      o
                                                      res))
                                       (first options)
                                       options)]
                      [short-put (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                               (= (option-strike o) (option-strike long-put))
                                                               (equal? (option-call-put o) 'put))
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
                                                           (equal? (option-call-put o) 'put)))
                                               options))]
                      [short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-front-dte))
                                                           (> (option-delta o) -4/10)
                                                           (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call)))
                                                options))]
                      [long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                           (= (option-strike o) (option-strike closest-strike))
                                                           (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call)))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (< (option-strike o) (option-strike closest-strike))
                                                          (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call)))
                                                options))]
                      [long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                            (> (option-mid short-call) (* 3 (option-mid o))) 
                                                            (equal? (option-call-put o) 'call)))
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
                                                            (equal? (option-call-put o) 'put)))
                                                options))]
                      [long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                          (> (option-mid short-put) (* 3 (option-mid o))) 
                                                          (equal? (option-call-put o) 'put)))
                                              options))])
                 (list short-put long-put)))]
        [(or (string-contains? patterns "RR")
             (string-contains? patterns "RP")
             (string-contains? patterns "DV"))
         (hash "Call Butterfly"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-call (foldl (λ (o res) (if (and (< (abs (- 5/10 (option-delta o)))
                                                                (abs (- 5/10 (option-delta res))))
                                                             (= (option-dte o) (option-dte closest-dte))
                                                             (equal? (option-call-put o) 'call))
                                                        o
                                                        res))
                                         (first options)
                                         options)]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (< (option-strike o) (- (option-strike short-call) (* 2 (option-mid short-call))))
                                                                 (equal? (option-call-put o) 'call)))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (> (option-strike o) (+ (option-strike short-call) (* 2 (option-mid short-call))))
                                                                   (equal? (option-call-put o) 'call)))
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
                                                                  (equal? (option-call-put o) 'call)))
                                                      options))]
                      [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                    (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                    (equal? (option-call-put o) 'call)))
                                                        options))]
                      [long-short-distance (* 1/2 (- (option-strike second-short-call) (option-strike first-short-call)))]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike first-short-call) long-short-distance))
                                                                 (equal? (option-call-put o) 'call)))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (>= (option-strike o) (+ (option-strike second-short-call) long-short-distance))
                                                                   (equal? (option-call-put o) 'call)))
                                                       options))])
                 (list first-long-call first-short-call second-short-call second-long-call))
               "Put Butterfly"
               (let* ([closest-dte (foldl (λ (o res) (if (< (abs (- 28 (option-dte o)))
                                                            (abs (- 28 (option-dte res))))
                                                         o
                                                         res))
                                          (first options)
                                          options)]
                      [short-put (foldl (λ (o res) (if (and (< (abs (- -5/10 (option-delta o)))
                                                               (abs (- -5/10 (option-delta res))))
                                                            (= (option-dte o) (option-dte closest-dte))
                                                            (equal? (option-call-put o) 'put))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (> (option-strike o) (+ (option-strike short-put) (* 2 (option-mid short-put))))
                                                                 (equal? (option-call-put o) 'put)))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (< (option-strike o) (- (option-strike short-put) (* 2 (option-mid short-put))))
                                                                 (equal? (option-call-put o) 'put)))
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
                                                                  (equal? (option-call-put o) 'put)))
                                                      options))]
                      [second-short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (- (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) 'put)))
                                                      options))]
                      [long-short-distance (* 1/2 (- (option-strike first-short-put) (option-strike second-short-put)))]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (>= (option-strike o) (+ (option-strike first-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) 'put)))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike second-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) 'put)))
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
                                                                  (equal? (option-call-put o) 'call)))
                                                      options))]
                      [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                    (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                    (equal? (option-call-put o) 'call)))
                                                        options))]
                      [long-short-distance (* 1/2 (- (option-strike second-short-call) (option-strike first-short-call)))]
                      [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike first-short-call) long-short-distance))
                                                                 (equal? (option-call-put o) 'call)))
                                                     options))]
                      [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (>= (option-strike o) (+ (option-strike second-short-call) long-short-distance))
                                                                   (equal? (option-call-put o) 'call)))
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
                                                                  (equal? (option-call-put o) 'put)))
                                                      options))]
                      [second-short-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                  (< (option-strike o) (- (option-strike closest-strike) (option-mid closest-strike)))
                                                                  (equal? (option-call-put o) 'put)))
                                                      options))]
                      [long-short-distance (* 1/2 (- (option-strike first-short-put) (option-strike second-short-put)))]
                      [first-long-put (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (>= (option-strike o) (+ (option-strike first-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) 'put)))
                                                     options))]
                      [second-long-put (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                 (<= (option-strike o) (- (option-strike second-short-put) long-short-distance))
                                                                 (equal? (option-call-put o) 'put)))
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
                                                            (equal? (option-call-put o) 'call))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) 'call))
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
                                                           (equal? (option-call-put o) 'put))
                                                      o
                                                      res))
                                       (first options)
                                       options)]
                      [short-put (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                               (= (option-strike o) (option-strike long-put))
                                                               (equal? (option-call-put o) 'put))
                                                          o]
                                                         [else res]))
                                        (first options)
                                        options)])
                 (list short-put long-put)))]
        [(or (string-contains? patterns "VR"))
         (hash "Call Horizontal Spread"
               (let* ([closest-back-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                                 (abs (- 56 (option-dte res))))
                                                              o
                                                              res))
                                               (first options)
                                               options)]
                      [closest-front-dte (foldl (λ (o res) (if (and (< (abs (- (- (option-dte closest-back-dte) 28) (option-dte o)))
                                                                       (abs (- (- (option-dte closest-back-dte) 28) (option-dte res))))
                                                                    (> (option-dte o) (- (option-dte closest-back-dte) 21)))
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
                                                            (equal? (option-call-put o) 'call))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) (option-dte closest-front-dte))
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) 'call))
                                                           o]
                                                          [else res]))
                                         (first options)
                                         options)])
                 (list short-call long-call))
               "Call Condor"
               (with-handlers ([exn:fail? (λ (e) (list))])
                 (let* ([closest-back-dte (foldl (λ (o res) (if (< (abs (- 56 (option-dte o)))
                                                                   (abs (- 56 (option-dte res))))
                                                                o
                                                                res))
                                                 (first options)
                                                 options)]
                        ; we first find the closest-back-dte to match the expiration found from the horizontal spread
                        ; once we expand risk per trade, we can reconsider a longer duration trade
                        [closest-dte (foldl (λ (o res) (if (and (< (abs (- (- (option-dte closest-back-dte) 28) (option-dte o)))
                                                                   (abs (- (- (option-dte closest-back-dte) 28) (option-dte res))))
                                                                (> (option-dte o) (- (option-dte closest-back-dte) 21)))
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
                                                                    (equal? (option-call-put o) 'call)))
                                                        options))]
                        [second-short-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                      (> (option-strike o) (+ (option-strike closest-strike) (option-mid closest-strike)))
                                                                      (equal? (option-call-put o) 'call)))
                                                          options))]
                        [long-short-distance (* 1/2 (- (option-strike second-short-call) (option-strike first-short-call)))]
                        [first-long-call (last (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                   (<= (option-strike o) (- (option-strike first-short-call) long-short-distance))
                                                                   (equal? (option-call-put o) 'call)))
                                                       options))]
                        [second-long-call (first (filter (λ (o) (and (= (option-dte o) (option-dte closest-dte))
                                                                     (>= (option-strike o) (+ (option-strike second-short-call) long-short-distance))
                                                                     (equal? (option-call-put o) 'call)))
                                                         options))])
                   (list first-long-call first-short-call second-short-call second-long-call))))]
        [(or (string-contains? patterns "FF"))
         (hash "Call Horizontal Spread"
               (let* ([exp-option-map (~> options
                                          (group-by (λ (o) (option-dte o)) _)
                                          (map (λ (os) (list (option-dte (first os))
                                                             (mean (map (λ (o) (option-vol o)) os)))) _))]
                      ; we should probably use the pivot (either 28 days out or the earnings date)
                      [front-dte (first (foldl (λ (pair res)
                                                 (if (and (> 28 (first pair))
                                                          (>= (second pair) (second res))) pair res))
                                               (list 0 0)
                                               exp-option-map))]
                      [back-dte (first (foldl (λ (pair res)
                                                (if (and (<= 28 (first pair))
                                                         (>= (second pair) (second res))) pair res))
                                              (list 0 0)
                                              exp-option-map))]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= front-dte (option-dte o))
                                                                                   (= back-dte (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [long-call (foldl (λ (o res) (if (and (= (option-dte o) back-dte)
                                                            (index-of eligible-strikes (option-strike o))
                                                            (<= (abs (- underlying-price (option-strike o)))
                                                                (abs (- underlying-price (option-strike res))))
                                                            (equal? (option-call-put o) 'call))
                                                       o
                                                       res))
                                        (first options)
                                        options)]
                      [short-call (foldl (λ (o res) (cond [(and (= (option-dte o) front-dte)
                                                                (= (option-strike o) (option-strike long-call))
                                                                (equal? (option-call-put o) 'call))
                                                           o]
                                                          [else res]))
                                         (first options)
                                         options)])
                 (list short-call long-call))
               "Call Double Horizontal Spread"
               (let* ([exp-option-map (~> options
                                          (group-by (λ (o) (option-dte o)) _)
                                          (map (λ (os) (list (option-dte (first os))
                                                             (mean (map (λ (o) (option-vol o)) os)))) _))]
                      ; we should probably use the pivot (either 28 days out or the earnings date)
                      [front-dte (first (foldl (λ (pair res)
                                                 (if (and (> 28 (first pair))
                                                          (>= (second pair) (second res))) pair res))
                                               (list 0 0)
                                               exp-option-map))]
                      [back-dte (first (foldl (λ (pair res)
                                                (if (and (<= 28 (first pair))
                                                         (>= (second pair) (second res))) pair res))
                                              (list 0 0)
                                              exp-option-map))]
                      [eligible-strikes (let* ([options-at-dtes (filter (λ (o) (or (= front-dte (option-dte o))
                                                                                   (= back-dte (option-dte o))))
                                                                        options)]
                                               [options-by-strike (group-by (λ (o) (option-strike o)) options-at-dtes)]
                                               [options-at-both-dtes (filter (λ (l) (<= 4 (length l))) options-by-strike)])
                                          (remove-duplicates (flatten (map (λ (l) (map (λ (o) (option-strike o)) l))
                                                                           options-at-both-dtes))))]
                      [low-long-call (foldl (λ (o res) (if (and (= (option-dte o) back-dte)
                                                                (index-of eligible-strikes (option-strike o))
                                                                (<= (abs (- 0.65 (option-delta o)))
                                                                    (abs (- 0.65 (option-delta res))))
                                                                (equal? (option-call-put o) 'call))
                                                           o
                                                           res))
                                            (first options)
                                            options)]
                      [low-short-call (foldl (λ (o res) (cond [(and (= (option-dte o) front-dte)
                                                                    (= (option-strike o) (option-strike low-long-call))
                                                                    (equal? (option-call-put o) 'call))
                                                               o]
                                                              [else res]))
                                             (first options)
                                             options)]
                      [high-long-call (foldl (λ (o res) (if (and (= (option-dte o) back-dte)
                                                                 (index-of eligible-strikes (option-strike o))
                                                                 (<= (abs (- 0.35 (option-delta o)))
                                                                     (abs (- 0.35 (option-delta res))))
                                                                 (equal? (option-call-put o) 'call))
                                                            o
                                                            res))
                                             (first options)
                                             options)]
                      [high-short-call (foldl (λ (o res) (cond [(and (= (option-dte o) front-dte)
                                                                     (= (option-strike o) (option-strike high-long-call))
                                                                     (equal? (option-call-put o) 'call))
                                                                o]
                                                               [else res]))
                                              (first options)
                                              options)])
                 (list low-short-call low-long-call high-short-call high-long-call)))]
        [else (hash)]))
