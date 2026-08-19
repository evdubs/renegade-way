#lang racket/base

(require math/distributions
         racket/contract)

(provide (contract-out
          [black-scholes (-> rational? rational? rational? (or/c 'call 'put) rational?
                             rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-delta (-> rational? rational? rational? (or/c 'call 'put) rational?
                                   rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-gamma (-> rational? rational? rational? (or/c 'call 'put) rational?
                                   rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-theta (-> rational? rational? rational? (or/c 'call 'put) rational?
                                   rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-vega (-> rational? rational? rational? (or/c 'call 'put) rational?
                                  rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-rho (-> rational? rational? rational? (or/c 'call 'put) rational?
                                 rational? (listof (vectorof rational?)) rational?)]
          [black-scholes-implied-vol (->* (rational? rational? rational? (or/c 'call 'put) rational?
                                                     rational? (listof (vectorof rational?)) rational?)
                                          (#:low-vol rational? #:high-vol rational?) rational?)]))

(define (black-scholes price years-left strike call-put rate vol divs)
  (let* ([price (exact->inexact price)]
         [years-left (exact->inexact years-left)]
         [strike (exact->inexact strike)]
         [rate (exact->inexact rate)]
         [vol (exact->inexact vol)]
         [discounted-price
          (- price (foldl (λ (div res)
                            (if (>= years-left (vector-ref div 0))
                                (+ res (* (vector-ref div 1)
                                          (exp (* -1 rate (vector-ref div 0)))))
                                res))
                          0
                          divs))]
         [d-1 (* (/ 1 (* vol (sqrt years-left)))
                 (+ (log (/ discounted-price strike))
                    (* (+ rate (/ (* vol vol) 2))
                       years-left)))]
         [d-2 (- d-1 (* vol (sqrt years-left)))]
         [pv (* strike (exp (* -1 rate years-left)))])
    ; use (flnormal-cdf) instead of (cdf (normal-dist)) for performance.
    ; condor analysis takes 2 minutes instead of 3 with this optimization.
    (cond [(equal? call-put 'call)
           (- (* (flnormal-cdf 0.0 1.0 d-1 #f #f) discounted-price)
              (* (flnormal-cdf 0.0 1.0 d-2 #f #f) pv))]
          [(equal? call-put 'put)
           (- (* (flnormal-cdf 0.0 1.0 (* d-2 -1) #f #f) pv)
              (* (flnormal-cdf 0.0 1.0 (* d-1 -1) #f #f) discounted-price))])))

(define (black-scholes-delta price years-left strike call-put rate vol divs)
  (* (- (black-scholes (+ price 1/100) years-left strike call-put rate vol divs)
        (black-scholes price years-left strike call-put rate vol divs))
     100))

(define (black-scholes-gamma price years-left strike call-put rate vol divs)
  (* (- (black-scholes-delta (+ price 1/100) years-left strike call-put rate vol divs)
        (black-scholes-delta price years-left strike call-put rate vol divs))
     100))

(define (black-scholes-theta price years-left strike call-put rate vol divs)
  (- (black-scholes price (max (- years-left 1/365) 1/1000000) strike call-put rate vol divs)
     (black-scholes price years-left strike call-put rate vol divs)))

(define (black-scholes-vega price years-left strike call-put rate vol divs)
  (- (black-scholes price years-left strike call-put rate (+ vol 1/100) divs)
     (black-scholes price years-left strike call-put rate vol divs)))

(define (black-scholes-rho price years-left strike call-put rate vol divs)
  (- (black-scholes price years-left strike call-put (+ rate 1/100) vol divs)
     (black-scholes price years-left strike call-put rate vol divs)))

; this implementation is a hybrid of the newton-raphson method and bisection.
; the guess - (price difference / vega) ratio is divided by 100 as vega represents a 1% change of vol (not a 1 unit change).
(define (black-scholes-implied-vol price years-left strike call-put rate option-price divs vol-guess
                                   #:low-vol [low-vol 1e-5] #:high-vol [high-vol 5.0])
  (define bs-price (black-scholes price years-left strike call-put rate vol-guess divs))
  (define vega (black-scholes-vega price years-left strike call-put rate vol-guess divs))
  (define next-vol-guess (if (< (abs vega) 1e-6) (/ (+ low-vol high-vol) 2.0)
                             (- vol-guess (/ (- bs-price option-price) vega 100.0))))

  (set! next-vol-guess (max low-vol (min high-vol next-vol-guess)))

  (if (or (= +nan.0 vol-guess)
          (= low-vol vol-guess high-vol)
          (< (abs (- bs-price option-price)) 1e-6))
      vol-guess
      (black-scholes-implied-vol price years-left strike call-put rate option-price divs next-vol-guess
                                 #:low-vol (if (> 0.0 (- bs-price option-price)) next-vol-guess low-vol)
                                 #:high-vol (if (< 0.0 (- bs-price option-price)) next-vol-guess high-vol))))
