#lang racket/base

(require math/distributions)

(provide black-scholes
         black-scholes-delta
         black-scholes-gamma
         black-scholes-theta
         black-scholes-vega
         black-scholes-rho)

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
    (cond [(or (equal? call-put 'Call) (equal? call-put 'call))
           (- (* (flnormal-cdf 0.0 1.0 d-1 #f #f) discounted-price)
              (* (flnormal-cdf 0.0 1.0 d-2 #f #f) pv))]
          [(or (equal? call-put 'Put) (equal? call-put 'put))
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
