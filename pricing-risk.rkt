#lang racket/base

(require math/distributions)

(provide black-scholes
         black-scholes-delta
         black-scholes-gamma
         black-scholes-theta
         black-scholes-vega
         black-scholes-rho)

(define (black-scholes price years-left strike call-put rate vol)
  (let* ([d-1 (* (/ 1 (* vol (sqrt years-left)))
                 (+ (log (/ price strike))
                    (* (+ rate (/ (* vol vol) 2))
                       years-left)))]
         [d-2 (- d-1 (* vol (sqrt years-left)))]
         [pv (* strike (exp (* -1 rate years-left)))])
    (cond [(equal? call-put 'Call)
           (- (* (cdf (normal-dist) d-1) price)
              (* (cdf (normal-dist) d-2) pv))]
          [(equal? call-put 'Put)
           (- (* (cdf (normal-dist) (* d-2 -1)) pv)
              (* (cdf (normal-dist) (* d-1 -1)) price))])))

(define (black-scholes-delta price years-left strike call-put rate vol)
  (* (- (black-scholes (+ price 1/100) years-left strike call-put rate vol)
        (black-scholes price years-left strike call-put rate vol))
     100))

(define (black-scholes-gamma price years-left strike call-put rate vol)
  (* (- (black-scholes-delta (+ price 1/100) years-left strike call-put rate vol)
        (black-scholes-delta price years-left strike call-put rate vol))
     100))

(define (black-scholes-theta price years-left strike call-put rate vol)
  (- (black-scholes price (max (+ years-left 1/365) 0) strike call-put rate vol)
     (black-scholes price years-left strike call-put rate vol)))

(define (black-scholes-vega price years-left strike call-put rate vol)
  (- (black-scholes price years-left strike call-put rate (+ vol 1/100))
     (black-scholes price years-left strike call-put rate vol)))

(define (black-scholes-rho price years-left strike call-put rate vol)
  (- (black-scholes price years-left strike call-put (+ rate 1/100) vol)
     (black-scholes price years-left strike call-put rate vol)))
