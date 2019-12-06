#lang racket/base

(require gregor
         racket/contract
         racket/list
         racket/stream) ; needed for gen:stream

(provide (struct-out dv)
         (struct-out dohlc)
         (struct-out test)
         test-timeframe-minus-1
         (struct-out trade)
         (struct-out position)
         (struct-out history)
         (struct-out msis)
         (struct-out option)
         (contract-out
          [struct order
            ((strategy (or/c 'long-call 'long-put
                             'bull-call-vertical-spread 'bear-call-vertical-spread
                             'bull-put-vertical-spread 'bear-put-vertical-spread))
             (symbol string?)
             (expiration date?)
             (strike rational?)
             (call-put (or/c 'call 'put))
             (quantity (or/c rational? #f))
             (price rational?)
             (stock-entry rational?)
             (stock-stop (or/c rational? #f))
             (stock-target (or/c rational? #f))
             (end-date (or/c date? #f)))]))

(struct dv (date value)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (cond
       [(dv? stream) #f]
       [else (empty? stream)]))
   (define (stream-first stream)
     (cond
       [(dv? stream) (dv-date stream)]
       [else (first stream)]))
   (define (stream-rest stream)
     (cond
       [(dv? stream) (list (dv-value stream))]
       [else (rest stream)]))])

(struct dohlc (date open high low close)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (cond
       [(dohlc? stream) #f]
       [else (empty? stream)]))
   (define (stream-first stream)
     (cond
       [(dohlc? stream) (dohlc-date stream)]
       [else (first stream)]))
   (define (stream-rest stream)
     (cond
       [(dohlc? stream) (list (dohlc-open stream)
                              (dohlc-high stream)
                              (dohlc-low stream)
                              (dohlc-close stream))]
       [else (rest stream)]))])

(struct test (timeframe entry stop target)
  #:transparent)

(define (test-timeframe-minus-1 t)
  (test (- (test-timeframe t) 1)
        (test-entry t)
        (test-stop t)
        (test-target t)))

(struct trade (date price amount test)
  #:transparent)

(struct position (price amount)
  #:transparent)

(struct history (test trade)
  #:transparent)

(struct msis (market sector sector-vs-market industry stock stock-vs-sector next-div-date earnings-date option-spread zacks-rank)
  #:transparent)

(struct option (symbol expiration dte strike call-put date bid mid ask vol delta gamma theta vega rho)
  #:transparent)

(struct order (strategy symbol expiration strike call-put quantity price stock-entry stock-stop stock-target end-date)
  #:transparent)
