#lang racket/base

(require gregor
         racket/contract
         racket/list
         racket/stream) ; needed for gen:stream

(provide (struct-out test)
         test-timeframe-minus-1
         (struct-out trade)
         (struct-out position)
         (struct-out history)
         (contract-out
          [struct dv
            ((date integer?) ; posix time for date
             (value any/c))]
          [struct dohlc
            ((date integer?) ; posix time for date
             (open rational?)
             (high rational?)
             (low rational?)
             (close rational?))]
          [struct price-analysis
            ((market string?)
             (sector string?)
             (sector-vs-market rational?)
             (industry (or/c string? #f))
             (stock string?)
             (stock-vs-sector rational?)
             (next-div-date (or/c date? #f))
             (earnings-date (or/c date? #f))
             (option-spread rational?)
             (zacks-rank (or/c 'strong-buy 'buy 'hold 'sell 'strong-sell #f))
             (is-weekly boolean?))]
          [struct rank-analysis
            ((market string?)
             (market-rank rational?)
             (sector string?)
             (sector-rank rational?)
             (industry (or/c string? #f))
             (industry-rank (or/c rational? #f))
             (stock string?)
             (stock-rank rational?)
             (stock-best-rank rational?)
             (stock-avg-rank rational?)
             (stock-worst-rank rational?)
             (earnings-date (or/c date? #f))
             (option-spread rational?)
             (is-weekly boolean?))]
          [struct vol-analysis
            ((market string?)
             (market-iv rational?)
             (market-iv-rank rational?)
             (sector (or/c string? #f))
             (sector-iv rational?)
             (sector-iv-rank rational?)
             (industry (or/c string? #f))
             (industry-iv rational?)
             (industry-iv-rank rational?)
             (stock string?)
             (stock-iv rational?)
             (stock-iv-rank rational?)
             (earnings-date (or/c date? #f))
             (option-spread rational?)
             (is-weekly boolean?))]
          [struct condor-analysis
            ((market string?)
             (market-rtg (or/c rational? #f))
             (market-rr (or/c rational? #f))
             (sector (or/c string? #f))
             (sector-rtg (or/c rational? #f))
             (sector-rr (or/c rational? #f))
             (industry (or/c string? #f))
             (industry-rtg (or/c rational? #f))
             (industry-rr (or/c rational? #f))
             (stock string?)
             (stock-rtg (or/c rational? #f))
             (stock-rr (or/c rational? #f))
             (earnings-date (or/c date? #f))
             (option-spread rational?)
             (is-weekly boolean?))]
          [struct earnings-vibes-analysis
            ((stock string?)
             (min-expiration date?)
             (max-expiration date?)
             (strike rational?)
             (vol-slope rational?)
             (iv-hv rational?)
             (price-strike-ratio (or/c rational? #f))
             (earnings-date date?)
             (earnings-when (or/c 'before-market-open 'after-market-close))
             (option-spread rational?)
             (30d-avg-volume rational?))]
          [struct etf-vrp-analysis
            ((etf string?)
             (iv-hv rational?)
             (ivp-1yr rational?)
             (30d-60d-fwd-vol (or/c number? #f))
             (30d-60d-flat-fwd-vol (or/c rational? #f))
             (flat-fwd-to-fwd-ratio (or/c number? #f))
             (option-spread rational?))]
          [struct forward-factor-analysis
            ((stock string?)
             (front-exp date?)
             (front-vol rational?)
             (back-exp date?)
             (back-vol rational?)
             (vol-ratio rational?)
             (forward-vol rational?)
             (forward-factor rational?)
             (earnings-date (or/c date? #f))
             (option-spread rational?))]
          [struct position-analysis
            ((sector string?)
             (stock string?)
             (expiration date?)
             (strike rational?)
             (call-put (or/c 'call 'put))
             (account string?)
             (signed-shares rational?)
             (stock-low-stop (or/c rational? #f))
             (stock-low-target (or/c rational? #f))
             (stock-close (or/c rational? #f))
             (stock-high-stop (or/c rational? #f))
             (stock-high-target (or/c rational? #f))
             (end-date (or/c date? #f))
             (strategy (or/c 'long-call 'long-put
                             'bull-call-vertical-spread 'bear-call-vertical-spread
                             'bull-put-vertical-spread 'bear-put-vertical-spread
                             'long-straddle 'long-strangle
                             'call-ratio-spread 'put-ratio-spread
                             'call-horizontal-spread 'put-horizontal-spread
                             'call-double-horizontal-spread 'put-double-horizontal-spread
                             'call-diagonal-spread 'put-diagonal-spread
                             'call-butterfly 'call-condor
                             'put-butterfly 'put-condor)))]
          [struct position-greeks
            ((sector string?)
             (stock string?)
             (account string?)
             (delta rational?)
             (gamma rational?)
             (theta rational?)
             (vega rational?)
             (rho rational?))]
          [struct option
            ((symbol string?)
             (expiration date?)
             (dte rational?)
             (strike rational?)
             (call-put (or/c 'call 'put))
             (date date?)
             (bid (or/c rational? #f))
             (mid (or/c rational? #f))
             (ask (or/c rational? #f))
             (vol (or/c rational? #f))
             (delta (or/c rational? #f))
             (gamma (or/c rational? #f))
             (theta (or/c rational? #f))
             (vega (or/c rational? #f))
             (rho (or/c rational? #f)))]
          [struct order
            ((pattern (or/c 'bull-pullback 'bear-rally
                            'high-base 'low-base
                            'ascending-triangle 'descending-triangle
                            'range-rally 'range-pullback
                            'increasing-rank 'decreasing-rank
                            'increasing-vol 'decreasing-vol
                            'call-condor 'earnings-calendar
                            'volatility-risk-premium 'forward-factor))
             (strategy (or/c 'long-call 'long-put
                             'bull-call-vertical-spread 'bear-call-vertical-spread
                             'bull-put-vertical-spread 'bear-put-vertical-spread
                             'long-straddle 'long-strangle
                             'call-ratio-spread 'put-ratio-spread
                             'call-horizontal-spread 'put-horizontal-spread
                             'call-double-horizontal-spread 'put-double-horizontal-spread
                             'call-diagonal-spread 'put-diagonal-spread
                             'call-butterfly 'call-condor
                             'put-butterfly 'put-condor))
             (symbol string?)
             (expiration date?)
             (strike rational?)
             (call-put (or/c 'call 'put))
             (quantity (or/c rational? #f))
             (price rational?)
             (vol rational?)
             (implied-vol (or/c rational? #f))
             (spread rational?)
             (stock-entry rational?)
             (stock-low-stop (or/c rational? #f))
             (stock-low-target (or/c rational? #f))
             (stock-high-stop (or/c rational? #f))
             (stock-high-target (or/c rational? #f))
             (entry-date (or/c date? #f))
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

(struct test (timeframe entry low-stop low-target high-stop high-target)
  #:transparent)

(define (test-timeframe-minus-1 t)
  (test (- (test-timeframe t) 1)
        (test-entry t)
        (test-low-stop t)
        (test-low-target t)
        (test-high-stop t)
        (test-high-target t)))

(struct trade (date price amount test)
  #:transparent)

(struct position (price amount)
  #:transparent)

(struct history (test trade)
  #:transparent)

(struct price-analysis (market sector sector-vs-market industry stock stock-vs-sector next-div-date earnings-date option-spread zacks-rank is-weekly)
  #:transparent)

(struct rank-analysis (market market-rank sector sector-rank industry industry-rank stock stock-rank stock-best-rank stock-avg-rank
                              stock-worst-rank earnings-date option-spread is-weekly)
  #:transparent)

(struct vol-analysis (market market-iv market-iv-rank sector sector-iv sector-iv-rank industry industry-iv industry-iv-rank
                             stock stock-iv stock-iv-rank earnings-date option-spread is-weekly)
  #:transparent)

(struct condor-analysis (market market-rtg market-rr sector sector-rtg sector-rr industry industry-rtg industry-rr stock stock-rtg
                                stock-rr earnings-date option-spread is-weekly)
  #:transparent)

(struct earnings-vibes-analysis (stock min-expiration max-expiration strike vol-slope iv-hv price-strike-ratio earnings-date earnings-when
                                       option-spread 30d-avg-volume)
  #:transparent)

(struct etf-vrp-analysis (etf iv-hv ivp-1yr 30d-60d-fwd-vol 30d-60d-flat-fwd-vol flat-fwd-to-fwd-ratio option-spread)
  #:transparent)

(struct forward-factor-analysis (stock front-exp front-vol back-exp back-vol vol-ratio forward-vol forward-factor earnings-date option-spread)
  #:transparent)

(struct position-analysis (sector stock expiration strike call-put account signed-shares stock-low-stop stock-low-target stock-close
                                  stock-high-stop stock-high-target end-date strategy)
  #:transparent)

(struct position-greeks (sector stock account delta gamma theta vega rho)
  #:transparent)

(struct option (symbol expiration dte strike call-put date bid mid ask vol delta gamma theta vega rho)
  #:transparent)

(struct order (pattern strategy symbol expiration strike call-put quantity price vol implied-vol spread stock-entry stock-low-stop stock-low-target
                       stock-high-stop stock-high-target entry-date end-date)
  #:transparent)
