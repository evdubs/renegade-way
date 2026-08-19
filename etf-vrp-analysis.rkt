#lang racket/base

(require gregor
         racket/contract
         racket/list
         "db-queries.rkt"
         "option-strategy.rkt"
         "pricing-risk.rkt"
         "structs.rkt")

(provide etf-vrp-analysis-list
         (contract-out
          [run-etf-vrp-analysis (->* (date?) (#:fit-vols boolean?) void?)]))

(define etf-vrp-analysis-list (list))

(define (run-etf-vrp-analysis end-date #:fit-vols [fit-vols? #f])
  (define eva (get-etf-vrp-analysis end-date))

  (set! etf-vrp-analysis-list
        (map (λ (analysis-for-symbol)
               (define symbol (etf-vrp-analysis-etf analysis-for-symbol))
               (define prices (get-date-ohlc symbol (-days end-date 7) end-date))
               (define options (get-updated-options symbol end-date (dohlc-close (last prices)) #:compute-all-greeks #f #:fit-vols fit-vols?))
               (define call-horizontal-options (hash-ref (suitable-options options "VR" (dohlc-close (last prices))) "Call Horizontal Spread"))

               (define short-call (first call-horizontal-options))
               (define long-call (second call-horizontal-options))

               (define days-in-this-year (days-in-year (->year end-date)))
               (define divs (map (λ (div) (vector (/ (vector-ref div 0) days-in-this-year)
                                                  (vector-ref div 1)))
                                 (get-dividend-estimates symbol
                                                         end-date
                                                         (+months end-date 2))))
               (define 1-month-rate (get-1-month-rate end-date))

               (define fwd-vol
                 (if (= (option-dte long-call) (option-dte short-call)) #f
                     (sqrt (/ (- (* (/ (option-dte long-call) days-in-this-year) (option-vol long-call) (option-vol long-call))
                                 (* (/ (option-dte short-call) days-in-this-year) (option-vol short-call) (option-vol short-call)))
                              (/ (- (option-dte long-call) (option-dte short-call)) days-in-this-year)))))
               
               (define spread-price (- (option-mid long-call) (option-mid short-call)))

               (define ffwd-vol
                 (if (= (option-dte long-call) (option-dte short-call)) #f
                     (flat-forward (option-vol long-call) spread-price short-call long-call
                                   (dohlc-close (last prices)) days-in-this-year 1-month-rate divs)))

               (struct-copy etf-vrp-analysis analysis-for-symbol
                            [30d-60d-fwd-vol fwd-vol]
                            [30d-60d-flat-fwd-vol ffwd-vol]
                            [flat-fwd-to-fwd-ratio (if (and ffwd-vol fwd-vol) (/ ffwd-vol fwd-vol) #f)]))
             eva)))

; newton's method
; the new-vol - (price difference / vega difference) ratio is divided by 100 as vega represents a 1% change of vol (not a 1 unit change).
(define (flat-forward vol target-spread-price short-call long-call underlying-price days-in-this-year rate divs)
  (define short-call-price (black-scholes underlying-price
                                          (/ (option-dte short-call) days-in-this-year)
                                          (option-strike short-call)
                                          (option-call-put short-call)
                                          rate
                                          vol
                                          divs))
  
  (define long-call-price (black-scholes underlying-price
                                         (/ (option-dte long-call) days-in-this-year)
                                         (option-strike long-call)
                                         (option-call-put long-call)
                                         rate
                                         vol
                                         divs))

  (define short-call-vega (black-scholes-vega underlying-price
                                              (/ (option-dte short-call) days-in-this-year)
                                              (option-strike short-call)
                                              (option-call-put short-call)
                                              rate
                                              vol
                                              divs))

  (define long-call-vega (black-scholes-vega underlying-price
                                             (/ (option-dte long-call) days-in-this-year)
                                             (option-strike long-call)
                                             (option-call-put long-call)
                                             rate
                                             vol
                                             divs))

  (cond [(or (< (abs (- (- long-call-price short-call-price) target-spread-price)) 1e-6)
             (< vol 0.0))
         vol]
        [else
         (define new-vol (- vol (/ (- (- long-call-price short-call-price) target-spread-price)
                                   (- long-call-vega short-call-vega)
                                   100)))
         (flat-forward new-vol target-spread-price short-call long-call underlying-price days-in-this-year rate divs)]))
