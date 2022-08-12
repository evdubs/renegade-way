#lang racket/base

(require math/statistics
         racket/list
         "db-queries.rkt"
         "option-strategy.rkt"
         "structs.rkt")

(provide condor-analysis-hash
         condor-analysis-list
         run-condor-analysis)

(define condor-analysis-list (list))

(define condor-analysis-hash (hash))

(define (condor-score symbol start-date end-date)
  (with-handlers ([exn:fail?
                   (λ (e) ; (displayln (string-append "Failed to produce a condor score for "
                          ;                           symbol " " start-date " " end-date))
                      (list 0 0))])
    (let* ([prices (get-date-ohlc symbol start-date end-date)]
           [options (get-updated-options symbol end-date (dohlc-close (last prices)) #:compute-all-greeks #f)]
           [condor-options (hash-ref (suitable-options options "CC") "Call Condor")]
           [low-strike (option-strike (second condor-options))]
           [high-strike (option-strike (third condor-options))]
           [cost (+ (option-mid (first condor-options))
                    (- (option-mid (second condor-options)))
                    (- (option-mid (third condor-options)))
                    (option-mid (fourth condor-options)))]
           [reward (- (option-strike (second condor-options))
                      (option-strike (first condor-options))
                      cost)]
           [risk (max cost
                      (+ cost
                         (option-strike (first condor-options))
                         (- (option-strike (second condor-options)))
                         (- (option-strike (third condor-options)))
                         (option-strike (fourth condor-options))))]
           [avg (mean (map (λ (price) (if (and (<= low-strike (dohlc-close price))
                                               (>= high-strike (dohlc-close price)))
                                          1 0)) prices))])
      (list (if (void? avg) 0 (* 100 avg))
            (if (= 0 risk) 0 (* 100 (/ reward risk)))))))

(define (run-condor-analysis market sector start-date end-date)
  (let ([new-condor-analysis-list (get-condor-analysis market end-date)]
        [new-condor-analysis-hash (make-hash)])
    ; this lets us avoid calling (condor-score) with an empty symbol
    (hash-set! new-condor-analysis-hash "" (list 0 0))
    (for-each (λ (ca)
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-market ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-market ca) (condor-score (condor-analysis-market ca) start-date end-date))])
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-sector ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-sector ca) (condor-score (condor-analysis-sector ca) start-date end-date))])
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-industry ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-industry ca) (condor-score (condor-analysis-industry ca) start-date end-date))])
                (hash-set! new-condor-analysis-hash (condor-analysis-stock ca) (condor-score (condor-analysis-stock ca) start-date end-date)))
              new-condor-analysis-list)
    (set! condor-analysis-list (sort new-condor-analysis-list >
                                     #:key (λ (x) (let ([ref (hash-ref new-condor-analysis-hash (condor-analysis-stock x))])
                                                    (* (first ref) (min 85/100 (second ref)))))))
    (set! condor-analysis-hash new-condor-analysis-hash)))
