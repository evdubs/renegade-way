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

(define (condor-score symbol start-date end-date #:fit-vols [fit-vols? #f])
  (with-handlers ([exn:fail?
                   (λ (e) (list 0 0))])
    (let* ([prices (get-date-ohlc symbol start-date end-date)]
           [options (get-updated-options symbol end-date (dohlc-close (last prices)) #:compute-all-greeks #f #:fit-vols fit-vols?)]
           [call-condor-options (hash-ref (suitable-options options "CC") "Call Condor")]
           [call-low-strike (option-strike (second call-condor-options))]
           [call-high-strike (option-strike (third call-condor-options))]
           [call-cost (+ (option-mid (first call-condor-options))
                         (- (option-mid (second call-condor-options)))
                         (- (option-mid (third call-condor-options)))
                         (option-mid (fourth call-condor-options)))]
           [call-reward (- (option-strike (second call-condor-options))
                           (option-strike (first call-condor-options))
                           call-cost)]
           [call-risk (max call-cost
                           (+ call-cost
                              (option-strike (first call-condor-options))
                              (- (option-strike (second call-condor-options)))
                              (- (option-strike (third call-condor-options)))
                              (option-strike (fourth call-condor-options))))]
           [call-avg (mean (map (λ (price) (if (and (<= call-low-strike (dohlc-close price))
                                                    (>= call-high-strike (dohlc-close price)))
                                               1 0)) prices))]
           [put-condor-options (hash-ref (suitable-options options "PC") "Put Condor")]
           [put-high-strike (option-strike (second put-condor-options))]
           [put-low-strike (option-strike (third put-condor-options))]
           [put-cost (+ (option-mid (first put-condor-options))
                        (- (option-mid (second put-condor-options)))
                        (- (option-mid (third put-condor-options)))
                        (option-mid (fourth put-condor-options)))]
           [put-reward (- (option-strike (first put-condor-options))
                          (option-strike (second put-condor-options))
                          put-cost)]
           [put-risk (max put-cost
                          (+ put-cost
                             (- (option-strike (first put-condor-options)))
                             (option-strike (second put-condor-options))
                             (option-strike (third put-condor-options))
                             (- (option-strike (fourth put-condor-options)))))]
           [put-avg (mean (map (λ (price) (if (and (<= put-low-strike (dohlc-close price))
                                                   (>= put-high-strike (dohlc-close price)))
                                              1 0)) prices))])
      ; choose the riskier condor as it is more likely to be marketable
      (if (>= call-risk put-risk)
          (list (if (void? call-avg) 0 (* 100 call-avg))
                (if (= 0 call-risk) 0 (* 100 (/ call-reward call-risk))))
          (list (if (void? put-avg) 0 (* 100 put-avg))
                (if (= 0 put-risk) 0 (* 100 (/ put-reward put-risk))))))))

(define (run-condor-analysis market sector start-date end-date #:fit-vols [fit-vols? #f])
  (let ([new-condor-analysis-list (get-condor-analysis market end-date)]
        [new-condor-analysis-hash (make-hash)])
    ; this lets us avoid calling (condor-score) with an empty symbol
    (hash-set! new-condor-analysis-hash "" (list 0 0))
    (for-each (λ (ca)
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-market ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-market ca)
                                  (condor-score (condor-analysis-market ca) start-date end-date #:fit-vols fit-vols?))])
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-sector ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-sector ca)
                                  (condor-score (condor-analysis-sector ca) start-date end-date #:fit-vols fit-vols?))])
                (cond [(not (hash-has-key? new-condor-analysis-hash (condor-analysis-industry ca)))
                       (hash-set! new-condor-analysis-hash (condor-analysis-industry ca)
                                  (condor-score (condor-analysis-industry ca) start-date end-date #:fit-vols fit-vols?))])
                (hash-set! new-condor-analysis-hash (condor-analysis-stock ca)
                           (condor-score (condor-analysis-stock ca) start-date end-date #:fit-vols fit-vols?)))
              new-condor-analysis-list)
    (set! condor-analysis-list (sort new-condor-analysis-list >
                                     #:key (λ (x) (let ([ref (hash-ref new-condor-analysis-hash (condor-analysis-stock x))])
                                                    (* (first ref) (min 85 (second ref)))))))
    (set! condor-analysis-hash new-condor-analysis-hash)))
