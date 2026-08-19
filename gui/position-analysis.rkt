#lang racket/base

(require gregor
         gregor/period
         racket/async-channel
         racket/class
         racket/contract
         racket/gui/base
         racket/list
         racket/string
         "../db-queries.rkt"
         "../option-strategy.rkt"
         "../web-prices.rkt"
         "../structs.rkt"
         "chart.rkt")

(provide (contract-out
          [position-analysis-box (-> (is-a?/c tab-panel%) date? date? void?)]
          [run-position-analysis (-> string? string? date? date? void?)]))

(define position-panel #f)

(define position-summary #f)

(define position-summary-text "")

(define position-history #f)

(define position-history-text "")

(define open-analysis-box-ref #f)

(define open-position-analysis-list (list))

(define stop-analysis-box-ref #f)

(define stop-position-analysis-list (list))

(define target-analysis-box-ref #f)

(define target-position-analysis-list (list))

(define expired-analysis-box-ref #f)

(define expired-position-analysis-list (list))

(define greeks-box-ref #f)

(define position-greeks-list (list))

(define (run-position-analysis market sector start-date end-date)
  (define position-analysis-list (get-position-analysis end-date))

  (define ref-prices (get-prices (remove-duplicates (map (λ (pa) (position-analysis-stock pa))
                                                         position-analysis-list))))

  (define updated-position-analysis-list
    (map (λ (pa)
           (struct-copy position-analysis pa [stock-close (hash-ref ref-prices (position-analysis-stock pa))]))
         position-analysis-list))

  (define min-max-strikes (get-min-max-strikes position-analysis-list))

  (define bull-bear-roo
    (foldl (λ (p m)
             (let ([s (position-analysis-strategy p)])
               (cond [(or (equal? "LONG CALL" s)
                          (equal? "BULL CALL VERTICAL SPREAD" s)
                          (equal? "BULL PUT VERTICAL SPREAD" s)
                          (equal? "CALL RATIO SPREAD" s)
                          (equal? "CALL DIAGONAL SPREAD" s))
                      (hash-set m (position-analysis-stock p) 'bull)]
                     [(or (equal? "LONG PUT" s)
                          (equal? "BEAR CALL VERTICAL SPREAD" s)
                          (equal? "BEAR PUT VERTICAL SPREAD" s)
                          (equal? "PUT RATIO SPREAD" s)
                          (equal? "PUT DIAGONAL SPREAD" s))
                      (hash-set m (position-analysis-stock p) 'bear)]
                     [(or (equal? "LONG STRADDLE" s)
                          (equal? "LONG STRANGLE" s)
                          (equal? "CALL BUTTERFLY" s)
                          (equal? "PUT BUTTERFLY" s)
                          (equal? "CALL CONDOR" s)
                          (equal? "PUT CONDOR" s)
                          (equal? "CALL HORIZONTAL SPREAD" s)
                          (equal? "PUT HORIZONTAL SPREAD" s)
                          (equal? "CALL DOUBLE HORIZONTAL SPREAD" s)
                          (equal? "PUT DOUBLE HORIZONTAL SPREAD" s))
                      (hash-set m (position-analysis-stock p) 'roo)]
                     [else (hash-set m (position-analysis-stock p) 'unknown)])))
           (hash)
           updated-position-analysis-list))
  (set! position-summary-text (string-append "Live - Bulls: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'bull)))
                                             " Roos: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'roo)))
                                             " Bears: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'bear)))))
  (send position-summary set-label position-summary-text)

  (set! position-history-text (string-append "History - " (get-position-history end-date)))
  (send position-history set-label position-history-text)

  (define position-greeks-hash (make-hash))

  (for-each (λ (pa)
              (define opt (option (position-analysis-stock pa)
                                  (position-analysis-expiration pa)
                                  (period-ref (period-between end-date
                                                              (position-analysis-expiration pa)
                                                              '(days)) 'days)
                                  (position-analysis-strike pa)
                                  (position-analysis-call-put pa)
                                  end-date
                                  #f ; bid
                                  #f ; mid
                                  #f ; ask
                                  (get-closest-vol (position-analysis-stock pa)
                                                   end-date
                                                   (position-analysis-expiration pa)
                                                   (position-analysis-strike pa)
                                                   (position-analysis-call-put pa))
                                  #f ; delta
                                  #f ; gamma
                                  #f ; theta
                                  #f ; vega
                                  #f ; rho
                                  ))
              (define priced-option (compute-price-greeks opt (hash-ref ref-prices (position-analysis-stock pa))))
              (define key (list (position-analysis-stock pa) (position-analysis-account pa)))
              (cond [(hash-has-key? position-greeks-hash key)
                     (define val (hash-ref position-greeks-hash key))
                     (hash-set! position-greeks-hash key
                                (struct-copy position-greeks val
                                             [delta (+ (position-greeks-delta val)
                                                       (* 100.0 (option-delta priced-option) (position-analysis-signed-shares pa)))]
                                             [gamma (+ (position-greeks-gamma val)
                                                       (* 100.0 (option-gamma priced-option) (position-analysis-signed-shares pa)))]
                                             [theta (+ (position-greeks-theta val)
                                                       (* 100.0 (option-theta priced-option) (position-analysis-signed-shares pa)))]
                                             [vega (+ (position-greeks-vega val)
                                                      (* 100.0 (option-vega priced-option) (position-analysis-signed-shares pa)))]
                                             [rho (+ (position-greeks-rho val)
                                                     (* 100.0 (option-rho priced-option) (position-analysis-signed-shares pa)))]))]
                    [else
                     (hash-set! position-greeks-hash key
                                (position-greeks (position-analysis-sector pa)
                                                 (position-analysis-stock pa)
                                                 (position-analysis-account pa)
                                                 (* 100.0 (option-delta priced-option) (position-analysis-signed-shares pa))
                                                 (* 100.0 (option-gamma priced-option) (position-analysis-signed-shares pa))
                                                 (* 100.0 (option-theta priced-option) (position-analysis-signed-shares pa))
                                                 (* 100.0 (option-vega priced-option) (position-analysis-signed-shares pa))
                                                 (* 100.0 (option-rho priced-option) (position-analysis-signed-shares pa))))]))
            updated-position-analysis-list)

  (set! position-greeks-list (sort (hash-values position-greeks-hash) string<? #:key position-greeks-stock))

  (set! target-position-analysis-list
        (filter (λ (pa) (or (and (equal? 'bull (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (> (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-high-target pa)))
                            (and (equal? 'bear (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (< (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-low-target pa)))))
                updated-position-analysis-list))

  (set! stop-position-analysis-list
        (filter (λ (pa) (or (and (equal? 'bull (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (< (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-low-stop pa)))
                            (and (equal? 'bear (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (> (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-high-stop pa)))
                            (and (or (equal? 'call-condor (position-analysis-strategy pa))
                                     (equal? 'put-condor (position-analysis-strategy pa)))
                                 (or (< (hash-ref ref-prices (position-analysis-stock pa))
                                        (first (hash-ref min-max-strikes (position-analysis-stock pa))))
                                     (> (hash-ref ref-prices (position-analysis-stock pa))
                                        (second (hash-ref min-max-strikes (position-analysis-stock pa))))))
                            (and (position-analysis-stock-low-stop pa)
                                 (not (= 0 (position-analysis-stock-low-stop pa)))
                                 (< (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-low-stop pa)))
                            (and (position-analysis-stock-high-stop pa)
                                 (not (= 0 (position-analysis-stock-high-stop pa)))
                                 (> (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-high-stop pa)))))
                updated-position-analysis-list))

  (define remaining-position-analysis-list
    (remove* target-position-analysis-list
             (remove* stop-position-analysis-list updated-position-analysis-list)))

  (set! expired-position-analysis-list (filter (λ (pa) (and (position-analysis-end-date pa)
                                                            (date>=? end-date
                                                                     (position-analysis-end-date pa))))
                                               remaining-position-analysis-list))

  (set! open-position-analysis-list (remove* expired-position-analysis-list remaining-position-analysis-list))

  (update-analysis-box open-analysis-box-ref open-position-analysis-list)
  (update-analysis-box stop-analysis-box-ref stop-position-analysis-list)
  (update-analysis-box target-analysis-box-ref target-position-analysis-list)
  (update-analysis-box expired-analysis-box-ref expired-position-analysis-list)
  (update-greeks-box position-greeks-list))

(define (get-min-max-strikes position-analysis-list)
  (foldl (λ (pa h)
           (if (hash-has-key? h (position-analysis-stock pa))
               (let [(hash-val (hash-ref h (position-analysis-stock pa)))]
                 (hash-set h (position-analysis-stock pa)
                           (list (min (first hash-val) (position-analysis-strike pa))
                                 (max (second hash-val) (position-analysis-strike pa)))))
               (hash-set h (position-analysis-stock pa)
                         (list (position-analysis-strike pa) (position-analysis-strike pa)))))
         (hash)
         position-analysis-list))

(define (update-analysis-box box-ref position-analysis-list)
  (send box-ref set
        (map (λ (m) (position-analysis-sector m)) position-analysis-list)
        (map (λ (m) (position-analysis-stock m)) position-analysis-list)
        (map (λ (m) (~t (position-analysis-expiration m) "yy-MM-dd")) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-strike m))) position-analysis-list)
        (map (λ (m) (symbol->string (position-analysis-call-put m))) position-analysis-list)
        (map (λ (m) (position-analysis-account m)) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-signed-shares m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-low-stop m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-low-target m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-close m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-high-stop m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-high-target m))) position-analysis-list)
        (map (λ (m) (~t (position-analysis-end-date m) "yy-MM-dd")) position-analysis-list))
  ; We set data here so that we can retrieve it later with `get-data`
  (for-each (λ (m i) (send box-ref set-data i m))
            position-analysis-list (range (length position-analysis-list))))

(define analysis-box-columns (list "Sector" "Stock" "Expiry" "Strike" "CallPut" "Account"
                                   "Qty" "StkLoStp" "StkLoTgt" "StkPrc" "StkHiStp" "StkHiTgt" "EndDt"))

(define (update-greeks-box position-greeks-list)
  (send greeks-box-ref set
        (map (λ (m) (position-greeks-sector m)) position-greeks-list)
        (map (λ (m) (position-greeks-stock m)) position-greeks-list)
        (map (λ (m) (position-greeks-account m)) position-greeks-list)
        (map (λ (m) (real->decimal-string (position-greeks-delta m))) position-greeks-list)
        (map (λ (m) (real->decimal-string (position-greeks-gamma m))) position-greeks-list)
        (map (λ (m) (real->decimal-string (position-greeks-theta m))) position-greeks-list)
        (map (λ (m) (real->decimal-string (position-greeks-vega m))) position-greeks-list)
        (map (λ (m) (real->decimal-string (position-greeks-rho m))) position-greeks-list))

  (for-each (λ (m i) (send greeks-box-ref set-data i m))
            position-greeks-list (range (length position-greeks-list))))

(define greeks-box-columns (list "Sector" "Stock" "Account" "Delta" "Gamma" "Theta" "Vega" "Rho"))

(define (position-analysis-box parent-panel start-date end-date)
  (set! position-panel (new vertical-panel% [parent parent-panel] [alignment '(left top)]))

  (set! position-summary (new message% [parent position-panel] [label position-summary-text]))

  (set! position-history (new message% [parent position-panel] [label position-history-text]))

  (define (analysis-box name height)
    (define box (new list-box%
                     [parent position-panel]
                     [label name]
                     [callback (λ (b e)
                                 (let ([market "SPY"]
                                       [sector (position-analysis-sector (send b get-data (first (send b get-selections))))]
                                       [industry ""]
                                       [stock (position-analysis-stock (send b get-data (first (send b get-selections))))]
                                       [earnings-date (position-analysis-end-date (send b get-data (first (send b get-selections))))])
                                   (refresh-chart market
                                                  sector
                                                  industry
                                                  stock
                                                  start-date
                                                  end-date)))]
                     [style (list 'single 'column-headers 'vertical-label)]
                     [columns analysis-box-columns]
                     [choices (list "")]
                     [min-height height]
                     [stretchable-height (not height)]))
    (let ([box-width (send box get-width)]
          [num-cols (length analysis-box-columns)])
      (for-each (λ (i) (send box set-column-width i 90 90 90))
                (range num-cols)))
    box)

  (set! open-analysis-box-ref (analysis-box "Open" #f))
  (update-analysis-box open-analysis-box-ref open-position-analysis-list)
  (set! stop-analysis-box-ref (analysis-box "Stop" 100))
  (update-analysis-box stop-analysis-box-ref stop-position-analysis-list)
  (set! target-analysis-box-ref (analysis-box "Target" 100))
  (update-analysis-box target-analysis-box-ref target-position-analysis-list)
  (set! expired-analysis-box-ref (analysis-box "Expired" 100))
  (update-analysis-box expired-analysis-box-ref expired-position-analysis-list)

  (define greeks-box (new list-box%
                          [parent position-panel]
                          [label "Position Greeks"]
                          [style (list 'single 'column-headers 'vertical-label)]
                          [columns greeks-box-columns]
                          [choices (list "")]
                          [min-height 150]
                          [stretchable-height (not 150)]))
  (let ([box-width (send greeks-box get-width)]
        [num-cols (length greeks-box-columns)])
    (for-each (λ (i) (send greeks-box set-column-width i 90 90 90))
              (range num-cols)))
  (set! greeks-box-ref greeks-box)
  (update-greeks-box position-greeks-list))
