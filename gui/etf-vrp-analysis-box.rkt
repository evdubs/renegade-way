#lang racket/base

(require racket/gui
         racket/list
         "../db-queries.rkt"
         "../etf-vrp-analysis.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide etf-vrp-analysis-box
         etf-vrp-analysis-filter
         update-etf-vrp-analysis-box)

(define analysis-box-ref #f)

(define hide-no-pattern (make-parameter #f))

(define hide-large-spread (make-parameter #f))

(define (etf-vrp-analysis-filter #:hide-no-pattern no-pattern #:hide-large-spread large-spread)
  (hide-no-pattern no-pattern)
  (hide-large-spread large-spread)
  (update-etf-vrp-analysis-box etf-vrp-analysis-list))

(define (update-etf-vrp-analysis-box etf-vrp-analysis-list)
  (let* ([filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (and (real? (etf-vrp-analysis-iv-hv m))
                                                 (<= 0.0 (etf-vrp-analysis-iv-hv m))
                                                 (real? (etf-vrp-analysis-ivp-1yr m))
                                                 (>= 0.80 (etf-vrp-analysis-ivp-1yr m))
                                                 (real? (etf-vrp-analysis-flat-fwd-to-fwd-ratio m))
                                                 (<= 0.95 (etf-vrp-analysis-flat-fwd-to-fwd-ratio m)))) etf-vrp-analysis-list)
                             etf-vrp-analysis-list)]
         [filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (> 0.30 (etf-vrp-analysis-option-spread m)))) filter-pattern)
                            filter-pattern)])
    (send analysis-box-ref set
          (map (λ (m) (etf-vrp-analysis-etf m)) filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-iv-hv m)) (real->decimal-string (etf-vrp-analysis-iv-hv m)) ""))
               filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-ivp-1yr m)) (real->decimal-string (etf-vrp-analysis-ivp-1yr m)) ""))
               filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-30d-60d-fwd-vol m)) (real->decimal-string (etf-vrp-analysis-30d-60d-fwd-vol m)) ""))
               filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-30d-60d-flat-fwd-vol m)) (real->decimal-string (etf-vrp-analysis-30d-60d-flat-fwd-vol m)) ""))
               filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-flat-fwd-to-fwd-ratio m)) (real->decimal-string (etf-vrp-analysis-flat-fwd-to-fwd-ratio m)) ""))
               filter-spread)
          (map (λ (m) (if (real? (etf-vrp-analysis-option-spread m)) (real->decimal-string (etf-vrp-analysis-option-spread m)) ""))
               filter-spread))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i m))
         filter-spread (range (length filter-spread)))))

(define analysis-box-columns (list "ETF" "IV/HV" "IVP" "FwdVol" "FFwdVol" "FFwd/Fwd" "OptSprd"))

(define (etf-vrp-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([stock (etf-vrp-analysis-etf (send b get-data (first (send b get-selections))))])
                       (refresh-chart ""
                                      ""
                                      ""
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                "VR")))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 100 100 100))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-etf-vrp-analysis-box etf-vrp-analysis-list))
