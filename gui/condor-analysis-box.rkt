#lang racket/base

(require racket/class
         racket/list
         racket/gui/base
         "../condor-analysis.rkt"
         "../db-queries.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide condor-analysis-box
         condor-analysis-filter
         update-condor-analysis-box)

(define analysis-box-ref #f)

(define hide-no-pattern (make-parameter #f))

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define (condor-analysis-filter #:hide-no-pattern no-pattern #:hide-large-spread large-spread #:hide-non-weekly non-weekly)
  (hide-no-pattern no-pattern)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (update-condor-analysis-box condor-analysis-list condor-analysis-hash))

(define (update-condor-analysis-box condor-analysis-list condor-analysis-hash)
  (let* ([filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (and (<= 65 (first (hash-ref condor-analysis-hash (condor-analysis-stock m))))
                                                 (<= 65 (second (hash-ref condor-analysis-hash (condor-analysis-stock m)))))) condor-analysis-list)
                             condor-analysis-list)]
         [filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (condor-analysis-option-spread m)))
                                                (> 30 (string->number (condor-analysis-option-spread m))))) filter-pattern)
                            filter-pattern)]
         [filter-weekly (if (hide-non-weekly)
                            (filter (λ (m) (condor-analysis-is-weekly m)) filter-spread)
                            filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (condor-analysis-market m)) filter-weekly)
          (map (λ (m) (real->decimal-string (first (hash-ref condor-analysis-hash (condor-analysis-market m))))) filter-weekly)
          (map (λ (m) (real->decimal-string (second (hash-ref condor-analysis-hash (condor-analysis-market m))))) filter-weekly)
          (map (λ (m) (condor-analysis-sector m)) filter-weekly)
          (map (λ (m) (real->decimal-string (first (hash-ref condor-analysis-hash (condor-analysis-sector m))))) filter-weekly)
          (map (λ (m) (real->decimal-string (second (hash-ref condor-analysis-hash (condor-analysis-sector m))))) filter-weekly)
          (map (λ (m) (condor-analysis-industry m)) filter-weekly)
          (map (λ (m) (real->decimal-string (first (hash-ref condor-analysis-hash (condor-analysis-industry m))))) filter-weekly)
          (map (λ (m) (real->decimal-string (second (hash-ref condor-analysis-hash (condor-analysis-industry m))))) filter-weekly)
          (map (λ (m) (condor-analysis-stock m)) filter-weekly)
          (map (λ (m) (real->decimal-string (first (hash-ref condor-analysis-hash (condor-analysis-stock m))))) filter-weekly)
          (map (λ (m) (real->decimal-string (second (hash-ref condor-analysis-hash (condor-analysis-stock m))))) filter-weekly)
          (map (λ (m) (condor-analysis-earnings-date m)) filter-weekly)
          (map (λ (m) (condor-analysis-option-spread m)) filter-weekly))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i (list m (hash-ref condor-analysis-hash (condor-analysis-stock m)))))
         filter-weekly (range (length filter-weekly)))))

(define analysis-box-columns (list "Market" "MktRtg" "MktRr" "Sector" "SctRtg" "SctRr" "Industry" "IndRtg" "IndRr" "Stock" "StkRtg"
                                   "StkRr" "ErnDt" "OptSprd"))

(define (condor-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([market (condor-analysis-market (first (send b get-data (first (send b get-selections)))))]
                           [sector (condor-analysis-sector (first (send b get-data (first (send b get-selections)))))]
                           [industry (condor-analysis-industry (first (send b get-data (first (send b get-selections)))))]
                           [stock (condor-analysis-stock (first (send b get-data (first (send b get-selections)))))])
                       (refresh-chart market
                                      sector
                                      industry
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                "DV")))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-condor-analysis-box condor-analysis-list condor-analysis-hash))
