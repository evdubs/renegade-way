#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         "chart.rkt"
         "db-queries.rkt"
         "option-strategy-frame.rkt"
         "structs.rkt")

(provide vol-analysis-box
         vol-analysis-filter
         run-vol-analysis)

(define vol-analysis-list (list))

(define analysis-box-ref #f)

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define (vol-analysis-filter #:hide-large-spread large-spread #:hide-non-weekly non-weekly)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (update-analysis-box vol-analysis-list))

(define (run-vol-analysis market sector start-date end-date)
  (set! vol-analysis-list (get-vol-analysis market end-date))
  (update-analysis-box vol-analysis-list))

(define (update-analysis-box vol-analysis-list)
  (let* ([filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (vol-analysis-option-spread m)))
                                                (> 30 (string->number (vol-analysis-option-spread m))))) vol-analysis-list)
                            vol-analysis-list)]
         [filter-weekly (if (hide-non-weekly)
                            (filter (λ (m) (vol-analysis-is-weekly m)) filter-spread)
                            filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (vol-analysis-market m)) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-market-iv m))) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-market-iv-rank m))) filter-weekly)
          (map (λ (m) (vol-analysis-sector m)) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-sector-iv m))) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-sector-iv-rank m))) filter-weekly)
          (map (λ (m) (vol-analysis-industry m)) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-industry-iv m))) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-industry-iv-rank m))) filter-weekly)
          (map (λ (m) (vol-analysis-stock m)) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-stock-iv m))) filter-weekly)
          (map (λ (m) (real->decimal-string (vol-analysis-stock-iv-rank m))) filter-weekly)
          (map (λ (m) (vol-analysis-earnings-date m)) filter-weekly)
          (map (λ (m) (vol-analysis-option-spread m)) filter-weekly))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i m))
         filter-weekly (range (length filter-weekly)))))

(define analysis-box-columns (list "Market" "MktIv" "MktIvRnk" "Sector" "SctIv" "SctIvRnk" "Industry" "IndIv" "IndIvRnk"
                                   "Stock" "StkIv" "StkIvRnk" "ErnDt" "OptSprd"))

(define (vol-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([market (vol-analysis-market (send b get-data (first (send b get-selections))))]
                           [sector (vol-analysis-sector (send b get-data (first (send b get-selections))))]
                           [industry (vol-analysis-industry (send b get-data (first (send b get-selections))))]
                           [stock (vol-analysis-stock (send b get-data (first (send b get-selections))))]
                           [earnings-date (vol-analysis-earnings-date (send b get-data (first (send b get-selections))))])
                       (refresh-chart market
                                      sector
                                      industry
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                (if (equal? "" earnings-date) "DV" "IV"))))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-analysis-box vol-analysis-list))
