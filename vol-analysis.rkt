#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         "chart.rkt"
         "db-queries.rkt"
         "option-strategy.rkt"
         "structs.rkt")

(provide vol-analysis-box
         run-vol-analysis)

(define vol-analysis-list (list))

(define (run-vol-analysis market sector start-date end-date)
  (set! vol-analysis-list (get-vol-analysis market end-date)))

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
  (send analysis-box set
        (map (λ (m) (vol-analysis-market m)) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-market-iv m))) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-market-iv-rank m))) vol-analysis-list)
        (map (λ (m) (vol-analysis-sector m)) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-sector-iv m))) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-sector-iv-rank m))) vol-analysis-list)
        (map (λ (m) (vol-analysis-industry m)) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-industry-iv m))) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-industry-iv-rank m))) vol-analysis-list)
        (map (λ (m) (vol-analysis-stock m)) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-stock-iv m))) vol-analysis-list)
        (map (λ (m) (real->decimal-string (vol-analysis-stock-iv-rank m))) vol-analysis-list)
        (map (λ (m) (vol-analysis-earnings-date m)) vol-analysis-list)
        (map (λ (m) (vol-analysis-option-spread m)) vol-analysis-list))
  ; We set data here so that we can retrieve it later with `get-data`
  (map (λ (m i) (send analysis-box set-data i m))
       vol-analysis-list (range (length vol-analysis-list))))
