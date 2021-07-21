#lang racket/base

(require racket/class
         racket/list
         racket/gui/base
         "chart.rkt"
         "db-queries.rkt"
         "option-strategy.rkt"
         "price-analysis.rkt"
         "structs.rkt")

(provide price-analysis-box
         price-analysis-filter
         update-price-analysis-box)

(define analysis-box-ref #f)

(define hide-hold (make-parameter #f))

(define hide-no-pattern (make-parameter #f))

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define (price-analysis-filter #:hide-hold hold #:hide-no-pattern no-pattern #:hide-large-spread large-spread #:hide-non-weekly non-weekly)
  (hide-hold hold)
  (hide-no-pattern no-pattern)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (update-price-analysis-box price-analysis-list analysis-hash))

(define (update-price-analysis-box price-analysis-list analysis-hash)
  (let* ([filter-hold (if (hide-hold)
                          (filter (λ (m) (not (equal? "Hold" (price-analysis-zacks-rank m)))) price-analysis-list)
                          price-analysis-list)]
         [filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (not (equal? "" (hash-ref analysis-hash (price-analysis-stock m))))) filter-hold)
                             filter-hold)]
         [filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (price-analysis-option-spread m)))
                                                (> 20 (string->number (price-analysis-option-spread m))))) filter-pattern)
                            filter-pattern)]
         [filter-weekly (if (hide-non-weekly)
                            (filter (λ (m) (price-analysis-is-weekly m)) filter-spread)
                            filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (price-analysis-market m)) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-market m)))) filter-weekly)
          (map (λ (m) (price-analysis-sector m)) filter-weekly)
          (map (λ (m) (real->decimal-string (price-analysis-sector-vs-market m))) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-sector m)))) filter-weekly)
          (map (λ (m) (price-analysis-industry m)) filter-weekly)
          (map (λ (m) (number->string (hash-ref analysis-hash (price-analysis-industry m)))) filter-weekly)
          (map (λ (m) (price-analysis-stock m)) filter-weekly)
          (map (λ (m) (real->decimal-string (price-analysis-stock-vs-sector m))) filter-weekly)
          (map (λ (m) (price-analysis-next-div-date m)) filter-weekly)
          (map (λ (m) (price-analysis-earnings-date m)) filter-weekly)
          (map (λ (m) (price-analysis-option-spread m)) filter-weekly)
          (map (λ (m) (price-analysis-zacks-rank m)) filter-weekly)
          (map (λ (m) (hash-ref analysis-hash (price-analysis-stock m))) filter-weekly))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i (list m (hash-ref analysis-hash (price-analysis-stock m)))))
         filter-weekly (range (length filter-weekly)))))

(define analysis-box-columns (list "Market" "MktRtg" "Sector" "Sct/Mkt" "SctRtg" "Industry" "IndRtg"
                                   "Stock" "Stk/Sct" "DivDt" "ErnDt" "OptSprd" "ZckRnk" "Patterns"))

(define (price-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([market (price-analysis-market (first (send b get-data (first (send b get-selections)))))]
                           [sector (price-analysis-sector (first (send b get-data (first (send b get-selections)))))]
                           [industry (price-analysis-industry (first (send b get-data (first (send b get-selections)))))]
                           [stock (price-analysis-stock (first (send b get-data (first (send b get-selections)))))])
                       (refresh-chart market
                                      sector
                                      industry
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                (second (send b get-data (first (send b get-selections)))))))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-price-analysis-box price-analysis-list analysis-hash))
