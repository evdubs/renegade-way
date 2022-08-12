#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         racket/match
         "../db-queries.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide rank-analysis-box
         rank-analysis-filter
         run-rank-analysis)

(define rank-analysis-list (list))

(define analysis-box-ref #f)

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define (rank-analysis-filter #:hide-large-spread large-spread #:hide-non-weekly non-weekly)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (update-analysis-box rank-analysis-list))

(define (run-rank-analysis market sector start-date end-date)
  (set! rank-analysis-list (get-rank-analysis market end-date))
  (update-analysis-box rank-analysis-list))

(define (update-analysis-box rank-analysis-list)
  (let* ([filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (rank-analysis-option-spread m)))
                                                (> 30 (string->number (rank-analysis-option-spread m))))) rank-analysis-list)
                            rank-analysis-list)]
         [filter-weekly (if (hide-non-weekly)
                            (filter (λ (m) (rank-analysis-is-weekly m)) filter-spread)
                            filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (rank-analysis-market m)) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-market-rank m))) filter-weekly)
          (map (λ (m) (rank-analysis-sector m)) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-sector-rank m))) filter-weekly)
          (map (λ (m) (rank-analysis-industry m)) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-industry-rank m))) filter-weekly)
          (map (λ (m) (rank-analysis-stock m)) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-stock-rank m))) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-stock-best-rank m))) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-stock-avg-rank m))) filter-weekly)
          (map (λ (m) (real->decimal-string (rank-analysis-stock-worst-rank m))) filter-weekly)
          (map (λ (m) (rank-analysis-earnings-date m)) filter-weekly)
          (map (λ (m) (rank-analysis-option-spread m)) filter-weekly))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i m))
         filter-weekly (range (length filter-weekly)))))

(define analysis-box-columns (list "Market" "MktRnk" "Sector" "SctRnk" "Industry" "IndRnk"
                                   "Stock" "StkRnk" "StkBst" "StkAvg" "StkWrst" "ErnDt" "OptSprd"))

(define (rank-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([market (rank-analysis-market (send b get-data (first (send b get-selections))))]
                           [sector (rank-analysis-sector (send b get-data (first (send b get-selections))))]
                           [industry (rank-analysis-industry (send b get-data (first (send b get-selections))))]
                           [stock (rank-analysis-stock (send b get-data (first (send b get-selections))))]
                           [stock-rank (rank-analysis-stock-rank (send b get-data (first (send b get-selections))))])
                       (refresh-chart market
                                      sector
                                      industry
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                (match stock-rank
                                                  [1 "IR"]
                                                  [2 "IR"]
                                                  [4 "DR"]
                                                  [5 "DR"]
                                                  [_ ""]))))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-analysis-box rank-analysis-list))
