#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         "chart.rkt"
         "db-queries.rkt"
         "option-strategy.rkt"
         "structs.rkt")

(provide position-analysis-box
         run-position-analysis)

(define position-analysis-list (list))

(define (run-position-analysis market sector start-date end-date)
  (set! position-analysis-list (get-position-analysis end-date)))

(define analysis-box-columns (list "Sector" "Stock" "Expiry" "Strike" "CallPut" "Account"
                                   "Qty" "StkStop" "StkPrc" "StkTgt" "EndDt"))

(define (position-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
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
                                      end-date)
                       ; (refresh-option-strategy stock
                       ;                          end-date
                       ;                          (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                       ;                          (if (equal? "" earnings-date) "DV" "IV"))
                       ))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (send analysis-box set
        (map (λ (m) (position-analysis-sector m)) position-analysis-list)
        (map (λ (m) (position-analysis-stock m)) position-analysis-list)
        (map (λ (m) (position-analysis-expiration m)) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-strike m))) position-analysis-list)
        (map (λ (m) (position-analysis-call-put m)) position-analysis-list)
        (map (λ (m) (position-analysis-account m)) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-signed-shares m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-stop m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-close m))) position-analysis-list)
        (map (λ (m) (real->decimal-string (position-analysis-stock-target m))) position-analysis-list)
        (map (λ (m) (position-analysis-end-date m)) position-analysis-list))
  ; We set data here so that we can retrieve it later with `get-data`
  (map (λ (m i) (send analysis-box set-data i m))
       position-analysis-list (range (length position-analysis-list))))
