#lang racket/base

(require gregor
         racket/class
         racket/contract
         racket/gui/base
         racket/list
         "../db-queries.rkt"
         "../web-prices.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide (contract-out
          [forward-factor-analysis-box (-> (is-a?/c tab-panel%) date? date? void?)]
          [forward-factor-analysis-filter (-> #:hide-no-pattern boolean? #:hide-large-spread boolean? #:use-live-data boolean? void?)]
          [run-forward-factor-analysis (-> date? void?)]))

(define forward-factor-analysis-list (list))

(define analysis-box-ref #f)

(define hide-no-pattern (make-parameter #f))

(define hide-large-spread (make-parameter #f))

(define use-live-data (make-parameter #f))

(define (forward-factor-analysis-filter #:hide-no-pattern no-pattern #:hide-large-spread large-spread #:use-live-data live-data)
  (hide-no-pattern no-pattern)
  (hide-large-spread large-spread)
  (use-live-data live-data)
  (update-analysis-box forward-factor-analysis-list))

(define (run-forward-factor-analysis end-date)
  (set! forward-factor-analysis-list (get-forward-factor-analysis end-date))
  (update-analysis-box forward-factor-analysis-list))

(define (update-analysis-box forward-factor-analysis-list)
  (let* ([filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (not (equal? "" (forward-factor-analysis-option-spread m)))
                                                (> 0.30 (forward-factor-analysis-option-spread m)))) forward-factor-analysis-list)
                            forward-factor-analysis-list)]
         [filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (<= 0.23 (forward-factor-analysis-forward-factor m))) filter-spread)
                             filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (forward-factor-analysis-stock m)) filter-pattern)
          (map (λ (m) (~t (forward-factor-analysis-front-exp m) "yy-MM-dd")) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-front-vol m))) filter-pattern)
          (map (λ (m) (~t (forward-factor-analysis-back-exp m) "yy-MM-dd")) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-back-vol m))) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-vol-ratio m))) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-forward-vol m))) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-forward-factor m))) filter-pattern)
          (map (λ (m) (if (forward-factor-analysis-earnings-date m) (~t (forward-factor-analysis-earnings-date m) "yy-MM-dd") "")) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-option-spread m))) filter-pattern))
    ; We set data here so that we can retrieve it later with `get-data`
    (for-each (λ (m i) (send analysis-box-ref set-data i m))
              filter-pattern (range (length filter-pattern)))))

(define analysis-box-columns (list "Stock" "FrontExp" "FrontVol" "BackExp" "BackVol" "VolRt" "FwdVol" "FwdFctr" "ErnDt" "OptSprd"))

(define (forward-factor-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([stock (forward-factor-analysis-stock (send b get-data (first (send b get-selections))))]
                           [earnings-date (forward-factor-analysis-earnings-date (send b get-data (first (send b get-selections))))])
                       (refresh-chart ""
                                      ""
                                      ""
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (if (use-live-data)
                                                  (hash-ref (get-prices (list stock)) stock)
                                                  (dohlc-close (last (get-date-ohlc stock start-date end-date))))
                                                "FF")))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 100 100 100))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-analysis-box forward-factor-analysis-list))
