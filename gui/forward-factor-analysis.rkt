#lang racket/base

(require gregor
         gregor/period
         racket/class
         racket/contract
         racket/gui/base
         racket/list
         interactive-brokers-api/response-messages
         "../db-queries.rkt"
         "../ibkr-market-data.rkt"
         "../structs.rkt"
         "../web-prices.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide (contract-out
          [forward-factor-analysis-box (-> (is-a?/c tab-panel%) date? date? void?)]
          [forward-factor-analysis-filter (-> #:hide-no-pattern boolean? #:hide-large-spread boolean? #:use-live-data boolean? void?)]
          [run-forward-factor-analysis (-> date? #:use-live-data boolean? void?)]))

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

(define (run-forward-factor-analysis end-date #:use-live-data live-data)
  (set! forward-factor-analysis-list (get-forward-factor-analysis end-date))

  (cond [live-data
         (define candidate-forward-factors (filter (λ (ff) (and (> 0.30 (forward-factor-analysis-option-spread ff))
                                                                (<= 0.00 (forward-factor-analysis-forward-factor ff))))
                                                   forward-factor-analysis-list))
         (define ref-prices (get-prices (map (λ (ff) (forward-factor-analysis-stock ff))
                                             candidate-forward-factors)))
         (set! forward-factor-analysis-list
               (map (λ (ff)
                      (define atm-strike (get-nearest-strike end-date
                                                             (forward-factor-analysis-stock ff)
                                                             (forward-factor-analysis-front-exp ff)
                                                             (forward-factor-analysis-back-exp ff)
                                                             (hash-ref ref-prices (forward-factor-analysis-stock ff)
                                                                       (dohlc-close (last (get-date-ohlc (forward-factor-analysis-stock ff)
                                                                                                         (-months end-date 1)
                                                                                                         end-date))))))
                      (define front-omd (get-option-market-data (forward-factor-analysis-stock ff)
                                                                (forward-factor-analysis-front-exp ff)
                                                                atm-strike
                                                                'call))
                      (define back-omd (get-option-market-data (forward-factor-analysis-stock ff)
                                                               (forward-factor-analysis-back-exp ff)
                                                               atm-strike
                                                               'call))
                      (cond [(and front-omd back-omd)
                             (define forward-vol-calc (sqrt (/ (- (* (/ (period-ref (period-between end-date (forward-factor-analysis-back-exp ff) '(days))
                                                                                    'days)
                                                                        (days-in-year (->year end-date)))
                                                                     (option-market-data-rsp-implied-volatility back-omd)
                                                                     (option-market-data-rsp-implied-volatility back-omd))
                                                                  (* (/ (period-ref (period-between end-date (forward-factor-analysis-front-exp ff) '(days))
                                                                                    'days)
                                                                        (days-in-year (->year end-date)))
                                                                     (option-market-data-rsp-implied-volatility front-omd)
                                                                     (option-market-data-rsp-implied-volatility front-omd)))
                                                               (/ (period-ref (period-between (forward-factor-analysis-front-exp ff)
                                                                                              (forward-factor-analysis-back-exp ff)
                                                                                              '(days))
                                                                              'days)
                                                                  (days-in-year (->year end-date))))))
                             (struct-copy forward-factor-analysis ff
                                          [front-vol (option-market-data-rsp-implied-volatility front-omd)]
                                          [back-vol (option-market-data-rsp-implied-volatility back-omd)]
                                          [vol-ratio (/ (option-market-data-rsp-implied-volatility front-omd)
                                                        (option-market-data-rsp-implied-volatility back-omd))]
                                          [forward-vol forward-vol-calc]
                                          [forward-factor (- (/ (option-market-data-rsp-implied-volatility front-omd)
                                                                forward-vol-calc)
                                                             1.0)])]
                            [else ff]))
                    candidate-forward-factors))])

  (set! forward-factor-analysis-list (sort forward-factor-analysis-list (λ (ff-1 ff-2) (> (forward-factor-analysis-forward-factor ff-1)
                                                                                          (forward-factor-analysis-forward-factor ff-2)))))
  (update-analysis-box forward-factor-analysis-list))

(define (update-analysis-box forward-factor-analysis-list)
  (let* ([filter-spread (if (hide-large-spread)
                            (filter (λ (m) (> 0.30 (forward-factor-analysis-option-spread m))) forward-factor-analysis-list)
                            forward-factor-analysis-list)]
         [filter-pattern (if (hide-no-pattern)
                             (filter (λ (m) (<= 0.23 (forward-factor-analysis-forward-factor m))) filter-spread)
                             filter-spread)])
    (send analysis-box-ref set
          (map (λ (m) (forward-factor-analysis-stock m)) filter-pattern)
          (map (λ (m) (~t (forward-factor-analysis-front-exp m) "yy-MM-dd")) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-front-vol m) 3)) filter-pattern)
          (map (λ (m) (~t (forward-factor-analysis-back-exp m) "yy-MM-dd")) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-back-vol m) 3)) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-vol-ratio m) 3)) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-forward-vol m) 3)) filter-pattern)
          (map (λ (m) (real->decimal-string (forward-factor-analysis-forward-factor m) 3)) filter-pattern)
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
