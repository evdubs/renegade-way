#lang racket/base

(require gregor
         gregor/period
         racket/class
         racket/gui/base
         racket/list
         interactive-brokers-api/response-messages
         "../db-queries.rkt"
         "../finviz-prices.rkt"
         "../ibkr-market-data.rkt"
         "../option-strategy.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide earnings-vibes-analysis-box
         earnings-vibes-analysis-filter
         run-earnings-vibes-analysis)

(define earnings-vibes-analysis-list (list))

(define analysis-box-ref #f)

(define hide-large-spread (make-parameter #f))

(define hide-non-weekly (make-parameter #f))

(define use-live-data (make-parameter #f))

(define (earnings-vibes-analysis-filter #:hide-large-spread large-spread #:hide-non-weekly non-weekly #:use-live-data live-data)
  (hide-large-spread large-spread)
  (hide-non-weekly non-weekly)
  (use-live-data live-data)
  (update-analysis-box earnings-vibes-analysis-list))

(define (run-earnings-vibes-analysis market sector start-date end-date #:use-live-data live-data)
  (set! earnings-vibes-analysis-list
        (get-earnings-vibes-analysis end-date #:live-prices (if live-data (get-prices (get-earnings-symbols-for-date end-date))
                                                                #f)))

  (cond [live-data
         (set! earnings-vibes-analysis-list
               (map (λ (eva)
                      (define max-omd (get-option-market-data (earnings-vibes-analysis-stock eva)
                                                              (earnings-vibes-analysis-max-expiration eva)
                                                              (earnings-vibes-analysis-strike eva)
                                                              'call))
                      (define min-omd (get-option-market-data (earnings-vibes-analysis-stock eva)
                                                              (earnings-vibes-analysis-min-expiration eva)
                                                              (earnings-vibes-analysis-strike eva)
                                                              'call))
                      (if (and max-omd min-omd)
                          (struct-copy
                           earnings-vibes-analysis eva
                           [vol-slope (/ (* 100 (- (option-market-data-rsp-implied-volatility max-omd)
                                                   (option-market-data-rsp-implied-volatility min-omd)))
                                         (period-ref (period-between (iso8601->date (earnings-vibes-analysis-min-expiration eva))
                                                                     (iso8601->date (earnings-vibes-analysis-max-expiration eva))
                                                                     '(days))
                                                     'days))]
                           [price-strike-ratio (* 100 (/ (- (option-market-data-rsp-price max-omd)
                                                            (option-market-data-rsp-price min-omd))
                                                         (earnings-vibes-analysis-strike eva)))])
                          eva))
                    earnings-vibes-analysis-list))]
        [else
         (set! earnings-vibes-analysis-list
               (map (λ (eva)
                      (define prices (get-date-ohlc (earnings-vibes-analysis-stock eva) start-date end-date))
                      (define options (get-updated-options (earnings-vibes-analysis-stock eva) end-date
                                                           (dohlc-close (last prices)) #:compute-all-greeks #f #:fit-vols #f))
                      (define call-horizontal-options (hash-ref (suitable-options options "EC" (dohlc-close (last prices)))
                                                                "Call Horizontal Spread"))
                      (struct-copy
                       earnings-vibes-analysis eva
                       [price-strike-ratio (* 100 (/ (- (option-mid (second call-horizontal-options))
                                                        (option-mid (first call-horizontal-options)))
                                                     (earnings-vibes-analysis-strike eva)))]))
                    earnings-vibes-analysis-list))])

  (set! earnings-vibes-analysis-list (sort earnings-vibes-analysis-list (λ (eva-1 eva-2) (< (earnings-vibes-analysis-vol-slope eva-1)
                                                                                            (earnings-vibes-analysis-vol-slope eva-2)))))

  (update-analysis-box earnings-vibes-analysis-list))

(define (update-analysis-box earnings-vibes-analysis-list)
  (let* ([filter-spread (if (hide-large-spread)
                            (filter (λ (m) (and (>= 0.50 (earnings-vibes-analysis-option-spread m))
                                                (<= 7.5 (earnings-vibes-analysis-30d-avg-volume m)))) earnings-vibes-analysis-list)
                            earnings-vibes-analysis-list)])
    (send analysis-box-ref set
          (map (λ (m) (earnings-vibes-analysis-stock m)) filter-spread)
          (map (λ (m) (substring (earnings-vibes-analysis-min-expiration m) 2)) filter-spread)
          (map (λ (m) (substring (earnings-vibes-analysis-max-expiration m) 2)) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-strike m))) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-vol-slope m))) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-iv-hv m))) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-price-strike-ratio m))) filter-spread)
          (map (λ (m) (earnings-vibes-analysis-earnings-date m)) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-option-spread m))) filter-spread)
          (map (λ (m) (real->decimal-string (earnings-vibes-analysis-30d-avg-volume m))) filter-spread))
    ; We set data here so that we can retrieve it later with `get-data`
    (map (λ (m i) (send analysis-box-ref set-data i m))
         filter-spread (range (length filter-spread)))))

(define analysis-box-columns (list "Stock" "FrntExp" "BckExp" "Strk" "VolSlp" "IvHv" "PxSrkRt" "ErnDt" "OptSprd" "30dVlm"))

(define (earnings-vibes-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (define stock (earnings-vibes-analysis-stock (send b get-data (first (send b get-selections)))))
                     (refresh-chart "" ; market
                                    "" ; sector
                                    "" ; industry
                                    stock
                                    start-date
                                    end-date)
                     (refresh-option-strategy stock
                                              end-date
                                              (if (use-live-data)
                                                  (hash-ref (get-prices (list stock)) stock)
                                                  (dohlc-close (last (get-date-ohlc stock start-date end-date))))
                                              "EC"))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 100 100 100))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-analysis-box earnings-vibes-analysis-list))
