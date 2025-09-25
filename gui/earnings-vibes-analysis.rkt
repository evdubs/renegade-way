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
  (set! earnings-vibes-analysis-list (get-earnings-vibes-analysis end-date))

  (cond [live-data
         (set! earnings-vibes-analysis-list
               (map (λ (eva) (define max-omd (get-option-market-data (earnings-vibes-analysis-stock eva)
                                                                     (earnings-vibes-analysis-max-expiration eva)
                                                                     (earnings-vibes-analysis-strike eva)
                                                                     'call))
                      (define min-omd (get-option-market-data (earnings-vibes-analysis-stock eva)
                                                                     (earnings-vibes-analysis-min-expiration eva)
                                                                     (earnings-vibes-analysis-strike eva)
                                                                     'call))
                      (struct-copy
                              earnings-vibes-analysis eva
                              [vol-slope (/ (* 100 (- (option-market-data-rsp-implied-volatility max-omd)
                                                      (option-market-data-rsp-implied-volatility min-omd)))
                                            (period-ref (period-between (iso8601->date (earnings-vibes-analysis-min-expiration eva))
                                                                        (iso8601->date (earnings-vibes-analysis-max-expiration eva))
                                                                        '(days))
                                                        'days))]))
                    earnings-vibes-analysis-list))])

  (update-analysis-box earnings-vibes-analysis-list))

(define (update-analysis-box earnings-vibes-analysis-list)
  (send analysis-box-ref set
        (map (λ (m) (earnings-vibes-analysis-stock m)) earnings-vibes-analysis-list)
        (map (λ (m) (real->decimal-string (earnings-vibes-analysis-vol-slope m))) earnings-vibes-analysis-list)
        (map (λ (m) (real->decimal-string (earnings-vibes-analysis-iv-hv m))) earnings-vibes-analysis-list)
        (map (λ (m) (earnings-vibes-analysis-earnings-date m)) earnings-vibes-analysis-list)
        (map (λ (m) (real->decimal-string (earnings-vibes-analysis-option-spread m))) earnings-vibes-analysis-list)
        (map (λ (m) (real->decimal-string (earnings-vibes-analysis-30d-avg-volume m))) earnings-vibes-analysis-list))
  ; We set data here so that we can retrieve it later with `get-data`
  (map (λ (m i) (send analysis-box-ref set-data i m))
       earnings-vibes-analysis-list (range (length earnings-vibes-analysis-list))))

(define analysis-box-columns (list "Stock" "VolSlp" "IvHv" "ErnDt" "OptSprd" "30dVlm"))

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
