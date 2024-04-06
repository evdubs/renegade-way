#lang racket/base

(require gregor
         racket/async-channel
         racket/class
         racket/gui/base
         racket/list
         racket/string
         interactive-brokers-api/request-messages
         interactive-brokers-api/response-messages
         "../db-queries.rkt"
         "../ibkr.rkt"
         "../structs.rkt"
         "chart.rkt")

(provide position-analysis-box
         run-position-analysis)

(define position-panel #f)

(define position-summary #f)

(define position-summary-text "")

(define position-history #f)

(define position-history-text "")

(define open-analysis-box-ref #f)

(define open-position-analysis-list (list))

(define stop-analysis-box-ref #f)

(define stop-position-analysis-list (list))

(define target-analysis-box-ref #f)

(define target-position-analysis-list (list))

(define expired-analysis-box-ref #f)

(define expired-position-analysis-list (list))

(define (run-position-analysis market sector start-date end-date)
  (define position-analysis-list (get-position-analysis end-date))

  (define ref-prices (get-ref-prices position-analysis-list))

  (define updated-position-analysis-list
    (map (λ (pa)
           (struct-copy position-analysis pa [stock-close (hash-ref ref-prices (position-analysis-stock pa))]))
         position-analysis-list))

  (define min-max-strikes (get-min-max-strikes position-analysis-list))

  (define bull-bear-roo
    (foldl (λ (p m)
             (let ([s (position-analysis-strategy p)])
               (cond [(or (equal? "LONG CALL" s)
                          (equal? "BULL CALL VERTICAL SPREAD" s)
                          (equal? "BULL PUT VERTICAL SPREAD" s)
                          (equal? "CALL RATIO SPREAD" s)
                          (equal? "CALL HORIZONTAL SPREAD" s)
                          (equal? "CALL DIAGONAL SPREAD" s))
                      (hash-set m (position-analysis-stock p) 'bull)]
                     [(or (equal? "LONG PUT" s)
                          (equal? "BEAR CALL VERTICAL SPREAD" s)
                          (equal? "BEAR PUT VERTICAL SPREAD" s)
                          (equal? "PUT RATIO SPREAD" s)
                          (equal? "PUT HORIZONTAL SPREAD" s)
                          (equal? "PUT DIAGONAL SPREAD" s))
                      (hash-set m (position-analysis-stock p) 'bear)]
                     [(or (equal? "LONG STRADDLE" s)
                          (equal? "LONG STRANGLE" s)
                          (equal? "CALL BUTTERFLY" s)
                          (equal? "PUT BUTTERFLY" s)
                          (equal? "CALL CONDOR" s)
                          (equal? "PUT CONDOR" s))
                      (hash-set m (position-analysis-stock p) 'roo)]
                     [else (hash-set m (position-analysis-stock p) 'unknown)])))
           (hash)
           updated-position-analysis-list))
  (set! position-summary-text (string-append "Live - Bulls: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'bull)))
                                             " Roos: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'roo)))
                                             " Bears: " (number->string (length (indexes-of (hash-values bull-bear-roo) 'bear)))))
  (send position-summary set-label position-summary-text)

  (set! position-history-text (string-append "History - " (get-position-history end-date)))
  (send position-history set-label position-history-text)

  (set! target-position-analysis-list
        (filter (λ (pa) (or (and (equal? 'bull (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (> (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-target pa)))
                            (and (equal? 'bear (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (< (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-target pa)))))
                updated-position-analysis-list))

  (set! stop-position-analysis-list
        (filter (λ (pa) (or (and (equal? 'bull (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (< (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-stop pa)))
                            (and (equal? 'bear (hash-ref bull-bear-roo (position-analysis-stock pa)))
                                 (> (hash-ref ref-prices (position-analysis-stock pa))
                                    (position-analysis-stock-stop pa)))
                            (and (string-contains? (position-analysis-strategy pa) "CONDOR")
                                 (or (< (hash-ref ref-prices (position-analysis-stock pa))
                                        (first (hash-ref min-max-strikes (position-analysis-stock pa))))
                                     (> (hash-ref ref-prices (position-analysis-stock pa))
                                        (second (hash-ref min-max-strikes (position-analysis-stock pa))))))))
                updated-position-analysis-list))

  (define remaining-position-analysis-list
    (remove* target-position-analysis-list
             (remove* stop-position-analysis-list updated-position-analysis-list)))

  (set! expired-position-analysis-list (filter (λ (pa) (and (not (equal? "" (position-analysis-end-date pa)))
                                                            (date>=? (today)
                                                                     (parse-date (position-analysis-end-date pa) "yy-MM-dd"))))
                                               remaining-position-analysis-list))

  (set! open-position-analysis-list (remove* expired-position-analysis-list remaining-position-analysis-list))

  (update-analysis-box open-analysis-box-ref open-position-analysis-list)
  (update-analysis-box stop-analysis-box-ref stop-position-analysis-list)
  (update-analysis-box target-analysis-box-ref target-position-analysis-list)
  (update-analysis-box expired-analysis-box-ref expired-position-analysis-list))

(struct market (bid ask) #:transparent)

(define prices (make-hash))

(define request-id-stock (make-hash))

(define req-id 1)

(define ref-price-channel (make-async-channel))

(send ibkr send-msg (new market-data-type-req% [market-data-type 'delayed-frozen]))

(ibkr-add-handler 'market-data
                  (λ (md)
                    (cond [(and (or (equal? 'last-price (market-data-rsp-type md))
                                    (equal? 'delayed-last (market-data-rsp-type md))
                                    (and (or (saturday? (today))
                                             (sunday? (today))
                                             (<= 12 (->hours (current-time))))
                                         (or (equal? 'close (market-data-rsp-type md))
                                             (equal? 'delayed-close (market-data-rsp-type md))
                                             (equal? 'mark-price (market-data-rsp-type md)))))
                                (not (= 0 (market-data-rsp-value md)))
                                (not (hash-has-key? prices (market-data-rsp-request-id md))))
                           (hash-set! prices (market-data-rsp-request-id md) (market-data-rsp-value md))
                           (async-channel-put ref-price-channel (list (hash-ref request-id-stock (market-data-rsp-request-id md))
                                                                      (market-data-rsp-value md)))
                           (send ibkr send-msg (new cancel-market-data-req% [request-id (market-data-rsp-request-id md)]))])))

(define (get-ref-prices position-analysis-list)
  (define stocks (remove-duplicates (map (λ (pa) (position-analysis-stock pa))
                                         position-analysis-list)))
  (define ref-prices (make-hash))
  (for-each (λ (s)
              (hash-set! request-id-stock req-id s)
              (send ibkr send-msg (new market-data-req% [request-id req-id]
                                       [symbol s]
                                       [security-type 'stk]
                                       [exchange "SMART"]
                                       [currency "USD"]
                                       [primary-exchange "NYSE"]
                                       [generic-tick-list "221"])) ; send along 'mark-price in our streaming data
              (set! req-id (add1 req-id))
              (define symbol-price (async-channel-get ref-price-channel))
              (hash-set! ref-prices (first symbol-price) (last symbol-price)))
            stocks)
  ref-prices)

(define (get-min-max-strikes position-analysis-list)
  (foldl (λ (pa h)
           (if (hash-has-key? h (position-analysis-stock pa))
               (let [(hash-val (hash-ref h (position-analysis-stock pa)))]
                 (hash-set h (position-analysis-stock pa)
                           (list (min (first hash-val) (position-analysis-strike pa))
                                 (max (second hash-val) (position-analysis-strike pa)))))
               (hash-set h (position-analysis-stock pa)
                         (list (position-analysis-strike pa) (position-analysis-strike pa)))))
         (hash)
         position-analysis-list))

(define (update-analysis-box box-ref position-analysis-list)
  (send box-ref set
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
  (map (λ (m i) (send box-ref set-data i m))
       position-analysis-list (range (length position-analysis-list))))

(define analysis-box-columns (list "Sector" "Stock" "Expiry" "Strike" "CallPut" "Account"
                                   "Qty" "StkStop" "StkPrc" "StkTgt" "EndDt"))

(define (position-analysis-box parent-panel start-date end-date)
  (set! position-panel (new vertical-panel% [parent parent-panel] [alignment '(left top)]))

  (set! position-summary (new message% [parent position-panel] [label position-summary-text]))

  (set! position-history (new message% [parent position-panel] [label position-history-text]))

  (define (analysis-box name height)
    (define box (new list-box%
                     [parent position-panel]
                     [label name]
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
                                                  end-date)))]
                     [style (list 'single 'column-headers 'vertical-label)]
                     [columns analysis-box-columns]
                     [choices (list "")]
                     [min-height height]
                     [stretchable-height (not height)]))
    (let ([box-width (send box get-width)]
          [num-cols (length analysis-box-columns)])
      (for-each (λ (i) (send box set-column-width i 80 80 80))
                (range num-cols)))
    box)

  (set! open-analysis-box-ref (analysis-box "Open" #f))
  (update-analysis-box open-analysis-box-ref open-position-analysis-list)
  (set! stop-analysis-box-ref (analysis-box "Stop" 150))
  (update-analysis-box stop-analysis-box-ref stop-position-analysis-list)
  (set! target-analysis-box-ref (analysis-box "Target" 150))
  (update-analysis-box target-analysis-box-ref target-position-analysis-list)
  (set! expired-analysis-box-ref (analysis-box "Expired" 150))
  (update-analysis-box expired-analysis-box-ref expired-position-analysis-list))
