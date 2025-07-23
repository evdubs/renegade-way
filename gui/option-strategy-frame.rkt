#lang racket/base

(require gregor
         racket/class
         racket/gui/base
         racket/list
         racket/match
         racket/string
         "../db-queries.rkt"
         "../option-strategy.rkt"
         "../structs.rkt"
         "position-order-manager.rkt")

(provide show-option-strategy
         refresh-option-strategy)

(define strategy-frame
  (new frame% [label "Option Strategy"] [width 1050] [height 600]))

(define strategy-input-pane
  (new horizontal-pane%
       [parent strategy-frame]
       [stretchable-height #f]))

(define symbol-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Symbol"]
       [init-value ""]))

(define date-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Date"]
       [init-value (date->iso8601 (today))]))

(define ref-price-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Ref Price"]
       [init-value ""]))

(define patterns-field
  (new text-field%
       [parent strategy-input-pane]
       [label "Patterns"]
       [init-value ""]))

(define fit-vols-check-box
  (new check-box%
       [parent strategy-input-pane]
       [label "Fit Vols"]
       [value #t]))

(define refresh-button
  (new button%
       [parent strategy-input-pane]
       [label "Refresh"]
       [callback (λ (c e) (refresh-option-strategy (send symbol-field get-value)
                                                   (send date-field get-value)
                                                   (string->number (send ref-price-field get-value))
                                                   (send patterns-field get-value)))]))

(define strategy-table-pane (new vertical-pane% [parent strategy-frame]))

(define option-columns (list "Symbol" "Expiry" "Strike" "Call/Put" "Date" "Bid" "Ask" "Spread" "BsPrc" "Vol" "Delta" "Gamma" "Theta" "Vega" "Rho"))

(define (refresh-option-strategy symbol date ref-price patterns)
  ; clear contents except for input pane
  (cond [(> (length (send strategy-frame get-children)) 1)
         (for-each (λ (vp) (for-each (λ (lb) (send vp delete-child lb))
                                     (send vp get-children)))
                   (drop (send strategy-frame get-children) 1))])
  (send symbol-field set-value symbol)
  (send date-field set-value date)
  (send ref-price-field set-value (real->decimal-string ref-price))
  (send patterns-field set-value patterns)
  (hash-for-each (suitable-options (get-updated-options symbol date ref-price #:fit-vols (send fit-vols-check-box get-value)) patterns)
                 (λ (k v)
                   (let ([table (new list-box% [parent strategy-table-pane]
                                     [label k]
                                     [style (list 'single 'column-headers 'vertical-label)]
                                     [columns option-columns]
                                     [choices (list "")]
                                     [callback (λ (b e)
                                                 (define pattern (match (first (string-split patterns " "))
                                                                   ["BP" 'bull-pullback]
                                                                   ["BR" 'bear-rally]
                                                                   ["HB" 'high-base]
                                                                   ["LB" 'low-base]
                                                                   ["AT" 'ascending-triangle]
                                                                   ["DT" 'descending-triangle]
                                                                   ["RR" 'range-rally]
                                                                   ["RP" 'range-pullback]
                                                                   ["IR" 'increasing-rank]
                                                                   ["DR" 'decreasing-rank]
                                                                   ["IV" 'increasing-vol]
                                                                   ["DV" 'decreasing-vol]
                                                                   ["CC" 'decreasing-vol] ; call condor
                                                                   ["EC" 'decreasing-vol]))
                                                 (define order-data
                                                   (map (λ (o)
                                                          (order pattern
                                                                 (string->symbol (string-replace (string-downcase k) " " "-"))
                                                                 (option-symbol o)
                                                                 (parse-date (option-expiration o) "yy-MM-dd")
                                                                 (option-strike o)
                                                                 (string->symbol (string-downcase (option-call-put o)))
                                                                 #f
                                                                 (option-mid o)
                                                                 (option-vol o)
                                                                 (- (option-ask o) (option-bid o))
                                                                 (string->number (send ref-price-field get-value))
                                                                 #f
                                                                 #f
                                                                 (+months (iso8601->date (send date-field get-value)) 1)))
                                                        v))
                                                 (define earnings-date (get-next-earnings-date (order-symbol (first order-data))
                                                                                               (today)
                                                                                               (order-end-date (first order-data))))
                                                 (define first-expiry (foldl (λ (o res) (if (date<? (order-expiration o) res)
                                                                                            (order-expiration o)
                                                                                            res))
                                                                             earnings-date
                                                                             order-data))
                                                 (define eval-date (if (date<? (order-end-date (first order-data)) first-expiry)
                                                                       (order-end-date (first order-data))
                                                                       first-expiry))

                                                 (set-order-data order-data eval-date))])])
                     (send table set
                           (map (λ (o) (option-symbol o)) v)
                           (map (λ (o) (option-expiration o)) v)
                           (map (λ (o) (real->decimal-string (option-strike o))) v)
                           (map (λ (o) (option-call-put o)) v)
                           (map (λ (o) (option-date o)) v)
                           (map (λ (o) (real->decimal-string (option-bid o))) v)
                           (map (λ (o) (real->decimal-string (option-ask o))) v)
                           (map (λ (o) (real->decimal-string (/ (- (option-ask o) (option-bid o)) (option-ask o)))) v)
                           (map (λ (o) (real->decimal-string (option-mid o))) v)
                           (map (λ (o) (real->decimal-string (option-vol o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-delta o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-gamma o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-theta o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-vega o) 3)) v)
                           (map (λ (o) (real->decimal-string (option-rho o) 3)) v))
                     (let ([box-width (send table get-width)]
                           [num-cols (length option-columns)])
                       (for-each (λ (i) (send table set-column-width i
                                              80
                                              80
                                              80))
                                 (range num-cols)))))))

(define (show-option-strategy)
  (send strategy-frame show #t))
