#lang racket/base

(require gregor
         racket/class
         racket/gui/base
         racket/match
         "position-analysis.rkt"
         "price-analysis.rkt"
         "rank-analysis.rkt"
         "vol-analysis.rkt")

(provide analysis-tab-panel
         show-analysis)

(define analysis-frame
  (new frame% [label "Analysis"] [width 900] [height 1000]))

(define analysis-input-pane
  (new horizontal-pane%
       [parent analysis-frame]
       [stretchable-height #f]))

(define market-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Market(s)"]
       [init-value "SPY,MDY,SLY"]))

(define sector-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Sector"]
       [init-value ""]))

(define start-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Start Date"]
       [init-value (date->iso8601 (-months (today) 5))]))

(define end-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "End Date"]
       [init-value (date->iso8601 (today))]))

(define filter-input-pane
  (new horizontal-pane%
       [parent analysis-frame]
       [stretchable-height #f]))

(define hide-hold-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Hold"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-20-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-no-pattern-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide No Pattern"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-20-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-spread-20-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Large Spread"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-20-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (rank-analysis-filter #:hide-large-spread (send hide-spread-20-check-box get-value)
                                         #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (vol-analysis-filter #:hide-large-spread (send hide-spread-20-check-box get-value)
                                        #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-non-weekly-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Non-Weekly"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-20-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (rank-analysis-filter #:hide-large-spread (send hide-spread-20-check-box get-value)
                                         #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (vol-analysis-filter #:hide-large-spread (send hide-spread-20-check-box get-value)
                                        #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define analysis-tab-panel
  (new tab-panel%
       [choices (list "Price" "Rank" "Vol" "Position")]
       [parent analysis-frame]
       [callback (λ (p e) (refresh-tab-panel))]))

(define (refresh-tab-panel)
  (map (λ (c) (send analysis-tab-panel delete-child c)) (send analysis-tab-panel get-children))
  (match (send analysis-tab-panel get-item-label (send analysis-tab-panel get-selection))
    ["Price" (price-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Rank" (rank-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Vol" (vol-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Position" (position-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]))

(define analyze-button
  (new button%
       [parent analysis-input-pane]
       [label "Analyze"]
       [callback (λ (b e)
                   (send b enable #f)
                   (match (send analysis-tab-panel get-item-label (send analysis-tab-panel get-selection))
                     ["Price" (refresh-tab-panel)
                              (run-price-analysis (send market-field get-value) (send sector-field get-value)
                                                  (send start-date-field get-value) (send end-date-field get-value))]
                     ["Rank" (refresh-tab-panel)
                             (run-rank-analysis (send market-field get-value) (send sector-field get-value)
                                                (send start-date-field get-value) (send end-date-field get-value))]
                     ["Vol" (refresh-tab-panel)
                            (run-vol-analysis (send market-field get-value) (send sector-field get-value)
                                              (send start-date-field get-value) (send end-date-field get-value))]
                     ["Position" (refresh-tab-panel)
                                 (run-position-analysis (send market-field get-value) (send sector-field get-value)
                                                        (send start-date-field get-value) (send end-date-field get-value))])
                   (send b enable #t))]))

(define (show-analysis)
  ; init everything so we don't get errors when selecting elements to hide
  (price-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))
  (rank-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))
  (vol-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))
  (position-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))

  (refresh-tab-panel)

  (send analysis-frame show #t))
