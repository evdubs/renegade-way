#lang racket/base

(require gregor
         racket/class
         racket/gui/base
         racket/match
         "../condor-analysis.rkt"
         "../price-analysis.rkt"
         "condor-analysis-box.rkt"
         "price-analysis-box.rkt"
         "rank-analysis.rkt"
         "vol-analysis.rkt"
         "earnings-vibes-analysis.rkt"
         "position-analysis.rkt")

(provide analysis-tab-panel
         show-analysis)

(define analysis-frame
  (new frame% [label "Analysis"] [width 1000] [height 1000]))

(define analysis-input-pane
  (new horizontal-pane%
       [parent analysis-frame]
       [stretchable-height #f]))

(define market-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Market(s)"]
       [init-value "SPY,MDY,SLY,SPSM"]))

(define sector-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Sector"]
       [init-value ""]))

(define start-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "Start Date"]
       [init-value (date->iso8601 (+days (-months (today) 5) 1))]))

(define end-date-field
  (new text-field%
       [parent analysis-input-pane]
       [label "End Date"]
       [init-value (date->iso8601 (+days (today) 1))]))

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
                                          #:hide-large-spread (send hide-spread-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-no-pattern-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide No Pattern"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (condor-analysis-filter #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                           #:hide-large-spread (send hide-spread-check-box get-value)
                                           #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-spread-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Large Spread"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (rank-analysis-filter #:hide-large-spread (send hide-spread-check-box get-value)
                                         #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (vol-analysis-filter #:hide-large-spread (send hide-spread-check-box get-value)
                                        #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (condor-analysis-filter #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                           #:hide-large-spread (send hide-spread-check-box get-value)
                                           #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define hide-non-weekly-check-box
  (new check-box%
       [parent filter-input-pane]
       [label "Hide Non-Weekly"]
       [callback (λ (b e)
                   (price-analysis-filter #:hide-hold (send hide-hold-check-box get-value)
                                          #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                          #:hide-large-spread (send hide-spread-check-box get-value)
                                          #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (rank-analysis-filter #:hide-large-spread (send hide-spread-check-box get-value)
                                         #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (vol-analysis-filter #:hide-large-spread (send hide-spread-check-box get-value)
                                        #:hide-non-weekly (send hide-non-weekly-check-box get-value))
                   (condor-analysis-filter #:hide-no-pattern (send hide-no-pattern-check-box get-value)
                                           #:hide-large-spread (send hide-spread-check-box get-value)
                                           #:hide-non-weekly (send hide-non-weekly-check-box get-value)))]))

(define fit-vols-check-box
  (new check-box%
       [parent filter-input-pane]
       [value #t]
       [label "Fit Vols"]))

(define analysis-tab-panel
  (new tab-panel%
       [choices (list "Price" "Rank" "Vol" "Condor" "ErnVibe" "Position")]
       [parent analysis-frame]
       [callback (λ (p e) (refresh-tab-panel))]))

(define (refresh-tab-panel)
  (map (λ (c) (send analysis-tab-panel delete-child c)) (send analysis-tab-panel get-children))
  (match (send analysis-tab-panel get-item-label (send analysis-tab-panel get-selection))
    ["Price" (price-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Rank" (rank-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Vol" (vol-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["Condor" (condor-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
    ["ErnVibe" (earnings-vibes-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))]
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
                                                  (send start-date-field get-value) (send end-date-field get-value))
                              (update-price-analysis-box price-analysis-list price-analysis-hash)]
                     ["Rank" (refresh-tab-panel)
                             (run-rank-analysis (send market-field get-value) (send sector-field get-value)
                                                (send start-date-field get-value) (send end-date-field get-value))]
                     ["Vol" (refresh-tab-panel)
                            (run-vol-analysis (send market-field get-value) (send sector-field get-value)
                                              (send start-date-field get-value) (send end-date-field get-value))]
                     ["Condor" (refresh-tab-panel)
                               (run-condor-analysis (send market-field get-value) (send sector-field get-value)
                                                    (send start-date-field get-value) (send end-date-field get-value)
                                                    #:fit-vols (send fit-vols-check-box get-value))
                               (update-condor-analysis-box condor-analysis-list condor-analysis-hash)]
                     ["ErnVibe" (refresh-tab-panel)
                                (run-earnings-vibes-analysis (send market-field get-value) (send sector-field get-value)
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
  (condor-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))
  (earnings-vibes-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))
  (position-analysis-box analysis-tab-panel (send start-date-field get-value) (send end-date-field get-value))

  (refresh-tab-panel)

  (send analysis-frame show #t))
