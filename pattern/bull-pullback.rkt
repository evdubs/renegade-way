#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide bull-pullback-entry)

(struct bull-pullback-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-20-high
   dc-20-low)
  #:transparent)

(define (bull-pullback-in-drop-1 lbi)
  (bull-pullback-in (stream-rest (bull-pullback-in-dohlc lbi))
                    (stream-rest (bull-pullback-in-sma-20 lbi))
                    (stream-rest (bull-pullback-in-sma-20-slope lbi))
                    (stream-rest (bull-pullback-in-sma-50 lbi))
                    (stream-rest (bull-pullback-in-sma-50-slope lbi))
                    (stream-rest (bull-pullback-in-satr-50 lbi))
                    (stream-rest (bull-pullback-in-dc-20-high lbi))
                    (stream-rest (bull-pullback-in-dc-20-low lbi))))

(define (bull-pullback h ; history
                       i) ; market data inputs
  (if (or (stream-empty? (bull-pullback-in-dohlc i))
          (stream-empty? (bull-pullback-in-sma-20 i))
          (stream-empty? (bull-pullback-in-sma-20-slope i))
          (stream-empty? (bull-pullback-in-sma-50 i))
          (stream-empty? (bull-pullback-in-sma-50-slope i))
          (stream-empty? (bull-pullback-in-satr-50 i))
          (stream-empty? (bull-pullback-in-dc-20-high i))
          (stream-empty? (bull-pullback-in-dc-20-low i)))
      h

      (let ([date (dohlc-date (stream-first (bull-pullback-in-dohlc i)))]
            [open (dohlc-open (stream-first (bull-pullback-in-dohlc i)))]
            [high (dohlc-high (stream-first (bull-pullback-in-dohlc i)))]
            [low (dohlc-low (stream-first (bull-pullback-in-dohlc i)))]
            [close (dohlc-close (stream-first (bull-pullback-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (bull-pullback-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (bull-pullback-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (bull-pullback-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (bull-pullback-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (bull-pullback-in-satr-50 i)))]
            [dc-20-high (dv-value (stream-first (bull-pullback-in-dc-20-high i)))]
            [dc-20-low (dv-value (stream-first (bull-pullback-in-dc-20-low i)))])
        (cond
          ; found satisfactory conditions for entry
          [(and (< satr (* close 4/100))
                (> sma-20 sma-50)
                (< 0 sma-50-slope)
                (> close sma-20)
                (< 0 sma-20-slope)
                (> close open)
                (> (- dc-20-high satr) high)
                (> high sma-20 low)
                (> (- sma-20 sma-50) (- dc-20-high sma-20))
                )
           (let ([new-test (test 20
                                 (+ high 5/100)
                                 (- high (* satr 2))
                                 (+ high (* satr 4)))])
             (bull-pullback (history (append (history-test h)
                                             (list (dv date new-test)))
                                     (history-trade h))
                            (bull-pullback-in-drop-1 i)))]
          ; no satisfactory conditions
          [else (bull-pullback h (bull-pullback-in-drop-1 i))]))))

(define (bull-pullback-entry dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-20-high dc-20-low) (donchian-channel dohlc-vector 20)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-20-high)
                                   (vector-length dc-20-low))])
    (bull-pullback (history (list) (list))
                   (bull-pullback-in (sequence->stream (vector-take-right dohlc-vector min-length))
                                     (sequence->stream (vector-take-right sma-20 min-length))
                                     (sequence->stream (vector-take-right sma-20-slope min-length))
                                     (sequence->stream (vector-take-right sma-50 min-length))
                                     (sequence->stream (vector-take-right sma-50-slope min-length))
                                     (sequence->stream (vector-take-right satr-50 min-length))
                                     (sequence->stream (vector-take-right dc-20-high min-length))
                                     (sequence->stream (vector-take-right dc-20-low min-length))))))
