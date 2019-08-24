#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide descending-triangle-entry)

(struct descending-triangle-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-25-high
   dc-25-low
   dc-25-prev-high
   dc-25-prev-low)
  #:transparent)

(define (descending-triangle h ; history
                             i) ; market data inputs
  (if (or (stream-empty? (descending-triangle-in-dohlc i))
          (stream-empty? (descending-triangle-in-sma-20 i))
          (stream-empty? (descending-triangle-in-sma-20-slope i))
          (stream-empty? (descending-triangle-in-sma-50 i))
          (stream-empty? (descending-triangle-in-sma-50-slope i))
          (stream-empty? (descending-triangle-in-satr-50 i))
          (stream-empty? (descending-triangle-in-dc-25-high i))
          (stream-empty? (descending-triangle-in-dc-25-low i))
          (stream-empty? (descending-triangle-in-dc-25-prev-high i))
          (stream-empty? (descending-triangle-in-dc-25-prev-low i)))
      h

      (let ([date (dohlc-date (stream-first (descending-triangle-in-dohlc i)))]
            [open (dohlc-open (stream-first (descending-triangle-in-dohlc i)))]
            [high (dohlc-high (stream-first (descending-triangle-in-dohlc i)))]
            [low (dohlc-low (stream-first (descending-triangle-in-dohlc i)))]
            [close (dohlc-close (stream-first (descending-triangle-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (descending-triangle-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (descending-triangle-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (descending-triangle-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (descending-triangle-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (descending-triangle-in-satr-50 i)))]
            [dc-25-high (dv-value (stream-first (descending-triangle-in-dc-25-high i)))]
            [dc-25-low (dv-value (stream-first (descending-triangle-in-dc-25-low i)))]
            [dc-25-prev-high (dv-value (stream-first (descending-triangle-in-dc-25-prev-high i)))]
            [dc-25-prev-low (dv-value (stream-first (descending-triangle-in-dc-25-prev-low i)))])
        (cond
          ; found satisfactory conditions for entry
          [(and (< satr (* close 4/100))
                (< sma-20 sma-50)
                (> 0 sma-50-slope)
                (< close sma-20)
                (> 0 sma-20-slope)
                (< close (+ dc-25-low (* satr 4/2)))
                (< dc-25-low dc-25-prev-low)
                (< dc-25-prev-low (* dc-25-low 101/100))
                (< (+ dc-25-high (* (- dc-25-high dc-25-low) 1/2)) dc-25-prev-high))
           (let ([new-test (test 20
                                 (- dc-25-low 5/100)
                                 (+ dc-25-low (* satr 2))
                                 (- dc-25-low (* satr 4)))])
             (descending-triangle (history (append (history-test h)
                                                   (list (dv date new-test)))
                                           (history-trade h))
                                  (descending-triangle-in-drop-1 i)))]
          ; no satisfactory conditions
          [else (descending-triangle h (descending-triangle-in-drop-1 i))]))))

(define (descending-triangle-in-drop-1 atid)
  (descending-triangle-in (stream-rest (descending-triangle-in-dohlc atid))
                          (stream-rest (descending-triangle-in-sma-20 atid))
                          (stream-rest (descending-triangle-in-sma-20-slope atid))
                          (stream-rest (descending-triangle-in-sma-50 atid))
                          (stream-rest (descending-triangle-in-sma-50-slope atid))
                          (stream-rest (descending-triangle-in-satr-50 atid))
                          (stream-rest (descending-triangle-in-dc-25-high atid))
                          (stream-rest (descending-triangle-in-dc-25-low atid))
                          (stream-rest (descending-triangle-in-dc-25-prev-high atid))
                          (stream-rest (descending-triangle-in-dc-25-prev-low atid))))

(define (descending-triangle-entry dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-25-high dc-25-low) (donchian-channel dohlc-vector 25)]
                [(dc-25-prev-high dc-25-prev-low) (values (shift dc-25-high 25) (shift dc-25-low 25))]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-25-high)
                                   (vector-length dc-25-low)
                                   (vector-length dc-25-prev-high)
                                   (vector-length dc-25-prev-low))])
    (descending-triangle (history (list) (list))
                         (descending-triangle-in (sequence->stream (vector-take-right dohlc-vector min-length))
                                                 (sequence->stream (vector-take-right sma-20 min-length))
                                                 (sequence->stream (vector-take-right sma-20-slope min-length))
                                                 (sequence->stream (vector-take-right sma-50 min-length))
                                                 (sequence->stream (vector-take-right sma-50-slope min-length))
                                                 (sequence->stream (vector-take-right satr-50 min-length))
                                                 (sequence->stream (vector-take-right dc-25-high min-length))
                                                 (sequence->stream (vector-take-right dc-25-low min-length))
                                                 (sequence->stream (vector-take-right dc-25-prev-high min-length))
                                                 (sequence->stream (vector-take-right dc-25-prev-low min-length))))))
