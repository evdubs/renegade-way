#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide high-base-entry)

(struct high-base-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-10-high
   dc-10-low
   dc-50-high
   dc-50-low)
  #:transparent)

(define (high-base-in-drop-1 lbi)
  (high-base-in (stream-rest (high-base-in-dohlc lbi))
                (stream-rest (high-base-in-sma-20 lbi))
                (stream-rest (high-base-in-sma-20-slope lbi))
                (stream-rest (high-base-in-sma-50 lbi))
                (stream-rest (high-base-in-sma-50-slope lbi))
                (stream-rest (high-base-in-satr-50 lbi))
                (stream-rest (high-base-in-dc-10-high lbi))
                (stream-rest (high-base-in-dc-10-low lbi))
                (stream-rest (high-base-in-dc-50-high lbi))
                (stream-rest (high-base-in-dc-50-low lbi))))

(define (high-base h ; history
                   i) ; market data inputs
  (if (or (stream-empty? (high-base-in-dohlc i))
          (stream-empty? (high-base-in-sma-20 i))
          (stream-empty? (high-base-in-sma-20-slope i))
          (stream-empty? (high-base-in-sma-50 i))
          (stream-empty? (high-base-in-sma-50-slope i))
          (stream-empty? (high-base-in-satr-50 i))
          (stream-empty? (high-base-in-dc-10-high i))
          (stream-empty? (high-base-in-dc-10-low i))
          (stream-empty? (high-base-in-dc-50-high i))
          (stream-empty? (high-base-in-dc-50-low i)))
      h

      (let ([date (dohlc-date (stream-first (high-base-in-dohlc i)))]
            [open (dohlc-open (stream-first (high-base-in-dohlc i)))]
            [high (dohlc-high (stream-first (high-base-in-dohlc i)))]
            [low (dohlc-low (stream-first (high-base-in-dohlc i)))]
            [close (dohlc-close (stream-first (high-base-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (high-base-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (high-base-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (high-base-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (high-base-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (high-base-in-satr-50 i)))]
            [dc-10-high (dv-value (stream-first (high-base-in-dc-10-high i)))]
            [dc-10-low (dv-value (stream-first (high-base-in-dc-10-low i)))]
            [dc-50-high (dv-value (stream-first (high-base-in-dc-50-high i)))]
            [dc-50-low (dv-value (stream-first (high-base-in-dc-50-low i)))])
        (cond
          ; found satisfactory conditions for entry
          [(and (< satr (* close 4/100))
                (> sma-20 sma-50)
                (< 0 sma-50-slope)
                (> close sma-20)
                (< 0 sma-20-slope)
                (< (- dc-10-high dc-10-low) (* satr 4/2))
                (< dc-50-high (+ dc-10-high (/ satr 5))))
           (let ([new-test (test 20
                                 (+ dc-10-high 5/100)
                                 (- dc-10-high (* satr 2))
                                 (+ dc-10-high (* satr 4)))])
             (high-base (history (append (history-test h)
                                         (list (dv date new-test)))
                                 (history-trade h))
                        (high-base-in-drop-1 i)))]
          ; no satisfactory conditions
          [else (high-base h (high-base-in-drop-1 i))]))))

(define (high-base-entry dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-10-high dc-10-low) (donchian-channel dohlc-vector 10)]
                [(dc-50-high dc-50-low) (donchian-channel dohlc-vector 50)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-10-high)
                                   (vector-length dc-10-low)
                                   (vector-length dc-50-high)
                                   (vector-length dc-50-low))])
    (high-base (history (list) (list))
               (high-base-in (sequence->stream (vector-take-right dohlc-vector min-length))
                             (sequence->stream (vector-take-right sma-20 min-length))
                             (sequence->stream (vector-take-right sma-20-slope min-length))
                             (sequence->stream (vector-take-right sma-50 min-length))
                             (sequence->stream (vector-take-right sma-50-slope min-length))
                             (sequence->stream (vector-take-right satr-50 min-length))
                             (sequence->stream (vector-take-right dc-10-high min-length))
                             (sequence->stream (vector-take-right dc-10-low min-length))
                             (sequence->stream (vector-take-right dc-50-high min-length))
                             (sequence->stream (vector-take-right dc-50-low min-length))))))
