#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide bear-rally-entry)

(struct bear-rally-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-20-high
   dc-20-low)
  #:transparent)

(define (bear-rally-in-drop-1 lbi)
  (bear-rally-in (stream-rest (bear-rally-in-dohlc lbi))
                 (stream-rest (bear-rally-in-sma-20 lbi))
                 (stream-rest (bear-rally-in-sma-20-slope lbi))
                 (stream-rest (bear-rally-in-sma-50 lbi))
                 (stream-rest (bear-rally-in-sma-50-slope lbi))
                 (stream-rest (bear-rally-in-satr-50 lbi))
                 (stream-rest (bear-rally-in-dc-20-high lbi))
                 (stream-rest (bear-rally-in-dc-20-low lbi))))

(define (bear-rally h ; history
                    i) ; market data inputs
  (if (or (stream-empty? (bear-rally-in-dohlc i))
          (stream-empty? (bear-rally-in-sma-20 i))
          (stream-empty? (bear-rally-in-sma-20-slope i))
          (stream-empty? (bear-rally-in-sma-50 i))
          (stream-empty? (bear-rally-in-sma-50-slope i))
          (stream-empty? (bear-rally-in-satr-50 i))
          (stream-empty? (bear-rally-in-dc-20-high i))
          (stream-empty? (bear-rally-in-dc-20-low i)))
      h

      (let ([date (dohlc-date (stream-first (bear-rally-in-dohlc i)))]
            [open (dohlc-open (stream-first (bear-rally-in-dohlc i)))]
            [high (dohlc-high (stream-first (bear-rally-in-dohlc i)))]
            [low (dohlc-low (stream-first (bear-rally-in-dohlc i)))]
            [close (dohlc-close (stream-first (bear-rally-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (bear-rally-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (bear-rally-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (bear-rally-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (bear-rally-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (bear-rally-in-satr-50 i)))]
            [dc-20-high (dv-value (stream-first (bear-rally-in-dc-20-high i)))]
            [dc-20-low (dv-value (stream-first (bear-rally-in-dc-20-low i)))])
        (cond
          ; found satisfactory conditions for entry
          [(and (< satr (* close 10/100))
                (< sma-20 sma-50)
                (> 0 sma-50-slope)
                (< close sma-20)
                (> 0 sma-20-slope)
                (< close open)
                (< (+ dc-20-low satr) low)
                (> high sma-20 low)
                (> (- sma-50 sma-20) (- sma-20 dc-20-low)))
           (let ([new-test (test 20
                                 (- low 5/100)
                                 (+ low (* satr 2))
                                 (- low (* satr 4)))])
             (bear-rally (history (append (history-test h)
                                          (list (dv date new-test)))
                                  (history-trade h))
                         (bear-rally-in-drop-1 i)))]
          ; no satisfactory conditions
          [else (bear-rally h (bear-rally-in-drop-1 i))]))))

(define (bear-rally-entry dohlc-list)
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
    (bear-rally (history (list) (list))
                (bear-rally-in (sequence->stream (vector-take-right dohlc-vector min-length))
                               (sequence->stream (vector-take-right sma-20 min-length))
                               (sequence->stream (vector-take-right sma-20-slope min-length))
                               (sequence->stream (vector-take-right sma-50 min-length))
                               (sequence->stream (vector-take-right sma-50-slope min-length))
                               (sequence->stream (vector-take-right satr-50 min-length))
                               (sequence->stream (vector-take-right dc-20-high min-length))
                               (sequence->stream (vector-take-right dc-20-low min-length))))))
