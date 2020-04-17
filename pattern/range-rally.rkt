#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide range-rally-entry)

(struct range-rally-in
  (dohlc
   sma-50
   sma-50-slope
   satr-50
   dc-25-high
   dc-25-low
   dc-50-high
   dc-50-low
   csr-3)
  #:transparent)

(define (range-rally h ; history
                     i) ; market data inputs
  (if (or (stream-empty? (range-rally-in-dohlc i))
          (stream-empty? (range-rally-in-sma-50 i))
          (stream-empty? (range-rally-in-sma-50-slope i))
          (stream-empty? (range-rally-in-satr-50 i))
          (stream-empty? (range-rally-in-dc-25-high i))
          (stream-empty? (range-rally-in-dc-25-low i))
          (stream-empty? (range-rally-in-dc-50-high i))
          (stream-empty? (range-rally-in-dc-50-low i))
          (stream-empty? (range-rally-in-csr-3 i)))
      h

      (let ([date (dohlc-date (stream-first (range-rally-in-dohlc i)))]
            [open (dohlc-open (stream-first (range-rally-in-dohlc i)))]
            [high (dohlc-high (stream-first (range-rally-in-dohlc i)))]
            [low (dohlc-low (stream-first (range-rally-in-dohlc i)))]
            [close (dohlc-close (stream-first (range-rally-in-dohlc i)))]
            [sma-50 (dv-value (stream-first (range-rally-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (range-rally-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (range-rally-in-satr-50 i)))]
            [dc-25-high (dv-value (stream-first (range-rally-in-dc-25-high i)))]
            [dc-25-low (dv-value (stream-first (range-rally-in-dc-25-low i)))]
            [dc-50-high (dv-value (stream-first (range-rally-in-dc-50-high i)))]
            [dc-50-low (dv-value (stream-first (range-rally-in-dc-50-low i)))]
            [csr-3 (dv-value (stream-first (range-rally-in-csr-3 i)))])
        (cond
          ; found satisfactory conditions for entry
          [(and (< satr (* close 10/100))
                ; (> sma-50 dc-50-low) ; (+ dc-25-low satr))
                ; (< sma-50 dc-50-high) ; (- dc-25-high satr))
                (< sma-50-slope 0)
                (> (- dc-50-high dc-50-low) (* satr 3))
                (< (- dc-50-high dc-50-low) (* satr 6))
                (> dc-25-low (* dc-50-low 101/100))
                (< dc-25-high (* dc-50-high 99/100))
                (< (+ dc-25-low (/ (- dc-25-high dc-25-low) 2)) low)
                (>= csr-3 2))
           (let ([new-test (test 20
                                 (- low 5/100)
                                 (+ low (* satr 1))
                                 (- low (* satr 2)))])
             (range-rally (history (append (history-test h)
                                           (list (dv date new-test)))
                                   (history-trade h))
                          (range-rally-in-drop-1 i)))]
          ; no satisfactory conditions
          [else (range-rally h (range-rally-in-drop-1 i))]))))

(define (range-rally-in-drop-1 rri)
  (range-rally-in (stream-rest (range-rally-in-dohlc rri))
                  (stream-rest (range-rally-in-sma-50 rri))
                  (stream-rest (range-rally-in-sma-50-slope rri))
                  (stream-rest (range-rally-in-satr-50 rri))
                  (stream-rest (range-rally-in-dc-25-high rri))
                  (stream-rest (range-rally-in-dc-25-low rri))
                  (stream-rest (range-rally-in-dc-50-high rri))
                  (stream-rest (range-rally-in-dc-50-low rri))
                  (stream-rest (range-rally-in-csr-3 rri))))

(define (range-rally-entry dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-25-high dc-25-low) (donchian-channel dohlc-vector 25)]
                [(dc-50-high dc-50-low) (donchian-channel dohlc-vector 50)]
                [(csr-3) (crow-soldier-reversal dohlc-vector 3)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-25-high)
                                   (vector-length dc-25-low)
                                   (vector-length dc-50-high)
                                   (vector-length dc-50-low)
                                   (vector-length csr-3))])
    (range-rally (history (list) (list))
                 (range-rally-in (sequence->stream (vector-take-right dohlc-vector min-length))
                                 (sequence->stream (vector-take-right sma-50 min-length))
                                 (sequence->stream (vector-take-right sma-50-slope min-length))
                                 (sequence->stream (vector-take-right satr-50 min-length))
                                 (sequence->stream (vector-take-right dc-25-high min-length))
                                 (sequence->stream (vector-take-right dc-25-low min-length))
                                 (sequence->stream (vector-take-right dc-50-high min-length))
                                 (sequence->stream (vector-take-right dc-50-low min-length))
                                 (sequence->stream (vector-take-right csr-3 min-length))))))
