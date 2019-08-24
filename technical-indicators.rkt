#lang racket/base

(require racket/sequence
         racket/vector
         "structs.rkt")

(provide simple-moving-average
         simple-average-true-range
         donchian-channel
         delta
         shift
         crow-soldier-reversal)

(define (vector-partition v period step)
  (if (> period (vector-length v)) (vector)
      (vector-append (vector (vector-take v period))
                     (vector-partition (vector-drop v step) period step))))

(define (simple-moving-average date-ohlc-vector period)
  (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                         (/ (sequence-fold (λ (acc el) (+ acc (dohlc-close el))) 0
                                           (vector-take v period)) period)))
              (vector-partition date-ohlc-vector period 1)))

(define (true-range high low previous-close)
  (max (- high low)
       (abs (- high previous-close))
       (abs (- low previous-close))))

(define (simple-average-true-range date-ohlc-vector period)
  (let ([true-ranges (vector-map (λ (v) (dv (dohlc-date (vector-ref v 1))
                                            (true-range (dohlc-high (vector-ref v 1))
                                                        (dohlc-low (vector-ref v 1))
                                                        (dohlc-close (vector-ref v 0)))))
                                 (vector-partition date-ohlc-vector 2 1))])
    (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                           (/ (sequence-fold (λ (acc el) (+ acc (dv-value el))) 0
                                             (vector-take v period)) period)))
                (vector-partition true-ranges period 1))))

(define (donchian-channel date-ohlc-vector period)
  (values (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                                 (sequence-fold (λ (acc el) (max acc (dohlc-high el)))
                                                (dohlc-high (vector-ref v 0)) v)))
                      (vector-partition date-ohlc-vector period 1))
          (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                                 (sequence-fold (λ (acc el) (min acc (dohlc-low el)))
                                                (dohlc-low (vector-ref v 0)) v)))
                      (vector-partition date-ohlc-vector period 1))))

(define (delta dv-vector period)
  (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                         (- (dv-value (vector-ref v (- period 1))) (dv-value (vector-ref v 0)))))
              (vector-partition dv-vector period 1)))

(define (shift dv-vector period)
  (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                         (dv-value (vector-ref v 0))))
              (vector-partition dv-vector period 1)))

; Technical indicator to count the number of consecutive crows or soldiers before a reversal.
; There is probably a better name for this.
; 
; https://en.wikipedia.org/wiki/Three_black_crows
; https://en.wikipedia.org/wiki/Three_white_soldiers
(define (crow-soldier-reversal date-ohlc-vector period)
  (let ([ud (vector-map (λ (v) (cond [(and (> (dohlc-high (vector-ref v 1))
                                              (dohlc-high (vector-ref v 0)))
                                           (> (dohlc-low (vector-ref v 1))
                                              (dohlc-low (vector-ref v 0))))
                                      (dv (dohlc-date (vector-ref v 1)) 1)]
                                     [(and (< (dohlc-high (vector-ref v 1))
                                              (dohlc-high (vector-ref v 0)))
                                           (< (dohlc-low (vector-ref v 1))
                                              (dohlc-low (vector-ref v 0))))
                                      (dv (dohlc-date (vector-ref v 1)) -1)]
                                     [else (dv (dohlc-date (vector-ref v 1)) 0)]))
                        (vector-partition date-ohlc-vector 2 1))])
    (vector-map (λ (v) (let ([sum (sequence-fold (λ (acc el) (+ (dv-value el) acc)) 0
                                                 (vector-take v (- (vector-length v) 1)))])
                         (cond [(and (= -1 (dv-value (vector-ref v (- (vector-length v) 1))))
                                     (= sum (- (vector-length v) 1)))
                                (dv (dv-date (vector-ref v (- (vector-length v) 1)))
                                    sum)]
                               [(and (= 1 (dv-value (vector-ref v (- (vector-length v) 1))))
                                     (= (* sum -1) (- (vector-length v) 1)))
                                (dv (dv-date (vector-ref v (- (vector-length v) 1)))
                                    sum)]
                               [else (dv (dv-date (vector-ref v (- (vector-length v) 1)))
                                         0)])))
                (vector-partition ud period 1))))
