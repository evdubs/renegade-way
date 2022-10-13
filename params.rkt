#lang racket/base

(require gregor)

(provide bearish?
         db-host
         db-user
         db-name
         db-pass
         filename
         ibkr-hostname
         ibkr-port-no
         save-markets
         save-end-date
         sim-start-date
         sim-end-date)

(define bearish? (make-parameter #f))

(define db-host (make-parameter "127.0.0.1"))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define filename (make-parameter ""))

(define ibkr-hostname (make-parameter "127.0.0.1"))

(define ibkr-port-no (make-parameter 7497))

(define save-markets (make-parameter "SPY,MDY,SLY"))

(define save-end-date (make-parameter (today)))

(define sim-start-date (make-parameter (-years (today) 1)))

(define sim-end-date (make-parameter (today)))
