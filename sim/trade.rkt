#lang racket/base

(require gregor
         racket/string)

(provide (struct-out trade)
         trade->tsv
         tsv->trade)

(struct trade
  (symbol
   expirations
   strikes
   rights
   open-date
   open-price
   open-contracts
   open-commission
   close-date
   close-price
   close-contracts
   close-commission
   profit-loss
   market-rating
   sector-rating
   industry-rating
   stock-rating
   stock-risk-reward
   earnings-date
   option-spread
   num
   patterns
   entry-price
   stop-price
   target-price
   vol-slope
   iv-hv)
  #:transparent)

(define (trade->tsv t)
  (string-append (trade-symbol t) "\t"
                 (string-join (map (λ (e) (date->iso8601 e)) (trade-expirations t)) " ") "\t"
                 (string-join (map (λ (s) (real->decimal-string s)) (trade-strikes t)) " ") "\t"
                 (string-join (trade-rights t) " ") "\t"
                 (date->iso8601 (trade-open-date t)) "\t"
                 (real->decimal-string (trade-open-price t)) "\t"
                 (number->string (trade-open-contracts t)) "\t"
                 (real->decimal-string (trade-open-commission t)) "\t"
                 (date->iso8601 (trade-close-date t)) "\t"
                 (real->decimal-string (trade-close-price t)) "\t"
                 (number->string (trade-close-contracts t)) "\t"
                 (real->decimal-string (trade-close-commission t)) "\t"
                 (real->decimal-string (trade-profit-loss t)) "\t"
                 (if (trade-market-rating t) (number->string (trade-market-rating t)) "") "\t"
                 (if (trade-sector-rating t) (number->string (trade-sector-rating t)) "") "\t"
                 (if (trade-industry-rating t) (number->string (trade-industry-rating t)) "") "\t"
                 (if (trade-stock-rating t) (real->decimal-string (trade-stock-rating t)) "") "\t"
                 (if (trade-stock-risk-reward t) (real->decimal-string (trade-stock-risk-reward t)) "") "\t"
                 (if (trade-earnings-date t) (date->iso8601 (trade-earnings-date t)) "") "\t"
                 (if (trade-option-spread t) (real->decimal-string (trade-option-spread t)) "") "\t"
                 (number->string (trade-num t)) "\t"
                 (if (trade-patterns t) (trade-patterns t) "") "\t"
                 (if (trade-entry-price t) (real->decimal-string (trade-entry-price t)) "") "\t"
                 (if (trade-stop-price t) (real->decimal-string (trade-stop-price t)) "") "\t"
                 (if (trade-target-price t) (real->decimal-string (trade-target-price t)) "") "\t"
                 (if (trade-vol-slope t) (real->decimal-string (trade-vol-slope t)) "") "\t"
                 (if (trade-iv-hv t) (real->decimal-string (trade-iv-hv t)) "") "\t"))

(define (tsv->trade t)
  (define s (string-split t "\t"))
  (trade (list-ref s 0) ; symbol
         (map (λ (e) (iso8601->date e)) (string-split (list-ref s 1) " ")) ; expirations
         (map (λ (s) (string->number s)) (string-split (list-ref s 2) " ")) ; strikes
         (string-split (list-ref s 3) " ") ; rights
         (iso8601->date (list-ref s 4)) ; open-date
         (string->number (list-ref s 5)) ; open-price
         (string->number (list-ref s 6)) ; open-contracts
         (string->number (list-ref s 7)) ; open-commission
         (iso8601->date (list-ref s 8)) ; close-date
         (string->number (list-ref s 9)) ; close-price
         (string->number (list-ref s 10)) ; close-contracts
         (string->number (list-ref s 11)) ; close-commission
         (string->number (list-ref s 12)) ; profit-loss
         (if (equal? "" (list-ref s 13)) #f (string->number (list-ref s 13))) ; market-rating
         (if (equal? "" (list-ref s 14)) #f (string->number (list-ref s 14))) ; sector-rating
         (if (equal? "" (list-ref s 15)) #f (string->number (list-ref s 15))) ; industry-rating
         (if (equal? "" (list-ref s 16)) #f (string->number (list-ref s 16))) ; stock-rating
         (if (equal? "" (list-ref s 17)) #f (string->number (list-ref s 17))) ; stock-risk-reward
         (if (equal? "" (list-ref s 18)) #f (iso8601->date (list-ref s 18))) ; earnings-date
         (if (equal? "" (list-ref s 19)) #f (string->number (list-ref s 19))) ; option-spread
         (string->number (list-ref s 20)) ; num
         (if (equal? "" (list-ref s 21)) #f (list-ref s 21)) ; patterns
         (if (equal? "" (list-ref s 22)) #f (string->number (list-ref s 22))) ; entry-price
         (if (equal? "" (list-ref s 23)) #f (string->number (list-ref s 23))) ; stop-price
         (if (equal? "" (list-ref s 24)) #f (string->number (list-ref s 24))) ; target-price
         (if (equal? "" (list-ref s 25)) #f (string->number (list-ref s 25))) ; vol-slope
         (if (equal? "" (list-ref s 26)) #f (string->number (list-ref s 26))) ; iv-hv
         ))
