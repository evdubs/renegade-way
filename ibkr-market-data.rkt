#lang racket/base

(require gregor
         racket/class
         racket/string
         interactive-brokers-api/request-messages
         interactive-brokers-api/response-messages
         racket/async-channel
         "ibkr.rkt")

(provide get-option-market-data)

(define req-id 1)

(define option-market-data-channel (make-async-channel))

(send ibkr send-msg (new market-data-type-req% [market-data-type 'delayed-frozen]))

(ibkr-add-handler 'option-market-data
                  (λ (omd)
                    (cond [(or (equal? 'model-option-computation (option-market-data-rsp-tick-type omd))
                               (equal? 'delayed-model-option-computation (option-market-data-rsp-tick-type omd)))
                           (async-channel-put option-market-data-channel omd)
                           (send ibkr send-msg (new cancel-market-data-req% [request-id (option-market-data-rsp-request-id omd)]))])))

(define (get-option-market-data sym exp strk rt)
  (send ibkr send-msg (new market-data-req%
                           [request-id req-id]
                           [security-type 'opt]
                           [exchange "SMART"]
                           [currency "USD"]
                           [symbol (string-replace sym "." " ")]
                           [expiry (iso8601->date exp)]
                           [strike strk]
                           [right rt]))
  (async-channel-get option-market-data-channel))
