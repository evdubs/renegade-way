#lang racket/base

(require gregor
         racket/async-channel
         racket/class
         racket/format
         racket/list
         racket/set
         racket/string
         interactive-brokers-api/request-messages
         interactive-brokers-api/response-messages
         "ibkr.rkt")

(provide get-option-market-data)

(define req-id 0)

(define option-market-data-channel (make-async-channel))

(send ibkr send-msg (new market-data-type-req% [market-data-type 'delayed-frozen]))

(define omd-hash (make-hash))

(ibkr-add-handler 'option-market-data
                  (λ (omd)
                    (cond [(or (equal? 'model-option-computation (option-market-data-rsp-tick-type omd))
                               (equal? 'delayed-model-option-computation (option-market-data-rsp-tick-type omd)))
                           (async-channel-put option-market-data-channel omd)
                           (cond [(not (hash-has-key? omd-hash (option-market-data-rsp-request-id omd)))
                                  (send ibkr send-msg (new cancel-market-data-req% [request-id (option-market-data-rsp-request-id omd)]))
                                  (hash-set! omd-hash (option-market-data-rsp-request-id omd) omd)])])))

(ibkr-add-handler 'err
                  (λ (err)
                    (cond [(= 200 (err-rsp-error-code err)) ; no security definition found
                           (async-channel-put option-market-data-channel (err-rsp-id err))
                           (cond [(not (hash-has-key? omd-hash (err-rsp-id err)))
                                  (send ibkr send-msg (new cancel-market-data-req% [request-id (err-rsp-id err)]))
                                  (hash-set! omd-hash (err-rsp-id err) #f)])])))

(define (get-option-market-data sym exp strk rt)
  (set! req-id (add1 req-id))
  (send ibkr send-msg (new market-data-req%
                           [request-id req-id]
                           [security-type 'opt]
                           [exchange "SMART"]
                           [currency "USD"]
                           [symbol (string-replace sym "." " ")]
                           [expiry (iso8601->date exp)]
                           [strike strk]
                           [right rt]))
  (let loop ()
    (define omd (async-channel-get option-market-data-channel))
    (cond [(hash-has-key? omd-hash req-id)
           (hash-ref omd-hash req-id)]
          [(and (number? omd) (= req-id omd))
           #f]
          [(and (struct? omd) (= req-id (option-market-data-rsp-request-id omd)))
           omd]
          [else
           (sleep 1)
           (loop)])))
