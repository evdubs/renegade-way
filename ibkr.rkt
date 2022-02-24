#lang racket/base

(require interactive-brokers-api/main
         interactive-brokers-api/request-messages
         interactive-brokers-api/response-messages
         racket/class
         racket/list
         racket/set
         "db-queries.rkt"
         "params.rkt")

(provide ibkr
         ibkr-account
         ibkr-add1-next-order-id
         ibkr-add-handler
         ibkr-next-order-id)

(define (ibkr-add-handler key handler)
  (hash-set! ibkr-handlers key (append (hash-ref ibkr-handlers key) (list handler))))

(define (ibkr-add1-next-order-id)
  (set! ibkr-next-order-id (add1 ibkr-next-order-id)))

(define ibkr-handlers
  (make-hash (list (cons 'account-value (list))
                   (cons 'accounts (list (λ (as) (set! ibkr-account (first as)))))
                   (cons 'commission-report (list (λ (cr) (insert-commission-report cr))))
                   (cons 'contract-details (list (λ (cd) (insert-contract cd))))
                   (cons 'err (list))
                   (cons 'execution (list (λ (e) (insert-execution e)
                                             (cond [(not (set-member? handled-contract-ids (execution-rsp-contract-id e)))
                                                    (set-add! handled-contract-ids (execution-rsp-contract-id e))
                                                    (thread (λ () (send ibkr send-msg
                                                                        (new contract-details-req% [contract-id (execution-rsp-contract-id e)]))))]))))
                   (cons 'historical-data (list))
                   (cons 'market-data (list))
                   (cons 'next-valid-id (list (λ (nvi) (set! ibkr-next-order-id (next-valid-id-rsp-order-id nvi)))))
                   (cons 'open-order (list (λ (oo) (insert-order oo))))
                   (cons 'order-status (list))
                   (cons 'portfolio-value (list))
                   (cons 'server-time (list)))))

(define ibkr (new ibkr-session%
                  [hostname (ibkr-hostname)]
                  [port-no (ibkr-port-no)]
                  [handle-account-value-rsp (λ (av) (for-each (λ (avh) (avh av)) (hash-ref ibkr-handlers 'account-value)))]
                  [handle-accounts-rsp (λ (a) (for-each (λ (ah) (ah a)) (hash-ref ibkr-handlers 'accounts)))]
                  [handle-commission-report-rsp (λ (cr) (for-each (λ (crh) (crh cr)) (hash-ref ibkr-handlers 'commission-report)))]
                  [handle-contract-details-rsp (λ (cd) (for-each (λ (cdh) (cdh cd)) (hash-ref ibkr-handlers 'contract-details)))]
                  [handle-err-rsp (λ (e) (for-each (λ (eh) (eh e)) (hash-ref ibkr-handlers 'err)))]
                  [handle-execution-rsp (λ (e) (for-each (λ (eh) (eh e)) (hash-ref ibkr-handlers 'execution)))]
                  [handle-historical-data-rsp (λ (hd) (for-each (λ (hdh) (hdh hd)) (hash-ref ibkr-handlers 'historical-data)))]
                  [handle-market-data-rsp (λ (md) (for-each (λ (mdh) (mdh md)) (hash-ref ibkr-handlers 'market-data)))]
                  [handle-next-valid-id-rsp (λ (nvi) (for-each (λ (nvih) (nvih nvi)) (hash-ref ibkr-handlers 'next-valid-id)))]
                  [handle-open-order-rsp (λ (oo) (for-each (λ (ooh) (ooh oo)) (hash-ref ibkr-handlers 'open-order)))]
                  [handle-order-status-rsp (λ (os) (for-each (λ (osh) (osh os)) (hash-ref ibkr-handlers 'order-status)))]
                  [handle-portfolio-value-rsp (λ (pv) (for-each (λ (pvh) (pvh pv)) (hash-ref ibkr-handlers 'portfolio-value)))]
                  [handle-server-time-rsp (λ (st) (for-each (λ (sth) (sth st)) (hash-ref ibkr-handlers 'server-time)))]
                  [write-messages #t]))

(define handled-contract-ids (mutable-set))

(define ibkr-next-order-id 0)

(define ibkr-account "")

(send ibkr connect)
