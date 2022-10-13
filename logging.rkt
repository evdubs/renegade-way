#lang racket/base

(require gregor)

(provide start-logging)

(define (start-logging)
  (define log (make-logger #f #f 'info #f))
  (current-logger log)
  (define rec (make-log-receiver log 'info))

  (define out (open-output-file (string-append "/var/tmp/renegade/" (~t (today) "yyyy-MM") ".log")
                                #:exists 'append))
  (file-stream-buffer-mode out 'line)

  (thread (λ ()
            (do ([log-event (sync rec) (sync rec)]) (#f)
              (displayln (string-append (~t (now) "yyyy-MM-dd hh:mm:ss") " " (vector-ref log-event 1)) out)))))
