#lang racket/base

(require net/http-easy
         racket/list
         racket/match
         racket/string
         threading
         "params.rkt")

(provide get-prices)

; #:max-redirects is set to 0 here as login_submit.ashx will redirect after
; success on login, and http-easy will follow the redirect, but the cookie
; that we're interested in will not be sent during redirect and we will lose it.
(define headers
  (response-headers (post "https://finviz.com/login_submit.ashx"
                          #:max-redirects 0
                          #:form (list (cons 'email (finviz-user))
                                       (cons 'password (finviz-pass))
                                       (cons 'remember "true")))))

(define aspx-auth
  (filter-map (λ (h) (match h [(regexp #rx"\\.ASPXAUTH=([0-9A-F]+);" (list str auth))
                               (bytes->string/utf-8 auth)]
                            [_ #f]))
              headers))

(define (get-prices symbols)
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Encountered error for " (first symbols) "-" (last symbols)))
                     (displayln error))])
    (~> (get (string-append "https://finviz.com/export.ashx?v=151&t=" (string-join symbols ","))
             #:headers (hash 'Cookie
                             (string-append "screenerUrl=screener.ashx%3Fv%3D151; "
                                            "screenerCustomTable=1%2C65; "
                                            ".ASPXAUTH=" (first aspx-auth) ";")))
        (response-body _)
        (bytes->string/utf-8 _)
        (string-replace _ "\"" "")
        (string-split _ "\r\n")
        (map (λ (s) (string-split s ",")) _)
        (flatten _)
        (apply hash _)
        (hash-map/copy _ (λ (k v) (values k (string->number v)))))))
