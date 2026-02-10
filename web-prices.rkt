#lang racket/base

(require json
         net/http-easy
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/string
         threading
         "params.rkt"
         "sim/list-partition.rkt")

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

(define (get-finviz-prices symbols)
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Finviz: Encountered error for " (first symbols) "-" (last symbols)))
                     (displayln error)
                     (apply hash (flatten (map (λ (symbol) (list symbol 0.0)) symbols))))])
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
        (hash-map/copy _ (λ (k v) (values k (string->number v))))
        (hash-filter _ (λ (k v) v)) ; remove entries that don't have prices
        )))

(define (displayln-and-return d)
  (displayln d)
  d)

(define (get-nasdaq-prices symbols)
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Nasdaq: Encountered error for " (first symbols) "-" (last symbols)))
                     (displayln error)
                     (apply hash (flatten (map (λ (symbol) (list symbol 0.0)) symbols))))])
    (~> (get (string-append "https://api.nasdaq.com/api/quote/watchlist?type=Rv&"
                            (string-join (map (λ (symbol) (string-append "symbol=" symbol "|stocks")) symbols) "&")))
        (response-body _)
        (bytes->string/utf-8 _)
        (string->jsexpr _)
        (hash-ref _ 'data)
        (hash-ref _ 'rows)
        (map (λ (row) (list (hash-ref row 'symbol) (string->number (string-replace (hash-ref row 'lastSale) "$" "")))) _)
        (flatten _)
        (apply hash _)
        )))

(define (get-prices symbols)
  ; we only need to break apart symbols when querying the nasdaq endpoint as their response is limited to 25 results
  (foldl (λ (l res)
           ; remove entries that don't have prices
           (define prices (hash-filter (get-nasdaq-prices l) (λ (k v) v)))
           (sleep 1)
           (hash-union res prices
                       #:combine (λ (a b) a)))
         (hash)
         (list-partition symbols 25 25)))
