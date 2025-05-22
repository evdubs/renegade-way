#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We want to first set up the command line args before initializing stuff in db-queries.rkt.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "params.rkt")

  (command-line
   #:program "racket email-positions.rkt"
   #:once-each
   [("-e" "--finviz-user") user
                           "FinViz username"
                           (finviz-user user)]
   [("-f" "--finviz-pass") pass
                           "FinViz password"
                           (finviz-pass pass)]
   [("-n" "--db-name") name
                       "Database name. Defaults to 'local'"
                       (db-name name)]
   [("-p" "--db-pass") password
                       "Database password"
                       (db-pass password)]
   [("-s" "--email-user") user
                          "Email user name and recipient"
                          (email-user user)]
   [("-t" "--api-token") token
                         "API Token"
                         (api-token token)]
   [("-u" "--db-user") user
                       "Database user name. Defaults to 'user'"
                       (db-user user)]
   [("-w" "--email-pass") pass
                          "Email password"
                          (email-pass pass)]))

(require 'cmd
         gregor
         json
         net/head
         net/http-easy
         net/smtp
         openssl
         racket/list
         racket/match
         racket/string
         threading
         "db-queries.rkt"
         "finviz-prices.rkt"
         "params.rkt"
         "structs.rkt")

(define (bull-bear-roo strategy)
  (cond [(or (equal? "LONG CALL" strategy)
             (equal? "BULL CALL VERTICAL SPREAD" strategy)
             (equal? "BULL PUT VERTICAL SPREAD" strategy)
             (equal? "CALL RATIO SPREAD" strategy)
             (equal? "CALL HORIZONTAL SPREAD" strategy)
             (equal? "CALL DIAGONAL SPREAD" strategy))
         'bull]
        [(or (equal? "LONG PUT" strategy)
             (equal? "BEAR CALL VERTICAL SPREAD" strategy)
             (equal? "BEAR PUT VERTICAL SPREAD" strategy)
             (equal? "PUT RATIO SPREAD" strategy)
             (equal? "PUT HORIZONTAL SPREAD" strategy)
             (equal? "PUT DIAGONAL SPREAD" strategy))
         'bear]
        [(or (equal? "LONG STRADDLE" strategy)
             (equal? "LONG STRANGLE" strategy)
             (equal? "CALL BUTTERFLY" strategy)
             (equal? "PUT BUTTERFLY" strategy)
             (equal? "CALL CONDOR" strategy)
             (equal? "PUT CONDOR" strategy))
         'roo]))

(define positions (get-position-analysis (date->iso8601 (today))))

(define symbols (remove-duplicates (map (λ (p) (position-analysis-stock p))
                                        positions)))

(define symbols-prices (get-prices symbols))

(define-values (expired-positions live-positions)
  (partition (λ (p) (and (not (equal? "" (position-analysis-end-date p)))
                         (date>=? (today) (parse-date (position-analysis-end-date p) "yy-MM-dd")))) positions))

(define (get-price-from-position position)
  (hash-ref symbols-prices (position-analysis-stock position)))

(define (get-strikes-for-symbol symbol)
  (map (λ (p) (position-analysis-strike p))
       (filter (λ (p) (equal? symbol (position-analysis-stock p))) positions)))

(define-values (stop-positions go-positions)
  (partition (λ (p) (match (bull-bear-roo (position-analysis-strategy p))
                      ['bull (>= (position-analysis-stock-stop p)
                                 (get-price-from-position p))]
                      ['bear (<= (position-analysis-stock-stop p)
                                 (get-price-from-position p))]
                      ['roo (and (or (string-contains? (position-analysis-strategy p) "BUTTERFLY")
                                     (string-contains? (position-analysis-strategy p) "CONDOR"))
                                 (or (> (apply min (get-strikes-for-symbol (position-analysis-stock p)))
                                        (get-price-from-position p))
                                     (< (apply max (get-strikes-for-symbol (position-analysis-stock p)))
                                        (get-price-from-position p))))]
                      [_ #f]))
             live-positions))

(define-values (target-positions off-target-positions)
  (partition (λ (p) (match (bull-bear-roo (position-analysis-strategy p))
                      ['bull (>= (get-price-from-position p)
                                 (position-analysis-stock-target p))]
                      ['bear (<= (get-price-from-position p)
                                 (position-analysis-stock-target p))]
                      ['roo #f]
                      [_ #f]))
             go-positions))

(define (position->html-str position price)
  (format "
<tr style=\"background-color:~a\">
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"text-align: right; padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"text-align: right; padding-right: 20px\">~a</td>
  <td style=\"text-align: right; padding-right: 20px\">~a</td>
  <td style=\"text-align: right; padding-right: 20px\">~a</td>
  <td style=\"text-align: right; padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
  <td style=\"padding-right: 20px\">~a</td>
</tr>"
          (cond [(and (equal? "CALL" (position-analysis-call-put position))
                      (<= (position-analysis-strike position) price))
                 "#eeffee"]
                [(and (equal? "CALL" (position-analysis-call-put position))
                      (> (position-analysis-strike position) price))
                 "#ffffee"]
                [(and (equal? "PUT" (position-analysis-call-put position))
                      (>= (position-analysis-strike position) price))
                 "#eeffee"]
                [(and (equal? "PUT" (position-analysis-call-put position))
                      (< (position-analysis-strike position) price))
                 "#ffffee"])
          (position-analysis-sector position)
          (position-analysis-stock position)
          (position-analysis-expiration position)
          (real->decimal-string (position-analysis-strike position))
          (position-analysis-call-put position)
          (position-analysis-account position)
          (real->decimal-string (position-analysis-signed-shares position))
          (real->decimal-string (position-analysis-stock-stop position))
          (real->decimal-string price)
          (real->decimal-string (position-analysis-stock-target position))
          (position-analysis-end-date position)
          (position-analysis-strategy position)))

(smtp-send-message "smtp.gmail.com"
                   (email-user)
                   (list (email-user))
                   (insert-field
                    "Content-Type"
                    "text/html"
                    (standard-message-header (email-user)
                                             (list (email-user))
                                             (list)
                                             (list)
                                             (format "Option Positions ~a" (date->iso8601 (today)))))
                   (list (format "
<table>
  <tr>
    <th style=\"padding-right: 20px\">Sector</th>
    <th style=\"padding-right: 20px\">Stock</th>
    <th style=\"padding-right: 20px\">Expiry</th>
    <th style=\"padding-right: 20px\">Strike</th>
    <th style=\"padding-right: 20px\">Right</th>
    <th style=\"padding-right: 20px\">Account</th>
    <th style=\"padding-right: 20px\">Quantity</th>
    <th style=\"padding-right: 20px\">Stop</th>
    <th style=\"padding-right: 20px\">Price</th>
    <th style=\"padding-right: 20px\">Target</th>
    <th style=\"padding-right: 20px\">End Date</th>
    <th style=\"padding-right: 20px\">Strategy</th>
  </tr>
  <tr>
    <td style=\"font-weight: bold;\">Expired</td>
  </tr>  
  ~a
  <tr>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td style=\"font-weight: bold;\">Stop</td>
  </tr>
  ~a
  <tr>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td style=\"font-weight: bold;\">Target</td>
  </tr>
  ~a
  <tr>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td style=\"font-weight: bold;\">Live</td>
  </tr>
  ~a
</table>
"
                                 (string-join (map (λ (p) (position->html-str p (get-price-from-position p))) expired-positions) "\n")
                                 (string-join (map (λ (p) (position->html-str p (get-price-from-position p))) stop-positions) "\n")
                                 (string-join (map (λ (p) (position->html-str p (get-price-from-position p))) target-positions) "\n")
                                 (string-join (map (λ (p) (position->html-str p (get-price-from-position p))) off-target-positions) "\n")))
                   #:port-no 465
                   #:auth-user (email-user)
                   #:auth-passwd (email-pass)
                   #:tcp-connect ssl-connect)
