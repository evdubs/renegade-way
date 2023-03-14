#lang racket/base

(require db
         gregor
         gregor/period
         "../pricing-risk.rkt"
         "../structs.rkt")

(provide price-option)

(define (price-option dbc option date stock-price)
  (define closest-exit-vol
    (query-value dbc "
select
  vol
from
  (select
    date,
    act_symbol,
    expiration,
    strike,
    call_put,
    vol,
    abs(expiration - $3::text::date) as date_diff,
    abs(strike - $4) as strike_diff
  from
    oic.option_chain oc 
  where 
    act_symbol = $1 and
    date = (select max(date) from oic.option_chain where date <= $2::text::date) and
    call_put = $5::text::oic.call_put
  order by
    abs(expiration - $3::text::date) + abs(strike - $4)) cv
limit 
  1;
"
                 (option-symbol option)
                 date
                 (date->iso8601 (parse-date (option-expiration option) "yy-MM-dd"))
                 (option-strike option)
                 (option-call-put option)))
  (define interest-rate
    (query-value dbc "
select
  \"1_month\" / 100
from
  ust.yield_curve
where
  date = (select max(date) from ust.yield_curve where date < $1::text::date)
"
                 date))
  (define dividends
    (query-rows dbc "
select
  ((previous.ex_date + interval '1 year')::date - $2::text::date) / 365.25,
  latest.amount
from
  yahoo.dividend latest
join
  yahoo.dividend previous
on
  previous.act_symbol = latest.act_symbol
where
  latest.act_symbol = $1 and
  latest.ex_date = (select max(ex_date) from yahoo.dividend where act_symbol = $1) and 
  previous.ex_date between $2::text::date - interval '1 year' and $3::text::date - interval '1 year';
"
                (option-symbol option)
                date
                (date->iso8601 (parse-date (option-expiration option) "yy-MM-dd"))))
  (define yte (if (date=? (iso8601->date date) (parse-date (option-expiration option) "yy-MM-dd"))
                  1/1000000
                  (/ (period-ref (date-period-between (iso8601->date date)
                                                      (parse-date (option-expiration option) "yy-MM-dd")
                                                      '(days))
                                 'days)
                     365.25)))
  ;; (printf "bs: ~s ~s ~s ~s ~s ~s ~s\n"
  ;;         stock-price
  ;;         yte
  ;;         (option-strike option)
  ;;         (option-call-put option)
  ;;         interest-rate
  ;;         closest-exit-vol
  ;;         dividends)
  (define option-closing-price
    (black-scholes stock-price
                   yte
                   (option-strike option)
                   (string->symbol (option-call-put option))
                   interest-rate
                   closest-exit-vol
                   dividends))
  ; (printf "price: ~a vol: ~a option: ~a\n" option-closing-price closest-exit-vol option)
  option-closing-price)
