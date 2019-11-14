#lang racket/base

(require db
         db/util/datetime
         gregor
         racket/list
         "../interactive-brokers-api/response-messages.rkt"
         "cmd-line.rkt"
         "structs.rkt")

(provide get-1-month-rate
         get-date-ohlc
         get-options
         get-msis
         get-security-name
         insert-execution)

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(define (get-date-ohlc ticker-symbol start-date end-date)
  (let ([price-query (query-rows dbc "
select
  date::text,
  open,
  case
    when high is null and open >= close then open
    when high is null and open < close then close
    else high
  end as high,
  case
    when low is null and open >= close then close
    when low is null and open < close then open
    else low
  end as low,
  close
from
  iex.chart
where
  act_symbol = $1 and
  case
    when $2::text::date > (select max(date) from iex.chart) then date >= (select max(date) from iex.chart)
    else date >= $2::text::date
  end and
  date <= $3::text::date
order by
  date;
"
                                 ticker-symbol
                                 start-date
                                 end-date)])
    (map (λ (row) (dohlc (->posix (iso8601->date (vector-ref row 0)))
                         (vector-ref row 1) (vector-ref row 2) (vector-ref row 3) (vector-ref row 4)))
         price-query)))

;; Get market/sector/industry/stock breakdown for ETF components
(define (get-msis market sector start-date end-date)
  (let ([msis-query (query-rows dbc "
select
  market.market_symbol as market,
  market.sector_symbol as sector,
  trunc((((sector_end_close.close - sector_start_close.close) / sector_start_close.close) - 
    ((market_end_close.close - market_start_close.close) / market_start_close.close)) * 100, 2) as sector_vs_market,
  coalesce(industry.etf_symbol, '') as industry,
  market.component_symbol,
  trunc((((stock_end_close.close - stock_start_close.close) / stock_start_close.close) - 
    ((sector_end_close.close - sector_start_close.close) / sector_start_close.close)) * 100, 2) as stock_vs_sector,
  coalesce(to_char(div.ex_date + interval '1 year', 'YY-MM-DD'), '') as anticipated_dividend_ex_date,
  coalesce(to_char(ec.date, 'YY-MM-DD'), '') as earnings_date,
  coalesce(trunc(option_spread.spread * 100, 2)::text, '') as option_spread,
  coalesce(replace(rank.rank::text, 'Strong', 'Str'), '') as rank
from
  (select
    etf_symbol as market_symbol,
    spdr.to_sector_etf(sector) as sector_symbol,
    component_symbol as component_symbol,
    date
  from
    spdr.etf_holding
  where
    etf_symbol = $1 and
    date = (select max(date) from spdr.etf_holding where date <= $4::text::date)
  union
  select
    etf_symbol as market_symbol,
    spdr.to_sector_etf(sector::text::spdr.sector) as sector_symbol,
    component_symbol,
    date
  from
    invesco.etf_holding
  where
    etf_symbol = $1 and
    date = (select max(date) from invesco.etf_holding where date <= $4::text::date)) as market
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  industry.sub_industry is not null
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select min(date) from iex.chart where date >= $3::text::date)) as market_start_close
on
  market.market_symbol = market_start_close.act_symbol
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select max(date) from iex.chart where date <= $4::text::date )) as market_end_close
on
  market.market_symbol = market_end_close.act_symbol
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select min(date) from iex.chart where date >= $3::text::date )) as sector_start_close
on
  market.sector_symbol = sector_start_close.act_symbol
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select max(date) from iex.chart where date <= $4::text::date )) as sector_end_close
on
  market.sector_symbol = sector_end_close.act_symbol
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select min(date) from iex.chart where date >= $3::text::date )) as stock_start_close
on
  market.component_symbol = stock_start_close.act_symbol
join
  (select
    act_symbol,
    close
  from
    iex.chart
  where
    date = (select max(date) from iex.chart where date <= $4::text::date )) as stock_end_close
on
  market.component_symbol = stock_end_close.act_symbol
left outer join
  yahoo.dividend div
on
  market.component_symbol = div.act_symbol and
  div.ex_date > $4::text::date - interval '1 year' and
  div.ex_date <= $4::text::date - interval '1 year' + interval '1 month'
left outer join
  ecnet.earnings_calendar ec
on
  market.component_symbol = ec.act_symbol and
  ec.date >= $4::text::date - interval '1 day' and
  ec.date <= $4::text::date + interval '1 month'
left outer join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    expiration > $4::text::date and
    expiration <= $4::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
left outer join
  zacks.rank_score rank
on
  market.component_symbol = rank.act_symbol and
  rank.date > (select max(date) - interval '3 days' from zacks.rank_score)
where
  case
    when $2::text != '' then market.sector_symbol = $2::text
    else true
  end
order by
  ((sector_end_close.close - sector_start_close.close) / sector_start_close.close) - 
    ((market_end_close.close - market_start_close.close) / market_start_close.close) desc,
  ((stock_end_close.close - stock_start_close.close) / stock_start_close.close) - 
    ((sector_end_close.close - sector_start_close.close) / sector_start_close.close) desc;
"
                                market
                                sector
                                start-date
                                end-date)])
    (map (λ (row) (msis (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)
                        (vector-ref row 4) (vector-ref row 5) (vector-ref row 6) (vector-ref row 7)
                        (vector-ref row 8) (vector-ref row 9)))
         msis-query)))

(define (get-security-name act-symbol)
  (query-value dbc "
select
  security_name
from
  nasdaq.symbol
where
  act_symbol = $1;
"
               act-symbol))

(define (get-options act-symbol date)
  (map (λ (row) (option (vector-ref row 0)
                        (vector-ref row 1)
                        (vector-ref row 2)
                        (vector-ref row 3)
                        (vector-ref row 4)
                        (vector-ref row 5)
                        (vector-ref row 6)
                        (vector-ref row 7)
                        (vector-ref row 8)
                        (vector-ref row 9)
                        (vector-ref row 10)
                        (vector-ref row 11)
                        (vector-ref row 12)
                        (vector-ref row 13)
                        (vector-ref row 14)))
       (query-rows dbc "
select
  act_symbol,
  to_char(expiration, 'YY-MM-DD'),
  expiration - $2::text::date as dte,
  strike,
  call_put::text,
  to_char(date, 'YY-MM-DD'),
  bid,
  (bid + ask) / 2 as mid,
  ask,
  vol,
  delta,
  gamma,
  theta,
  vega,
  rho
from
  oic.option_chain
where
  act_symbol = $1 and
  expiration > $2::text::date and
  date =
    (select
      max(date)
    from
      oic.option_chain
    where
      act_symbol = $1 and
      date <= $2::text::date)
order by
  expiration, strike, call_put;
"
                   act-symbol
                   date)))

(define (get-1-month-rate date)
  (query-value dbc "
select
  \"1_month\" / 100
from
  ust.yield_curve
where
  date = (select max(date) from ust.yield_curve where date < $1::text::date)
"
               date))

(define (insert-execution execution)
  (query-exec dbc "
insert into ibkr.execution (
  order_id,
  contract_id,
  act_symbol,
  security_type,
  expiry,
  strike,
  \"right\",
  multiplier,
  exchange,
  currency,
  local_symbol,
  trading_class,
  execution_id,
  \"timestamp\",
  account,
  executing_exchange,
  side,
  shares,
  price,
  perm_id,
  client_id,
  liquidation,
  cumulative_quantity,
  average_price,
  order_reference,
  ev_rule,
  ev_multiplier,
  model_code
) values (
  $1,
  $2,
  $3,
  $4::text::ibkr.security_type,
  $5::text::date,
  $6,
  $7::text::ibkr.right,
  $8,
  $9,
  $10,
  $11,
  $12,
  $13,
  $14::text::timestamptz,
  $15,
  $16,
  $17,
  $18,
  $19,
  $20,
  $21,
  $22,
  $23,
  $24,
  $25,
  $26,
  $27,
  $28
) on conflict (execution_id) do nothing;
"
              (execution-rsp-order-id execution)
              (execution-rsp-contract-id execution)
              (execution-rsp-symbol execution)
              (string-upcase (symbol->string (execution-rsp-security-type execution)))
              (if (execution-rsp-expiry execution) (date->iso8601 (execution-rsp-expiry execution)) sql-null)
              (execution-rsp-strike execution)
              (string-upcase (symbol->string (execution-rsp-right execution)))
              (if (execution-rsp-multiplier execution) (execution-rsp-multiplier execution) sql-null)
              (execution-rsp-exchange execution)
              (execution-rsp-currency execution)
              (execution-rsp-local-symbol execution)
              (execution-rsp-trading-class execution)
              (execution-rsp-execution-id execution)
              (~t (execution-rsp-timestamp execution) "yyyy-MM-dd'T'HH:mm:ss")
              (execution-rsp-account execution)
              (execution-rsp-executing-exchange execution)
              (execution-rsp-side execution)
              (execution-rsp-shares execution)
              (execution-rsp-price execution)
              (execution-rsp-perm-id execution)
              (execution-rsp-client-id execution)
              (execution-rsp-liquidation execution)
              (execution-rsp-cumulative-quantity execution)
              (execution-rsp-average-price execution)
              (execution-rsp-order-reference execution)
              (execution-rsp-ev-rule execution)
              (if (execution-rsp-ev-multiplier execution) (execution-rsp-ev-multiplier execution) sql-null)
              (execution-rsp-model-code execution)))
