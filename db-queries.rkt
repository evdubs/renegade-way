#lang racket/base

(require db
         db/util/datetime
         gregor
         racket/list
         racket/string
         interactive-brokers-api/base-structs
         interactive-brokers-api/response-messages
         "cmd-line.rkt"
         "structs.rkt")

(provide get-1-month-rate
         get-date-ohlc
         get-dividend-estimates
         get-next-earnings-date
         get-options
         get-position-analysis
         get-price-analysis
         get-rank-analysis
         get-security-name
         get-vol-analysis
         insert-contract
         insert-execution
         insert-order
         insert-order-note)

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
(define (get-price-analysis market sector start-date end-date)
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
  div.ex_date <= $4::text::date - interval '1 year' + interval '2 months'
left outer join
  zacks.earnings_calendar ec
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
    (map (λ (row) (price-analysis (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)
                                  (vector-ref row 4) (vector-ref row 5) (vector-ref row 6) (vector-ref row 7)
                                  (vector-ref row 8) (vector-ref row 9)))
         msis-query)))

(define (get-rank-analysis market date)
  (map (λ (row) (rank-analysis (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)
                               (vector-ref row 4) (vector-ref row 5) (vector-ref row 6) (vector-ref row 7)
                               (vector-ref row 8) (vector-ref row 9) (vector-ref row 10) (vector-ref row 11)
                               (vector-ref row 12)))
       (query-rows dbc "
with etf_rank as (
  select
    etf_symbol,
    sum(eh.weight * zacks.to_integer_rank(r.rank)) / sum(eh.weight) as \"rank\"
  from
    spdr.etf_holding eh
  join
    zacks.rank_score r
  on
    eh.component_symbol = r.act_symbol and
    r.date = (select max(date) from zacks.rank_score where date <= $2::text::date)
  where
    eh.date = (select max(date) from spdr.etf_holding where date <= $2::text::date)
  group by
    etf_symbol
  order by
    \"rank\")
select
  market.etf_symbol as market_symbol,
  trunc(market_rank.rank, 2) as market_rank,
  spdr.to_sector_etf(market.sector) as sector_symbol,
  trunc(sector_rank.rank, 2) as sector_rank,
  coalesce(industry.etf_symbol, '') as industry_symbol,
  coalesce(trunc(industry_rank.rank, 2), 0.00) as industry_rank,
  market.component_symbol,
  zacks.to_integer_rank(component_rank.rank) as component_rank,
  best_rank as component_best_rank,
  trunc(component_avg_rank.rank, 2) as component_avg_rank,
  worst_rank as component_worst_rank,
  coalesce(to_char(ec.date, 'YY-MM-DD'), '') as earnings_date,
  coalesce(trunc(option_spread.spread * 100, 2)::text, '') as option_spread
from
  spdr.etf_holding market
join
  etf_rank market_rank
on
  market.etf_symbol = market_rank.etf_symbol
join
  etf_rank sector_rank
on
  spdr.to_sector_etf(market.sector) = sector_rank.etf_symbol
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  industry.sub_industry is not null
left outer join
  etf_rank industry_rank
on
  industry.etf_symbol = industry_rank.etf_symbol
join
  zacks.rank_score component_rank
on
  market.component_symbol = component_rank.act_symbol and
  component_rank.date = (select max(date) from zacks.rank_score where date <= $2::text::date)
join
  (select
    act_symbol,
    min(zacks.to_integer_rank(rank)) as best_rank,
    avg(zacks.to_integer_rank(rank)) as \"rank\",
    max(zacks.to_integer_rank(rank)) as worst_rank
   from
    zacks.rank_score
   where
    date between ($2::text::date - interval '5 weeks') and
      ($2::text::date - interval '1 week')
   group by
    act_symbol) as component_avg_rank
on
  market.component_symbol = component_avg_rank.act_symbol
left outer join
  zacks.earnings_calendar ec
on
  market.component_symbol = ec.act_symbol and
  ec.date >= $2::text::date - interval '1 week' and
  ec.date <= $2::text::date + interval '1 month'
left outer join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    expiration > $2::text::date and
    expiration <= $2::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
where
  market.etf_symbol = $1 and
  market.date = (select max(date) from spdr.etf_holding where date <= $2::text::date) and
  component_rank.rank in ('Strong Buy', 'Buy', 'Sell', 'Strong Sell')
order by
  component_rank.rank, zacks.to_integer_rank(component_rank.rank) - component_avg_rank.rank;
"
                   market
                   date)))

(define (get-vol-analysis market date)
  (map (λ (row) (vol-analysis (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)
                              (vector-ref row 4) (vector-ref row 5) (vector-ref row 6) (vector-ref row 7)
                              (vector-ref row 8) (vector-ref row 9) (vector-ref row 10) (vector-ref row 11)
                              (vector-ref row 12) (vector-ref row 13)))
       (query-rows dbc "
select
  market.etf_symbol,
  coalesce(trunc(market_vol.iv_current * 100, 2), 0.00) as market_iv,
  coalesce(trunc((market_vol.iv_current - market_vol.iv_year_low) / (market_vol.iv_year_high - market_vol.iv_year_low) * 100, 2), 0.00) as market_iv_rank,
  spdr.to_sector_etf(market.sector),
  coalesce(trunc(sector_vol.iv_current * 100, 2), 0.00) as sector_iv,
  coalesce(trunc((sector_vol.iv_current - sector_vol.iv_year_low) / (sector_vol.iv_year_high - sector_vol.iv_year_low) * 100, 2), 0.00) as sector_iv_rank,
  coalesce(industry.etf_symbol, ''),
  coalesce(trunc(industry_vol.iv_current * 100, 2), 0.00) as industry_iv,
  coalesce(trunc((industry_vol.iv_current - industry_vol.iv_year_low) / (industry_vol.iv_year_high - industry_vol.iv_year_low) * 100, 2), 0.00) as industry_iv_rank,
  market.component_symbol,
  coalesce(trunc(component_vol.iv_current * 100, 2), 0.00) as component_iv,
  coalesce(trunc((component_vol.iv_current - component_vol.iv_year_low) / (component_vol.iv_year_high - component_vol.iv_year_low) * 100, 2), 0.00) as component_iv_rank,
  coalesce(to_char(ec.date, 'YY-MM-DD'), '') as earnings_date,
  coalesce(trunc(option_spread.spread * 100, 2)::text, '') as option_spread
from
  spdr.etf_holding market
join
  oic.volatility_history market_vol
on
  market.etf_symbol = market_vol.act_symbol and
  market_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
join
  oic.volatility_history sector_vol
on
  spdr.to_sector_etf(market.sector) = sector_vol.act_symbol and 
  sector_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  industry.sub_industry is not null
left outer join
  oic.volatility_history industry_vol
on
  industry.etf_symbol = industry_vol.act_symbol and
  industry_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
join
  oic.volatility_history component_vol
on
  market.component_symbol = component_vol.act_symbol and
  component_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
left outer join
  zacks.earnings_calendar ec
on
  market.component_symbol = ec.act_symbol and
  ec.date >= $2::text::date - interval '1 week' and
  ec.date <= $2::text::date + interval '1 month'
left outer join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    expiration > $2::text::date and
    expiration <= $2::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
where
  market.etf_symbol = $1 and
  market.date = (select max(date) from spdr.etf_holding where date <= $2::text::date)
order by
  component_iv_rank desc;
"
                   market
                   date)))

(define (get-position-analysis date)
  (map (λ (row) (position-analysis (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)
                                   (vector-ref row 4) (vector-ref row 5) (vector-ref row 6) (vector-ref row 7)
                                   (vector-ref row 8) (vector-ref row 9) (vector-ref row 10)))
       (query-rows dbc "
with earnings_end_date as (
  select
    act_symbol,
    case when \"when\" = 'Before market open'
      then case when date_part('dow', date) = 1
        then (date - interval '3 days')::date
        else (date - interval '1 days')::date
      end
      else date
    end as end_date
  from
    zacks.earnings_calendar
  where
    date >= $1::text::date
)
select
  spdr.to_sector_etf(eh.sector) as etf_symbol,
  c.symbol,
  to_char(c.expiry, 'YY-MM-DD'),
  c.strike,
  c.right::text,
  e.account,
  e.signed_shares,
  coalesce(trunc(n.underlying_stop_price, 2), 0.00),
  coalesce(trunc(ch.close, 2), 0.00),
  coalesce(trunc(n.underlying_target_price, 2), 0.00),
  coalesce(to_char(case when ed.end_date is not null and ed.end_date < n.end_date
    then ed.end_date
    else n.end_date
  end, 'YY-MM-DD'), '') as end_date
from
  (select
    max(order_id) as order_id,
    contract_id,
    account,
     sum(
        case execution.side
            when 'BOT'::text then execution.shares
            when 'SLD'::text then execution.shares * '-1'::integer::numeric
            else NULL::numeric
        end) as signed_shares
  from
    ibkr.execution
  group by
    contract_id, account) e
join
  ibkr.contract c
on
  e.contract_id = c.contract_id
left outer join
  ibkr.order_note n
on
  e.account = n.account and
  e.order_id = n.order_id
join
  (select distinct
    component_symbol,
    sector
  from
    spdr.etf_holding
  where
    date = (select max(date) from spdr.etf_holding where date <= $1::text::date) and
    sector is not null) eh
on
  c.symbol = eh.component_symbol
left outer join
  earnings_end_date ed
on
  c.symbol = ed.act_symbol
left outer join
  iex.chart ch
on
  c.symbol = ch.act_symbol and
  ch.date = (select max(date) from iex.chart where date <= $1::text::date)
where
  c.expiry > $1::text::date and
  signed_shares != 0
order by
  etf_symbol,
  symbol,
  expiry,
  strike,
  \"right\";
"
                   date)))

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

(define (get-dividend-estimates symbol start-date end-date)
  (query-rows dbc "
select
  (previous.ex_date + interval '1 year')::date - $2::text::date,
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
               symbol
               (date->iso8601 start-date)
               (date->iso8601 end-date)))

(define (get-next-earnings-date symbol start-date end-date)
  (iso8601->date (query-value dbc "
select
  coalesce(ec.date::text, ed.end_date)
from
  (select
    $1 as symbol,
    $3::text as end_date) ed
left outer join
  zacks.earnings_calendar ec
on
  ec.act_symbol = $1 and
  ec.date >= $2::text::date and
  ec.date <= $3::text::date;
"
                              symbol
                              (date->iso8601 start-date)
                              (date->iso8601 end-date))))

(define (insert-execution execution)
  (query-exec dbc "
insert into ibkr.execution (
  order_id,
  contract_id,
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
  model_code
) values (
  $1,
  $2,
  $3,
  $4::text::timestamptz,
  $5,
  $6,
  $7,
  $8,
  $9,
  $10,
  $11,
  $12,
  $13,
  $14,
  $15,
  $16
) on conflict (execution_id) do nothing;
"
              (execution-rsp-order-id execution)
              (execution-rsp-contract-id execution)
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
              (execution-rsp-model-code execution)))

(define (insert-contract contract)
  (query-exec dbc "
insert into ibkr.contract (
  symbol,
  security_type,
  expiry,
  strike,
  \"right\",
  exchange,
  currency,
  local_symbol,
  market_name,
  trading_class,
  contract_id,
  minimum_tick_increment,
  multiplier,
  price_magnifier,
  underlying_contract_id,
  long_name,
  primary_exchange,
  contract_month,
  industry,
  category,
  subcategory,
  time_zone,
  ev_rule,
  ev_multiplier
) values (
  $1,
  $2::text::ibkr.security_type,
  $3::text::date,
  $4,
  $5::text::ibkr.right,
  $6,
  $7,
  $8,
  $9,
  $10,
  $11,
  $12,
  $13::text::numeric,
  $14,
  $15,
  $16,
  $17,
  $18,
  $19,
  $20,
  $21,
  $22,
  $23,
  $24
) on conflict (contract_id) do nothing;
"
              (contract-details-rsp-symbol contract)
              (string-upcase (symbol->string (contract-details-rsp-security-type contract)))
              (if (contract-details-rsp-expiry contract) (date->iso8601 (contract-details-rsp-expiry contract)) sql-null)
              (if (contract-details-rsp-strike contract) (contract-details-rsp-strike contract) sql-null)
              (if (contract-details-rsp-right contract) (string-upcase (symbol->string (contract-details-rsp-right contract))) sql-null)
              (contract-details-rsp-exchange contract)
              (contract-details-rsp-currency contract)
              (contract-details-rsp-local-symbol contract)
              (contract-details-rsp-market-name contract)
              (contract-details-rsp-trading-class contract)
              (contract-details-rsp-contract-id contract)
              (contract-details-rsp-minimum-tick-increment contract)
              (if (equal? "" (contract-details-rsp-multiplier contract)) sql-null (contract-details-rsp-multiplier contract))
              (contract-details-rsp-price-magnifier contract)
              (contract-details-rsp-underlying-contract-id contract)
              (contract-details-rsp-long-name contract)
              (contract-details-rsp-primary-exchange contract)
              (contract-details-rsp-contract-month contract)
              (contract-details-rsp-industry contract)
              (contract-details-rsp-category contract)
              (contract-details-rsp-subcategory contract)
              (contract-details-rsp-time-zone-id contract)
              (contract-details-rsp-ev-rule contract)
              (contract-details-rsp-ev-multiplier contract)))

(define (insert-order order)
  (query-exec dbc "
insert into ibkr.order (
  order_id,
  contract_id,
  \"action\",
  total_quantity,
  order_type,
  limit_price,
  aux_price,
  time_in_force,
  account,
  open_close,
  order_ref,
  client_id,
  perm_id,
  \"timestamp\"
) values (
  $1,
  $2,
  $3::text::ibkr.action,
  $4,
  $5,
  $6,
  $7,
  $8::text::ibkr.time_in_force,
  $9,
  $10::text::ibkr.open_close,
  $11,
  $12,
  $13,
  current_timestamp
) on conflict (account, order_id) do nothing;
"
              (open-order-rsp-order-id order)
              (open-order-rsp-contract-id order)
              (string-upcase (symbol->string (open-order-rsp-action order)))
              (open-order-rsp-total-quantity order)
              (open-order-rsp-order-type order)
              (open-order-rsp-limit-price order)
              (open-order-rsp-aux-price order)
              (string-upcase (symbol->string (open-order-rsp-time-in-force order)))
              (open-order-rsp-account order)
              (if (open-order-rsp-open-close order) (string-upcase (symbol->string (open-order-rsp-open-close order))) sql-null)
              (open-order-rsp-order-ref order)
              (open-order-rsp-client-id order)
              (open-order-rsp-perm-id order))
  (for-each (λ (leg)
              (query-exec dbc "
insert into ibkr.order_leg (
  account,
  order_id,
  contract_id,
  ratio,
  \"action\",
  exchange,
  open_close,
  short_sale_slot,
  designated_location,
  exempt_code
) values (
  $1,
  $2,
  $3,
  $4,
  $5::text::ibkr.action,
  $6,
  $7::text::ibkr.open_close,
  $8,
  $9,
  $10
) on conflict (account, order_id, contract_id) do nothing;
"
                          (open-order-rsp-account order)
                          (open-order-rsp-order-id order)
                          (combo-leg-contract-id leg)
                          (combo-leg-ratio leg)
                          (string-upcase (symbol->string (combo-leg-action leg)))
                          (combo-leg-exchange leg)
                          (string-upcase (symbol->string (combo-leg-open-close leg)))
                          (combo-leg-short-sale-slot leg)
                          (combo-leg-designated-location leg)
                          (combo-leg-exempt-code leg)))
            (open-order-rsp-combo-legs order))
  (for-each (λ (con)
              (query-exec dbc "
insert into ibkr.order_condition (
  account,
  order_id,
  contract_id,
  \"type\",
  \"operator\",
  \"comparator\",
  \"value\",
  exchange,
  trigger_method
) values (
  $1,
  $2,
  $3,
  $4::text::ibkr.condition_type,
  $5::text::ibkr.condition_operator,
  $6::text::ibkr.condition_comparator,
  $7,
  $8,
  $9::text::ibkr.condition_trigger_method
) on conflict (account, order_id, \"type\") do nothing;
"
                          (open-order-rsp-account order)
                          (open-order-rsp-order-id order)
                          (condition-contract-id con)
                          (string-replace (string-upcase (symbol->string (condition-type con))) "-" " ")
                          (string-upcase (symbol->string (condition-boolean-operator con)))
                          (string-replace (string-upcase (symbol->string (condition-comparator con))) "-" " ")
                          (cond
                            [(moment? (condition-value con)) (moment->iso8601 (condition-value con))]
                            [(rational? (condition-value con)) (real->decimal-string (condition-value con) 2)])
                          (condition-exchange con)
                          (string-replace (string-upcase (symbol->string (condition-trigger-method con))) "-" " ")))
            (open-order-rsp-conditions order)))

(define (insert-order-note account order-id order-note)
  (query-exec dbc "
insert into ibkr.order_note (
  account,
  order_id,
  order_strategy,
  underlying_entry_price,
  underlying_stop_price,
  underlying_target_price,
  end_date
) values (
  $1,
  $2,
  $3::text::ibkr.order_strategy,
  $4,
  $5,
  $6,
  $7::text::date
) on conflict (account, order_id) do nothing;
"
              account
              order-id
              (string-replace (string-upcase (symbol->string (order-strategy order-note))) "-" " ")
              (order-stock-entry order-note)
              (order-stock-stop order-note)
              (order-stock-target order-note)
              (date->iso8601 (order-end-date order-note))))
