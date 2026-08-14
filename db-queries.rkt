#lang racket/base

(require db
         gregor
         racket/format
         racket/list
         racket/string
         interactive-brokers-api/base-structs
         interactive-brokers-api/response-messages
         "logging.rkt"
         "params.rkt"
         "structs.rkt")

(provide get-1-month-rate
         get-atm-curve
         get-closest-vol
         get-condor-analysis
         get-date-ohlc
         get-date-variance-history
         get-date-vol-history
         get-date-vol-curve-history
         get-dividend-dates
         get-dividend-estimates
         get-earnings-dates
         get-earnings-symbols-for-date
         get-earnings-vibes-analysis
         get-earnings-vol-premium
         get-etf-vrp-analysis
         get-execution-tick
         get-forward-factor-analysis
         get-is-etf
         get-next-earnings-date
         get-options
         get-position-history
         get-position-analysis
         get-price-analysis
         get-rank-analysis
         get-security-name
         get-vol-analysis
         get-vol-surface
         insert-commission-report
         insert-condor-analysis
         insert-contract
         insert-execution
         insert-execution-tick
         insert-price-analysis
         insert-order
         insert-order-note)

(define dbc (postgresql-connect #:server (db-host) #:user (db-user) #:database (db-name) #:password (db-pass)))

(define (get-closest-vol symbol date expiration strike call-put)
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
                 symbol
                 date
                 expiration
                 strike
                 call-put))

(define (get-date-ohlc ticker-symbol start-date end-date)
  (let ([price-query (query-rows dbc "
select
  date::text,
  open,
  high,
  low,
  close
from
  polygon.split_adjusted_ohlc(
    $1,
    case
      when $2::text::date > (select max(date) from polygon.ohlc) then (select max(date) from polygon.ohlc)
      else $2::text::date
    end,
    $3::text::date,
    false);
"
                                 ticker-symbol
                                 start-date
                                 end-date)])
    (map (λ (row) (dohlc (->posix (iso8601->date (vector-ref row 0)))
                         (vector-ref row 1)
                         (vector-ref row 2)
                         (vector-ref row 3)
                         (vector-ref row 4)))
         price-query)))

(define (get-date-variance-history ticker-symbol start-date end-date)
  (let ([variance-history-query (query-rows dbc "
select
  date::text,
  hv_current
from
  oic.volatility_history
where
  act_symbol = $1 and
  date >= $2::text::date and
  date <= $3::text::date and
  hv_current is not null
order by
  date;
"
                                            ticker-symbol
                                            start-date
                                            end-date)])
    (map (λ (row) (dv (->posix (iso8601->date (vector-ref row 0))) (vector-ref row 1)))
         variance-history-query)))

(define (get-date-vol-history ticker-symbol start-date end-date)
  (let ([vol-history-query (query-rows dbc "
select
  date::text,
  iv_current
from
  oic.volatility_history
where
  act_symbol = $1 and
  date >= $2::text::date and
  date <= $3::text::date and
  iv_current is not null
order by
  date;
"
                                       ticker-symbol
                                       start-date
                                       end-date)])
    (map (λ (row) (dv (->posix (iso8601->date (vector-ref row 0))) (vector-ref row 1)))
         vol-history-query)))

(define (get-date-vol-curve-history ticker-symbol start-date end-date)
  (let ([vol-curve-history-query (query-rows dbc "
with closest_curve_points as (select
  ac.date,
  ac.act_symbol,
  closest(expiration, (ac.date + '28 days'::interval)::date) as expiration
from
  oic.atm_curve ac
join
  polygon.ohlc o
on
  ac.date = o.date and
  ac.act_symbol = o.act_symbol
where
  ac.act_symbol = $1 and
  ac.date >= $2::text::date and
  ac.date <= $3::text::date and
  ac.expiration >= ac.date + '2 days'::interval
group by
  ac.date,
  ac.act_symbol)
select
  ccp.date::text,
  min(ac.vol)
from
  closest_curve_points ccp
join
  oic.atm_curve ac
on
  ccp.date = ac.date and
  ccp.act_symbol = ac.act_symbol and
  ccp.expiration = ac.expiration
group by
  ccp.date
order by
  ccp.date;
"
                                             ticker-symbol
                                             start-date
                                             end-date)])
    (map (λ (row) (dv (->posix (iso8601->date (vector-ref row 0))) (vector-ref row 1)))
         vol-curve-history-query)))

(define (get-vol-surface ticker-symbol date)
  (query-rows dbc "
select
  expiration::text,
  call_put::text,
  strike,
  vol
from
  oic.option_chain
where
  act_symbol = $1 and
  date = (select max(date) from oic.option_chain where date <= $2::text::date) and
  expiration >= $2::text::date
order by
  expiration,
  call_put,
  strike;
"
              ticker-symbol
              date))

(define (get-atm-curve ticker-symbol date)
  (query-rows dbc "
select
  alc.expiration::date::text,
  round(((ahc.strike - alc.strike) - (o.close - alc.strike)) / (ahc.strike - alc.strike) * alc.vol / 2 +
  ((ahc.strike - alc.strike) - (ahc.strike - o.close)) / (ahc.strike - alc.strike) * ahc.vol / 2 +
  ((ahp.strike - alp.strike) - (o.close - alp.strike)) / (ahp.strike - alp.strike) * alp.vol / 2 +
  ((ahp.strike - alp.strike) - (ahp.strike - o.close)) / (ahp.strike - alp.strike) * ahp.vol / 2, 4),
  alc.vol,
  ahc.vol,
  alp.vol,
  ahp.vol
from
  oic.atm_curve alc
left outer join
  oic.atm_curve ahc
on
  alc.date = ahc.date and
  alc.act_symbol = ahc.act_symbol and
  alc.expiration = ahc.expiration and
  ahc.call_put = 'Call' and
  alc.strike < ahc.strike
left outer join
  oic.atm_curve alp
on
  alc.date = alp.date and
  alc.act_symbol = alp.act_symbol and
  alc.expiration = alp.expiration and
  alp.call_put = 'Put' and
  alp.strike < ahc.strike
left outer join
  oic.atm_curve ahp
on
  alc.date = ahp.date and
  alc.act_symbol = ahp.act_symbol and
  alc.expiration = ahp.expiration and
  ahp.call_put = 'Put' and
  alp.strike < ahp.strike
join
  polygon.ohlc o
on
  alc.date = o.date and
  alc.act_symbol = o.act_symbol
where
  alc.date = (select max(date) from polygon.ohlc where date <= $1::text::date) and
  alc.act_symbol = $2 and
  alc.call_put = 'Call' and
  alc.strike < ahc.strike and
  alc.date != alc.expiration
order by
  alc.expiration;
"
              date
              ticker-symbol))

(define (get-dividend-dates ticker-symbol start-date end-date)
  (map (λ (el) (->posix (iso8601->date el)))
       (query-list dbc "
select
  coalesce(dc.ex_date::text, d.ex_date::text)
from
  zacks.dividend_calendar dc
full outer join
  polygon.dividend d
on
  dc.act_symbol = d.act_symbol and
  dc.ex_date = d.ex_date
where
  (dc.act_symbol = $1 or d.act_symbol = $1) and
  ((dc.ex_date >= $2::text::date and
  dc.ex_date <= $3::text::date) or
  (d.ex_date >= $2::text::date and
  d.ex_date <= $3::text::date))
order by
  coalesce(dc.ex_date, d.ex_date);
"
                   ticker-symbol
                   start-date
                   end-date)))

(define (get-earnings-dates ticker-symbol start-date end-date)
  (map (λ (el) (->posix (iso8601->date el)))
       (query-list dbc "
select
  date::text
from
  zacks.earnings_calendar
where
  act_symbol = $1 and
  date >= $2::text::date and
  date <= $3::text::date
order by
  date;
"
                   ticker-symbol
                   start-date
                   end-date)))

(define (get-earnings-symbols-for-date date)
  (query-list dbc "
select
  ec.act_symbol
from
  zacks.earnings_calendar ec
where
  ((ec.date = $1::text::date and ec.\"when\" = 'After market close'::zacks.\"when\") or
   (ec.date = $1::text::date + interval '1 day' and ec.\"when\" = 'Before market open'::zacks.\"when\"));
"
              date))

(define (get-earnings-vol-premium date symbol)
  (query-value dbc "
with past_earnings as (
select
  act_symbol,
  date
from
  zacks.earnings_calendar ec
where
  date <= $1::text::date and
  act_symbol = $2 and
  date >= $1::text::date - '3 years'::interval
), pre_earnings_vol_dates as (
select
  vh.act_symbol,
  pe.date as earnings_date,
  max(vh.date) as vol_date
from
  past_earnings pe
join
  oic.volatility_history vh
on
  vh.act_symbol = pe.act_symbol and
  vh.date < pe.date and
  vh.date > pe.date - '7 days'::interval
group by
  vh.act_symbol,
  pe.date
), post_earnings_vol_dates as (
select
  vh.act_symbol,
  pe.date as earnings_date,
  min(vh.date) as vol_date
from
  past_earnings pe
join
  oic.volatility_history vh
on
  vh.act_symbol = pe.act_symbol and
  vh.date > pe.date and
  vh.date < pe.date + '7 days'::interval
group by
  vh.act_symbol,
  pe.date
)
select
  coalesce(avg(vh_pre.iv_current - vh_post.iv_current), 0.00)
from
  past_earnings pe
join
  pre_earnings_vol_dates pevd
on
  pe.act_symbol = pevd.act_symbol and
  pe.date = pevd.earnings_date
join
  post_earnings_vol_dates povd
on
  pe.act_symbol = povd.act_symbol and
  pe.date = povd.earnings_date
join
  oic.volatility_history vh_pre
on
  pe.act_symbol = vh_pre.act_symbol and
  pevd.vol_date = vh_pre.date
join
  oic.volatility_history vh_post
on
  pe.act_symbol = vh_post.act_symbol and
  povd.vol_date = vh_post.date;
"
               (date->iso8601 date)
               symbol))

(define (get-execution-tick execution-id)
  (query-rows dbc "
select
  execution_id,
  \"timestamp\",
  bid_price,
  bid_size,
  ask_price,
  ask_size
from
  ibkr.execution_tick
where
  execution_id = $1
"
              execution-id))

;; Get market/sector/industry/stock breakdown for ETF components
(define (get-price-analysis market sector start-date end-date)
  (let ([msis-query (query-rows dbc "
with start_close as (
  select
    c.act_symbol,
    c.close / coalesce(split_ratio, 1) as close
  from
    polygon.ohlc c
  left join
    (select
      act_symbol,
      mul(to_factor / for_factor) as split_ratio
    from
      polygon.split
    where
      ex_date >= $3::text::date and
      ex_date <= $4::text::date
    group by
      act_symbol) s
  on
    c.act_symbol = s.act_symbol
  where
    c.date = (select min(date) from polygon.ohlc where date >= $3::text::date)
), end_close as (
  select
    act_symbol,
    close
  from
    polygon.ohlc
  where
    date = (select max(date) from polygon.ohlc where date <= $4::text::date )
)
select
  market.market_symbol as market,
  market.sector_symbol as sector,
  (((sector_end_close.close - sector_start_close.close) / sector_start_close.close) -
    ((market_end_close.close - market_start_close.close) / market_start_close.close)) as sector_vs_market,
  coalesce(industry.etf_symbol, '') as industry,
  market.component_symbol,
  (((stock_end_close.close - stock_start_close.close) / stock_start_close.close) -
    ((sector_end_close.close - sector_start_close.close) / sector_start_close.close)) as stock_vs_sector,
  coalesce((div.ex_date + interval '1 year')::text, '') as anticipated_dividend_ex_date,
  coalesce(ec.date::text, '') as earnings_date,
  option_spread.spread as option_spread,
  coalesce(rank.rank::text, '') as rank,
  case
    when w.act_symbol is not null then true
    else false
  end as is_weekly
from
  (select
    etf_symbol as market_symbol,
    spdr.to_sector_etf(sector) as sector_symbol,
    component_symbol as component_symbol,
    date
  from
    spdr.etf_holding
  where
    etf_symbol = any(string_to_array($1, ',')) and
    date = (select max(date) from spdr.etf_holding where date <= $4::text::date)) as market
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  spdr.is_industry_etf(industry.etf_symbol)
join
  start_close as market_start_close
on
  market.market_symbol = market_start_close.act_symbol
join
  end_close as market_end_close
on
  market.market_symbol = market_end_close.act_symbol
join
  start_close as sector_start_close
on
  market.sector_symbol = sector_start_close.act_symbol
join
  end_close as sector_end_close
on
  market.sector_symbol = sector_end_close.act_symbol
join
  start_close as stock_start_close
on
  market.component_symbol = stock_start_close.act_symbol
join
  end_close as stock_end_close
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
  ec.date >= $4::text::date and
  ec.date <= $4::text::date + interval '1 month'
join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    date = (select max(date) from oic.option_chain where date <= $4::text::date) and
    expiration > $4::text::date and
    expiration <= $4::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0 and
    ((delta >= 0.2 and delta <= 0.8) or
    (delta <= -0.2 and delta >= -0.8))
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
left outer join
  zacks.rank_score rank
on
  market.component_symbol = rank.act_symbol and
  rank.date > (select max(date) - interval '3 days' from zacks.rank_score)
left outer join
  oic.weekly w
on
  market.component_symbol = w.act_symbol and
  w.last_seen >= (select max(last_seen) from oic.weekly where last_seen <= $4::text::date)
where
  case
    when $2::text != '' then market.sector_symbol = $2::text
    else true
  end
order by
  ((stock_end_close.close - stock_start_close.close) / stock_start_close.close) desc;
"
                                market
                                sector
                                start-date
                                end-date)])
    (map (λ (row) (price-analysis (vector-ref row 0)
                                  (vector-ref row 1)
                                  (vector-ref row 2)
                                  (if (equal? "" (vector-ref row 3)) #f (vector-ref row 3))
                                  (vector-ref row 4)
                                  (vector-ref row 5)
                                  (if (equal? "" (vector-ref row 6)) #f (iso8601->date (vector-ref row 6)))
                                  (if (equal? "" (vector-ref row 7)) #f (iso8601->date (vector-ref row 7)))
                                  (vector-ref row 8)
                                  (if (equal? "" (vector-ref row 9)) #f (string->symbol (string-replace (string-downcase (vector-ref row 9)) " " "-")))
                                  (vector-ref row 10)))
         msis-query)))

(define (get-rank-analysis market date)
  (map (λ (row) (rank-analysis (vector-ref row 0)
                               (vector-ref row 1)
                               (vector-ref row 2)
                               (vector-ref row 3)
                               (if (equal? "" (vector-ref row 4)) #f (vector-ref row 4))
                               (vector-ref row 5)
                               (vector-ref row 6)
                               (vector-ref row 7)
                               (vector-ref row 8)
                               (vector-ref row 9)
                               (vector-ref row 10)
                               (if (equal? "" (vector-ref row 11)) #f (iso8601->date (vector-ref row 11)))
                               (vector-ref row 12)
                               (vector-ref row 13)))
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
  market_rank.rank as market_rank,
  spdr.to_sector_etf(market.sector) as sector_symbol,
  sector_rank.rank as sector_rank,
  coalesce(industry.etf_symbol, '') as industry_symbol,
  coalesce(industry_rank.rank, 0.00) as industry_rank,
  market.component_symbol,
  zacks.to_integer_rank(component_rank.rank) as component_rank,
  best_rank as component_best_rank,
  component_avg_rank.rank as component_avg_rank,
  worst_rank as component_worst_rank,
  coalesce(ec.date::text, '') as earnings_date,
  option_spread.spread as option_spread,
  case
    when w.act_symbol is not null then true
    else false
  end as is_weekly
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
  spdr.is_industry_etf(industry.etf_symbol)
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
  ec.date >= $2::text::date and
  ec.date <= $2::text::date + interval '1 month'
join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    date = (select max(date) from oic.option_chain where date <= $2::text::date ) and
    expiration > $2::text::date and
    expiration <= $2::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0 and
    ((delta >= 0.2 and delta <= 0.8) or
    (delta <= -0.2 and delta >= -0.8))
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
left outer join
  oic.weekly w
on
  market.component_symbol = w.act_symbol and
  w.last_seen >= (select max(last_seen) from oic.weekly where last_seen <= $2::text::date)
where
  market.etf_symbol = any(string_to_array($1, ',')) and
  market.date = (select max(date) from spdr.etf_holding where date <= $2::text::date) and
  component_rank.rank in ('Strong Buy', 'Buy', 'Sell', 'Strong Sell')
order by
  component_rank.rank, zacks.to_integer_rank(component_rank.rank) - component_avg_rank.rank, market.component_symbol;
"
                   market
                   date)))

(define (get-vol-analysis market date)
  (map (λ (row) (vol-analysis (vector-ref row 0)
                              (vector-ref row 1)
                              (vector-ref row 2)
                              (if (equal? "" (vector-ref row 3)) #f (vector-ref row 3))
                              (vector-ref row 4)
                              (vector-ref row 5)
                              (if (equal? "" (vector-ref row 6)) #f (vector-ref row 6))
                              (vector-ref row 7)
                              (vector-ref row 8)
                              (vector-ref row 9)
                              (vector-ref row 10)
                              (vector-ref row 11)
                              (if (equal? "" (vector-ref row 12)) #f (iso8601->date (vector-ref row 12)))
                              (vector-ref row 13)
                              (vector-ref row 14)))
       (query-rows dbc "
with hist_vol as (
  select
    vh.act_symbol,
    min(vh.iv_current) as iv_year_low,
    max(vh.iv_current) as iv_year_high
  from
    oic.volatility_history vh
  where
    vh.date <= $2::text::date and
    vh.date > $2::text::date - '1 year'::interval
  group by
    vh.act_symbol
)
select
  market.etf_symbol,
  coalesce(market_vol.iv_current, 0.00) as market_iv,
  case when market_vol.iv_year_low is null or market_vol.iv_year_high is null
    then coalesce((market_vol.iv_current - market_hist_vol.iv_year_low) / (market_hist_vol.iv_year_high - market_hist_vol.iv_year_low), 0.00)
    else coalesce((market_vol.iv_current - market_vol.iv_year_low) / (market_vol.iv_year_high - market_vol.iv_year_low), 0.00)
  end as market_iv_rank,
  coalesce(spdr.to_sector_etf(market.sector), ''),
  coalesce(sector_vol.iv_current, 0.00) as sector_iv,
  case when sector_vol.iv_year_low is null or sector_vol.iv_year_high is null
    then coalesce((sector_vol.iv_current - sector_hist_vol.iv_year_low) / (sector_hist_vol.iv_year_high - sector_hist_vol.iv_year_low), 0.00)
    else coalesce((sector_vol.iv_current - sector_vol.iv_year_low) / (sector_vol.iv_year_high - sector_vol.iv_year_low), 0.00)
  end as sector_iv_rank,
  coalesce(industry.etf_symbol, ''),
  coalesce(industry_vol.iv_current, 0.00) as industry_iv,
  case when industry_vol.iv_year_low is null or industry_vol.iv_year_high is null
    then coalesce((industry_vol.iv_current - industry_hist_vol.iv_year_low) / (industry_hist_vol.iv_year_high - industry_hist_vol.iv_year_low), 0.00)
    else coalesce((industry_vol.iv_current - industry_vol.iv_year_low) / (industry_vol.iv_year_high - industry_vol.iv_year_low), 0.00)
  end as industry_iv_rank,
  market.component_symbol,
  coalesce(component_vol.iv_current, 0.00) as component_iv,
  case when component_vol.iv_year_low is null or component_vol.iv_year_high is null
    then coalesce((component_vol.iv_current - component_hist_vol.iv_year_low) / (component_hist_vol.iv_year_high - component_hist_vol.iv_year_low), 0.00)
    else coalesce((component_vol.iv_current - component_vol.iv_year_low) / (component_vol.iv_year_high - component_vol.iv_year_low), 0.00)
  end as component_iv_rank,
  coalesce(ec.date::text, '') as earnings_date,
  option_spread.spread as option_spread,
  case
    when w.act_symbol is not null then true
    else false
  end as is_weekly
from
  spdr.etf_holding market
left outer join
  oic.volatility_history market_vol
on
  market.etf_symbol = market_vol.act_symbol and
  market_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
left outer join
  hist_vol market_hist_vol
on
  market.etf_symbol = market_hist_vol.act_symbol
left outer join
  oic.volatility_history sector_vol
on
  spdr.to_sector_etf(market.sector) = sector_vol.act_symbol and
  sector_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
left outer join
  hist_vol sector_hist_vol
on
  spdr.to_sector_etf(market.sector) = sector_hist_vol.act_symbol
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  spdr.is_industry_etf(industry.etf_symbol)
left outer join
  oic.volatility_history industry_vol
on
  industry.etf_symbol = industry_vol.act_symbol and
  industry_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
left outer join
  hist_vol industry_hist_vol
on
  industry.etf_symbol = industry_hist_vol.act_symbol
join
  oic.volatility_history component_vol
on
  market.component_symbol = component_vol.act_symbol and
  component_vol.date = (select max(date) from oic.volatility_history where date <= $2::text::date)
join
  hist_vol component_hist_vol
on
  market.component_symbol = component_hist_vol.act_symbol
left outer join
  zacks.earnings_calendar ec
on
  market.component_symbol = ec.act_symbol and
  ec.date >= $2::text::date and
  ec.date <= $2::text::date + interval '1 month'
join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    date = (select max(date) from oic.option_chain where date <= $2::text::date ) and
    expiration > $2::text::date and
    expiration <= $2::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0 and
    ((delta >= 0.2 and delta <= 0.8) or
    (delta <= -0.2 and delta >= -0.8))
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
left outer join
  oic.weekly w
on
  market.component_symbol = w.act_symbol and
  w.last_seen >= (select max(last_seen) from oic.weekly where last_seen <= $2::text::date)
where
  market.etf_symbol = any(string_to_array($1, ',')) and
  market.date = (select max(date) from spdr.etf_holding where date <= $2::text::date)
order by
  component_iv_rank desc;
"
                   market
                   date)))

(define (get-condor-analysis market date)
  (map (λ (row) (condor-analysis (vector-ref row 0)
                                 #f
                                 #f
                                 (if (equal? "" (vector-ref row 1)) #f (vector-ref row 1))
                                 #f
                                 #f
                                 (if (equal? "" (vector-ref row 2)) #f (vector-ref row 2))
                                 #f
                                 #f
                                 (if (equal? "" (vector-ref row 3)) #f (vector-ref row 3))
                                 #f
                                 #f
                                 (if (equal? "" (vector-ref row 4)) #f (iso8601->date (vector-ref row 4)))
                                 (vector-ref row 5)
                                 (vector-ref row 6)))
       (query-rows dbc "
select
  market.etf_symbol as market,
  coalesce(spdr.to_sector_etf(market.sector), '') as sector,
  coalesce(industry.etf_symbol, '') as industry,
  market.component_symbol as stock,
  coalesce(ec.date::text, '') as earnings_date,
  option_spread.spread as option_spread,
  case
    when w.act_symbol is not null then true
    else false
  end as is_weekly
from
  spdr.etf_holding market
left outer join
  spdr.etf_holding industry
on
  market.component_symbol = industry.component_symbol and
  market.date = industry.date and
  spdr.is_industry_etf(industry.etf_symbol)
left outer join
  zacks.earnings_calendar ec
on
  market.component_symbol = ec.act_symbol and
  ec.date >= $2::text::date and
  ec.date <= $2::text::date + interval '1 month'
join
  (select
    act_symbol,
    avg((ask - bid) / ask) as spread
  from
    oic.option_chain
  where
    date = (select max(date) from oic.option_chain where date <= $2::text::date ) and
    expiration > $2::text::date and
    expiration <= $2::text::date + interval '3 months' and
    bid > 0.0 and
    ask > 0.0 and
    ((delta >= 0.2 and delta <= 0.8) or
    (delta <= -0.2 and delta >= -0.8))
  group by
    act_symbol) option_spread
on
  market.component_symbol = option_spread.act_symbol
left outer join
  oic.weekly w
on
  market.component_symbol = w.act_symbol and
  w.last_seen >= (select max(last_seen) from oic.weekly where last_seen <= $2::text::date)
where 
  market.etf_symbol = any(string_to_array($1, ',')) and
  market.date = (select max(date) from spdr.etf_holding where date <= $2::text::date);
"
                   market
                   date)))

(define (get-earnings-vibes-analysis date #:live-prices [live-prices-arg #f])
  (define prices (if live-prices-arg live-prices-arg
                     (apply hash (flatten (map (λ (row) (list (vector-ref row 0) (vector-ref row 1)))
                                               (query-rows dbc "
select
  o.act_symbol,
  o.close
from
  polygon.ohlc o
join
  zacks.earnings_calendar ec
on
  o.act_symbol = ec.act_symbol
where
  o.date = (select max(date) from polygon.ohlc where date <= $1::text::date) and
  ((ec.date = $1::text::date and ec.\"when\" = 'After market close'::zacks.\"when\") or
   (ec.date = $1::text::date + interval '1 day' and ec.\"when\" = 'Before market open'::zacks.\"when\"));
"
                                                           date))))))

  (map (λ (row) (earnings-vibes-analysis (vector-ref row 0)
                                         (iso8601->date (vector-ref row 1))
                                         (iso8601->date (vector-ref row 2))
                                         (vector-ref row 3)
                                         (vector-ref row 4)
                                         (vector-ref row 5)
                                         #f ; price-strike-ratio to be filled in
                                         (iso8601->date (vector-ref row 6))
                                         (string->symbol (string-replace (string-downcase (vector-ref row 7)) " " "-"))
                                         (vector-ref row 8)
                                         (vector-ref row 9)))
       (query-rows dbc (string-append "
with prices as (
select
  act_symbol,
  price
from (values " (if (hash-empty? prices)
                   "(null, null::numeric)"
                   (string-join (map (λ (kv) (~a "('" (car kv) "', " (real->decimal-string (cdr kv)) ")")) (hash->list prices)) ", "))
                                      ")
  as prices (act_symbol, price)),
all_expirations as (
select distinct
  oc.act_symbol,
  oc.expiration
from
  oic.option_chain oc
where
  date = (select max(date) from oic.option_chain where date <= $1::text::date) and
  act_symbol in (select distinct act_symbol from prices)
),
exprs as (
select
  ae1.act_symbol,
  min(ae1.expiration) as min_expiration,
  min(ae2.expiration) as max_expiration
from
  all_expirations ae1
join
  all_expirations ae2
on
  ae1.act_symbol = ae2.act_symbol and
  ae1.expiration + interval '21 days' < ae2.expiration and
  ae1.expiration >= $1::text::date
group by
  ae1.act_symbol
),
eligible_strikes as (
select
  oc1.act_symbol,
  oc1.strike,
  p.price
from
  oic.option_chain oc1
join
  oic.option_chain oc2
on
  oc1.date = oc2.date and
  oc1.act_symbol = oc2.act_symbol and
  oc1.strike = oc2.strike and
  oc1.call_put = oc2.call_put
join
  exprs e
on
  oc1.act_symbol = e.act_symbol and
  oc2.act_symbol = e.act_symbol and
  oc1.expiration = e.min_expiration and
  oc2.expiration = e.max_expiration
join
  prices p
on
  oc1.act_symbol = p.act_symbol
where
  oc1.date = (select max(date) from oic.option_chain where date <= $1::text::date) and
  oc1.call_put = 'Call'
),
strks as (
select
  es.act_symbol,
  es.strike
from
  eligible_strikes es
join
  (select
    act_symbol,
    min(abs(strike - price)) as strike_price_diff
  from
    eligible_strikes
  group by
    act_symbol) es_atm
on
  es.act_symbol = es_atm.act_symbol and
  abs(es.strike - es.price) = es_atm.strike_price_diff
),
earnings_near_date as (
select
  act_symbol,
  date,
  \"when\"
from
  zacks.earnings_calendar ec
where
  ((ec.date = $1::text::date and ec.\"when\" = 'After market close'::zacks.\"when\") or
  (ec.date = $1::text::date + interval '1 day' and ec.\"when\" = 'Before market open'::zacks.\"when\")) and
  ec.act_symbol in (select distinct act_symbol from prices)
order by
  act_symbol,
  date
),
iv_hv as (
select
  act_symbol,
  avg(iv_current) as avg_iv,
  avg(hv_current) as avg_hv
from
  oic.volatility_history
where
  date >= $1::text::date - interval '1 year' and
  date <= $1::text::date and
  act_symbol in (select distinct act_symbol from prices)
group by
  act_symbol
order by
  act_symbol
),
vlm as (
select
  act_symbol,
  trunc(log10(greatest(avg(close * volume), 1)), 1) as avg_vlm
from
  polygon.ohlc o
where
  date >= $1::text::date - interval '30 days' and
  date <= $1::text::date and
  act_symbol in (select distinct act_symbol from prices)
group by
  act_symbol
),
sprds as (
select
  act_symbol,
  avg((ask - bid) / ask) as spread
from
  oic.option_chain
where
  date = (select max(date) from oic.option_chain where date <= $1::text::date ) and
  act_symbol in (select distinct act_symbol from prices) and
  expiration > $1::text::date and
  expiration <= $1::text::date + interval '3 months' and
  bid > 0.0 and
  ask > 0.0 and
  ((delta >= 0.2 and delta <= 0.8) or
  (delta <= -0.2 and delta >= -0.8))
group by
  act_symbol
)
select
  exprs.act_symbol,
  exprs.min_expiration::text,
  exprs.max_expiration::text,
  strks.strike,
  coalesce((back_vol.vol - front_vol.vol) / nullif(exprs.max_expiration - exprs.min_expiration, 0), 0) as vol_slope,
  coalesce(iv_hv.avg_iv / nullif(iv_hv.avg_hv, 0), 0) as iv_hv,
  en.date::text as earnings_date,
  en.\"when\"::text as earnings_when,
  sprds.spread as opt_spread,
  vlm.avg_vlm
from
  exprs
join
  strks
on
  exprs.act_symbol = strks.act_symbol
join
  earnings_near_date en
on
  exprs.act_symbol = en.act_symbol
join
  iv_hv
on
  exprs.act_symbol = iv_hv.act_symbol
join
  vlm
on
  exprs.act_symbol = vlm.act_symbol
join
  sprds
on
  exprs.act_symbol = sprds.act_symbol
join
  oic.option_chain front_vol
on
  front_vol.date = (select max(date) from oic.option_chain where date <= $1::text::date ) and
  exprs.act_symbol = front_vol.act_symbol and
  exprs.min_expiration = front_vol.expiration and
  strks.strike = front_vol.strike and
  front_vol.call_put = 'Call'
join
  oic.option_chain back_vol
on
  back_vol.date = (select max(date) from oic.option_chain where date <= $1::text::date ) and
  exprs.act_symbol = back_vol.act_symbol and
  exprs.max_expiration = back_vol.expiration and
  strks.strike = back_vol.strike and
  back_vol.call_put = 'Call'
order by
  coalesce((back_vol.vol - front_vol.vol) / nullif(exprs.max_expiration - exprs.min_expiration, 0), 0) asc;
")
                   date)))

(define (get-etf-vrp-analysis date)
  (map (λ (row) (etf-vrp-analysis (vector-ref row 0)
                                  (vector-ref row 1)
                                  (vector-ref row 2)
                                  #f
                                  #f
                                  #f
                                  (vector-ref row 3)))
       (query-rows dbc "
with etfs as (
select distinct
  etf_symbol as act_symbol
from
  spdr.etf_holding
where
  date = (select max(date) from spdr.etf_holding where date <= $1::text::date)
order by
  etf_symbol
), log_iv_hv as (
select
  vh.act_symbol,
  log(vh.iv_current / vh.hv_current) as iv_hv,
  vh.iv_current
from
  oic.volatility_history vh
join
  etfs
on
  vh.act_symbol = etfs.act_symbol
where
  date = (select max(date) from oic.volatility_history vh where date <= $1::text::date)
), ivp_1yr as (
select
  vh.act_symbol,
  sum(
    case when vh.iv_current < log_iv_hv.iv_current then 1.0
    else 0.0
    end
  ) / count(*) as ivp
from
  oic.volatility_history vh
join
  etfs
on
  vh.act_symbol = etfs.act_symbol
join
  log_iv_hv
on
  vh.act_symbol = log_iv_hv.act_symbol
where
  vh.date >= $1::text::date - '1 year'::interval and
  vh.date <= $1::text::date
group by
  vh.act_symbol
), sprds as (
select
  act_symbol,
  avg((ask - bid) / ask) as spread
from
  oic.option_chain
where
  date = (select max(date) from oic.option_chain where date <= $1::text::date ) and
  act_symbol in (select distinct act_symbol from etfs) and
  expiration > $1::text::date and
  expiration <= $1::text::date + interval '3 months' and
  bid > 0.0 and
  ask > 0.0 and
  ((delta >= 0.2 and delta <= 0.8) or
  (delta <= -0.2 and delta >= -0.8))
group by
  act_symbol
)
select
  etfs.act_symbol,
  coalesce(log_iv_hv.iv_hv, 0.0) as iv_hv,
  coalesce(ivp_1yr.ivp, 0.0) as ivp,
  sprds.spread as spread
from
  etfs
left outer join
  log_iv_hv
on
  etfs.act_symbol = log_iv_hv.act_symbol
left outer join
  ivp_1yr
on
  etfs.act_symbol = ivp_1yr.act_symbol
left outer join
  sprds
on
  etfs.act_symbol = sprds.act_symbol
order by
  log_iv_hv.iv_hv desc;
"
                   date)))

(define (get-forward-factor-analysis date)
  (map (λ (row) (forward-factor-analysis (vector-ref row 0)
                                         (iso8601->date (vector-ref row 1))
                                         (vector-ref row 2)
                                         (iso8601->date (vector-ref row 3))
                                         (vector-ref row 4)
                                         (vector-ref row 5)
                                         (vector-ref row 6)
                                         (vector-ref row 7)
                                         (if (equal? "" (vector-ref row 8)) #f (iso8601->date (vector-ref row 8)))
                                         (vector-ref row 9)))
       (query-rows dbc "
with vol_by_exp as (select
  act_symbol,
  expiration,
  avg(vol) as avg_vol
from
  oic.atm_curve ac
where
  date = (select max(date) from oic.atm_curve where date <= $1::text::date)
group by
  act_symbol,
  expiration),
earnings_date as (
select distinct
  vol_by_exp.act_symbol,
  ec.date as actual,
  coalesce(ec.date, $1::text::date + '28 days'::interval) as pivot
from
  vol_by_exp
left outer join
  zacks.earnings_calendar ec
on
  vol_by_exp.act_symbol = ec.act_symbol and
  ec.date >= $1::text::date - '1 day'::interval and
  ec.date <= $1::text::date + '28 days'::interval),
max_front_vol as (select
  vol_by_exp.act_symbol,
  max(avg_vol) as vol
from
  vol_by_exp
join
  earnings_date
on
  vol_by_exp.act_symbol = earnings_date.act_symbol
where
  expiration < pivot and
  expiration >= $1::text::date + '7 days'::interval
group by
  vol_by_exp.act_symbol),
min_back_vol as (select
  vol_by_exp.act_symbol,
  min(avg_vol) as vol
from
  vol_by_exp
join
  earnings_date
on
  vol_by_exp.act_symbol = earnings_date.act_symbol
where
  expiration > earnings_date.pivot + '7 days'::interval and
  expiration <= earnings_date.pivot + '56 days'::interval
group by
  vol_by_exp.act_symbol),
sprds as (
select
  act_symbol,
  avg((ask - bid) / ask) as spread
from
  oic.option_chain
where
  date = (select max(date) from oic.option_chain where date <= $1::text::date) and
  expiration > $1::text::date and
  expiration <= $1::text::date + interval '3 months' and
  bid > 0.0 and
  ask > 0.0 and
  ((delta >= 0.2 and delta <= 0.8) or
  (delta <= -0.2 and delta >= -0.8))
group by
  act_symbol)
select
  max_front_vol.act_symbol,
  fvx.expiration::text as front_exp,
  max_front_vol.vol as front_vol,
  bvx.expiration::text as back_exp,
  min_back_vol.vol as back_vol,
  max_front_vol.vol / min_back_vol.vol as vol_ratio,
  sqrt(greatest(0.0001, (((bvx.expiration - $1::text::date)::decimal /
        (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal
        * min_back_vol.vol * min_back_vol.vol)
      - ((fvx.expiration - $1::text::date)::decimal /
        (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal
        * max_front_vol.vol * max_front_vol.vol))
    / ((bvx.expiration - fvx.expiration)::decimal /
      (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal)
    )) as forward_vol,
  (max_front_vol.vol /
  sqrt(greatest(0.0001, (((bvx.expiration - $1::text::date)::decimal /
        (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal
        * min_back_vol.vol * min_back_vol.vol)
      - ((fvx.expiration - $1::text::date)::decimal /
        (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal
        * max_front_vol.vol * max_front_vol.vol))
    / ((bvx.expiration - fvx.expiration)::decimal /
      (make_date(date_part('year', $1::text::date)::integer + 1, 1, 1) - make_date(date_part('year', $1::text::date)::integer, 1, 1))::decimal)
    ))
  ) - 1.0 as forward_factor,
  coalesce(earnings_date.actual::text, '') as earnings_date,
  sprds.spread as opt_spread
from
  max_front_vol
join
  min_back_vol
on
  max_front_vol.act_symbol = min_back_vol.act_symbol
join
  earnings_date
on
  max_front_vol.act_symbol = earnings_date.act_symbol
join
  nasdaq.symbol as sym
on
  max_front_vol.act_symbol = sym.act_symbol
join
  sprds
on
  max_front_vol.act_symbol = sprds.act_symbol
join
  (select
    act_symbol,
    min(expiration) as expiration,
    avg_vol
  from
    vol_by_exp
  where
    expiration < $1::text::date + '28 days'::interval
  group by
    act_symbol,
    avg_vol) fvx
on
  max_front_vol.act_symbol = fvx.act_symbol and
  max_front_vol.vol = fvx.avg_vol
join
  (select
    act_symbol,
    max(expiration) as expiration,
    avg_vol
  from
    vol_by_exp
  where
    expiration >= $1::text::date + '28 days'::interval
  group by
    act_symbol,
    avg_vol) bvx
on
  min_back_vol.act_symbol = bvx.act_symbol and
  min_back_vol.vol = bvx.avg_vol
where
  sym.is_etf = false
order by
  forward_factor desc;
"
                   date)))

(define (get-position-analysis date)
  (map (λ (row) (position-analysis (vector-ref row 0)
                                   (vector-ref row 1)
                                   (iso8601->date (vector-ref row 2))
                                   (vector-ref row 3)
                                   (string->symbol (string-downcase (vector-ref row 4)))
                                   (vector-ref row 5)
                                   (vector-ref row 6)
                                   (if (equal? 0.00 (vector-ref row 7)) #f (vector-ref row 7))
                                   (if (equal? 0.00 (vector-ref row 8)) #f (vector-ref row 8))
                                   (if (equal? 0.00 (vector-ref row 9)) #f (vector-ref row 9))
                                   (if (equal? 0.00 (vector-ref row 10)) #f (vector-ref row 10))
                                   (if (equal? 0.00 (vector-ref row 11)) #f (vector-ref row 11))
                                   (if (equal? "" (vector-ref row 12)) #f (iso8601->date (vector-ref row 12)))
                                   (string->symbol (string-replace (string-downcase (vector-ref row 13)) " " "-"))))
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
), expiry_end_date as (
  select
    o.account,
    o.order_id,
    min(c.expiry) as expiry
  from
    ibkr.order o
  join
    ibkr.order_leg ol
  on
    o.account = ol.account and
    o.order_id = ol.order_id
  join
    ibkr.contract c
  on
    ol.contract_id = c.contract_id
  where
    c.expiry >= $1::text::date
  group by
    o.account,
    o.order_id
)
select
  coalesce(spdr.to_sector_etf(eh.sector), '') as etf_symbol,
  c.symbol,
  c.expiry::text,
  c.strike,
  c.right::text,
  e.account,
  e.signed_shares,
  coalesce(n.underlying_low_stop_price, 0.00),
  coalesce(n.underlying_low_target_price, 0.00),
  coalesce(ch.close, 0.00),
  coalesce(n.underlying_high_stop_price, 0.00),
  coalesce(n.underlying_high_target_price, 0.00),
  coalesce((case when ed.end_date is not null and ed.end_date < n.end_date
    then case when eed.expiry is not null and eed.expiry < ed.end_date
      then eed.expiry else ed.end_date end
    else case when eed.expiry is not null and eed.expiry < n.end_date
      then eed.expiry else n.end_date end
  end)::text, '') as end_date,
  coalesce(n.order_strategy::text, '') as order_strategy
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
  where
    timestamp <= ($1::text::timestamp + '20 hours'::interval)
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
left outer join
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
  polygon.ohlc ch
on
  c.symbol = ch.act_symbol and
  ch.date = (select max(date) from polygon.ohlc where date <= $1::text::date)
left outer join
  expiry_end_date eed
on
  e.account = eed.account and
  e.order_id = eed.order_id
where
  c.expiry >= $1::text::date and
  signed_shares != 0
order by
  etf_symbol,
  symbol,
  expiry,
  strike,
  \"right\";
"
                   date)))

(define (get-position-history date)
  (query-value dbc "
select
  coalesce('Bulls: ' ||
  sum(
    case n.order_strategy
      when 'LONG CALL' then 1
      when 'BULL CALL VERTICAL SPREAD' then 1
      when 'BULL PUT VERTICAL SPREAD' then 1
      when 'CALL RATIO SPREAD' then 1
      when 'CALL DIAGONAL SPREAD' then 1
    else 0
    end
  ) || ' Roos: ' ||
  sum(
    case n.order_strategy
      when 'LONG STRADDLE' then 1
      when 'LONG STRANGLE' then 1
      when 'CALL BUTTERFLY' then 1
      when 'PUT BUTTERFLY' then 1
      when 'CALL CONDOR' then 1
      when 'PUT CONDOR' then 1
      when 'CALL HORIZONTAL SPREAD' then 1
      when 'PUT HORIZONTAL SPREAD' then 1
      when 'CALL DOUBLE HORIZONTAL SPREAD' then 1
      when 'PUT DOUBLE HORIZONTAL SPREAD' then 1
    else 0
    end
  ) || ' Bears: ' ||
  sum(
    case n.order_strategy
      when 'LONG PUT' then 1
      when 'BEAR CALL VERTICAL SPREAD' then 1
      when 'BEAR PUT VERTICAL SPREAD' then 1
      when 'PUT RATIO SPREAD' then 1
      when 'PUT DIAGONAL SPREAD' then 1
    else 0
    end
  ), 'No history for past month') as summary
from
  (select distinct
    order_id
  from
    ibkr.\"position\" p
  where
    p.entry_timestamp >= $1::text::date - '1 month'::interval) oids
join
  ibkr.order_note n
on
  n.order_id = oids.order_id;
"
               date))

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

(define (get-is-etf act-symbol)
  (query-value dbc "
select
  is_etf
from
  nasdaq.symbol
where
  act_symbol = $1;
"
               act-symbol))

(define (get-options act-symbol date)
  (map (λ (row) (option (vector-ref row 0)
                        (iso8601->date (vector-ref row 1))
                        (vector-ref row 2)
                        (vector-ref row 3)
                        (string->symbol (string-downcase (vector-ref row 4)))
                        (iso8601->date (vector-ref row 5))
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
  expiration::text,
  expiration - $2::text::date as dte,
  strike,
  call_put::text,
  date::text,
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
                   (date->iso8601 date))))

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
  ex_date - $2::text::date - 1,
  amount
from
  zacks.dividend_calendar
where
  act_symbol = $1 and
  ex_date > $2::text::date and
  ex_date <= $3::text::date;
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

(define (insert-commission-report commission-report)
  (log-message file-log 'info (format "insert-commission-report ~v" commission-report))
  (with-handlers ([exn:fail? (λ (error)
                               (displayln "Could not insert commission report into DB")
                               (displayln commission-report)
                               (displayln error))])
    (query-exec dbc "
insert into ibkr.commission_report (
  execution_id,
  commission,
  currency,
  realized_pnl,
  yield,
  yield_redemption_date
) values (
  $1,
  $2,
  $3,
  $4,
  $5,
  $6
) on conflict (execution_id) do nothing;
"
                (commission-report-rsp-execution-id commission-report)
                (commission-report-rsp-commission commission-report)
                (commission-report-rsp-currency commission-report)
                (if (commission-report-rsp-realized-pnl commission-report)
                    (commission-report-rsp-realized-pnl commission-report)
                    sql-null)
                (if (commission-report-rsp-yield commission-report)
                    (commission-report-rsp-yield commission-report)
                    sql-null)
                (if (commission-report-rsp-yield-redemption-date commission-report)
                    (commission-report-rsp-yield-redemption-date commission-report)
                    sql-null))))

(define (insert-execution execution)
  (log-message file-log 'info (format "insert-execution ~v" execution))
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
              (~t (execution-rsp-timestamp execution) "yyyy-MM-dd'T'HH:mm:ssZ")
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

(define (insert-execution-tick execution-id tick)
  (log-message file-log 'info (format "insert-execution-tick ~v ~v" execution-id tick))
  (query-exec dbc "
insert into ibkr.execution_tick (
  execution_id,
  \"timestamp\",
  bid_price,
  bid_size,
  ask_price,
  ask_size
) values (
  $1,
  $2::text::timestamptz,
  $3,
  $4,
  $5,
  $6
) on conflict (execution_id) do nothing;
"
              execution-id
              (~t (historical-tick-moment tick) "yyyy-MM-dd'T'HH:mm:ssZ")
              (historical-tick-bid-price tick)
              (historical-tick-bid-size tick)
              (historical-tick-ask-price tick)
              (historical-tick-ask-size tick)))

(define (insert-condor-analysis date
                                condor-analysis
                                market-rating
                                market-risk-reward
                                sector-rating
                                sector-risk-reward
                                industry-rating
                                industry-risk-reward
                                stock-rating
                                stock-risk-reward)
  (query-exec dbc "
insert into renegade.condor_analysis (
  date,
  market_act_symbol,
  market_rating,
  market_risk_reward,
  sector_act_symbol,
  sector_rating,
  sector_risk_reward,
  industry_act_symbol,
  industry_rating,
  industry_risk_reward,
  stock_act_symbol,
  stock_rating,
  stock_risk_reward,
  earnings_date,
  option_spread
) values (
  $1::text::date,
  $2,
  case
    when $3::numeric = 0 then null
    else round($3::numeric, 2)
  end,
  case
    when $4::numeric = 0 then null
    else round($4::numeric, 2)
  end,
  $5,
  case
    when $6::numeric = 0 then null
    else round($6::numeric, 2)
  end,
  case
    when $7::numeric = 0 then null
    else round($7::numeric, 2)
  end,
  $8,
  case
    when $9::numeric = 0 then null
    else round($9::numeric, 2)
  end,
  case
    when $10::numeric = 0 then null
    else round($10::numeric, 2)
  end,
  $11,
  case
    when $12::numeric = 0 then null
    else round($12::numeric, 2)
  end,
  case
    when $13::numeric = 0 then null
    else round($13::numeric, 2)
  end,
  case
    when $14::text = '' then null
    else to_date($14::text, 'YY-MM-DD')
  end,
  case
    when $15::text = '' then null
    else $15::text::numeric
  end
) on conflict (date, stock_act_symbol) do nothing;
"
              date
              (condor-analysis-market condor-analysis)
              market-rating
              market-risk-reward
              (condor-analysis-sector condor-analysis)
              sector-rating
              sector-risk-reward
              (condor-analysis-industry condor-analysis)
              industry-rating
              industry-risk-reward
              (condor-analysis-stock condor-analysis)
              stock-rating
              stock-risk-reward
              (condor-analysis-earnings-date condor-analysis)
              (condor-analysis-option-spread condor-analysis)))

(define (insert-contract contract)
  (log-message file-log 'info (format "insert-contract ~v" contract))
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
              (if (contract-details-rsp-expiry contract)
                  (date->iso8601 (contract-details-rsp-expiry contract))
                  sql-null)
              (if (contract-details-rsp-strike contract)
                  (contract-details-rsp-strike contract)
                  sql-null)
              (if (contract-details-rsp-right contract)
                  (string-upcase (symbol->string (contract-details-rsp-right contract)))
                  sql-null)
              (contract-details-rsp-exchange contract)
              (contract-details-rsp-currency contract)
              (contract-details-rsp-local-symbol contract)
              (contract-details-rsp-market-name contract)
              (contract-details-rsp-trading-class contract)
              (contract-details-rsp-contract-id contract)
              (contract-details-rsp-minimum-tick-increment contract)
              (if (equal? "" (contract-details-rsp-multiplier contract))
                  sql-null
                  (contract-details-rsp-multiplier contract))
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

(define (insert-price-analysis date price-analysis market-rating sector-rating industry-rating stock-patterns)
  (query-exec dbc "
insert into renegade.price_analysis (
  date,
  market_act_symbol,
  market_rating,
  sector_act_symbol,
  sector_vs_market,
  sector_rating,
  industry_act_symbol,
  industry_rating,
  stock_act_symbol,
  stock_vs_sector,
  dividend_date,
  earnings_date,
  option_spread,
  zacks_rank,
  patterns
) values (
  $1::text::date,
  case
    when $2 = '' then null
    else $2
  end,
  case
    when $2 = '' then null
    else $3::smallint
  end,
  case
    when $4 = '' then null
    else $4
  end,
  case
    when $4 = '' then null
    else $5::numeric
  end,
  case
    when $4 = '' then null
    else $6::smallint
  end,
  case
    when $7 = '' then null
    else $7
  end,
  case
    when $7 = '' then null
    else $8::smallint
  end,
  $9,
  $10,
  case
    when $11::text = '' then null
    else to_date($11::text, 'YY-MM-DD')
  end,
  case
    when $12::text = '' then null
    else to_date($12::text, 'YY-MM-DD')
  end,
  case
    when $13::text = '' then null
    else $13::text::numeric
  end,
  case
    when $14::text = '' then null
    else zacks.to_integer_rank($14::text::zacks.rank)
  end,
  $15
) on conflict (date, stock_act_symbol) do nothing;
"
              date
              (price-analysis-market price-analysis)
              market-rating
              (price-analysis-sector price-analysis)
              (price-analysis-sector-vs-market price-analysis)
              sector-rating
              (price-analysis-industry price-analysis)
              industry-rating
              (price-analysis-stock price-analysis)
              (price-analysis-stock-vs-sector price-analysis)
              (price-analysis-next-div-date price-analysis)
              (price-analysis-earnings-date price-analysis)
              (price-analysis-option-spread price-analysis)
              (string-replace (price-analysis-zacks-rank price-analysis) "Str" "Strong")
              stock-patterns))

(define (insert-order order)
  (log-message file-log 'info (format "insert-order ~v" order))
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
              (if (open-order-rsp-open-close order)
                  (string-upcase (symbol->string (open-order-rsp-open-close order)))
                  sql-null)
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
) on conflict (account, order_id, \"type\", \"comparator\") do nothing;
"
                          (open-order-rsp-account order)
                          (open-order-rsp-order-id order)
                          (if (condition-contract-id con)
                              (condition-contract-id con)
                              sql-null)
                          (string-replace (string-upcase (symbol->string (condition-type con))) "-" " ")
                          (string-upcase (symbol->string (condition-boolean-operator con)))
                          (string-replace (string-upcase (symbol->string (condition-comparator con))) "-" " ")
                          (cond
                            [(moment? (condition-value con)) (moment->iso8601 (condition-value con))]
                            [(rational? (condition-value con)) (real->decimal-string (condition-value con) 2)])
                          (if (condition-exchange con)
                              (condition-exchange con)
                              sql-null)
                          (if (condition-trigger-method con)
                              (string-replace (string-upcase (symbol->string (condition-trigger-method con))) "-" " ")
                              sql-null)))
            (open-order-rsp-conditions order)))

(define (insert-order-note account order-id order-note)
  (log-message file-log 'info (format "insert-order-note ~v ~v ~v" account order-id order-note))
  (query-exec dbc "
insert into ibkr.order_note (
  account,
  order_id,
  order_strategy,
  underlying_entry_price,
  underlying_low_stop_price,
  underlying_low_target_price,
  underlying_high_stop_price,
  underlying_high_target_price,
  end_date,
  pattern
) values (
  $1,
  $2,
  $3::text::ibkr.order_strategy,
  $4,
  $5,
  $6,
  $7,
  $8,
  $9::text::date,
  $10::text::ibkr.pattern
) on conflict (account, order_id) do nothing;
"
              account
              order-id
              (string-replace (string-upcase (symbol->string (order-strategy order-note))) "-" " ")
              (order-stock-entry order-note)
              (if (order-stock-low-stop order-note) (order-stock-low-stop order-note) sql-null)
              (if (order-stock-low-target order-note) (order-stock-low-target order-note) sql-null)
              (if (order-stock-high-stop order-note) (order-stock-high-stop order-note) sql-null)
              (if (order-stock-high-target order-note) (order-stock-high-target order-note) sql-null)
              (date->iso8601 (order-end-date order-note))
              (string-replace (string-upcase (symbol->string (order-pattern order-note))) "-" " ")))
