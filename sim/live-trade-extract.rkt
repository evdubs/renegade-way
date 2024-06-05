#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We need to set up the command line params to later access in our (define dbc ...) call.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "../params.rkt")

  (command-line
   #:program "racket live-trade-extract.rkt"
   #:once-each
   [("-b" "--bearish") "Run analysis for bearish trades. Defaults (via omission) to bullish"
                       (bearish? #t)]
   [("-f" "--filename") file-name
                        "File name to save trades as a TSV"
                        (filename file-name)]
   [("-n" "--db-name") name
                       "Database name. Defaults to 'local'"
                       (db-name name)]
   [("-p" "--db-pass") password
                       "Database password"
                       (db-pass password)]
   [("-u" "--db-user") user
                       "Database user name. Defaults to 'user'"
                       (db-user user)]))

(require 'cmd
         db
         db/util/postgresql
         gregor
         gregor/period
         racket/list
         racket/string
         threading
         "../params.rkt"
         (except-in "../structs.rkt"
                    trade
                    struct:trade
                    trade?)
         "trade.rkt")

(define dbc (postgresql-connect #:server (db-host) #:user (db-user) #:database (db-name) #:password (db-pass)))

(define rows (query-rows dbc "
select 
  o.symbol as sym,
  o.expiries as exp,
  o.strikes as stk,
  o.rights as rt,
  o.open_date as odt,
  o.price as opx,
  o.shares as osh,
  o.commission as ocm,
  c.close_date as cdt,
  c.value / o.shares as cpx,
  o.shares as csh,
  coalesce(c.commission, 0) as ccm,
  trunc(-100 * coalesce((o.shares * o.price), 0)
    - coalesce((100 * c.value), 0)
    - coalesce(o.commission, 0) - coalesce(c.commission, 0), 2) as pl,
  '' as mrtg,
  '' as scrtg,
  '' as irtg,
  '' as skrtg,
  '' as srr,
  '' as edt,
  '' as osprd,
  '' as num,
  on2.\"pattern\"::text as ptrn,
  trunc(on2.underlying_entry_price, 2) as ent,
  trunc(on2.underlying_stop_price, 2) as stp,
  trunc(on2.underlying_target_price, 2) as tgt
from
(select 
  ot.account,
  ot.order_id,
  c.symbol,
  array_agg(distinct c.expiry::text) as expiries,
  array_agg(distinct c.strike) as strikes,
  array_agg(distinct c.right::text) as rights,
  min(ot.open_date)::text as open_date,
  min(ot.shares) as shares,
  trunc(sum(case ot.side
    when 'BOT' then
      case c.\"security_type\"
        when 'OPT' then ot.price * ot.shares
        when 'STK' then ot.price * ot.shares / 100
      end
    when 'SLD' then
      case c.\"security_type\"
        when 'OPT' then -1 * ot.price * ot.shares
        when 'STK' then -1 * ot.price * ot.shares / 100
      end
  end) / min(ot.shares), 4) as price,
  trunc(sum(commission), 4) as commission
from
  (select
    e.account,
    e.order_id,
    e.contract_id,
    min(e.timestamp::date) as open_date,
    e.side,
    sum(e.shares) as shares,
    sum(e.shares * e.price) / sum(e.shares) as price,
    sum(cr.commission) as commission
  from
    ibkr.execution e
  join
    ibkr.contract c
  on
    e.contract_id = c.contract_id
  left outer join
    ibkr.commission_report cr 
  on
    e.execution_id = cr.execution_id
  where 
    e.order_id > 0 and 
    e.account like 'U%' and
    c.\"security_type\" in ('OPT', 'STK')
  group by
    e.account,
    e.order_id,
    e.contract_id,
    e.side
  order by 
    e.account,
    e.order_id,
    e.contract_id,
    e.side) ot
join
  ibkr.contract c
on
  ot.contract_id = c.contract_id
group by 
  ot.account,
  ot.order_id,
  c.symbol) o
join
  (select
    e.account,
    cta.opening_order_id as order_id,
    min(e.timestamp::date)::text as close_date,
    trunc(sum(case e.side
      when 'BOT' then
        case c.\"security_type\"
          when 'OPT' then e.price * e.shares
          when 'STK' then e.price * e.shares / 100
        end
      when 'SLD' then
        case c.\"security_type\"
          when 'OPT' then -1 * e.price * e.shares
          when 'STK' then -1 * e.price * e.shares / 100
        end
    end), 4) as value,
    trunc(sum(cr.commission), 4) as commission
  from
    ibkr.execution e
  join
    ibkr.contract c
  on
    e.contract_id = c.contract_id
  join 
    ibkr.closing_trade_association cta 
  on
    e.execution_id = cta.execution_id
  left outer join
    ibkr.commission_report cr 
  on
    e.execution_id = cr.execution_id
  where 
    e.order_id <= 0 and 
    e.account like 'U%' and
    c.\"security_type\" in ('OPT', 'STK')
  group by
    e.account,
    cta.opening_order_id
  order by 
    e.account,
    cta.opening_order_id) c
on 
  o.account = c.account and
  o.order_id = c.order_id
left outer join 
  ibkr.order_note on2
on
  o.account = on2.account and 
  o.order_id = on2.order_id;"))

(call-with-output-file* (filename)
  (λ (out)
    (for-each (λ (r) (displayln (trade->tsv (trade (vector-ref r 0)
                                                   (map (λ (d) (iso8601->date d)) (pg-array->list (vector-ref r 1)))
                                                   (pg-array->list (vector-ref r 2))
                                                   (pg-array->list (vector-ref r 3))
                                                   (iso8601->date (vector-ref r 4))
                                                   (vector-ref r 5)
                                                   (vector-ref r 6)
                                                   (vector-ref r 7)
                                                   (iso8601->date (vector-ref r 8))
                                                   (vector-ref r 9)
                                                   (vector-ref r 10)
                                                   (vector-ref r 11)
                                                   (vector-ref r 12)
                                                   #f
                                                   #f
                                                   #f
                                                   #f
                                                   #f
                                                   #f
                                                   #f
                                                   0
                                                   (vector-ref r 21)
                                                   (vector-ref r 22)
                                                   (vector-ref r 23)
                                                   (vector-ref r 24))) out))
              ; do the filter here because it can get messy in the query
              (filter (λ (r) (date<=? (iso8601->date (vector-ref r 8)) (today))) rows)))
  #:exists 'replace)
