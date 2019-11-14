CREATE TYPE ibkr."right" AS ENUM
    ('CALL',
     'PUT');

CREATE TYPE ibkr.security_type AS ENUM
    ('STK',
     'OPT',
     'FUT',
     'CASH',
     'BOND',
     'CFD',
     'FOP',
     'WAR',
     'IOPT',
     'FWD',
     'BAG',
     'IND',
     'BILL',
     'FUND',
     'FIXED',
     'SLB',
     'NEWS',
     'CMDTY',
     'BSK',
     'ICU',
     'ICS');

CREATE TABLE ibkr.execution
(
    order_id integer NOT NULL,
    contract_id integer,
    act_symbol text,
    security_type ibkr.security_type,
    expiry date,
    strike numeric,
    "right" ibkr."right",
    multiplier numeric,
    exchange text,
    currency text,
    local_symbol text,
    trading_class text,
    execution_id text,
    "timestamp" timestamp with time zone,
    account text,
    executing_exchange text,
    side text,
    shares numeric,
    price numeric,
    perm_id integer,
    client_id integer,
    liquidation integer,
    cumulative_quantity integer,
    average_price numeric,
    order_reference text,
    ev_rule text,
    ev_multiplier numeric,
    model_code text,
    CONSTRAINT execution_execution_id_key UNIQUE (execution_id),
    CONSTRAINT execution_act_symbol_fkey FOREIGN KEY (act_symbol)
        REFERENCES nasdaq.symbol (act_symbol) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE INDEX ON ibkr.execution ("timestamp");

CREATE OR REPLACE VIEW ibkr."position"
 AS
 SELECT execution.contract_id,
    execution.act_symbol,
    execution.security_type,
    execution.expiry,
    execution.strike,
    execution."right",
    execution.multiplier,
    execution.currency,
    execution.local_symbol,
    execution.trading_class,
    execution.account,
    sum(
        CASE execution.side
            WHEN 'BOT'::text THEN execution.shares
            WHEN 'SLD'::text THEN execution.shares * '-1'::integer::numeric
            ELSE NULL::numeric
        END) AS signed_shares,
    min(execution."timestamp") AS entry_timestamp
   FROM ibkr.execution
  GROUP BY 
    execution.contract_id, 
    execution.act_symbol, 
    execution.security_type, 
    execution.expiry, 
    execution.strike, 
    execution."right", 
    execution.multiplier, 
    execution.currency, 
    execution.local_symbol, 
    execution.trading_class, 
    execution.account;

