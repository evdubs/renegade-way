CREATE TYPE ibkr.action AS ENUM
    ('BUY',
     'SELL',
     'SSHORT');

CREATE TYPE ibkr.condition_comparator AS ENUM
    ('LESS THAN',
     'GREATER THAN');

CREATE TYPE ibkr.condition_operator AS ENUM
    ('AND',
     'OR');

CREATE TYPE ibkr.condition_trigger_method AS ENUM
    ('DEFAULT',
     'DOUBLE BID/ASK',
     'LAST',
     'DOUBLE LAST',
     'BID/ASK',
     'LAST OF BID/ASK',
     'MID POINT');

CREATE TYPE ibkr.condition_type AS ENUM
    ('PRICE',
     'TIME',
     'MARGIN',
     'EXECUTION',
     'VOLUME',
     'PERCENT CHANGE');

CREATE TYPE ibkr.open_close AS ENUM
    ('OPEN',
     'CLOSE',
     'SAME');

CREATE TYPE ibkr.order_strategy AS ENUM
    ('LONG CALL',
     'LONG PUT',
     'BULL CALL VERTICAL SPREAD',
     'BEAR CALL VERTICAL SPREAD',
     'BULL PUT VERTICAL SPREAD',
     'BEAR PUT VERTICAL SPREAD');


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

CREATE TYPE ibkr.time_in_force AS ENUM
    ('DAY',
     'GTC',
     'OPG',
     'IOC',
     'GTD',
     'GTT',
     'AUC',
     'FOK',
     'GTX',
     'DTC');

CREATE TABLE ibkr.contract
(
    symbol text,
    security_type ibkr.security_type,
    expiry date,
    strike numeric,
    "right" ibkr."right",
    exchange text,
    currency text,
    local_symbol text,
    market_name text,
    trading_class text,
    contract_id bigint NOT NULL,
    minimum_tick_increment numeric,
    multiplier numeric,
    price_magnifier integer,
    underlying_contract_id bigint,
    long_name text,
    primary_exchange text,
    contract_month text,
    industry text,
    category text,
    subcategory text,
    time_zone text,
    ev_rule text,
    ev_multiplier text,
    CONSTRAINT contract_pkey PRIMARY KEY (contract_id)
);

CREATE TABLE ibkr.execution
(
    order_id integer NOT NULL,
    contract_id bigint,
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
    model_code text,
    CONSTRAINT execution_execution_id_key UNIQUE (execution_id),
    CONSTRAINT execution_act_symbol_fkey FOREIGN KEY (act_symbol)
        REFERENCES nasdaq.symbol (act_symbol) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE INDEX ON ibkr.execution ("timestamp");

CREATE TABLE ibkr."order"
(
    order_id integer NOT NULL,
    contract_id bigint,
    action ibkr.action,
    total_quantity numeric,
    order_type text,
    limit_price numeric,
    aux_price numeric,
    time_in_force ibkr.time_in_force,
    account text,
    open_close ibkr.open_close,
    order_ref text,
    client_id integer,
    perm_id integer,
    "timestamp" timestamp with time zone,
    CONSTRAINT order_pkey PRIMARY KEY (order_id)
);

CREATE TABLE ibkr.order_condition
(
    order_id integer NOT NULL,
    contract_id bigint,
    type ibkr.condition_type NOT NULL,
    operator ibkr.condition_operator,
    comparator ibkr.condition_comparator,
    value text,
    exchange text,
    trigger_method ibkr.condition_trigger_method,
    CONSTRAINT order_condition_pkey PRIMARY KEY (order_id, type),
    CONSTRAINT order_condition_order_id_fkey FOREIGN KEY (order_id)
        REFERENCES ibkr."order" (order_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE TABLE ibkr.order_leg
(
    order_id integer NOT NULL,
    contract_id bigint NOT NULL,
    ratio integer,
    action ibkr.action,
    exchange text,
    open_close ibkr.open_close,
    short_sale_slot smallint,
    designated_location text,
    exempt_code integer,
    CONSTRAINT order_leg_pkey PRIMARY KEY (order_id, contract_id),
    CONSTRAINT order_leg_order_id_fkey FOREIGN KEY (order_id)
        REFERENCES ibkr."order" (order_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
);

CREATE TABLE ibkr.order_note
(
    order_id integer NOT NULL,
    order_strategy ibkr.order_strategy,
    underlying_entry_price numeric,
    underlying_stop_price numeric,
    underlying_target_price numeric,
    end_date date,
    CONSTRAINT order_note_order_id_key UNIQUE (order_id)
);

CREATE OR REPLACE VIEW ibkr."position"
 AS
 SELECT execution.contract_id,
    contract.symbol,
    contract.security_type,
    contract.expiry,
    contract.strike,
    contract."right",
    execution.account,
    sum(
        CASE execution.side
            WHEN 'BOT'::text THEN execution.shares
            WHEN 'SLD'::text THEN execution.shares * '-1'::integer::numeric
            ELSE NULL::numeric
        END) AS signed_shares,
    min(execution."timestamp") AS entry_timestamp
   FROM ibkr.execution
     JOIN ibkr.contract ON execution.contract_id = contract.contract_id
  GROUP BY execution.contract_id,
    contract.symbol,
    contract.security_type,
    contract.expiry,
    contract.strike,
    contract."right",
    execution.account;
