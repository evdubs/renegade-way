# renegade-way Setup

Renegade Way is a Racket application which is only useful when data has been populated in a database. The following are required:
 * [Racket](https://racket-lang.org) - a "batteries included" Lisp
 * [PostgreSQL](https://postgresql.org) - an open source Relational Database Management System (RDBMS)

If you are looking for a recommendation for a graphical tool to use for PostgreSQL, I have enjoyed using [DBeaver](https://dbeaver.io).

You will likely find that doing the following is a pain and is not worth it when all you want to do is poke around. I understand.

## Schema

Once Racket and PostgreSQL are installed, you will need to set up your database schema. That can be done with the following schema.sql files:
 * [iex-stocks schema](https://github.com/evdubs/iex-stocks/blob/master/schema.sql). These tables will provide end of day Open/High/Low/Close/Volume values for stocks as well as stock split information.
 * [nasdaq-symbols schema](https://github.com/evdubs/nasdaq-symbols/blob/master/schema.sql). These tables will provide all symbols available for trading on US Stock Exchanges.
 * [oic-options-chains schema](https://github.com/evdubs/oic-options-chains/blob/master/schema.sql). These tables will provide option prices, vols, and greeks.
 * [spdr-etf-holdings schema](https://github.com/evdubs/spdr-etf-holdings/blob/master/schema.sql). These tables will provide SPDR ETF components. This is useful for having the compoents for the S&P 500, 400, 600 and sectors and industries.
 * [yahoo-dividends-splits schema](https://github.com/evdubs/yahoo-dividends-splits/blob/master/schema.sql). These tables will provide dividend history. Splits are also available here, but we don't make use of splits from Yahoo.
 * [zacks-estimates-financial-statements schema](https://github.com/evdubs/zacks-estimates-financial-statements/blob/master/schema.sql). These tables provide analyst estimates and sentiment.

Simply pass these `schema.sql` files to `psql` or paste and run the contents in DBeaver. Please ensure you have configured your database in such a way that you can create schemas, tables, types (enums), and functions as that is what these `schema.sql` files will be doing.

## Data

Next, we need to populate our database with data. The above `schema.sql` files are part of repositories that include `extract` and `transform-load` programs that will populate our data. Data is required from:
* [iex-stocks](https://github.com/evdubs/iex-stocks) This extract program requires an account at iexcloud.io
* [nasdaq-symbols](https://github.com/evdubs/nasdaq-symbols)
* [oic-options-chains](https://github.com/evdubs/oic-options-chains)
* [spdr-etf-holdings](https://github.com/evdubs/spdr-etf-holdings)
* [yahoo-dividends-splits](https://github.com/evdubs/yahoo-dividends-splits)
* [zacks-estimates-financial-statements](https://github.com/evdubs/zacks-estimates-financial-statements)

As an example, to load Nasdaq Symbol data, we clone `nasdaq-symbols` locally and run:

```bash
$ cd nasdaq-symbols
$ racket extract.rkt
$ racket transform-load.rkt -u <database username> -p <database password>
```

The `extract.rkt` step will retrieve the Nasdaq Symbol file and the `transform-load.rkt` step will insert the data into our database.

Each of the above ETL programs have their own dependencies that must be installed before you run them. For example, `nasdaq-symbols` requires:

```bash
$ raco pkg install --skip-installed gregor threading
```

Please consult the READMEs for those ETL programs for more information.

## Launching Renegade Way

Now that we have set up our database and populated it with data, we can run Renegade Way. We clone `renegade-way` locally and run:

```bash
$ cd renegade-way
$ racket main.rkt -u <database username> -p <database password>
```

This should launch the application and let you run your analysis. View the [README](https://github.com/evdubs/renegade-way/README.md) for more information.
