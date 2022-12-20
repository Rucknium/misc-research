
if (!require(RSQLite)) { install.packages("RSQLite") }
if (!require(data.table)) { install.packages("data.table") }
if (!require(RJSONIO)) { install.packages("RJSONIO") }
if (!require(RCurl)) { install.packages("RCurl") }

con <- DBI::dbConnect(RSQLite::SQLite(), "xmr-mempool-archive.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

DBI::dbExecute(con, "CREATE TABLE txs (
id_hash TEXT,
fee TEXT,
weight TEXT,
receive_time TEXT,
unique(id_hash)
)")
# unique(id_hash) prevents the same txs being inserted more than once

DBI::dbExecute(con, "CREATE TABLE blocks (
block_hash TEXT,
prev_block_hash TEXT,
block_height TEXT,
block_timestamp TEXT,
block_receive_time TEXT,
unique(block_hash)
)")
# unique(block_hash) prevents the same blocks being inserted more than once

