
if (!require(RSQLite)) { install.packages("RSQLite") }
if (!require(data.table)) { install.packages("data.table") }
if (!require(RJSONIO)) { install.packages("RJSONIO") }
if (!require(RCurl)) { install.packages("RCurl") }
if (!require(rbch)) { install.packages("rbch") }

script.args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(script.args) == 1 && grepl("(bch)|(ltc)|(doge)", script.args, ignore.case = TRUE))

blockchain.name <- tolower(script.args)

con <- DBI::dbConnect(RSQLite::SQLite(), paste0(blockchain.name, "-mempool-archive.db"))
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

DBI::dbExecute(con, "CREATE TABLE blocks (
block_hash TEXT,
prev_block_hash TEXT,
block_height TEXT,
block_timestamp TEXT,
block_receive_time TEXT,
unique(block_hash)
)")

