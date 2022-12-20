
script.args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(script.args) == 1 && grepl("(bch)|(ltc)|(doge)", script.args, ignore.case = TRUE))
blockchain.name <- tolower(script.args)

con <- DBI::dbConnect(RSQLite::SQLite(), paste0(blockchain.name, "-mempool-archive.db"))
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

file.time <- Sys.time()

txs <- DBI::dbGetQuery(con, "SELECT * FROM txs")
txs$receive_time_UTC <- as.POSIXct(as.integer(txs$receive_time), origin = "1970-01-01")
write.csv(txs, paste0(blockchain.name, "-mempool-archive-", gsub("( )|([:])", "-", file.time), ".csv"), row.names = FALSE)

blocks <- DBI::dbGetQuery(con, "SELECT * FROM blocks")
blocks$block_receive_time_UTC <- as.POSIXct(as.integer(blocks$block_receive_time), origin = "1970-01-01")
write.csv(blocks, paste0(blockchain.name, "-block-archive-", gsub("( )|([:])", "-", file.time), ".csv"), row.names = FALSE)
