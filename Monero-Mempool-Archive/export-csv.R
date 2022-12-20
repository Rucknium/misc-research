
con <- DBI::dbConnect(RSQLite::SQLite(), "xmr-mempool-archive.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

file.time <- Sys.time()

txs <- DBI::dbGetQuery(con, "SELECT * FROM txs")
txs$receive_time_UTC <- as.POSIXct(as.integer(txs$receive_time), origin = "1970-01-01")
write.csv(txs, paste0("xmr-mempool-archive-", gsub("( )|([:])", "-", file.time), ".csv"), row.names = FALSE)

blocks <- DBI::dbGetQuery(con, "SELECT * FROM blocks")
blocks$block_receive_time_UTC <- as.POSIXct(as.integer(blocks$block_receive_time), origin = "1970-01-01")
write.csv(blocks, paste0("xmr-block-archive-", gsub("( )|([:])", "-", file.time), ".csv"), row.names = FALSE)
