
script.args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(script.args) == 2 && grepl("(bch)|(ltc)|(doge)", script.args[1], ignore.case = TRUE))

blockchain.name <- tolower(script.args[1])

blockchain.conf.file <- script.args[2]

blockchain.config <- rbch::conrpc(blockchain.conf.file)
rpcport <- readLines(blockchain.conf.file)
rpcport <- rpcport[grepl("rpcport", rpcport) ]
if (length(rpcport) > 0) {
  blockchain.config@url <- paste0("http://127.0.0.1:", gsub("[^0-9]", "", rpcport))
}


tx.pool <- c()

# Check that node is responding
while(length(tx.pool) == 0) {
  tx.pool <- rbch::getrawmempool(blockchain.config)@result
  Sys.sleep(1)
}

con <- DBI::dbConnect(RSQLite::SQLite(), paste0(blockchain.name, "-mempool-archive.db"))
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

while (TRUE) {
  
  compute.time <- system.time({
    
    tx.pool <- rbch::getrawmempool(blockchain.config)@result
    
    bestblockhash <- rbch::getbestblockhash(blockchain.config)@result
    
    block.header <- rbch::getblockheader(blockchain.config, bestblockhash)@result
    
    block_receive_time <- round(Sys.time())
    # One second time resolution
    
    if (length(tx.pool) > 0) {
      
      txs <- vector(mode = "list", length = length(tx.pool))
      
      if (blockchain.name != "ltc") {
        for (i in seq_along(tx.pool)) {
          txs[[i]] <- data.table::data.table(
            id_hash = names(tx.pool)[i],
            fee = tx.pool[[i]]$fee,
            weight = tx.pool[[i]]$size,
            receive_time = tx.pool[[i]]$time)
        }
      } else {
        for (i in seq_along(tx.pool)) {
          txs[[i]] <- data.table::data.table(
            id_hash = names(tx.pool)[i],
            fee = tx.pool[[i]]$fee,
            weight = tx.pool[[i]]$vsize,
            receive_time = tx.pool[[i]]$time)
        }
      }
      
      txs <- data.table::rbindlist(txs)
      
      tx.statement <- DBI::dbSendQuery(con,
        "INSERT OR IGNORE INTO txs VALUES (:id_hash,:fee,:weight,:receive_time)")
      # "IGNORE" prevents the same txs from being inserted more than once
      DBI::dbBind(tx.statement, params = txs)
      DBI::dbClearResult(tx.statement)

      blocks <- data.table::data.table(
        block_hash = block.header$hash,
        prev_block_hash = block.header$previousblockhash,
        block_height = block.header$height,
        block_timestamp = block.header$time,
        block_receive_time = as.character(as.numeric(block_receive_time))
      )
      
      block.statement <- DBI::dbSendQuery(con,
        "INSERT OR IGNORE INTO blocks VALUES (:block_hash,:prev_block_hash,:block_height,:block_timestamp,:block_receive_time)")
      # "IGNORE" prevents the same blocks from being inserted more than once
      DBI::dbBind(block.statement, params = blocks)
      DBI::dbClearResult(block.statement)

    }
  })
  print(compute.time["elapsed"])
  Sys.sleep(max(c(0, 1 - compute.time["elapsed"])))
  # Should poll once per second unless data processing takes more than one second. In
  # that case, polls as frequently as possible.
}



