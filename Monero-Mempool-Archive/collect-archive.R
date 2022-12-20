
# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
  url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = TRUE,
  nonce.as.string = FALSE,
  keep.trying.rpc = FALSE,
  ...
){
  
  json.ret <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0", 
      id = "0", 
      method = method,
      params = params
    ), digits = 50
  )
  
  rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
    .opts = list(
      userpwd = userpwd,
      postfields = json.ret,
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
    )
  ), error = function(e) {NULL})
  
  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        )
      ), error = function(e) {NULL})
    }
  }
  
  if (is.null(rcp.ret)) {
    stop("Cannot connect to monerod. Is monerod running?")
  }
  
  if (num.as.string) {
    rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }
  
  if (nonce.as.string & ! num.as.string) {
    rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }
  
  RJSONIO::fromJSON(rcp.ret) # , simplify = FALSE
}

tx.pool <- c()

# Check that node is responding
while(length(tx.pool) == 0) {
  tx.pool <- xmr.rpc("http://127.0.0.1:18081/get_transaction_pool")$transactions
  
  if (length(tx.pool) > 0 && tx.pool[[1]]$receive_time == 0) {
    error("Transaction receive_time is missing. Possible solution: remove '--restricted-rpc' monerod flag.")
  }
  Sys.sleep(1)
}

con <- DBI::dbConnect(RSQLite::SQLite(), "xmr-mempool-archive.db")
DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
# export-csv.R can read while collect-archive.R writes
# https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

while (TRUE) {
  
  compute.time <- system.time({
    
    tx.pool <- xmr.rpc("http://127.0.0.1:18081/get_transaction_pool", keep.trying.rpc = TRUE)$transactions
    
    block.header <- xmr.rpc(url.rpc = "http://127.0.0.1:18081/json_rpc", method = "get_last_block_header")$result$block_header
    
    block_receive_time <- round(Sys.time())
    # One second time resolution
    
    if (length(tx.pool) > 0) {
      
      txs <- vector(mode = "list", length = length(tx.pool))
      
      for (i in seq_along(tx.pool)) {
        txs[[i]] <- data.table::data.table(
          id_hash = tx.pool[[i]]$id_hash,
          fee = tx.pool[[i]]$fee,
          weight = tx.pool[[i]]$weight,
          receive_time = tx.pool[[i]]$receive_time)
      }
      
      txs <- data.table::rbindlist(txs)
      
      tx.statement <- DBI::dbSendQuery(con,
        "INSERT OR IGNORE INTO txs VALUES (:id_hash,:fee,:weight,:receive_time)")
      # "IGNORE" prevents the same txs from being inserted more than once
      DBI::dbBind(tx.statement, params = txs)
      DBI::dbClearResult(tx.statement)

      blocks <- data.table::data.table(
        block_hash = block.header$hash,
        prev_block_hash = block.header$prev_hash,
        block_height = block.header$height,
        block_timestamp = block.header$timestamp,
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

