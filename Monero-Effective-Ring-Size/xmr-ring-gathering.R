# Must install:
# install.packages(c("data.table", "RCurl", "RJSONIO", "future", "future.apply", "parallelly", "lubridate"))

library(data.table)

current.height <- 2847180
# 2847180 is last block of March 21, 2023
# current.height should be the most recent height that you want to collect data for

block.heights <- 1220516:current.height
# 1220516 is hard fork height that allowed the first RingCT transactions
# https://github.com/monero-project/monero#scheduled-softwarenetwork-upgrades

url.rpc <- "http://127.0.0.1:18081"
# Set the IP address and port of your node. Should usually be "http://127.0.0.1:18081"



# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
    url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = FALSE,
  nonce.as.string = FALSE,
  keep.trying.rpc = FALSE,
  curl = RCurl::getCurlHandle(),
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
    ),
    curl = curl
  ), error = function(e) {NULL})
  
  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        ),
        curl = curl
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
  
  RJSONIO::fromJSON(rcp.ret, asText = TRUE) # , simplify = FALSE
}



system.time({
  
  n.workers <- min(floor(parallelly::availableCores()/2), 32L)
  
  future::plan(future::multisession(workers = n.workers))
  options(future.globals.maxSize= 8000*1024^2)
  
  set.seed(314)
  
  # Randomize block heights to make processing time more uniform between parallel processes
  block.heights <- split(block.heights, sample(cut(block.heights, n.workers)))
  # First randomly put heights into list elements (split() will sort them ascendingly in each list element)
  block.heights <- lapply(block.heights, sample)
  # Then order the heights randomly within each list element
  block.heights <- unname(block.heights)
  
  returned <- future.apply::future_lapply(block.heights, function(block.heights) {
    
    handle <- RCurl::getCurlHandle()
    
    return.data <- vector("list", length(block.heights))
    
    for (height.iter in seq_along(block.heights)) {
      
      height <- block.heights[height.iter]
      
      block.data <- xmr.rpc(url.rpc = paste0(url.rpc, "/json_rpc"),
        method = "get_block",
        params = list(height = height ),
        keep.trying.rpc = TRUE,
        curl = handle)$result
      
      txs.to.collect <- c(block.data$miner_tx_hash, block.data$tx_hashes)
      
      rcp.ret <- 	tryCatch(RCurl::postForm(paste0(url.rpc, "/get_transactions"),
        .opts = list(
          postfields = paste0('{"txs_hashes":["', paste0(txs.to.collect, collapse = '","'), '"],"decode_as_json":true}'),
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        ),
        curl = handle
      ), error = function(e) {NULL})
      
      if (length(rcp.ret) == 0) {
        while (length(rcp.ret) == 0) {
          rcp.ret <- tryCatch(RCurl::postForm(paste0(url.rpc, "/get_transactions"),
            .opts = list(
              postfields = paste0('{"txs_hashes":["', paste0(txs.to.collect, collapse = '","'), '"],"decode_as_json":true}'),
              httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
            ),
            curl = handle
          ), error = function(e) {NULL})
        }
      }
      
      rcp.ret <- RJSONIO::fromJSON(rcp.ret, asText = TRUE)
      
      output.index.collected <- vector("list", length(txs.to.collect))
      rings.collected <- vector("list", length(txs.to.collect) - 1)
      
      for (i in seq_along(txs.to.collect)) {
        
        tx.json <- tryCatch(
          RJSONIO::fromJSON(rcp.ret$txs[[i]]$as_json, asText = TRUE),
          error = function(e) {NULL} )
        
        if (is.null(tx.json)) {
          # stop()
          cat(paste0("tx: ", i, " block: ", height, "\n"), file = "~/RingCT-problems.txt", append = TRUE)
          next
        }
        
        output.amounts <- sapply(tx.json$vout, FUN = function(x) {x$amount})
        
        output.index.collected[[i]] <- data.table(
          block_height = height,
          block_timestamp = block.data$block_header$timestamp,
          tx_num = i,
          tx_hash = txs.to.collect[i],
          output_num = seq_along(rcp.ret$txs[[i]]$output_indices),
          output_index = rcp.ret$txs[[i]]$output_indices,
          output_amount = output.amounts,
          output_unlock_time = tx.json$unlock_time)
        
        
        if (i == 1L) { next }
        # Skip first tx since it is the coinbase and has no inputs
        
        tx_hash <- txs.to.collect[i]
        
        rings <- vector("list", length(tx.json$vin))
        
        for (j in seq_along(tx.json$vin)) {
          rings[[j]] <- data.table(
            tx_hash = tx_hash,
            input_num = j,
            input_amount = tx.json$vin[[j]]$key$amount,
            key_offset_num = seq_along(tx.json$vin[[j]]$key$key_offsets),
            key_offsets = tx.json$vin[[j]]$key$key_offsets
          )
        }
        
        rings.collected[[i-1]] <- rbindlist(rings)
        
      }
      
      output.index.collected <- data.table::rbindlist(output.index.collected)
      rings.collected <- rbindlist(rings.collected)
      
      return.data[[height.iter]] <- list(
        output.index.collected = output.index.collected,
        rings.collected = rings.collected)
      
    }
    
    return.data
    
  } )
})


returned.temp <- vector("list", length(returned))

for (i in seq_along(returned)) {
  returned.temp[[i]] <- list(
    output.index.collected = rbindlist(lapply(returned[[i]],
      FUN = function(y) { y$output.index.collected })),
    rings.collected = rbindlist(lapply(returned[[i]],
      FUN = function(y) { y$rings.collected }))
  )
}

returned.temp <- list(
  output.index.collected = rbindlist(lapply(returned.temp,
    FUN = function(y) { y$output.index.collected })),
  rings.collected = rbindlist(lapply(returned.temp,
    FUN = function(y) { y$rings.collected }))
)

output.index <- returned.temp$output.index.collected
returned.temp$output.index.collected <- NULL
rings <- returned.temp$rings.collected
rm(returned.temp)

setorder(rings, tx_hash, input_num, key_offset_num)

rings[, output_index := cumsum(key_offsets), by = c("tx_hash", "input_num")]

rings <- merge(rings, unique(output.index[, .(tx_hash, block_height, block_timestamp)]), by = "tx_hash")

setnames(rings, c("block_height", "block_timestamp"),
  c("block_height_ring", "block_timestamp_ring"))

output.index[, output_amount_for_index := ifelse(tx_num == 1, 0, output_amount)]

xmr.rings <- merge(rings, output.index[, .(block_height, block_timestamp, tx_num, output_num,
  output_index, output_amount, output_amount_for_index, output_unlock_time)],
  # only dont need tx_hash column from output.index
  by.x = c("input_amount", "output_index"),
  by.y = c("output_amount_for_index", "output_index")) #, all = TRUE)




xmr.rings[, num_times_referenced := .N, by = "output_index"]

coinbase.ring.members <- xmr.rings[, .(n.coinbase.ring.members = sum(tx_num == 1), ring.size = .N), 
  by = c("tx_hash", "input_num", "block_timestamp_ring")]

coinbase.ring.members[, share.coinbase.ring.members := n.coinbase.ring.members/ring.size]
coinbase.ring.members[, effective.ring.size := ring.size - n.coinbase.ring.members]
coinbase.ring.members[, block_timestamp_ring.date := as.POSIXct(block_timestamp_ring, origin = "1970-01-01")]

coinbase.ring.members.date <- unique(coinbase.ring.members[, .(block_timestamp_ring.date = block_timestamp_ring.date)])

coinbase.ring.members.date[, block_timestamp_ring.date.isoweek :=
    paste(lubridate::isoyear(block_timestamp_ring.date),
      formatC(lubridate::isoweek(block_timestamp_ring.date), width = 2, flag = "0"), sep = "-")]

coinbase.ring.members <- merge(coinbase.ring.members, coinbase.ring.members.date)
# speed improvement by splitting and then merging

coinbase.ring.members.stats <- coinbase.ring.members[, .(
  effective.ring.size.mean = as.numeric(mean(effective.ring.size)),
  effective.ring.size.median = as.numeric(median(effective.ring.size)),
  effective.ring.size.percentile.05 = as.numeric(quantile(effective.ring.size, probs = 0.05))
  # as.numeric() to make sure results of each "by" subset are all floats
  ), by = "block_timestamp_ring.date.isoweek"]





