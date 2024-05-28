

# Install packages:
# install.packages(c("ggplot2", "data.table", "RJSONIO", "RCurl", "parallelly", "future", "future.apply", "huxtable", "gt", "animation", "Cairo"))
# Cairo is helpful, but not necessary


library(data.table)



current.height <- 3097307
# Last block of March 4, 2024 UTC
# current.height should be the most recent height that you want to collect data for
start.height <- 3077201
# First block of February 5, 2024 UTC

url.rpc <- "http://127.0.0.1:18081"
# Set the IP address and port of your node. Should usually be "http://127.0.0.1:18081"


stopifnot(!is.na(current.height))

block.heights <- start.height:current.height




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
        
        tx_size_bytes <- ifelse(i == 1,
          nchar(rcp.ret$txs[[i]]$pruned_as_hex) / 2,
          nchar(rcp.ret$txs[[i]]$as_hex) / 2)
        # Coinbase has special structure
        # Reference:
        # https://libera.monerologs.net/monero-dev/20221231
        # https://github.com/monero-project/monero/pull/8691
        # https://github.com/monero-project/monero/issues/8311
        
        calc.tx.weight.clawback <- function(p, is.bpp) {
          pow.of.two <- 2^(1:4)
          pow.of.two.index <- findInterval(p, pow.of.two, left.open = TRUE) + 1
          
          n_padded_outputs <- pow.of.two[pow.of.two.index]
          
          if (is.bpp) {
            multiplier <- 6
          } else {
            multiplier <- 9
          }
          
          bp_base <- (32 * (multiplier + 7 * 2)) / 2
          nlr <- ceiling(log2(64 * p))
          bp_size <- 32 * (multiplier + 2 * nlr)
          transaction_clawback <- (bp_base * n_padded_outputs - bp_size) * 4 / 5
          floor(transaction_clawback) # With bpp, this is sometimes (always?) not an integer
        }
        # Equation from page 63 of Zero to Monero 2.0
        # Updated with Bulletproofs+
        # https://github.com/monero-project/monero/blame/c8214782fb2a769c57382a999eaf099691c836e7/src/cryptonote_basic/cryptonote_format_utils.cpp#L106
        
        
        if (length(tx.json$vout) == 2 || i == 1) {
          # i == 1 means the first tx, which is the coinbase tx
          tx_weight_bytes <- tx_size_bytes
        } else {
          tx_weight_bytes <- tx_size_bytes +
            calc.tx.weight.clawback(length(tx.json$vout), length(tx.json$rctsig_prunable$bpp) > 0)
        }
        
        
        tx_fee <- ifelse(i == 1 || is.null(tx.json$rct_signatures), NA, tx.json$rct_signatures$txnFee)
        # missing non-RingCT tx fee
        
        is.mordinal <-
          height >= 2838965 &&
          length(tx.json$vout) == 2 &&
          i > 1 && # not the first tx, which is the coinbase tx
          length(tx.json$extra) > 44 &&
          tx.json$extra[45] == 16
        # With "&&", evaluates each expression sequentially until it is false (if ever). Then stops.
        # If all are TRUE, then returns true.
        
        is.mordinal.transfer <-
          height >= 2838965 &&
          length(tx.json$vout) == 2 &&
          i > 1 && # not the first tx, which is the coinbase tx
          length(tx.json$extra) > 44 &&
          tx.json$extra[45] == 17
        
        output.index.collected[[i]] <- data.table(
          block_height = height,
          block_timestamp = block.data$block_header$timestamp,
          block_size = block.data$block_size,
          block_reward = block.data$reward,
          tx_num = i,
          tx_hash = txs.to.collect[i],
          tx_version = tx.json$version,
          tx_fee = tx_fee,
          tx_size_bytes = tx_size_bytes,
          tx_weight_bytes = tx_weight_bytes,
          number_of_inputs = length(tx.json$vin),
          number_of_outputs = length(tx.json$vout),
          output_num = seq_along(rcp.ret$txs[[i]]$output_indices),
          output_index = rcp.ret$txs[[i]]$output_indices,
          output_amount = output.amounts,
          output_unlock_time = tx.json$unlock_time,
          is_mordinal = is.mordinal,
          is_mordinal_transfer = is.mordinal.transfer)
        
        
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

future::plan(future::sequential)
# Stop thread workers to free RAM

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
rm(returned.temp)

output.index[, output_amount_for_index := ifelse(tx_num == 1, 0, output_amount)]

output.index <- output.index[ !(tx_num == 1 & tx_version == 1), ]
# Remove coinbase outputs that are ineligible for use in a RingCT ring
# See https://libera.monerologs.net/monero-dev/20230323#c224570



output.index.date <- unique(output.index[, .(block_timestamp = block_timestamp)])

output.index.date[, block_date := as.Date(as.POSIXct(block_timestamp, origin = "1970-01-01"))]

output.index <- merge(output.index, output.index.date)
# speed improvement by splitting and then merging

gc()



