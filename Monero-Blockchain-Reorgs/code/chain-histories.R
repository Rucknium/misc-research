
# Need to install these packages:
# install.packages("RCurl", "RJSONIO", "data.table")


url.rpc <- "http://127.0.0.1:18081"
# Input URL of _unrestricted_ Monero RPC URL and port

block.reorg.depth <- 18

handle <- RCurl::getCurlHandle()

monerod.rpc.req <- function(url.rpc, method, params = "", handle = RCurl::getCurlHandle()) {
  
  json.post <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = method,
      params = params
    )
  )
  
  RJSONIO::fromJSON(
    RCurl::postForm(paste0(url.rpc, "/json_rpc"),
      .opts = list(
        userpwd = "",
        postfields = json.post,
        httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      ),
      curl = handle
    ), asText = TRUE
  )
  
}



get_transactions <- function(url.rpc, txs.to.collect, handle = RCurl::getCurlHandle()) {
  
  txs <- RCurl::postForm(paste0(url.rpc, "/get_transactions"),
    .opts = list(
      postfields = paste0('{"txs_hashes":["', paste0(txs.to.collect, collapse = '","'), '"],"decode_as_json":true}'),
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
    ),
    curl = handle
  )
  
  txs <- RJSONIO::fromJSON(txs, asText = TRUE)$txs
  
  txs <- lapply(txs, FUN = function(x) {
    tx_hash <- unname(x$tx_hash)
    y <- RJSONIO::fromJSON(x$as_json, asText = TRUE)
    y$tx_hash <- tx_hash
    y$output_indices <- unname(x$output_indices)
    y
  })
  
  txs
  
}


alt_chains <- monerod.rpc.req(url.rpc,
  method = "get_alternate_chains", handle = handle)$result$chains

alt_chains.lengths <- sapply(alt_chains, FUN = function(x) {x$length} )

stopifnot(sum(alt_chains.lengths == block.reorg.depth) == 1)
# Must have exactly one alt chain that is length 18

last.common.block_hash <- alt_chains[[ which(alt_chains.lengths == block.reorg.depth) ]]$main_chain_parent_block

last.common.block <- monerod.rpc.req(url.rpc, method = "get_block",
  params = list(hash = last.common.block_hash), handle = handle)$result

last.common.block_height <- last.common.block$block_header$height

reorg.block_hashes <- alt_chains[[ which(alt_chains.lengths == block.reorg.depth) ]]$block_hashes

reorg.block_hashes <- rev(reorg.block_hashes)
# Reverse the order so the first block in the vector is the lowest-height block



mainchain.block_hashes <- monerod.rpc.req(url.rpc, method = "get_block_headers_range",
  params = list(start_height = last.common.block_height + 1,
    end_height = last.common.block_height + length(reorg.block_hashes)) )$result$headers

mainchain.block_hashes <- sapply(mainchain.block_hashes, FUN = function(x) {  x$hash })

last.common.tx_hash <- last.common.block$tx_hashes[ length(last.common.block$tx_hashes) ]

last.common.tx <- get_transactions(url.rpc, last.common.tx_hash)[[1]]

last.common.output.index <- as.integer(max(last.common.tx$output_indices))

# Get:
# 1) Block hash
# 2) Block height (what would have been)
# 3) TXID
# 4) Output public key

get_block_txs <- function(url.rpc, block_hash = NULL, block_height = NULL,
  full.txs = FALSE, include.coinbase = TRUE, handle = RCurl::getCurlHandle()) {
  
  stopifnot(xor(length(block_hash) > 0, length(block_height) > 0))
  
  if (length(block_hash) > 0) {
    block <- monerod.rpc.req(url.rpc, method = "get_block",
      params = list(hash = block_hash), handle = handle)$result
  }
  
  if (length(block_height) > 0) {
    block <- monerod.rpc.req(url.rpc, method = "get_block",
      params = list(height = block_height), handle = handle)$result
  }
  
  tx_hashes <- block$tx_hashes
  
  txs <- get_transactions(url.rpc, tx_hashes, handle = handle)
  
  if (include.coinbase) {
    miner_tx <- RJSONIO::fromJSON(block$json, asText = TRUE)$miner_tx
    miner_tx$tx_hash <- block$miner_tx_hash
    txs <- c(list(miner_tx), txs)
  }
  
  if (full.txs) {
    return(txs)
  }
  
  result <- lapply(txs, FUN = function(x) {
    
    output.public.keys <- sapply(x$vout, FUN = function(x) { x$target$tagged_key[["key"]] } )
    
    data.frame(
      tx_hash = x$tx_hash,
      output_public_key = output.public.keys,
      stringsAsFactors = FALSE)
    
  })
  
  if (length(result) > 0) {
    result <- do.call(rbind, unname(result))
    result$block_hash <- block$block_header$hash
  } else {
    result <- data.frame(tx_hash = character(0), output_public_key = character(0),
      block_hash = character(0))
    # If only tx is the coinbase tx, return a data.fraem with zero rows.
  }
  
  result
  
}

orphaned.blocks <- list()

block_height <- last.common.block_height

for (block_hash in reorg.block_hashes) {
  block_height <- block_height + 1
  orphaned.blocks[[block_hash]] <- get_block_txs(url.rpc,
    block_hash = block_hash, include.coinbase = FALSE, handle = handle)
  if ( nrow(orphaned.blocks[[block_hash]]) > 0) {
    # nrow() would be zero if block only contains the coinbase tx.
    orphaned.blocks[[block_hash]]$block_height <- block_height
  }
}

orphaned.blocks <- do.call(rbind, unname(orphaned.blocks))

orphaned.blocks$global_output_index <- last.common.output.index + seq_len(nrow(orphaned.blocks))

orphaned.blocks <- orphaned.blocks[, c("block_height", "block_hash",
  "tx_hash", "global_output_index", "output_public_key")]



mainchain.blocks <- list()

block_height <- last.common.block_height

for (block_hash in mainchain.block_hashes) {
  block_height <- block_height + 1
  mainchain.blocks[[block_hash]] <- get_block_txs(url.rpc,
    block_hash = block_hash, include.coinbase = FALSE, handle = handle)
  if ( nrow(mainchain.blocks[[block_hash]]) > 0) {
    # nrow() would be zero if block only contains the coinbase tx.
    mainchain.blocks[[block_hash]]$block_height <- block_height
  }
}

mainchain.blocks <- do.call(rbind, mainchain.blocks)
row.names(mainchain.blocks) <- NULL

mainchain.blocks$global_output_index <- last.common.output.index + seq_len(nrow(mainchain.blocks))

mainchain.blocks <- mainchain.blocks[, c("block_height", "block_hash",
  "tx_hash", "global_output_index", "output_public_key")]


