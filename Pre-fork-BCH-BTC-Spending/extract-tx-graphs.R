# install.packages("rbch")
# install.packages("data.table")
# install.packages("future.apply")

library(rbch)
library(data.table)
library(future.apply)

# 478,558 is last block that BCH and BTC have in common


bitcoin.conf.file <- ""
# Input filepath for your bitcoin.conf file

data.dir <- ""
# Input data directory here, with trailing "/"

dir.create(paste0(data.dir, "tx_graphs"))

bch.config <- rbch::conrpc(bitcoin.conf.file)

# current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks
# current.block.height <- 733867
# 733867 is for BCH
# current.block.height <- 729896
# 729896 is for BTC

cut.seq <- seq(20, current.block.height, by = 20)
cut.seq <- c(-1, cut.seq, current.block.height)

heights.to.process <- 0:current.block.height
heights.to.process <- split(heights.to.process, 
  cut(heights.to.process, cut.seq))

future::plan(future::multiprocess())


for (height.set in heights.to.process) {
  
  extracted.txs <- future.apply::future_lapply(height.set, function(iter.block.height) {
    
    if (iter.block.height %% 1000 == 0) {
      cat(iter.block.height, base::date(), "\n")
    }
    
    block.hash <- rbch::getblockhash(bch.config, iter.block.height)
    block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l2")
    # Argument verbose = 2 gives full transaction data
    # For some reason it doesn't give the fee: 
    # https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html
    
    raw.txs.ls <- block.data@result$tx
    
    coinbase.tx <- raw.txs.ls[[1]]
    
    value <- vector("numeric", length(coinbase.tx$vout) )
    
    for (j in seq_along(coinbase.tx$vout)) { 
      value[j] <- coinbase.tx$vout[[j]]$value
    }
    
    outgoing.coinbase <- data.table(txid = coinbase.tx$txid, 
      position = seq_along(coinbase.tx$vout), value = value, block_height = iter.block.height, stringsAsFactors = FALSE)
    
    coinbase.return.value <- list(incoming = 
        data.table(txid = character(0), origin.txid = character(0),
          origin.position = numeric(0), block_height = integer(0), stringsAsFactors = FALSE),
      outgoing = outgoing.coinbase)
    
    if ( length(raw.txs.ls) < 2) {
      return(list(coinbase.return.value))
    }
    # No incoming txs for coinbase-only
    
    # Results of this lapply below are returned
    
    return.value <- lapply(2:length(raw.txs.ls), function(iter) {
      # Start at 2 since the first tx is the coinbase tx
      
      latest.tx <- raw.txs.ls[[iter]]
      
      # addresses <- vector("character", length(latest.tx$vout) )
      value <- vector("numeric", length(latest.tx$vout) )
      
      for (j in seq_along(latest.tx$vout)) { 
        extracted.address <- latest.tx$vout[[j]]$scriptPubKey$addresses
        if (length(extracted.address) > 1) {
          extracted.address <- list(paste0(sort(unlist(extracted.address)), collapse = "|"))
          # sort() so that the address order is always the same
        }
        
        stopifnot(length(extracted.address[[1]]) <= 1)
        if (length(extracted.address) == 0) {next}
        
        # addresses[j] <- extracted.address[[1]]
        value[j] <- latest.tx$vout[[j]]$value
      }
      
      outgoing <- data.table(txid = latest.tx$txid, 
        # address = addresses, 
        position = seq_along(latest.tx$vout), value = value, block_height = iter.block.height, stringsAsFactors = FALSE)
      
      origin.txid <- vector("character", length(latest.tx$vin) )
      origin.position <- vector("numeric", length(latest.tx$vin) )
      
      for (j in seq_along(latest.tx$vin)) { 
        extracted.address <- latest.tx$vin[[j]]$txid
        stopifnot(length(extracted.address) <= 1)
        stopifnot(length(extracted.address[[1]]) <= 1)
        if (length(extracted.address) == 0) {next}
        
        origin.txid[j] <- latest.tx$vin[[j]]$txid
        origin.position[j] <- latest.tx$vin[[j]]$vout + 1
      }
      
      incoming <- data.table(txid = latest.tx$txid, origin.txid = origin.txid,
        origin.position = origin.position, block_height = iter.block.height, stringsAsFactors = FALSE)
      
      list(incoming = incoming, outgoing = outgoing)
      
    })
    
    return.value[[length(return.value) + 1]] <- coinbase.return.value
    # Note that this means that coinbase txs are now "last in the block"
    
    return.value
    
  })
  
  
  print(object.size(extracted.txs), units = "Mb")
  
  extracted.txs <- unlist(extracted.txs, recursive = FALSE)
  
  incoming <- data.table::rbindlist(lapply(extracted.txs, function(x) {
    x[[1]]
  })
  )
  
  outgoing <- data.table::rbindlist(lapply(extracted.txs, function(x) {
    x[[2]]
  })
  )
  
  rm(extracted.txs)
  
  saveRDS(list(incoming = incoming, outgoing = outgoing), 
    file = paste0(data.dir, "tx_graphs/tx_graph_height_", 
      paste0(formatC(range(height.set), width = 6, flag = "0"), collapse = "_to_"), ".rds"), 
    compress = FALSE)
  
  rm(incoming)
  rm(outgoing)
  
}

