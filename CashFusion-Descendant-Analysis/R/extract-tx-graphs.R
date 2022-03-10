# install.packages("rbch")
# install.packages("data.table")
# install.packages("future.apply")

library(rbch)
library(data.table)
library(future.apply)


bitcoin.conf.file <- ""
# Input filepath for your bitcoin.conf file

data.dir <- ""
# Input data directory here, with trailing "/"

bch.config <- rbch::conrpc(bitcoin.conf.file)


first.fusion.height <- 646085

current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks

heights.to.process <- first.fusion.height:current.block.height
heights.to.process <- split(heights.to.process, 
  cut(heights.to.process, ceiling(length(heights.to.process)/50)))

future::plan(future::multiprocess())


for (height.set in heights.to.process) {
  
  fused.all.ls <- future.apply::future_lapply(height.set, function(iter.block.height) {
    
    if (iter.block.height %% 1000 == 0) {
      cat(iter.block.height, base::date(), "\n")
    }
    
    block.hash <- rbch::getblockhash(bch.config, iter.block.height)
    block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l2")
    # Argument verbose = 2 gives full transaction data
    # For some reason it doesn't give the fee: 
    # https://docs.bitcoincashnode.org/doc/json-rpc/getrawtransaction.html
    
    raw.txs.ls <- block.data@result$tx
    
    if ( length(raw.txs.ls) < 2) { return(list(incoming = NULL, outgoing = NULL)) }
    
    # Results of this lapply below are returned
    
    lapply(2:length(raw.txs.ls), function(iter) {
      # Start at 2 since the first tx is the coinbase tx
      
      latest.tx <- raw.txs.ls[[iter]]
      
      addresses <- vector("character", length(latest.tx$vout) )
      value <- vector("numeric", length(latest.tx$vout) )
      
      for (j in seq_along(latest.tx$vout)) { 
        extracted.address <- latest.tx$vout[[j]]$scriptPubKey$addresses
        if (length(extracted.address) > 1) {
          extracted.address <- list(paste0(sort(unlist(extracted.address)), collapse = "|"))
          # sort() so that the address order is always the same
        }
        
        stopifnot(length(extracted.address[[1]]) <= 1)
        if (length(extracted.address) == 0) {next}
        
        addresses[j] <- extracted.address[[1]]
        value[j] <- latest.tx$vout[[j]]$value
      }
      
      outgoing <- data.table(txid = latest.tx$txid, address = addresses, 
        position = seq_along(latest.tx$vout), value = value, stringsAsFactors = FALSE)
      
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
        origin.position = origin.position, stringsAsFactors = FALSE)
      
      list(incoming = incoming, outgoing = outgoing)
      
    })
    
  })
  
  
  print(object.size(fused.all.ls), units = "Mb")
  
  fused.all.ls <- unlist(fused.all.ls, recursive = FALSE)
  
  incoming <- data.table::rbindlist(lapply(fused.all.ls, function(x) {
    x[[1]]
  })
  )
  
  outgoing <- data.table::rbindlist(lapply(fused.all.ls, function(x) {
    x[[2]]
  })
  )
  
  rm(fused.all.ls)
  
  saveRDS(list(incoming = incoming, outgoing = outgoing), 
    file = paste0(data.dir, "tx_graph_height_", paste0(range(height.set), collapse = "_to_"), ".rds"), 
    compress = FALSE)
  
  rm(incoming)
  rm(outgoing)
  
}

