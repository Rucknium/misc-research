
# install.packages("rbch")
# install.packages("data.table")
# install.packages("future.apply")
# install.packages("stringr")

library(rbch)
library(data.table)
library(future.apply)
library(stringr)


bitcoin.conf.file <- ""
# Input filepath for your bitcoin.conf file

data.dir <- ""
# Input data directory here, with trailing "/"

bch.config <- rbch::conrpc(bitcoin.conf.file)


con <- DBI::dbConnect(RSQLite::SQLite(), paste0(data.dir, "tx-graph-node-indices.db"))


# first.fusion.height <- 646085

# current.block.height <- rbch::getblockchaininfo(bch.config)@result$blocks

tx.graph.files <- list.files(data.dir)
tx.graph.files <- tx.graph.files[grepl("^tx_graph.+rds$", tx.graph.files)]

first.fusion.height <- min(as.numeric(unlist(stringr::str_extract_all(tx.graph.files, "[0-9]+"))))
current.block.height <- max(as.numeric(unlist(stringr::str_extract_all(tx.graph.files, "[0-9]+"))))


heights.to.process <- first.fusion.height:current.block.height
heights.to.process <- split(heights.to.process, 
  cut(heights.to.process, ceiling(length(heights.to.process)/50)))

future::plan(future::multiprocess())

coinbases <- list()

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
    
    outgoing <- data.table(txid = coinbase.tx$txid, 
      position = seq_along(coinbase.tx$vout), value = value, stringsAsFactors = FALSE)
    
    list(outgoing)
    
  })
  
  
  print(object.size(extracted.txs), units = "Mb")
  
  extracted.txs <- unlist(extracted.txs, recursive = FALSE)
  
  outgoing <- data.table::rbindlist(extracted.txs)
  
  coinbases[[length(coinbases) + 1]] <- outgoing
  
  rm(extracted.txs)
  rm(outgoing)
  
}


coinbases <- data.table::rbindlist(coinbases)

nrow(coinbases)


coinbases$txid_position <- paste0(coinbases$txid, "-", 
  formatC(coinbases$position, width = 4, format = "f", flag = "0", digits = 0))


DBI::dbWriteTable(con, "coinbases", coinbases, overwrite = TRUE)


DBI::dbWriteTable(con, "coinbases_intermediate_1", 
  data.frame(txid_position = character(0), stringsAsFactors = FALSE),
  overwrite = TRUE)


base::date()
DBI::dbExecute(con, "INSERT INTO coinbases_intermediate_1 SELECT 
  txid_position FROM
  coinbases EXCEPT 
  SELECT origin FROM edgelist")
base::date()


unspent.coinbases <- DBI::dbGetQuery(con, 
  'SELECT * FROM coinbases_intermediate_1')

unspent.coinbases <- merge(unspent.coinbases, coinbases)


saveRDS(unspent.coinbases, 
  file = paste0(data.dir, "unspent_coinbases.rds"), 
  compress = FALSE)




