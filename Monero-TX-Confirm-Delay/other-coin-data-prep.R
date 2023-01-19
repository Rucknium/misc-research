

data.dir <- ""
# Must have trailing "/"

data.begin.time <- as.integer(as.POSIXct("2022-12-21 18:00:00 UTC"))
data.end.time <- as.integer(as.POSIXct("2023-01-18 17:59:59 UTC"))

datasets <- c("", "")

coin.config <- list(
  ltc = "",
  bch = "",
  doge = "")


tx.time.fn <- min
block.time.fn <- median


for (coin in c("ltc", "bch", "doge")) {
  
  blockchain.conf.file <- coin.config[[coin]]
  
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
  
  
  blocks.collection <- list()
  mempool.collection <- list()
  
  for (i in datasets) {
    coin.dir <- paste0(data.dir, i, "/", coin, "/")
    coin.files <- list.files(coin.dir)
    coin.files <- sort(coin.files, decreasing = TRUE)
    blocks.collection[[i]] <- read.csv(paste0(coin.dir,
      coin.files[grepl("block-archive.*csv", coin.files)][1]), stringsAsFactors = FALSE)
    mempool.collection[[i]] <- read.csv(paste0(coin.dir,
      coin.files[grepl("mempool-archive.*csv", coin.files)][1]), stringsAsFactors = FALSE)
    
    blocks.collection[[i]] <- blocks.collection[[i]][
      blocks.collection[[i]]$block_receive_time %between% c(data.begin.time, data.end.time), ]
    
    blocks.collection[[i]] <- blocks.collection[[i]][ blocks.collection[[i]]$block_height != 0, ]
    
    mempool.collection[[i]] <- mempool.collection[[i]][
      mempool.collection[[i]]$receive_time %between% c(data.begin.time, data.end.time), ]
    
    colnames(blocks.collection[[i]])[colnames(blocks.collection[[i]]) != "block_hash"] <-
      paste0(colnames(blocks.collection[[i]])[colnames(blocks.collection[[i]]) != "block_hash"], ".", i)
    colnames(mempool.collection[[i]])[colnames(mempool.collection[[i]]) != "id_hash"] <-
      paste0(colnames(mempool.collection[[i]])[colnames(mempool.collection[[i]]) != "id_hash"], ".", i)
  }
  
  blocks <- blocks.collection[[ datasets[1] ]]
  mempool <- mempool.collection[[ datasets[1] ]]
  
  for (i in datasets[-1]) {
    blocks <- merge(blocks, blocks.collection[[i]], all = TRUE)
    mempool <- merge(mempool, mempool.collection[[i]], all = TRUE)
  }
  
  mempool$canon.receive_time <- apply(mempool[, grepl("receive_time[.]", colnames(mempool))], 1,
    function(x) {tx.time.fn(x, na.rm = TRUE)} )
  
  blocks$canon.block_receive_time <- apply(blocks[, grepl("block_receive_time[.]", colnames(blocks))], 1,
    function(x) {block.time.fn(x, na.rm = TRUE)} )
  
  mempool$canon.fee <- apply(mempool[, grepl("fee[.]", colnames(mempool))], 1,
    function(x) {unique(x[!is.na(x)])} )
  
  mempool$canon.weight <- apply(mempool[, grepl("weight[.]", colnames(mempool))], 1,
    function(x) {unique(x[!is.na(x)])} )
  
  
  check.block.heights.duplicated <- apply(blocks[, grepl("block_height[.]", colnames(blocks))], 2,
    function(x) {sum(duplicated(x, incomparables = NA))})
  # Check if there are "duplicate" heights, i.e. two block hashes with the same height,
  # which would suggest blockchain re-orgs
  stopifnot(all(check.block.heights.duplicated == 0))
  
  check.block.heights.unique <- apply(blocks[, grepl("block_height[.]", colnames(blocks))], 1,
    function(x) {uniqueN(x, na.rm = TRUE)})
  # Check if there are any differences in block height between same block hashes,
  # which would suggest blockchain re-orgs
  stopifnot(all(check.block.heights.unique == 1))
  
  blocks$block_height <- apply(blocks[, grepl("block_height[.]", colnames(blocks))], 1,
    function(x) {unique(na.omit(x), incomparables = NA)})
  
  block_height.unique <- na.omit(unique(unlist(blocks[, grepl("block_height[.]", colnames(blocks))])))
  
  all.blocks <- min(block_height.unique[block_height.unique > 0]):max(block_height.unique)
  # min():max() since some blocks are "skipped"
  # Need to have positive since rarely block height is corrupted in RPC response
  # to "0"
  
  blockchain.data <- vector("list", length(all.blocks))
  
  
  for (i in seq_along(all.blocks)) {
    
    blockhash <- rbch::getblockhash(blockchain.config, height = all.blocks[i])@result
    
    if (coin == "doge") {
      block.data <- rbch::rpcpost(blockchain.config, "getblock", plist = list(blockhash, TRUE))@result
      tx_hashes <- unlist(block.data$tx)
    } else {
      block.data <- rbch::getblock(blockchain.config, blockhash, verbosity = "l1")@result
      tx_hashes <- unlist(block.data$tx)
    }
    
    block_reward <- rbch::getrawtransaction(blockchain.config, tx_hashes[1],
      verbose = TRUE)@result$vout[[1]]$value 
    
    if (coin == "ltc") {
      tx_hashes <- tx_hashes[(-1) * 1:2]
      # Remove first transaction since it is the coinbase
      # Remove second LTC transaction since it is the MWEB transaction
    } else {
      tx_hashes <- tx_hashes[-1]
    }
    
    
    if (length(tx_hashes) > 1) {
      blockchain.data[[i]] <- data.table::data.table(
        block_height = all.blocks[i],
        id_hash = tx_hashes,
        block_num_txes = length(tx_hashes),
        block_reward = block_reward
      )
    } else {
      blockchain.data[[i]] <- data.table::data.table(
        block_height = all.blocks[i],
        id_hash = "<NO_TXS_IN_BLOCK>",
        block_num_txes = 0L,
        block_reward = block_reward
      )
    }
    if (all.blocks[i] %% 1000 == 0) {
      cat("Block", all.blocks[i], "processed\n")
    }
    
  }
  
  
  
  blockchain.data <- data.table::rbindlist(blockchain.data)
  
  blocks.filled <- merge(data.table(block_height = all.blocks),
    blocks[, c("block_height", "canon.block_receive_time")], all = TRUE)
  
  blocks.filled$canon.block_receive_time <- zoo::na.locf(blocks.filled$canon.block_receive_time, fromLast = TRUE)
  
  blockchain.data <- merge(blocks.filled, blockchain.data)
  
  blockchain.data <- merge(blockchain.data, mempool, by = "id_hash", all = TRUE)
  
  
  receive_time.unique <- na.omit(sort(unique(blockchain.data$canon.receive_time)))
  block_receive_time.unique <- na.omit(sort(unique(blockchain.data$canon.block_receive_time)))
  
  earliest.confirm <- vector("list", length(block_receive_time.unique) - 1)
  
  for (i in seq_along(earliest.confirm)) {
    receive_time.confirm = receive_time.unique[
      (receive_time.unique + 1) %between%
        c(block_receive_time.unique[i] - 1, block_receive_time.unique[i + 1])
    ]
    
    if (length(receive_time.confirm) > 0) {
      earliest.confirm[[i]] <- data.table(
        earliest.possible.confirmation.time = block_receive_time.unique[i + 1],
        canon.receive_time = receive_time.confirm
      )
    } else {
      earliest.confirm[[i]] <- data.table(
        earliest.possible.confirmation.time = integer(0),
        canon.receive_time = integer(0)
      )
    }
  }
  
  earliest.confirm <- data.table::rbindlist(earliest.confirm)
  
  blockchain.data <- merge(blockchain.data, earliest.confirm, by = "canon.receive_time", all = TRUE)
  
  max.receive_time.range <- apply(blockchain.data[, 
    grepl("^receive_time[.]", colnames(blockchain.data)), with = FALSE], 1, 
    function(x) {diff(range(x))})
  
  cat(paste0(coin, " max.receive_time.range\n"))
  cat("Summary stats:\n")
  print(summary(max.range))
  cat("Quantiles:\n")
  print(quantile(max.range, probs = sort(c(0.05, 0.95, (0:10)/10)), na.rm = TRUE))
  
  saveRDS(blockchain.data, paste0(coin, "-blockchain-data.rds"))
  
}




