
library(data.table)

data.dir <- ""
# Must have trailing "/"

data.begin.time <- as.integer(as.POSIXct("2022-12-21 18:00:00 UTC"))
data.end.time <- as.integer(as.POSIXct("2023-01-18 17:59:59 UTC"))


p2pool <- read.csv("", stringsAsFactors = FALSE)
mining.pool.labels.dir <- ""

datasets <- c("", "", "", "", "")

tx.time.fn <- median
block.time.fn <- median


mining.pool.labels.files <- list.files(mining.pool.labels.dir, full.names = TRUE)

mining.pool.labels <- list()

for (i in mining.pool.labels.files) {
  mining.pool.labels[[i]] <- read.csv(i, stringsAsFactors = FALSE)
  
  orphaned.blocks <- c(
    "e2936a6f13f9d5e98fd70def38a58db4af86488016edf5a34249f1c1b70ef1c7",
    "d64488e574ea237e9f6c803717d2b93271b86f6bc3d374f3922cf317e8ea4fe7"
  )
  
  mining.pool.labels[[i]] <- mining.pool.labels[[i]][
    ! mining.pool.labels[[i]]$Id %in% orphaned.blocks, ]
  # Remove known orphaned blocks
  
  mining.pool.labels[[i]] <- unique(mining.pool.labels[[i]][, c("Height", "Pool")])
  stopifnot( ! any(duplicated(mining.pool.labels[[i]]$Height)))
  
}

mining.pool.labels <- do.call(rbind, mining.pool.labels)
mining.pool.labels <- unique(mining.pool.labels)
rownames(mining.pool.labels) <- NULL

stopifnot( ! any(duplicated(mining.pool.labels$Height)))



blocks.collection <- list()
mempool.collection <- list()

for (i in datasets) {
  xmr.dir <- paste0(data.dir, i, "/xmr/")
  xmr.files <- list.files(xmr.dir)
  xmr.files <- sort(xmr.files, decreasing = TRUE)
  blocks.collection[[i]] <- read.csv(paste0(xmr.dir,
    xmr.files[grepl("block-archive.*csv", xmr.files)][1]), stringsAsFactors = FALSE)
  mempool.collection[[i]] <- read.csv(paste0(xmr.dir,
    xmr.files[grepl("mempool-archive.*csv", xmr.files)][1]), stringsAsFactors = FALSE)
  
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

mempool$canon.receive_time <- apply(mempool[, grepl("receive_time[.]", colnames(mempool)), drop = FALSE], 1,
  function(x) {tx.time.fn(x, na.rm = TRUE)} )

blocks$canon.block_receive_time <- apply(blocks[, grepl("block_receive_time[.]", colnames(blocks)), drop = FALSE], 1,
  function(x) {block.time.fn(x, na.rm = TRUE)} )

mempool$canon.fee <- apply(mempool[, grepl("fee[.]", colnames(mempool)), drop = FALSE], 1,
  function(x) {unique(x[!is.na(x)])} )
# Fee is part of the data hashed for the transaction ID, so there should
# never be more than one unique fee for a given tx ID. Source:
# Section 7.4.1 of Zero to Monero 2.0

mempool$canon.weight <- apply(mempool[, grepl("weight[.]", colnames(mempool)), drop = FALSE], 1,
  function(x) {unique(x[!is.na(x)])} )
# Weight is implicitly part of the data hashed for the transaction ID, so there should
# never be more than one unique weight for a given tx ID. Source:
# https://libera.monerologs.net/monero-dev/20230112#c188158


check.block.heights.duplicated <- apply(blocks[, grepl("block_height[.]", colnames(blocks)), drop = FALSE], 2,
  function(x) {sum(duplicated(x, incomparables = NA))})
# Check if there are "duplicate" heights, i.e. two block hashes with the same height,
# which would suggest blockchain re-orgs
stopifnot(all(check.block.heights.duplicated == 0))

check.block.heights.unique <- apply(blocks[, grepl("block_height[.]", colnames(blocks)), drop = FALSE], 1,
  function(x) {uniqueN(x, na.rm = TRUE)})
# Check if there are any differences in block height between same block hashes,
# which would suggest blockchain re-orgs
stopifnot(all(check.block.heights.unique == 1))

blocks$block_height <- apply(blocks[, grepl("block_height[.]", colnames(blocks)), drop = FALSE], 1,
  function(x) {unique(na.omit(x), incomparables = NA)})

block_height.unique <- na.omit(unique(unlist(blocks[, grepl("block_height[.]", colnames(blocks)), drop = FALSE])))

all.blocks <- min(block_height.unique[block_height.unique > 0]):max(block_height.unique)
# min():max() since some blocks are "skipped"
# Need to have positive since rarely block height is corrupted  in RPC response
# to "0"


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


curl.handle <- RCurl::getCurlHandle()

blockchain.data <- vector("list", length(all.blocks))

for (i in seq_along(all.blocks)) {
  
  block.data <- xmr.rpc(method = "get_block",
    params = list(height = all.blocks[i]), curl = curl.handle)$result
  
  if (length(block.data$tx_hashes) > 0) {
    blockchain.data[[i]] <- data.table::data.table(
      block_height = all.blocks[i],
      id_hash = block.data$tx_hashes,
      block_num_txes = block.data$block_header$num_txes,
      block_reward = block.data$block_header$reward
    )
  } else {
    blockchain.data[[i]] <- data.table::data.table(
      block_height = all.blocks[i],
      id_hash = "<NO_TXS_IN_BLOCK>",
      block_num_txes = block.data$block_header$num_txes,
      block_reward = block.data$block_header$reward
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

# Monero-specific data:
blockchain.data <- merge(blockchain.data, p2pool[, c("block_height", "is_p2pool")], by = "block_height", all.x = TRUE)

colnames(mining.pool.labels)[colnames(mining.pool.labels) == "Height"] <- "block_height"

blockchain.data <- merge(blockchain.data, mining.pool.labels[, c("block_height", "Pool")], by = "block_height", all.x = TRUE)

blockchain.data[is.na(Pool), Pool := "other"]

blockchain.data[(is_p2pool), Pool := "P2Pool"]

max.receive_time.range <- apply(blockchain.data[, 
  grepl("^receive_time[.]", colnames(blockchain.data)), with = FALSE, drop = FALSE], 1, 
  function(x) {diff(range(x))})

cat("xmr max.receive_time.range\n")
cat("Summary stats:\n")
print(summary(max.receive_time.range))
cat("Quantiles:\n")
print(quantile(max.receive_time.range, probs = sort(c(0.05, 0.95, (0:10)/10)), na.rm = TRUE))

max.block.receive_time.range <- apply(blocks[, 
  grepl("^block_receive_time[.]", colnames(blocks)), drop = FALSE], 1, 
  function(x) {diff(range(x))})

cat("xmr max.block.receive_time.range\n")
cat("Summary stats:\n")
print(summary(max.block.receive_time.range))
cat("Quantiles:\n")
print(quantile(max.block.receive_time.range, probs = sort(c(0.05, 0.95, (0:10)/10)), na.rm = TRUE))

saveRDS(blockchain.data, "")



