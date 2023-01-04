
sapply(c("data.table", "RJSONIO", "RCurl"),
  FUN = function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE))) {
    tryCatch(suppressWarnings(install.packages(x)),
      error = function(e) {
        suppressWarnings(install.packages(x, lib = Sys.getenv('R_LIBS_USER')))
      }
    )
  }
  return(TRUE)
})
# Attempt to install packages


script.args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(script.args) == 3)

block.start <- as.numeric(script.args[1])
block.stop <- as.numeric(script.args[2])
stopifnot(block.start < block.stop)
blockchain.stats.utility.location <- script.args[3]

# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
  url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = FALSE,
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
  
  RJSONIO::fromJSON(rcp.ret, asText = TRUE) # , simplify = FALSE
}


get.coinbase.tx.size <- function(miner_tx_hash) {
  
  rcp.ret <- RCurl::postForm("http://127.0.0.1:18081/get_transactions",
    .opts = list(
      postfields = paste0('{"txs_hashes":["', miner_tx_hash, '"]}'),
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
    )
  )
  
  rcp.ret <- RJSONIO::fromJSON(rcp.ret, asText = TRUE)
  nchar(rcp.ret$txs[[1]]$pruned_as_hex) / 2
  # Reference:
  # https://libera.monerologs.net/monero-dev/20221231
  # https://github.com/monero-project/monero/pull/8691
  # https://github.com/monero-project/monero/issues/8311
}

cat("\nmonero-blockchain-stats starting to gather aggregate tx output data...\n")

blockchain.stats <- system(paste0(blockchain.stats.utility.location, 
  " --with-outputs --block-start=", block.start, " --block-stop=", block.stop), intern = TRUE)

tf <- tempfile()
writeLines(blockchain.stats[(grep("# DATA", blockchain.stats) + 1):length(blockchain.stats)], tf)
blockchain.stats <- read.delim(tf)
colnames(blockchain.stats) <- gsub("[.]", "_per_", colnames(blockchain.stats))

blockchain.stats$OutTotal <- round(blockchain.stats$Txs_per_Day * blockchain.stats$OutAvg)

cat("monero-blockchain-stats finished gathering aggregate tx output data.\n")
cat("\nStart gathering p2pool tx output data...\n")

blocks <- block.start:block.stop

detect.p2pool <- vector("list", length(blocks))

for (i in seq_along(blocks)) {
  
  block.data <- xmr.rpc(method = "get_block",
    params = list(height = blocks[i]))$result
  
  miner_tx_hash <- block.data$miner_tx_hash
  
  block.data <- RJSONIO::fromJSON(block.data$json, asText = TRUE)
  
  detect.p2pool[[i]] <- data.table::data.table(
    block_height = blocks[i],
    timestamp = block.data$timestamp,
    is_p2pool = grepl("(X3X32X)|(X3X32$)", paste0(block.data$miner_tx$extra, collapse = "X")),
    # tx_extra with "3" followed by "32" indicates merge mining with p2pool
    n_outputs = length(block.data$miner_tx$vout),
    tx_size_bytes = get.coinbase.tx.size(miner_tx_hash)
  )
  if (blocks[i] %% 1000 == 0) {
    cat("Block", blocks[i], "processed\n")
  }
}

detect.p2pool <- data.table::rbindlist(detect.p2pool)

detect.p2pool$is_p2pool[detect.p2pool$n_outputs == 1] <- FALSE
# Remove false positives

write.csv(blockchain.stats, paste0("blockchain-stats-", block.start, "-to-", block.stop, ".csv"), row.names = FALSE)
cat("\n", paste0("blockchain-stats-", block.start, "-to-", block.stop, ".csv"), " created\n", sep = "")
write.csv(detect.p2pool, paste0("miner-payouts-", block.start, "-to-", block.stop, ".csv"), row.names = FALSE)
cat(paste0("miner-payouts-", block.start, "-to-", block.stop, ".csv"), " created\n", sep = "")

cat("\nTotal number of blocks: ", nrow(detect.p2pool), "\n", sep = "")
cat("Number of blocks found by p2pool: ", sum(detect.p2pool$is_p2pool),
  " (", round(100 * sum(detect.p2pool$is_p2pool) / nrow(detect.p2pool), 2), "% of total)", "\n", sep ="")

cat("Summary statistics on number of payout outputs per block found by p2pool:\n")
print(summary(detect.p2pool$n_outputs[detect.p2pool$is_p2pool]))


cat("Total number of transaction outputs: ",
  formatC(round(sum(blockchain.stats$OutTotal)), format = "f", big.mark = ",", digits = 0), "\n", sep = "")

cat("Number of p2pool payout transaction outputs: ",
  formatC(sum(detect.p2pool$n_outputs[detect.p2pool$is_p2pool]), format = "f", big.mark = ",", digits = 0),
  " (", round(100 * sum(detect.p2pool$n_outputs[detect.p2pool$is_p2pool]) / sum(blockchain.stats$OutTotal), 2), "% of total)", "\n", sep ="")

cat("Total number of bytes (i.e. blockchain size on disk) within chosen blocks interval: ",
  formatC(round(sum(blockchain.stats$Bytes_per_Day)), format = "f", big.mark = ",", digits = 0), "\n", sep = "")

cat("Total number of bytes of p2pool coinbase transactions: ",
  formatC(sum(detect.p2pool$tx_size_bytes[detect.p2pool$is_p2pool]), format = "f", big.mark = ",", digits = 0),
  " (", round(100 * sum(detect.p2pool$tx_size_bytes[detect.p2pool$is_p2pool]) / sum(blockchain.stats$Bytes_per_Day), 2), "% of total)", "\n", sep ="")


