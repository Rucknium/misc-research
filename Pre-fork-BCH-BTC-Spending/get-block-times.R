library(data.table)

bitcoin.conf.file <- ""
# Input filepath for your bitcoin.conf file

data.dir <- ""
# Input data directory here, with trailing "/"

bch.config <- rbch::conrpc(bitcoin.conf.file)

initial.fork.height <- 478558 - 1

# current.block.height <- 733867
# 733867 is for BCH
 current.block.height <- 729896
# 729896 is for BTC

block.times <- vector(length(initial.fork.height:current.block.height), mode ="list")

for (iter.block.height in initial.fork.height:current.block.height) {
  
  if (iter.block.height %% 1000 == 0) {
    cat(iter.block.height, base::date(), "\n")
  }
  
  block.hash <- rbch::getblockhash(bch.config, iter.block.height)
  block.data <- rbch::getblock(bch.config, blockhash = block.hash@result, verbosity = "l1")
  block.times[[iter.block.height - initial.fork.height + 1]] <- 
    data.frame(block_height = iter.block.height, block_time = block.data@result$time)
}

block.times <- data.table::rbindlist(block.times)

saveRDS(block.times, file = paste0(data.dir, "block_times.rds"))



