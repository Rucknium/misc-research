
library(data.table)
library(RSQLite)
library(DBI)
library(scales)
# NOTE: Also need lubridate package installed, but not loading it due to 
# it masking functions

bch.data.dir <- ""
btc.data.dir <- ""
# Input data directory here, with trailing "/"


con.bch <- DBI::dbConnect(RSQLite::SQLite(), paste0(bch.data.dir, "tx-graph-node-indices.db"))
con.btc <- DBI::dbConnect(RSQLite::SQLite(), paste0(btc.data.dir, "tx-graph-node-indices.db"))

pre.fork.edgelist <- DBI::dbGetQuery(con.bch, 
  "SELECT origin_index, destination_index FROM edgelist_intermediate_2 WHERE block_height <= 478558")
# 478558 is last block height that BCH and BTC share a block

pre.fork.utxo.set <- setdiff(pre.fork.edgelist$destination_index, pre.fork.edgelist$origin_index)

DBI::dbWriteTable(con.bch, "pre_fork_utxo_set", 
  data.frame(destination_index = pre.fork.utxo.set, stringsAsFactors = FALSE))

pre.fork.utxo.set.value <- DBI::dbGetQuery(con.bch, 
  'SELECT destination_index, value FROM edgelist_intermediate_2 WHERE destination_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
setDT(pre.fork.utxo.set.value)
pre.fork.bitcoin.supply <- 50 * length(0:209999) + 25 * length(210000:419999) + 12.5 * length(420000:478558)
pre.fork.utxo.set.value[, sum(value)] / pre.fork.bitcoin.supply
# [1] 0.99984
pre.fork.bitcoin.supply - pre.fork.utxo.set.value[, sum(value)]
# [1] 2637.559

pre.fork.utxo.set.value <- pre.fork.utxo.set.value[ ! destination_index %in% c(1740174960, 1740175469), ]
# Removes the transactions that are coinbases of blocks 91722, 91812, 91842, 91880
# Since they are duplicated transaction hashes. See:
# https://bitcoin.stackexchange.com/questions/40444/what-happens-when-two-txids-collide
# https://github.com/bitcoin/bitcoin/commit/ab91bf39b7c11e9c86bb2043c24f0f377f1cf514

excluded.duplicate.tx.hashes.output.count <- 4
excluded.duplicate.tx.hashes.value <- 50 * 4


spent.status <- DBI::dbGetQuery(con.bch, 
  'SELECT origin_index, block_height FROM edgelist_intermediate_2 WHERE origin_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
colnames(spent.status) <- c("destination_index", "bch.spent.block_height")
setDT(spent.status)


spent.status <- merge(pre.fork.utxo.set.value, spent.status, all.x = TRUE)
# rm(pre.fork.utxo.set.value)

# aggregate(spent.status$value, by = list(! is.na(spent.status$bch.spent.block_height)), FUN = sum)
#   Group.1        x
# 1   FALSE  5699742
# 2    TRUE 10779508
# table(spent.status[value > 0, ! is.na(bch.spent.block_height)])
#    FALSE     TRUE 
# 29154762 21307064 


bch.block.times <- readRDS(paste0(bch.data.dir, "block_times.rds"))

bch.block.times[, block_time := as.POSIXct(block_time,  origin = "1970-01-01", tz = "GMT")]
colnames(bch.block.times) <- c("bch.spent.block_height", "block_time")

spent.status <- merge(spent.status, bch.block.times, all = TRUE, by = "bch.spent.block_height")
# Note that due to all = TRUE this will get all blocks, 
# even if there are no target spent outputs within the block

spent.status.by.block <- spent.status[, 
  .(value = sum(value, na.rm= TRUE), n.outputs = length(destination_index[ (! is.na(value)) & value > 0])), 
  by = .(bch.spent.block_height, block_time)]

spent.status.by.block[, block_time.date := lubridate::date(block_time)]

spent.status.by.date <- spent.status.by.block[, 
  .(value = sum(value, na.rm= TRUE), n.outputs = sum(n.outputs, na.rm= TRUE)), 
  by = .(block_time.date)]

unspent <- spent.status.by.date[is.na(block_time.date), value]
cumsum.na.rm <- function(x) {x[is.na(x)] <- 0; cumsum(x)}
spent.status.by.date[, value.cumsum := cumsum.na.rm(value) - unspent]

spent.status.by.date[, perc.value.cumsum := 100 * value.cumsum / sum(value, na.rm = TRUE)]
spent.status.by.date[, unspent.perc.value.cumsum := 100 - perc.value.cumsum]



btc.spent.status <- DBI::dbGetQuery(con.btc, 
  'SELECT origin_index, block_height FROM edgelist_intermediate_2 WHERE origin_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
colnames(btc.spent.status) <- c("destination_index, btc.spent.block_height")
setDT(btc.spent.status)

spent.status <- merge(spent.status, btc.spent.status, all.x = TRUE)









