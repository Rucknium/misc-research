
library(data.table)
library(RSQLite)
library(DBI)
# NOTE: Also need lubridate package installed, but not loading it due to 
# it masking functions

# WARNING: This code assumes that pre-fork bitcoin has been spent every day since the fork,
# which is true up to March 31, 2022. If this code runs on later data, then will 
# have to pre-fill data.frames with dates

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
DBI::dbWriteTable(con.btc, "pre_fork_utxo_set", 
  data.frame(destination_index = pre.fork.utxo.set, stringsAsFactors = FALSE))
# Need to do this operation for both the BCH and BTC databases

pre.fork.utxo.set.value <- DBI::dbGetQuery(con.bch, 
  'SELECT destination_index, value FROM edgelist_intermediate_2 WHERE destination_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
setDT(pre.fork.utxo.set.value)
pre.fork.bitcoin.supply <- 50 * length(0:209999) + 25 * length(210000:419999) + 12.5 * length(420000:478558)
pre.fork.utxo.set.value[, sum(value)] / pre.fork.bitcoin.supply
# [1] 0.99984
pre.fork.bitcoin.supply - pre.fork.utxo.set.value[, sum(value)]
# [1] 2637.559

duplicated.destination_index <-
  unlist(pre.fork.utxo.set.value[duplicated(destination_index), .(destination_index)])

pre.fork.utxo.set.value <- pre.fork.utxo.set.value[ ! destination_index %in% duplicated.destination_index, ]
# Removes the transactions that are coinbases of blocks 91722, 91812, 91842, 91880
# Since they are duplicated transaction hashes. See:
# https://bitcoin.stackexchange.com/questions/40444/what-happens-when-two-txids-collide
# https://github.com/bitcoin/bitcoin/commit/ab91bf39b7c11e9c86bb2043c24f0f377f1cf514

excluded.duplicate.tx.hashes.output.count <- 4
excluded.duplicate.tx.hashes.value <- 50 * 4


bch.spent.status <- DBI::dbGetQuery(con.bch, 
  'SELECT origin_index, block_height FROM edgelist_intermediate_1 WHERE origin_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
colnames(bch.spent.status) <- c("destination_index", "bch.spent.block_height")
setDT(bch.spent.status)


btc.spent.status <- DBI::dbGetQuery(con.btc, 
  'SELECT origin_index, block_height FROM edgelist_intermediate_1 WHERE origin_index IN (SELECT destination_index FROM pre_fork_utxo_set)')
colnames(btc.spent.status) <- c("destination_index", "btc.spent.block_height")
setDT(btc.spent.status)


spent.status <- merge(pre.fork.utxo.set.value, bch.spent.status, all.x = TRUE)
rm(pre.fork.utxo.set.value, bch.spent.status)
spent.status <- merge(spent.status, btc.spent.status, all.x = TRUE)
rm(btc.spent.status)



bch.block.times <- readRDS(paste0(bch.data.dir, "block_times.rds"))

bch.block.times[, block_time := as.POSIXct(block_time,  origin = "1970-01-01", tz = "GMT")]
colnames(bch.block.times) <- c("bch.spent.block_height", "bch.block_time")

spent.status <- merge(spent.status, bch.block.times, all = TRUE, by = "bch.spent.block_height")
# Note that due to all = TRUE this will get all blocks, 
# even if there are no target spent outputs within the block


btc.block.times <- readRDS(paste0(btc.data.dir, "block_times.rds"))

btc.block.times[, block_time := as.POSIXct(block_time,  origin = "1970-01-01", tz = "GMT")]
colnames(btc.block.times) <- c("btc.spent.block_height", "btc.block_time")

spent.status <- merge(spent.status, btc.block.times, all = TRUE, by = "btc.spent.block_height")


write.csv(spent.status, file = paste0(bch.data.dir, "spent_status-test.csv"), row.names = FALSE)


spent.status[, bch.block_time.date := lubridate::date(bch.block_time)]
spent.status[, btc.block_time.date := lubridate::date(btc.block_time)]





# Column format below is:
# {BTC spent status}{BCH spent status}.to.{BTC spent status}{BCH spent status}
# u = unspent; s = spent

spent.status[, ff.to.tf := as.Date(ifelse(
  ifelse(is.na(btc.block_time.date), Inf, btc.block_time.date) < ifelse(is.na(bch.block_time.date), Inf, bch.block_time.date),
  btc.block_time.date, rep(NA, .N)), origin = "1970-01-01")]

spent.status[, ff.to.ft := as.Date(ifelse(
  ifelse(is.na(bch.block_time.date), Inf, bch.block_time.date) < ifelse(is.na(btc.block_time.date), Inf, btc.block_time.date),
  bch.block_time.date, rep(NA, .N)), origin = "1970-01-01")]

spent.status[, ff.to.tt := as.Date(ifelse(
  ifelse(is.na(btc.block_time.date), Inf, btc.block_time.date) == ifelse(is.na(bch.block_time.date), Inf, bch.block_time.date),
  btc.block_time.date, rep(NA, .N)), origin = "1970-01-01")]

spent.status[, tf.to.tt := as.Date(ifelse(
  (! is.na(ff.to.tf)) & 
    ifelse(is.na(bch.block_time.date), Inf, bch.block_time.date) > ifelse(is.na(btc.block_time.date), Inf, btc.block_time.date),
  bch.block_time.date, rep(NA, .N)), origin = "1970-01-01")]

spent.status[, ft.to.tt := as.Date(ifelse(
  (! is.na(ff.to.ft)) & 
    ifelse(is.na(btc.block_time.date), Inf, btc.block_time.date) > ifelse(is.na(bch.block_time.date), Inf, bch.block_time.date),
  btc.block_time.date, rep(NA, .N)), origin = "1970-01-01")]


ff.to.tf <- spent.status[ (! is.na(ff.to.tf)), 
  .(value.ff.to.tf = sum(value, na.rm = TRUE), outputs.ff.to.tf = .N), by = ff.to.tf]
names(ff.to.tf)[1] <- "block_time.date"

ff.to.ft <- spent.status[ (! is.na(ff.to.ft)), 
  .(value.ff.to.ft = sum(value, na.rm = TRUE), outputs.ff.to.ft = .N), by = ff.to.ft]
names(ff.to.ft)[1] <- "block_time.date"

ff.to.tt <- spent.status[ (! is.na(ff.to.tt)), 
  .(value.ff.to.tt = sum(value, na.rm = TRUE), outputs.ff.to.tt = .N), by = ff.to.tt]
names(ff.to.tt)[1] <- "block_time.date"

tf.to.tt <- spent.status[ (! is.na(tf.to.tt)), 
  .(value.tf.to.tt = sum(value, na.rm = TRUE), outputs.tf.to.tt = .N), by = tf.to.tt]
names(tf.to.tt)[1] <- "block_time.date"

ft.to.tt <- spent.status[ (! is.na(ft.to.tt)), 
  .(value.ft.to.tt = sum(value, na.rm = TRUE), outputs.ft.to.tt = .N), by = ft.to.tt]
names(ft.to.tt)[1] <- "block_time.date"


state.trans.by.day <- 
  data.table(block_time.date = sort(unique(lubridate::date(c(spent.status$bch.block_time, spent.status$btc.block_time)))))

state.trans.by.day <- merge(state.trans.by.day, ff.to.tf, all = TRUE)
state.trans.by.day <- merge(state.trans.by.day, ff.to.ft, all = TRUE)
state.trans.by.day <- merge(state.trans.by.day, ff.to.tt, all = TRUE)
state.trans.by.day <- merge(state.trans.by.day, tf.to.tt, all = TRUE)
state.trans.by.day <- merge(state.trans.by.day, ft.to.tt, all = TRUE)


state.trans.by.day[is.na(state.trans.by.day)] <- 0



spent.status.by.day <-
  data.table(block_time.date = sort(unique(lubridate::date(c(spent.status$bch.block_time, spent.status$btc.block_time)))),
    value.btc.unspent.bch.unspent = NA_real_,
    outputs.btc.unspent.bch.unspent = NA_integer_,
    value.btc.spent.bch.unspent = NA_real_,
    outputs.btc.spent.bch.unspent = NA_integer_,
    value.btc.unspent.bch.spent = NA_real_,
    outputs.btc.unspent.bch.spent = NA_integer_,
    value.btc.spent.bch.spent = NA_real_,
    outputs.btc.spent.bch.spent = NA_integer_)

for (day.i in spent.status.by.day$block_time.date) {
  
  spent.status[, btc.spent := (btc.block_time.date <= day.i) & (! is.na(btc.block_time.date))]
  spent.status[, bch.spent := (bch.block_time.date <= day.i) & (! is.na(bch.block_time.date))]
  
  spent.status.by.day$value.btc.unspent.bch.unspent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (! btc.spent) & (! bch.spent), sum(value, na.rm = TRUE)]
  
  spent.status.by.day$outputs.btc.unspent.bch.unspent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (! btc.spent) & (! bch.spent), .N]
  
  
  spent.status.by.day$value.btc.spent.bch.unspent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (  btc.spent) & (! bch.spent), sum(value, na.rm = TRUE)]
  
  spent.status.by.day$outputs.btc.spent.bch.unspent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (  btc.spent) & (! bch.spent), .N]
  
  
  spent.status.by.day$value.btc.unspent.bch.spent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (! btc.spent) & (  bch.spent), sum(value, na.rm = TRUE)]
  
  spent.status.by.day$outputs.btc.unspent.bch.spent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (! btc.spent) & (  bch.spent), .N]
  
  
  spent.status.by.day$value.btc.spent.bch.spent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (  btc.spent) & (  bch.spent), sum(value, na.rm = TRUE)]
  
  spent.status.by.day$outputs.btc.spent.bch.spent[spent.status.by.day$block_time.date == day.i] <-
    spent.status[ (  btc.spent) & (  bch.spent), .N]
  
  cat(base::date(), which(spent.status.by.day$block_time.date == day.i),
    "of", nrow(spent.status.by.day),"\n")
  
}
# Fairly inefficient implementation, but gets the job done



## Data validity check below

value.row.sum.check <- rowSums(spent.status.by.day[, .(
  value.btc.unspent.bch.unspent, value.btc.spent.bch.unspent,
    value.btc.unspent.bch.spent, value.btc.spent.bch.spent)
])

stopifnot(max(value.row.sum.check) - min(value.row.sum.check) < 0.000001)
# Some small error allowed for floating point arithmetic inaccuracy

outputs.row.sum.check <- rowSums(spent.status.by.day[, .(
  outputs.btc.unspent.bch.unspent, outputs.btc.spent.bch.unspent,
  outputs.btc.unspent.bch.spent, outputs.btc.spent.bch.spent)
])

stopifnot(max(outputs.row.sum.check) - min(outputs.row.sum.check) == 0)


saveRDS(spent.status.by.day, file = paste0(bch.data.dir, "spent_status_by_day.rds"))
saveRDS(state.trans.by.day, file = paste0(bch.data.dir, "state_trans_by_day.rds"))

write.csv(spent.status.by.day, file = paste0(bch.data.dir, "spent_status_by_day.csv"), row.names = FALSE)
write.csv(state.trans.by.day, file = paste0(bch.data.dir, "state_trans_by_day.csv"), row.names = FALSE)





