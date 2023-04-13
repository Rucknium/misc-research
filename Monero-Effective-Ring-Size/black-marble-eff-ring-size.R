# Must install:
# install.packages("data.table")

library(data.table)


black.marble.share <- output.index[, .(
  coinbase.share = 100 * sum(tx_num == 1)/.N, 
  mordinal.share = 100 * sum(is_mordinal)/.N), by = "block_date"]

setorder(black.marble.share, block_date)


# Get number of Mordinals, fees paid, and tx size
output.index[is_mordinal & output_num == 1, .N]

output.index[is_mordinal & output_num == 1, sum(tx_fee) * 0.000000000001]

output.index[is_mordinal & output_num == 1, sum(tx_size_bytes)]



eff.ring.size <- xmr.rings[, .(
  n.coinbase.ring.members = sum(tx_num == 1), n.mordinal.ring.members = sum(is_mordinal), ring.size = .N), 
  by = c("tx_hash", "input_num", "block_timestamp_ring")]

eff.ring.size[, share.mordinal.ring.members := n.mordinal.ring.members/ring.size]
eff.ring.size[, effective.ring.size.coinbase := ring.size - n.coinbase.ring.members]
eff.ring.size[, effective.ring.size.mordinal := ring.size - n.mordinal.ring.members]
eff.ring.size[, effective.ring.size.coinbase.mordinal := ring.size - n.coinbase.ring.members - n.mordinal.ring.members]
eff.ring.size[, block_timestamp_ring.time := as.POSIXct(block_timestamp_ring, origin = "1970-01-01")]

eff.ring.size.date <- unique(eff.ring.size[, .(block_timestamp_ring.time = block_timestamp_ring.time)])

eff.ring.size.date[, block_timestamp_ring.time.isoweek :=
    paste(lubridate::isoyear(block_timestamp_ring.time),
      formatC(lubridate::isoweek(block_timestamp_ring.time), width = 2, flag = "0"), sep = "-")]

eff.ring.size.date[, block_timestamp_ring.time.date := as.Date(block_timestamp_ring.time)]

eff.ring.size <- merge(eff.ring.size, eff.ring.size.date)
# speed improvement by splitting and then merging

setorder(eff.ring.size, block_timestamp_ring.time.date)


eff.ring.size.stats <- eff.ring.size[, .(
  effective.ring.size.coinbase.mean = as.numeric(mean(effective.ring.size.coinbase)),
  effective.ring.size.coinbase.median = as.numeric(median(effective.ring.size.coinbase)),
  effective.ring.size.coinbase.percentile.05 = as.numeric(quantile(effective.ring.size.coinbase, probs = 0.05)),
  effective.ring.size.mordinal.mean = as.numeric(mean(effective.ring.size.mordinal)),
  effective.ring.size.mordinal.median = as.numeric(median(effective.ring.size.mordinal)),
  effective.ring.size.mordinal.percentile.05 = as.numeric(quantile(effective.ring.size.mordinal, probs = 0.05)),
  effective.ring.size.coinbase.mordinal.mean = as.numeric(mean(effective.ring.size.coinbase.mordinal)),
  effective.ring.size.coinbase.mordinal.median = as.numeric(median(effective.ring.size.coinbase.mordinal)),
  effective.ring.size.coinbase.mordinal.percentile.05 = as.numeric(quantile(effective.ring.size.coinbase.mordinal, probs = 0.05))
  # as.numeric() to make sure results of each "by" subset are all floats
), by = "block_timestamp_ring.time.date"]


setorder(eff.ring.size.stats, block_timestamp_ring.time.date)
