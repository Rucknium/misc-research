

# install.packages("data.table")
# install.packages("lubridate")

# To get the xmr.rings and output.index objects, run this first:
# https://github.com/Rucknium/misc-research/blob/main/Monero-Effective-Ring-Size/xmr-ring-gathering.R
# Setting initial block height to 1220516 will use over 150GB of RAM.
# A smaller portion of the blockchain can be analyzed, but the full PPV
# calculation cannot be done without all RingCT outputs that may have the
# fee fungibility defect.



xmr.rings[, fee_per_byte := tx_fee / tx_size_bytes]
xmr.rings[, fee_per_byte_nanoneros := floor(fee_per_byte/1000)]

fees <- output.index[, .(
  block_height = block_height[1],
  block_timestamp = block_timestamp[1],
  tx_fee = tx_fee[1],
  tx_size_bytes = tx_size_bytes[1],
  n.outputs = max(output_num)),
  by = tx_hash]

fees[, fee_per_byte := tx_fee/tx_size_bytes]
fees[, fee_per_byte_nanoneros := floor(fee_per_byte/1000)]

fees[, block_timestamp_date := as.Date(as.POSIXct(block_timestamp, origin = "1970-01-01", tz = "UTC"))]

fees[, block_timestamp_isoweek := paste0(lubridate::isoyear(as.POSIXct(block_timestamp, origin = "1970-01-01", tz = "UTC")), "-",
  formatC(lubridate::isoweek(as.POSIXct(block_timestamp, origin = "1970-01-01", tz = "UTC")), width = 2, flag = "0"))]


fees <- fees[is.finite(fee_per_byte), ]
# Removes coinbase transactions

v16.fork.height <- 2689608 # 2022-08-14


fee.clusters.week <- fees[block_height >= v16.fork.height & n.outputs == 2, .(
  total_txs = .N,
  z500_520_fee_per_byte = sum(fee_per_byte_nanoneros %between% c(500, 520)),
  z98_109_fee_per_byte  = sum(fee_per_byte_nanoneros %between% c(98, 109)),
  z29_32_fee_per_byte   = sum(fee_per_byte_nanoneros %between% c(29, 32)),
  z24_34_44_fee         = sum(tx_fee %in% c(240600000, 342450000, 444300000)),
  z317_453_fee          = sum(tx_fee %in% c(31720000000, 45300000000)),
  z500_520_perc_fee_per_byte = 100*sum(fee_per_byte_nanoneros %between% c(500, 520)/.N),
  z98_109_perc_fee_per_byte  = 100*sum(fee_per_byte_nanoneros %between% c(98, 109)/.N),
  z29_32_perc_fee_per_byte   = 100*sum(fee_per_byte_nanoneros %between% c(29, 32)/.N),
  z24_34_44_perc_fee         = 100*sum(tx_fee %in% c(240600000, 342450000, 444300000)/.N),
  z317_453_perc_fee          = 100*sum(tx_fee %in% c(31720000000, 45300000000)/.N)
  ),
  by = "block_timestamp_isoweek"]

names(fee.clusters.week) <- gsub("z", "", names(fee.clusters.week))



fee.clusters.day <- fees[block_height >= v16.fork.height & n.outputs == 2, .(
  total_txs = .N,
  z500_520_fee_per_byte = sum(fee_per_byte_nanoneros %between% c(500, 520)),
  z98_109_fee_per_byte  = sum(fee_per_byte_nanoneros %between% c(98, 109)),
  z29_32_fee_per_byte   = sum(fee_per_byte_nanoneros %between% c(29, 32)),
  z24_34_44_fee         = sum(tx_fee %in% c(240600000, 342450000, 444300000)),
  z317_453_fee          = sum(tx_fee %in% c(31720000000, 45300000000)),
  z500_520_perc_fee_per_byte = 100*sum(fee_per_byte_nanoneros %between% c(500, 520)/.N),
  z98_109_perc_fee_per_byte  = 100*sum(fee_per_byte_nanoneros %between% c(98, 109)/.N),
  z29_32_perc_fee_per_byte   = 100*sum(fee_per_byte_nanoneros %between% c(29, 32)/.N),
  z24_34_44_perc_fee         = 100*sum(tx_fee %in% c(240600000, 342450000, 444300000)/.N),
  z317_453_perc_fee          = 100*sum(tx_fee %in% c(31720000000, 45300000000)/.N)
  ),
  by = "block_timestamp_date"]

names(fee.clusters.day) <- gsub("z", "", names(fee.clusters.day))



fee.freq <- fees[block_height >= v16.fork.height & n.outputs == 2, table(fee_per_byte_nanoneros)]

raw.fee.sort.fee.week <- fees[block_height >= v16.fork.height & n.outputs == 2,
  c(total = sum(.N),
    lapply(names(fee.freq), FUN = function(x) sum(fee_per_byte_nanoneros == as.numeric(x) )) ),
  by = "block_timestamp_isoweek"]

names(raw.fee.sort.fee.week)[-(1:2)] <- paste0(names(fee.freq), "_per_byte")

raw.fee.sort.fee.day <- fees[block_height >= v16.fork.height & n.outputs == 2,
  c(total = sum(.N),
    lapply(names(fee.freq), FUN = function(x) sum(fee_per_byte_nanoneros == as.numeric(x) )) ),
  by = "block_timestamp_date"]

names(raw.fee.sort.fee.day)[-(1:2)] <- paste0(names(fee.freq), "_per_byte")

set.seed(314)

exact.fees <- c(240600000, 342450000, 444300000, 31720000000, 45300000000)

example.tx.hashes <- fees[, c(
  lapply(exact.fees, FUN = function(x) {
    y <- which(tx_fee == as.numeric(x))
    if (length(y) == 1) { return(c(tx_hash[y], rep("", 9))) }
    y <- tx_hash[sample(y, size = min(c(10, length(y))))]
    c(y, rep("", 10 - length(y)))
  }),
  lapply(names(fee.freq), FUN = function(x) {
    y <- which(fee_per_byte_nanoneros == as.numeric(x))
    if (length(y) == 1) { return(c(tx_hash[y], rep("", 9))) }
    y <- tx_hash[sample(y, size = min(c(10, length(y))))]
    c(y, rep("", 10 - length(y)))
  })
  )
]

names(example.tx.hashes) <- c(
  paste0(exact.fees/1000, "_fee_tx_id"),
  paste0(names(fee.freq), "_fee_per_byte_tx_id")
)

example.tx.hashes <- t(example.tx.hashes)


fee.freq <- sort(fee.freq, decreasing = TRUE)

raw.fee.sort.prevalence.week <- fees[block_height >= v16.fork.height & n.outputs == 2,
  c(total = sum(.N),
    lapply(names(fee.freq), FUN = function(x) sum(fee_per_byte_nanoneros == as.numeric(x) )) ),
  by = "block_timestamp_isoweek"]

names(raw.fee.sort.prevalence.week)[-(1:2)] <- paste0(names(fee.freq), "_per_byte")


raw.fee.sort.prevalence.day <- fees[block_height >= v16.fork.height & n.outputs == 2,
  c(total = sum(.N),
    lapply(names(fee.freq), FUN = function(x) sum(fee_per_byte_nanoneros == as.numeric(x) )) ),
  by = "block_timestamp_date"]

names(raw.fee.sort.prevalence.day)[-(1:2)] <- paste0(names(fee.freq), "_per_byte")



write.csv(fee.clusters.week, file = "fee-clusters-by-week.csv", row.names = FALSE)
write.csv(fee.clusters.day, file = "fee-clusters-by-day.csv", row.names = FALSE)

write.csv(raw.fee.sort.fee.week, file = "raw-fee-counts-by-week.csv", row.names = FALSE)
write.csv(raw.fee.sort.fee.day, file = "raw-fee-counts-by-day.csv", row.names = FALSE)

write.csv(raw.fee.sort.prevalence.week, file = "raw-fee-counts-by-week-prevalence-sort.csv", row.names = FALSE)
write.csv(raw.fee.sort.prevalence.day, file = "raw-fee-counts-by-day-prevalence-sort.csv", row.names = FALSE)

write.csv(example.tx.hashes, file = "example-tx-ids-by-fee.csv")




est.PPV <- function(criteria.type = "fee_per_byte", criteria.set,
  block.height.limits = c(0, Inf), fees, ring.size = 16) {

  if (criteria.type == "fee_per_byte") {

    beta.hat <- fees[block_height %between% block.height.limits & n.outputs == 2,
      mean(fee_per_byte_nanoneros %between% criteria.set)]

    tx.hashes.w.defects <- unique(fees[block_height %between% block.height.limits &
        n.outputs == 2 & fee_per_byte_nanoneros %between% criteria.set, tx_hash])

    number.of.defects.per.ring <- xmr.rings[tx_hash %chin% tx.hashes.w.defects,
      .(n.ring.members.w.defect = sum(fee_per_byte_nanoneros %between% criteria.set, na.rm = TRUE)),
      by = c("tx_hash", "input_num")]

  }

  if (criteria.type == "fee") {

    beta.hat <- fees[block_height %between% block.height.limits & n.outputs == 2,
      mean(tx_fee %in% criteria.set)]

    tx.hashes.w.defects <- unique(fees[block_height %between% block.height.limits &
        n.outputs == 2 & tx_fee %in% criteria.set, tx_hash])

    number.of.defects.per.ring <- xmr.rings[tx_hash %chin% tx.hashes.w.defects,
      .(n.ring.members.w.defect = sum(tx_fee %in% criteria.set, na.rm = TRUE)),
      by = c("tx_hash", "input_num")]

  }

  mu_D0.hat <- number.of.defects.per.ring[, mean(n.ring.members.w.defect == 0)]

  mu_C.hat <- 1 - mu_D0.hat/(1-beta.hat)^n

  PPV <- function(n, beta, mu_C) {
    d <- 1:n
    (1/n)*(1-beta)^n*(1-mu_C) +
      sum( (1/d) * dbinom(d-1, n-1, beta) * (mu_C+beta*(1-mu_C)) )
  }
  # Formula for PPV estimator
  # https://github.com/Rucknium/misc-research/tree/main/Monero-Fungibility-Defect-Classifier/pdf

  c(PPV.hat  = PPV(n = ring.size, beta = beta.hat, mu_C = mu_C.hat),
    beta.hat = beta.hat,
    mu_C.hat = mu_C.hat)

}

start.block <- 2941340 # First block of 2023-07-31
end.block <- 2981597 # Last block of 2023-09-24

100 * est.PPV(criteria.type = "fee_per_byte", criteria.set = c(500, 520),
  block.height.limits = c(start.block, end.block), fees, ring.size = 16)

100 * est.PPV(criteria.type = "fee_per_byte", criteria.set = c(98, 109),
  block.height.limits = c(start.block, end.block), fees, ring.size = 16)

100 * est.PPV(criteria.type = "fee_per_byte", criteria.set = c(29, 32),
  block.height.limits = c(start.block, end.block), fees, ring.size = 16)

100 * est.PPV(criteria.type = "fee", criteria.set = c(240600000, 342450000, 444300000),
  block.height.limits = c(start.block, end.block), fees, ring.size = 16)

100 * est.PPV(criteria.type = "fee", criteria.set = c(31720000000, 45300000000),
  block.height.limits = c(start.block, end.block), fees, ring.size = 16)

