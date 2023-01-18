
# MUST install data.table and lubridate packages if not already installed

library(data.table)

# Must run misc-research/Monero-p2pool-Output-Stats/p2pool-output-stats.R
# for blocks 2443120 to 2800470. Then get the file paths of the csv files below

miner.payouts <- read.csv("miner-payouts-2443120-to-2800470.csv", stringsAsFactors = FALSE)
blockchain.stats <- read.csv("blockchain-stats-2443120-to-2800470.csv", stringsAsFactors = FALSE)

setDT(miner.payouts)
setDT(blockchain.stats)

miner.payouts[, date := as.Date(as.POSIXct(as.integer(timestamp), origin = "1970-01-01"))]
p2pool.hashpower <- miner.payouts[, .(p2pool.hashpower.share = mean(is_p2pool)), by =  "date"]


miner.payouts[, week := paste0(lubridate::isoyear(as.Date(as.POSIXct(as.integer(timestamp),
  origin = "1970-01-01"))), "-",
  formatC(lubridate::isoweek(as.Date(as.POSIXct(as.integer(timestamp),
    origin = "1970-01-01"))), width = 2, flag = "0"))]

blockchain.stats[, week := paste0(lubridate::isoyear(as.Date(Date)), "-",
  formatC(lubridate::isoweek(as.Date(Date)), width = 2, flag = "0"))]

miner.payouts.week <- miner.payouts[, .(p2pool.outputs = sum(n_outputs[is_p2pool]),
  p2pool.share.mined.blocks = mean(is_p2pool)), by = "week"]
blockchain.stats.week <- blockchain.stats[, .(OutTotal = sum(OutTotal)), by = "week"]



data.week <- merge(miner.payouts.week, blockchain.stats.week, by = "week")

data.week[, p2pool.output.share := p2pool.outputs / OutTotal]

lubridate::isoweek(as.Date("2023-01-16"))

data.week <- data.week[week != "2022-03"]
# Trim week


png("Monero-p2pool-Output-Stats/analysis/images/p2pool-mined-blocks.png")

par(mar = c(6, 4, 4, 3) + 0.1)

plot(data.week$p2pool.share.mined.blocks * 100, type = "l", xaxt = "n",
  main = "Share of Monero blocks mined by P2Pool, by week",
  sub = "ISO Week", 
  ylab = "Percentage",
  xlab = NA, ylim = c(0, 10))
axis(4)
axis(1, at = seq(4, nrow(data.week), by = 4),
  labels = data.week$week[seq(4, nrow(data.week), by = 4)], las = 2, cex.axis = 0.8)
legend("bottomright", legend = "Hard fork", lty = 1, col = "#FF6600")
legend("topleft", legend = "github.com/Rucknium", bty = "n")

lubridate::isoweek(as.Date("2022-08-15"))
# Hard fork happened Saturday Aug 13. ISO week starts on Mondays

abline(v = which(data.week$week == "2022-33"), col = "#FF6600")

dev.off()


png("Monero-p2pool-Output-Stats/analysis/images/outputs-total.png")

par(mar = c(6, 4, 4, 3) + 0.1)

plot(data.week$OutTotal, type = "l", xaxt = "n", xlab = NA,
  main = "Total number of Monero transaction outputs per week",
  sub = "ISO Week", 
  ylab = "Transaction outputs",
  ylim = c(0, max(data.week$OutTotal)))
axis(4)
axis(1, at = seq(4, nrow(data.week), by = 4),
  labels = data.week$week[seq(4, nrow(data.week), by = 4)], las = 2, cex.axis = 0.8)
abline(v = which(data.week$week == "2022-33"), col = "#FF6600")
legend("bottomright", legend = "Hard fork", lty = 1, col = "#FF6600")
legend("bottomleft", legend = "github.com/Rucknium", bty = "n")

dev.off()


png("Monero-p2pool-Output-Stats/analysis/images/p2pool-outputs-share.png")

par(mar = c(6, 4, 4, 3) + 0.1)

plot(data.week$p2pool.output.share * 100, type = "l", xaxt = "n", xlab = NA,
  main = "Share of Monero transaction outputs produced by P2Pool, by week",
  cex.main = 1.1,
  sub = "ISO Week",
  ylab = "Percentage")
axis(4)
axis(1, at = seq(4, nrow(data.week), by = 4),
  labels = data.week$week[seq(4, nrow(data.week), by = 4)], las = 2, cex.axis = 0.8)
abline( v = which(data.week$week == "2022-33"), col = "#FF6600")
legend("bottomright", legend = "Hard fork", lty = 1, col = "#FF6600")
legend("topleft", legend = "github.com/Rucknium", bty = "n")

dev.off()




ring.size <- ifelse(1:nrow(data.week) < which(data.week$week == "2022-33"), 11, 16)


png("Monero-p2pool-Output-Stats/analysis/images/median-effective-ring-size.png")

par(mar = c(6, 4, 4, 3) + 0.1)

# This is the median for the number of non-p2pool outputs chosen as decoys.
# Then, one is added to represent the real spend. This gives us effective ring size.
plot(1 + qbinom(0.5, ring.size - 1, 1 - data.week$p2pool.output.share), type = "l",
  ylim = c(0, 16), xaxt = "n", xlab = NA,
  main = "Median effective ring size due to P2Pool transaction outputs, by week",
  cex.main = 1.1,
  sub = "ISO Week",
  ylab = "Effective ring size")
axis(4)
axis(1, at = seq(4, nrow(data.week), by = 4),
  labels = data.week$week[seq(4, nrow(data.week), by = 4)], las = 2, cex.axis = 0.8)
abline( v = which(data.week$week == "2022-33"), col = "#FF6600")
legend("bottomright", legend = "Hard fork", lty = 1, col = "#FF6600")
legend("bottomleft", legend = "github.com/Rucknium", bty = "n")

dev.off()


png("Monero-p2pool-Output-Stats/analysis/images/unlucky-5-percent-effective-ring-size.png")

par(mar = c(6, 4, 4, 3) + 0.1)

plot(1 + qbinom(0.05, ring.size - 1, 1 - data.week$p2pool.output.share), type = "l",
  ylim = c(0, 16), xaxt = "n", xlab = NA,
  main = "Effective ring size due to P2Pool transaction outputs\nfor the \"unluckiest\" 5 percent of rings, by week",
  cex.main = 1.1,
  sub = "ISO Week",
  ylab = "Effective ring size")
axis(4)
axis(1, at = seq(4, nrow(data.week), by = 4),
  labels = data.week$week[seq(4, nrow(data.week), by = 4)], las = 2, cex.axis = 0.8)
abline( v = which(data.week$week == "2022-33"), col = "#FF6600")
legend("bottomright", legend = "Hard fork", lty = 1, col = "#FF6600")
legend("bottomleft", legend = "github.com/Rucknium", bty = "n")

dev.off()


