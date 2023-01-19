library(ggplot2)
library(data.table)
library(Cairo)
library(fixest)

xmr.blockchain <- readRDS("")


block.template.update.adjustment <- xmr.blockchain[,
  mean(diff(sort(unique(canon.receive_time))), na.rm = TRUE)]

p2pool.confirm.delay <- xmr.blockchain[Pool == "P2Pool",
  .(block.template.update =
      min(canon.block_receive_time - canon.receive_time -
          block.template.update.adjustment, na.rm = TRUE) ),
  by = c("block_height")]$block.template.update
# Removes transactions with no canon.receive_time. The nodes did not see
# those transactions before they appeared in a received block
# WARNING: Produces Infs when block has no transactions

mean.p2pool.confirm.delay <- mean(p2pool.confirm.delay[is.finite(p2pool.confirm.delay)])


receive_time.unique <- sort(unique(xmr.blockchain$canon.receive_time))
receive_time.unique <- receive_time.unique[is.finite(receive_time.unique)]
block_receive_time.unique <- sort(unique(xmr.blockchain$canon.block_receive_time))
block_receive_time.unique <- block_receive_time.unique[is.finite(block_receive_time.unique)]

p2pool.sim.confirm <- vector("list", length(block_receive_time.unique) - 1)

for (i in seq_along(p2pool.sim.confirm)) {
  receive_time.confirm = receive_time.unique[
    (receive_time.unique + 1 + mean.p2pool.confirm.delay) %between%
      c(block_receive_time.unique[i] - 1, block_receive_time.unique[i + 1])
  ]
  
  if (length(receive_time.confirm) > 0) {
    p2pool.sim.confirm[[i]] <- data.table(
      p2pool.sim.confirm.time = block_receive_time.unique[i + 1],
      canon.receive_time = receive_time.confirm
    )
  } else {
    p2pool.sim.confirm[[i]] <- data.table(
      p2pool.sim.confirm.time = integer(0),
      canon.receive_time = integer(0)
    )
  }
}

p2pool.sim.confirm <- data.table::rbindlist(p2pool.sim.confirm)

xmr.blockchain <- merge(xmr.blockchain, p2pool.sim.confirm, by = "canon.receive_time", all.x = TRUE)

xmr.blockchain[, mean(canon.block_receive_time - p2pool.sim.confirm.time, na.rm = TRUE)]

block.reward <- unique(xmr.blockchain[, .(block_reward, Pool, is_p2pool)])


block.reward[, mean(block_reward, na.rm = TRUE) / 1e12, by = "Pool"]
block.reward[, mean(block_reward, na.rm = TRUE) / 1e12, by = "is_p2pool"]

block.reward[, (mean(block_reward[is_p2pool], na.rm = TRUE) - 
    mean(block_reward[!is_p2pool], na.rm = TRUE) )/ 1e12]



block.reward[, Pool := relevel(factor(Pool), "other")]

summary(feols(I(block_reward/1e12) ~ Pool, data = block.reward), vcov = "hetero")
# Check statistical significance

block.num.txs <- unique(xmr.blockchain[, .(block_num_txes, Pool, is_p2pool)])


block.num.txs[, mean(block_num_txes, na.rm = TRUE), by = "Pool"]
block.num.txs[, mean(block_num_txes, na.rm = TRUE), by = "is_p2pool"]

block.num.txs[, (mean(block_num_txes[is_p2pool], na.rm = TRUE) - 
    mean(block_num_txes[!is_p2pool], na.rm = TRUE) )]

block.num.txs[, Pool := relevel(factor(Pool), "other")]

summary(feols(block_num_txes ~ Pool, data = block.num.txs), vcov = "hetero")
# Check statistical significance



block.template.update.adjustment <- xmr.blockchain[,
  mean(diff(sort(unique(canon.receive_time))))]
print(block.template.update.adjustment)

xmr.tx.eligiibility <- xmr.blockchain[,
  .(block.template.update =
      min(canon.block_receive_time - canon.receive_time -
          block.template.update.adjustment, na.rm = TRUE) / 60),
  by = c("block_height", "Pool")]
# Removes transactions with no canon.receive_time. The nodes did not see
# those transactions before they appeared in a received block
# WARNING: Produces Infs when block has no transactions

xmr.tx.eligiibility <- xmr.tx.eligiibility[is.finite(block.template.update), ]

xmr.tx.eligiibility[block.template.update < 0, block.template.update := 0]


sum.stats <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
# https://stackoverflow.com/questions/21915286/r-ggplot2-geom-boxplot-with-custom-quantiles

png("Monero-TX-Confirm-Delay/images/pool-comparison-tx-eligibility-boxplot.png", width = 800, height = 400)

ggplot(xmr.tx.eligiibility, aes(y = Pool, x = block.template.update)) +
  stat_summary(fun.data = sum.stats, geom = "boxplot", position = "dodge", width = 0.75,
    fill = "#FF6600") +
  ggtitle("Boxplots of Time Between Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Mining Pool") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  scale_x_continuous(breaks = 0:10, expand = expansion(mult = c(0, 0.05))) +
  expand_limits(x = 0) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())

dev.off()


png("Monero-TX-Confirm-Delay/images/pool-comparison-tx-eligibility-barchart.png", width = 800, height = 400)

ggplot(xmr.tx.eligiibility, aes(y = Pool, x = block.template.update)) +
  ggtitle("Average Time Between Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Mining Pool") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "bar", col = "black", fill = "#FF6600", width = 0.75) +
  scale_x_continuous(breaks = 0:10, expand = expansion(mult = c(0, 0.05))) +
  expand_limits(x = 0) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())

dev.off()






prev.block.time <- unique(xmr.blockchain[, .(block_height, canon.block_receive_time)])
setorder(prev.block.time, block_height)
prev.block.time[, canon.prev.block_receive_time := shift(canon.block_receive_time, type = "lag")]

xmr.blockchain <- merge(xmr.blockchain,
  prev.block.time[, .(block_height, canon.prev.block_receive_time)], by = "block_height")

prev.block.summary <- xmr.blockchain[,
  .(elapsed = (max(canon.receive_time, na.rm = TRUE) - unique(canon.prev.block_receive_time)) / 60),
  by = c("block_height", "Pool")]
# max() will produce -Inf if there are no txs in the block

prev.block.summary[! is.finite(elapsed), elapsed := 0]
# elapsed will be NA if the block contains no transactions. Therefore,
# the amount of time that has elapsed between last block and this block's
# block template is assumed to be zero.


line.frame <- seq(0, 10, by = 0.01)

png("Monero-TX-Confirm-Delay/images/mining-pool-behavior-histogram.png", width = 800, height = 800)

plot.xlim <- c(-1, 8.5)

ggplot(prev.block.summary, aes(elapsed)) +
  geom_line(aes(x = x, y = y), data = data.frame(x = line.frame, y = dexp(line.frame, rate = 0.5))) +
  geom_histogram(aes(y = ..density..), bins = diff(plot.xlim) * 60, fill = "#FF6600FF") +
  ggtitle("Density Histogram of Age of Youngest Transaction in a Mined Block Minus\nAge of Previous Mined Block, by Mining Pool") +
  xlab("                                                                         Minutes                                          github.com/Rucknium") +
  ylab ("Density") +
  facet_wrap(~ Pool, ncol = 3, scales = "free_y") +
  coord_cartesian(xlim = plot.xlim) +
  scale_x_continuous(breaks = -1:10) +
  theme(plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))
  
dev.off()




