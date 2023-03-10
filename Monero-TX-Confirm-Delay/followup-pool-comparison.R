library(ggplot2)
library(data.table)
library(Cairo)

xmr.blockchain <- readRDS("")

block.template.update.adjustment <- xmr.blockchain[,
  mean(diff(sort(unique(canon.receive_time))))]
print(block.template.update.adjustment)


xmr.blockchain[, canon.block_receive_time.week := paste(
    lubridate::isoyear(as.POSIXct(as.integer(canon.block_receive_time), origin = "1970-01-01")),
  formatC(lubridate::isoweek(as.POSIXct(as.integer(canon.block_receive_time), origin = "1970-01-01")), width = 2, flag = "0"), sep = "-")
]


###############################
## Overall confirmation delay
###############################
xmr.blockchain[, confirm.delay := (canon.block_receive_time - canon.receive_time) / 60]

xmr.blockchain[, canon.block_receive_time.day := 
    as.Date(as.POSIXct(as.integer(canon.block_receive_time), origin = "1970-01-01"))
]

xmr.tx.confirm.delay.point <- xmr.blockchain[!is.na(confirm.delay),
  .(Metric = "Daily average",
    confirm.delay = mean(confirm.delay, na.rm = TRUE)),
  by = c("canon.block_receive_time.day")]

xmr.tx.confirm.delay.rolling <- xmr.tx.confirm.delay.point[,
  .(canon.block_receive_time.day = canon.block_receive_time.day[-(1:6)],
    Metric = "Seven-day rolling average",
    confirm.delay = zoo::rollapply(confirm.delay, 7, mean))]

xmr.tx.confirm.delay <- rbind(xmr.tx.confirm.delay.point, xmr.tx.confirm.delay.rolling)

png("Monero-TX-Confirm-Delay/images/tx-delay-all.png", width = 800, height = 800)

ggplot(aes(x = canon.block_receive_time.day, y = confirm.delay, colour = Metric), data = xmr.tx.confirm.delay) +
  geom_line(size = 2) +
  geom_vline(aes(xintercept = as.Date("2023-01-19"), colour = "Research release date"), size = 2) +
  scale_color_manual(name = "", values = c("Seven-day rolling average" = "purple", "Daily average" = "black", "Research release date" = "#FF6600FF")) +
  ggtitle("Average Time Between Monero Transactions Entering the Mempool and\nBeing Confirmed on the Blockchain") +
  xlab("                                                          Date                              github.com/Rucknium") +
  ylab("Minutes") +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.title = element_blank(), legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

dev.off()


######################################
## Eligibility delay by mining pool
######################################

xmr.tx.eligiibility <- xmr.blockchain[,
  .(block.template.update =
      min(canon.block_receive_time - canon.receive_time -
          block.template.update.adjustment, na.rm = TRUE) / 60),
  by = c("block_height", "Pool", "canon.block_receive_time.week")]
# Removes transactions with no canon.receive_time. The nodes did not see
# those transactions before they appeared in a received block
# WARNING: Produces Infs when block has no transactions

xmr.tx.eligiibility <- xmr.tx.eligiibility[is.finite(block.template.update), ]

xmr.tx.eligiibility[block.template.update < 0, block.template.update := 0]

xmr.tx.eligiibility <- xmr.tx.eligiibility[, .(block.template.update = mean(block.template.update)), 
  by = c("Pool", "canon.block_receive_time.week")]


png("Monero-TX-Confirm-Delay/images/tx-eligibility-ISO-week.png", width = 800, height = 800)

ggplot(aes(x = as.factor(canon.block_receive_time.week), y = block.template.update, group = 1), data = xmr.tx.eligiibility) +
  geom_line() +
  geom_vline(aes(xintercept = xmr.tx.eligiibility[, which(unique(as.factor(canon.block_receive_time.week)) == "2023-03")],
    colour = "Research release date")) +
  scale_color_manual(name = "", values = c("Weekly average" = "black", "Research release date" = "#FF6600FF")) +
  ggtitle("Average Time Between Monero Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Mining Pool and Week") +
  xlab("Note: 2miners data is incomplete                               ISO Week                                          github.com/Rucknium") +
  ylab ("Minutes") +
  facet_wrap(~ Pool, ncol = 3) +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

dev.off()




xmr.tx.eligiibility <- xmr.blockchain[,
  .(block.template.update =
      min(canon.block_receive_time - canon.receive_time -
          block.template.update.adjustment, na.rm = TRUE) / 60),
  by = c("block_height", "Pool", "canon.block_receive_time.day")]

xmr.tx.eligiibility <- xmr.tx.eligiibility[is.finite(block.template.update), ]

xmr.tx.eligiibility[block.template.update < 0, block.template.update := 0]

xmr.tx.eligiibility <- xmr.tx.eligiibility[, .(block.template.update = mean(block.template.update)), 
  by = c("Pool", "canon.block_receive_time.day")]

xmr.tx.eligiibility.rolling <- xmr.tx.eligiibility[,
  .(canon.block_receive_time.day = canon.block_receive_time.day[-(1:6)],
    block.template.update = zoo::rollapply(block.template.update, 7, mean)),
  by = "Pool"]

png("Monero-TX-Confirm-Delay/images/tx-eligibility-moving-average.png", width = 800, height = 800)

ggplot(aes(x = canon.block_receive_time.day, y = block.template.update), data = xmr.tx.eligiibility.rolling) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date("2023-01-19"), colour = "Research release date")) +
  scale_color_manual(name = "", values = c("7-day moving average" = "black", "Research release date" = "#FF6600FF")) +
  ggtitle("Average Time Between Monero Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Mining Pool") +
  xlab("Note: 2miners data is incomplete                               Date                                          github.com/Rucknium") +
  ylab ("Minutes") +
  facet_wrap(~ Pool, ncol = 3) +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

dev.off()

######################################
## Block reward revenue by mining pool
######################################


block.reward.comparison <- unique(xmr.blockchain[
  canon.block_receive_time.day >= as.Date("2023-02-23"), .(block_height, block_reward, Pool, is_p2pool)])


block.reward.comparison[, mean(block_reward, na.rm = TRUE) / 1e12, by = "Pool"]
block.reward.comparison[, mean(block_reward, na.rm = TRUE) / 1e12, by = "is_p2pool"]

block.reward.comparison[, (mean(block_reward[is_p2pool], na.rm = TRUE) - 
    mean(block_reward[!is_p2pool], na.rm = TRUE) )/ 1e12]




block.reward <- unique(xmr.blockchain[, .(block_height, block_reward, Pool, is_p2pool, canon.block_receive_time.week)])

block.reward <- block.reward[!is.na(block_reward), ]

block.reward[, mean(block_reward, na.rm = TRUE) / 1e12, by = "Pool"]
block.reward[, mean(block_reward, na.rm = TRUE) / 1e12, by = "is_p2pool"]

block.reward[, (mean(block_reward[is_p2pool], na.rm = TRUE) - 
    mean(block_reward[!is_p2pool], na.rm = TRUE) )/ 1e12]

block.reward.mean <- block.reward[, .(block_reward = mean(block_reward)), 
  by = c("Pool", "canon.block_receive_time.week")]
block.reward.mean[, Metric := "Mean"]

block.reward.median <- block.reward[, .(block_reward = median(block_reward)), 
  by = c("Pool", "canon.block_receive_time.week")]
block.reward.median[, Metric := "Median"]

block.reward <- rbind(block.reward.mean, block.reward.median)

png("Monero-TX-Confirm-Delay/images/mining-reward-ISO-week.png", width = 800, height = 800)

ggplot(aes(x = as.factor(canon.block_receive_time.week), y = block_reward/1e12, colour = Metric, group = Metric), data = block.reward) +
  geom_line() +
  geom_vline(aes(xintercept = xmr.tx.eligiibility[, which(unique(as.factor(canon.block_receive_time.week)) == "2023-03")],
    colour = "Research release date")) +
  scale_color_manual(name = "", values = c(Mean = "purple", Median = "black", "Research release date" = "#FF6600FF")) +
  ggtitle("Total Mining Reward per Monero Block, by Mining Pool") +
  xlab("Note: 2miners data is incomplete                               ISO Week                                          github.com/Rucknium") +
  ylab ("XMR") +
  facet_wrap(~ Pool, ncol = 3) +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  expand_limits(y = 0.6) + # 0.6 is the tail emission
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


dev.off()





block.reward <- unique(xmr.blockchain[, .(block_height, block_reward, Pool, is_p2pool, canon.block_receive_time.day)])

block.reward <- block.reward[!is.na(block_reward), ]

block.reward.mean.daily <- block.reward[, .(block_reward = mean(block_reward)), 
  by = c("Pool", "canon.block_receive_time.day")]

block.reward.median.daily <- block.reward[, .(block_reward = median(block_reward)), 
  by = c("Pool", "canon.block_receive_time.day")]

block.reward.mean.daily <- block.reward.mean.daily[,
  .(canon.block_receive_time.day = canon.block_receive_time.day[-(1:6)],
    Metric = "Mean: 7-day moving average",
    block_reward = zoo::rollapply(block_reward, 7, mean)),
  by = "Pool"]

block.reward.median.daily <- block.reward.median.daily[,
  .(canon.block_receive_time.day = canon.block_receive_time.day[-(1:6)],
    Metric = "Median: 7-day moving average",
    block_reward = zoo::rollapply(block_reward, 7, mean)),
  by = "Pool"]

block.reward <- rbind(block.reward.mean.daily, block.reward.median.daily)

png("Monero-TX-Confirm-Delay/images/mining-reward-moving-average.png", width = 800, height = 800)

ggplot(aes(x = canon.block_receive_time.day, y = block_reward/1e12, colour = Metric, group = Metric), data = block.reward) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date("2023-01-19"), colour = "Research release date")) +
  scale_color_manual(name = "", 
    values = c("Mean: 7-day moving average" = "purple",
      "Median: 7-day moving average" = "black",
      "Research release date" = "#FF6600FF")) +
  ggtitle("Total Mining Reward per Monero Block, by Mining Pool") +
  xlab("Note: 2miners data is incomplete                               Date                                          github.com/Rucknium") +
  ylab ("XMR") +
  facet_wrap(~ Pool, ncol = 3) +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  expand_limits(y = 0.6) + # 0.6 is the tail emission
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

dev.off()








