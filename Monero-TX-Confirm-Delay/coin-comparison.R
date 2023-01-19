
library(ggplot2)
library(data.table)
library(Cairo)

multi.chain <- list(
  xmr = readRDS(""),
  ltc = readRDS(""),
  bch = readRDS(""),
  doge = readRDS("")
)

for (i in names(multi.chain)) {
  print(i)
  print(summary(multi.chain[[i]][,
    diff(sort(unique(canon.receive_time))) ]))
}

multi.chain.tx.eligiibility <- list()

for (i in c("P2Pool", names(multi.chain))) {
  print(i)
  if (i == "P2Pool") {
    block.template.update.adjustment <- multi.chain[["xmr"]][,
      mean(diff(sort(unique(canon.receive_time))), na.rm = TRUE)]
    print(block.template.update.adjustment)
    
    multi.chain.tx.eligiibility[[i]] <- multi.chain[["xmr"]][ (is_p2pool),
      .(coin = "P2Pool", block.template.update =
          min(canon.block_receive_time - canon.receive_time -
              block.template.update.adjustment, na.rm = TRUE) / 60), by = "block_height"]
    # Removes transactions with no canon.receive_time. The nodes did not see
    # those transactions before they appeared in a received block
    # WARNING: Produces Infs when block has no transactions
  } else {
    block.template.update.adjustment <- multi.chain[[i]][,
      mean(diff(sort(unique(canon.receive_time))), na.rm = TRUE)]
    print(block.template.update.adjustment)
    
    multi.chain.tx.eligiibility[[i]] <- multi.chain[[i]][,
      .(coin = i, block.template.update =
          min(canon.block_receive_time - canon.receive_time -
              block.template.update.adjustment, na.rm = TRUE) / 60), by = "block_height"]
    # Removes transactions with no canon.receive_time. The nodes did not see
    # those transactions before they appeared in a received block
    # WARNING: Produces Infs when block has no transactions
  }
  
}

multi.chain.tx.eligiibility <- data.table::rbindlist(multi.chain.tx.eligiibility)

multi.chain.tx.eligiibility <- multi.chain.tx.eligiibility[is.finite(block.template.update), ]
multi.chain.tx.eligiibility <- multi.chain.tx.eligiibility[block.template.update < 0, block.template.update := 0]

multi.chain.tx.eligiibility[, coin := factor(coin, levels = c("doge", "bch", "ltc", "P2Pool", "xmr"),
  labels = c("Dogecoin", "Bitcoin Cash", "Litecoin", "Monero (P2Pool only)", "Monero (all blocks)"))]


sum.stats <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
# https://stackoverflow.com/questions/21915286/r-ggplot2-geom-boxplot-with-custom-quantiles

png("Monero-TX-Confirm-Delay/images/coin-comparison-tx-eligibility-boxplot.png", width = 800, height = 200)

ggplot(multi.chain.tx.eligiibility, aes(y = coin, x = block.template.update)) +
  stat_summary(fun.data = sum.stats, geom = "boxplot", position = "dodge", width = 0.75,
    fill = c("#d9bd62", "#0AC18E", "#A6A9AA", "#FF6600", "#FF6600")) +
  ggtitle("Boxplots of Time Between Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Coin") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  scale_x_continuous(breaks = 0:10, expand = expansion(mult = c(0, 0.05))) +
  expand_limits(x = 0) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())

dev.off()


# "#0AC18E"
# https://bitcoincash.org/graphics/
# "#A6A9AA"
# https://www.litecoin.net/litecoin-foundation#resources
# "#d9bd62"
# https://www.reddit.com/r/dogecoin/comments/2lpwtv/what_is_the_exact_color_name_we_use_on_doge_the/
# "#FF6600"
# Monero orange


png("Monero-TX-Confirm-Delay/images/coin-comparison-tx-eligibility-barchart.png", width = 800, height = 200)

ggplot(multi.chain.tx.eligiibility, aes(y = coin, x = block.template.update)) +
  ggtitle("Average Time Between Transactions Entering the Mempool and\nBecoming Candidates for Blockchain Confirmation, by Coin") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "bar", col = "black",
    fill = c("#d9bd62", "#0AC18E", "#A6A9AA", "#FF6600", "#FF6600"), width = 0.75) +
  scale_x_continuous(breaks = 0:10, expand = expansion(mult = c(0, 0.05))) +
  expand_limits(x = 0) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())

dev.off()






multi.chain.tx.confirmation <- list()

for (i in names(multi.chain)) {
  print(i)
  multi.chain.tx.confirmation[[i]] <- multi.chain[[i]][,
    .(coin = i, confirm.delay =
        (canon.block_receive_time - canon.receive_time) / 60)]
  print(mean(multi.chain.tx.confirmation[[i]]$confirm.delay, na.rm = TRUE))
}


multi.chain.tx.confirmation <- data.table::rbindlist(multi.chain.tx.confirmation)

multi.chain.tx.confirmation <- multi.chain.tx.confirmation[! is.na(confirm.delay), ]
multi.chain.tx.confirmation <- multi.chain.tx.confirmation[confirm.delay < 0, confirm.delay := 0]

multi.chain.tx.confirmation[, coin := factor(coin, levels = c("doge", "bch", "ltc", "xmr"),
  labels = c("Dogecoin", "Bitcoin Cash", "Litecoin", "Monero"))]

multi.chain.tx.confirmation[, as.list(summary(confirm.delay * 60)), by = "coin"]
# Summary stats of delay of confirmation time for each coin


sum.stats <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
# https://stackoverflow.com/questions/21915286/r-ggplot2-geom-boxplot-with-custom-quantiles

png("Monero-TX-Confirm-Delay/images/coin-comparison-tx-confirmation-boxplot.png", width = 800, height = 200)

ggplot(multi.chain.tx.confirmation, aes(y = coin, x = confirm.delay)) +
  stat_summary(fun.data = sum.stats, geom = "boxplot", position = "dodge", width = 0.75,
    fill = c("#d9bd62", "#0AC18E", "#A6A9AA", "#FF6600")) +
  ggtitle("Boxplots of Time Between Transactions Entering the Mempool and\nBeing Confirmed on the Blockchain") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  scale_x_continuous(breaks = 0:50, expand = expansion(mult = c(0, 0.05))) +
  expand_limits(x = 0) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())  

dev.off()



png("Monero-TX-Confirm-Delay/images/coin-comparison-tx-confirmation-barchart.png", width = 800, height = 200)

ggplot(multi.chain.tx.confirmation,
  aes(y = coin, x = confirm.delay)) +
  ggtitle("Average Time Between Transactions Entering the Mempool and\nBeing Confirmed on the Blockchain") +
  xlab("                                                          Minutes                              github.com/Rucknium") +
  stat_summary(fun = mean, geom = "bar", col = "black",
    fill = c("#d9bd62", "#0AC18E", "#A6A9AA", "#FF6600"), width = 0.75) +
  scale_x_continuous(breaks = 0:50, expand = expansion(mult = c(0, 0.05))) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(size = 20),
    axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
    axis.title.y = element_blank())  

dev.off()


