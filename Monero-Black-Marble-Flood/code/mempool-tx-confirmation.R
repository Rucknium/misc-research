



mempool[, confirmation.latency := block_receive_time - receive_time]

mempool[, block_receive_time.hour := as.character(cut(as.POSIXct(block_receive_time), "hour"))]
mempool[nchar(block_receive_time.hour) == 10, block_receive_time.hour := paste0(block_receive_time.hour, " 00:00:00")]
mempool[, block_receive_time.hour := as.POSIXct(block_receive_time.hour)]

mempool.hourly <- mempool[, .(confirmation.latency = mean(confirmation.latency)), by = "block_receive_time.hour"]

png("mean-delay-first-confirmation.png", width = 500, height = 600)

ggplot(mempool.hourly[block_receive_time.hour >= as.POSIXct(start.spam.date - 3),], aes(x = block_receive_time.hour, y = confirmation.latency/60)) +
  geom_line() +
  geom_vline(xintercept = mempool[block_height == start.spam.height, block_receive_time_UTC[1]], linetype = 2) +
  scale_y_continuous(breaks = seq(0, 600, by = 30), limits = c(0, NA), expand = c(0, 0)) +
  scale_x_datetime(breaks = "day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Mean delay to first transaction confirmation") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Mean delay (minutes)")  +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()





hourly.max.confirmation.latency <- lapply(unique(mempool$block_receive_time.hour), FUN = function(hour.bin) {
  
  mempool[, time.to.hour := hour.bin - receive_time]
  leftover.txs <- mempool[time.to.hour > 0 & hour.bin < block_receive_time.hour, ]
  # These are tx that stay in the mempool longer than an hour
  
  if (nrow(leftover.txs) > 0) {
    return(data.table(hour.bin = hour.bin,
      confirmation.latency = leftover.txs[, max(as.numeric(time.to.hour))]))
  } else {
    return(data.table(hour.bin = hour.bin,
      confirmation.latency = mempool[hour.bin == block_receive_time.hour, max(confirmation.latency)]))
  }
})

hourly.max.confirmation.latency <- rbindlist(hourly.max.confirmation.latency)


png("max-delay-first-confirmation.png", width = 500, height = 600)

ggplot(hourly.max.confirmation.latency[hour.bin >= as.POSIXct(start.spam.date - 3),], aes(x = hour.bin, y = confirmation.latency/60^2)) +
  geom_line() +
  geom_vline(xintercept = mempool[block_height == start.spam.height, block_receive_time_UTC[1]], linetype = 2) +
  scale_y_continuous(breaks = seq(0, 24*5, by = 3),limits = c(0, NA), expand = c(0, 0)) +
  scale_x_datetime(breaks = "day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Maximum delay to first transaction confirmation") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Maximum delay (hours)")  +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()


long.wait.txs <- mempool[confirmation.latency >= 3*60^2, ]

long.wait.txs[, summary(fee/weight)]
long.wait.txs <- merge(long.wait.txs, output.index[!duplicated(tx_hash), .(tx_hash, number_of_inputs, number_of_outputs)], by = "tx_hash")

long.wait.txs[, table(number_of_inputs)]
long.wait.txs[, table(number_of_outputs)]

long.wait.txs[number_of_inputs == 1, ]

output.index[number_of_inputs == 1 & number_of_outputs == 2, summary(tx_weight_bytes)]
output.index[number_of_inputs == 1 & number_of_outputs == 2, summary(tx_fee)]


