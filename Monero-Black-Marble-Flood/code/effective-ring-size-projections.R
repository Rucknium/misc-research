

mean.coinbase.tx.size <- output.index[!duplicated(tx_hash) & tx_num == 1, mean(tx_size_bytes)]

num.blocks.since.spam <- output.index[block_height >= start.spam.height, uniqueN(block_height)]

mean.size.1in.2out <- spam.results[[1]]$non.spam.fingerprint.tx[number_of_inputs == 1 & number_of_outputs == 2, mean(tx_size_bytes)]
mean.size.2in.2out <- spam.results[[1]]$non.spam.fingerprint.tx[number_of_inputs == 2 & number_of_outputs == 2, mean(tx_size_bytes)]
mean.size.16.ring.input <- mean.size.2in.2out - mean.size.1in.2out

mean.size.zero.ring.tx.size <- mean.size.1in.2out - mean.size.16.ring.input
# This "size zero ring" is so that the variable ring size can be added later

mean.size.16.ring.input <- mean.size.16.ring.input - 32
# Subtract key image bytes since there is just one key image per ring
mean.size.one.ring.member <- mean.size.16.ring.input / 16




sim.spam <- lapply(c(11, 16, 25, 40, 60), FUN = function(ring.size.sim) {
  
  non.spam.fingerprint.tx.sim <- copy(spam.results[[1]]$non.spam.fingerprint.tx)
  
  non.spam.fingerprint.tx.sim[, tx_weight_bytes.sim :=
      tx_weight_bytes - mean.size.16.ring.input * number_of_inputs + number_of_inputs * mean.size.one.ring.member * ring.size.sim]
  
  mean.non.spam.kb.per.block <- mean.coinbase.tx.size/1000 +
    (sum(non.spam.fingerprint.tx.sim[block_height >= start.spam.height, tx_weight_bytes.sim])/num.blocks.since.spam)/1000
  
  
  mean.non.spam.output.per.block <- nrow(non.spam.fingerprint[block_height >= start.spam.height, ])/num.blocks.since.spam
  
  mean.effective.ring.size <- ring.size.sim
  simulated.adversary.owned.outputs <- 0
  mean.kb.per.block <- mean.non.spam.kb.per.block
  block.size <- mean.non.spam.kb.per.block
  
  mean.kb.per.block.data <- vector("numeric", 100000)
  mean.effective.ring.size.data <- vector("numeric", 100000)
  median.effective.ring.size.data <- vector("numeric", 100000)
  simulated.adversary.owned.share.data <-  vector("numeric", 100000)
  
  i <- 0
  # while (mean.effective.ring.size >= 2) {
  while (mean.kb.per.block <= 3000) {
    i <- i + 1
    simulated.adversary.owned.share <- simulated.adversary.owned.outputs/(simulated.adversary.owned.outputs + mean.non.spam.output.per.block)
    mean.effective.ring.size <- 1 + (ring.size.sim - 1) * (1 - simulated.adversary.owned.share)
    mean.kb.per.block.data[i] <- mean.kb.per.block
    mean.effective.ring.size.data[i] <- mean.effective.ring.size
    median.effective.ring.size.data[i] <- 1 + qbinom(0.5, size = ring.size.sim - 1, prob = 1 - simulated.adversary.owned.share)
    simulated.adversary.owned.share.data[i] <- simulated.adversary.owned.share
    
    simulated.adversary.owned.outputs <- simulated.adversary.owned.outputs + 2
    mean.kb.per.block <- mean.kb.per.block + mean.size.zero.ring.tx.size / 1000 + mean.size.one.ring.member * ring.size.sim / 1000
    # Add adversary outputs at the end so the first iteration has zero adversary outputs
  }
  
  sim.spam <- data.table(mean.kb.per.block.data = mean.kb.per.block.data[seq_len(i)],
    mean.effective.ring.size.data = mean.effective.ring.size.data[seq_len(i)],
    median.effective.ring.size.data = median.effective.ring.size.data[seq_len(i)],
    simulated.adversary.owned.share.data = simulated.adversary.owned.share.data[seq_len(i)],
    ring.size.sim = ring.size.sim,
    ring.size.sim.label = paste0(ring.size.sim, " (", round(mean.non.spam.kb.per.block), " kB non-spam)"))
  
  sim.spam
})

sim.spam <- rbindlist(sim.spam)



max.ring.size.sim <- max(sim.spam$ring.size.sim)

png("projected-effective-ring-size-non-log.png", width = 600, height = 600)

ggplot(sim.spam, aes(x = mean.kb.per.block.data, y = mean.effective.ring.size.data, colour = ring.size.sim.label)) +
  geom_line() +
  scale_y_continuous(breaks = seq(2, max.ring.size.sim, by = 2), limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 10000, by = 500), limits = c(0, NA), expand = c(0, 0)) +
  ggtitle("Long-term projected mean effective ring size") +
  xlab("                  Block weight in kilobytes (10^3 bytes)     github.com/Rucknium") +
  ylab("Effective ring size")  +
  labs(colour = "Ring size") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()



min.mean.kb.per.block.data <- ceiling(min(sim.spam$mean.kb.per.block.data))

png("projected-effective-ring-size-log-log.png", width = 600, height = 600)

ggplot(sim.spam, aes(x = mean.kb.per.block.data, y = mean.effective.ring.size.data, colour = ring.size.sim.label)) +
  geom_line() +
  scale_y_log10(breaks = c(1, seq(2, max.ring.size.sim, by = 2)), limits = c(1, NA), expand = c(0, 0) ) +
  scale_x_log10(breaks = c(min.mean.kb.per.block.data, seq(0, 10000, by = 250)), guide = guide_axis(angle = 90), expand = c(0, 0)) +
  ggtitle("Long-term projected mean effective ring size (log-log scale)") +
  xlab("       Block weight in kilobytes (10^3 bytes) (log scale)     github.com/Rucknium") +
  ylab("Effective ring size (log scale)")  +
  labs(colour = "Ring size") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()




png("projected-ring-size-one.png", width = 600, height = 600)

ggplot(sim.spam, aes(x = mean.kb.per.block.data,
  y = dbinom(0, size = ring.size.sim - 1, prob = 1 - simulated.adversary.owned.share.data),
  colour = ring.size.sim.label)) +
  geom_line() +
  scale_y_continuous( limits = c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(0, 10000, by = 250), limits = c(0, NA), expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Long-term projected share of rings with effective ring size 1") +
  xlab("       Block weight in kilobytes (10^3 bytes) (log scale)     github.com/Rucknium") +
  ylab("Share of rings")  +
  labs(colour = "Ring size") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()






