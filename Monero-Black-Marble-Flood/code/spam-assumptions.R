

start.spam.height <- 3097764 # 2024-03-04 15:21:24
start.spam.date <- as.Date("2024-03-04")


end.spam.height <- 3114046 # 2024-03-27 06:30:37 UTC
end.spam.date <- as.Date("2024-03-27")



library(ggplot2)

output.index[, block_date.week.day := weekdays(block_date)]


spam.types <- list(list(
  fingerprint.text = "1in/2out 20 nanoneros/byte",
  fingerprint.crieria = substitute(
    floor((tx_fee/tx_size_bytes)/1000) %between% c(18, 22) &
      number_of_inputs == 1 &
      number_of_outputs == 2)),
  list(
    fingerprint.text = "1in/2out 20 or 320 nanoneros/byte",
    fingerprint.crieria = substitute(
      floor((tx_fee/tx_size_bytes)/1000) %between% c(315, 325) &
        number_of_inputs == 1 &
        number_of_outputs == 2)))


spam.results <- list()

for (spam.type in seq_along(spam.types)) {
  
  spam.fingerprint.all <- list()
  spam.fingerprint.tx.all <- list()
  
  for (spam.type.sub in 1:spam.type) {
    
    pre.spam.level.week.day <- output.index[
      # block_height < start.spam.height &
      block_date < start.spam.date &
        tx_num != 1 &
        eval(spam.types[[spam.type.sub]]$fingerprint.crieria),
      .(txs.rm.from.spam.set = round(uniqueN(tx_hash)/4)),
      # NOTE: /4 assumes number of pre-spam weeks in data is 4.
      by = "block_date.week.day"]
    
    spam.fingerprint <- output.index[
      block_height %between% c(start.spam.height, end.spam.height) &
        tx_num != 1 &
        eval(spam.types[[spam.type.sub]]$fingerprint.crieria),  ]
    
    spam.fingerprint[, fingerprint := spam.types[[spam.type.sub]]$fingerprint.text]
    
    spam.fingerprint.tx <- spam.fingerprint[!duplicated(tx_hash), ]
    
    spam.fingerprint.tx <- merge(spam.fingerprint.tx,
      pre.spam.level.week.day[, .(block_date.week.day, txs.rm.from.spam.set)], by = "block_date.week.day")
    
    set.seed(314)
    
    
    tx_hash.to.rm <- spam.fingerprint.tx[, .(tx_hash.to.rm = sample(tx_hash,
      min(c(unique(txs.rm.from.spam.set), length(tx_hash))), replace = FALSE)), by = "block_date"]
    spam.fingerprint.tx[, txs.rm.from.spam.set := NULL]
    spam.fingerprint.tx <- spam.fingerprint.tx[ ! tx_hash %chin% tx_hash.to.rm$tx_hash.to.rm, ]
    
    spam.fingerprint.all[[spam.type.sub]] <- spam.fingerprint
    spam.fingerprint.tx.all[[spam.type.sub]] <- spam.fingerprint.tx
    
  }
  
  spam.fingerprint <- rbindlist(spam.fingerprint.all)
  spam.fingerprint.tx <- rbindlist(spam.fingerprint.tx.all)
  
  non.spam.fingerprint <- output.index[ tx_num != 1 &
      (
        (! block_height %between% c(start.spam.height, end.spam.height)) |
          (block_height %between% c(start.spam.height, end.spam.height)  &
              ! (tx_hash %chin% spam.fingerprint.tx$tx_hash))
      ), ]
  
  non.spam.fingerprint.tx <- non.spam.fingerprint[!duplicated(tx_hash), ]
  
  spam.results[[spam.type]] <- list(
    spam.fingerprint = spam.fingerprint, spam.fingerprint.tx = spam.fingerprint.tx,
    non.spam.fingerprint = non.spam.fingerprint, non.spam.fingerprint.tx = non.spam.fingerprint.tx
  )
  
}


print(sum(spam.results[[1]]$spam.fingerprint.tx$tx_fee)/1e+12)
print(sum(spam.results[[1]]$spam.fingerprint.tx$tx_size_bytes) / 1000000000)
sum(spam.results[[1]]$spam.fingerprint.tx$tx_weight_bytes) / 1000000000

print(sum(spam.results[[2]]$spam.fingerprint.tx$tx_fee)/1e+12)
print(sum(spam.results[[2]]$spam.fingerprint.tx$tx_size_bytes) / 1000000000)
sum(spam.results[[2]]$spam.fingerprint.tx$tx_weight_bytes) / 1000000000
# Weight and size should be the same since all suspected spam is 2 outputs



all.tx.volume <- rbind(spam.results[[1]]$spam.fingerprint.tx, spam.results[[1]]$non.spam.fingerprint.tx, fill = TRUE)
all.tx.volume <- all.tx.volume[eval(spam.types[[1]]$fingerprint.crieria), ]
all.tx.volume.by.day <- all.tx.volume[, .(n.all.fingerprint.txs = .N), by = "block_date"]
setorder(all.tx.volume.by.day, block_date)
all.tx.volume.by.day <- all.tx.volume.by.day[-.N, ]
# Remove most recent day because it doesn't have full day of data

png("spam-fingerprint-tx-volume.png", width = 600, height = 600)

ggplot(all.tx.volume.by.day, aes(x = as.POSIXct(block_date), y = n.all.fingerprint.txs / 1000)) +
  geom_line() +
  scale_y_continuous(limit = c(0, NA), expand = c(0, 0)) +
  scale_x_datetime(date_breaks = "3 day", guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Volume of Monero transactions with spam fingerprint",
    subtitle = "1in/2out, 20 nanoneros/byte") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Number of transactions (thousands)")   +
  theme(plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))

dev.off()




all.tx.volume <- rbind(spam.results[[1]]$spam.fingerprint.tx, spam.results[[1]]$non.spam.fingerprint.tx, fill = TRUE)

all.tx.volume[, type.in.out := paste0(number_of_inputs, "in/", number_of_outputs, "out")]

txs.type.in.out <- all.tx.volume[, .(n.type.in.out = .N), by = c("block_date", "type.in.out")]

txs.type.in.out.sum <- txs.type.in.out[, .(sum.n.type.in.out = sum(n.type.in.out)), by = "type.in.out"]

setorder(txs.type.in.out.sum, - sum.n.type.in.out)

most.common.tx.type <- txs.type.in.out.sum$type.in.out[1:8]

txs.type.in.out <- txs.type.in.out[type.in.out %in% most.common.tx.type, ]

txs.type.in.out <- txs.type.in.out[block_date != max(block_date), ]
# Remove most recent date that does not have full day of data

setorder(txs.type.in.out, block_date, n.type.in.out)


png("in-out-tx-type-volume.png", width = 800, height = 800)

ggplot(txs.type.in.out, aes(x = block_date, y = n.type.in.out / 1000,
  colour = factor(type.in.out, levels = rev(unique(type.in.out))))) +
  geom_line(linewidth = 1.25) +
  scale_y_log10() +
  scale_x_date(expand = c(0, 0), date_breaks = "2 day", guide = guide_axis(angle = 90)) +
  ggtitle("Transaction volume by number of inputs and outputs (log scale)") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Thousands of transactions (log scale)")  +
  labs(colour = "Type") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = FALSE, override.aes = list(linewidth = 5))) +
  scale_color_brewer(palette = "Accent")

dev.off()





all.output.volume <- rbind(spam.results[[1]]$spam.fingerprint, spam.results[[1]]$non.spam.fingerprint, fill = TRUE)

all.output.volume.by.day <- all.output.volume[, .(non.spam = sum(is.na(fingerprint)), spam = sum(!is.na(fingerprint))), by = "block_date"]

all.output.volume.by.day[, spam.share.outputs := spam/(non.spam + spam) ]

all.output.volume.by.day <- all.output.volume.by.day[-.N, ]
# Remove most recent day because it doesn't have full day of data

png("spam-share-outputs.png", width = 600, height = 600)

ggplot(all.output.volume.by.day[block_date %between% c(start.spam.date, end.spam.date), ], aes(x = as.POSIXct(block_date), y = spam.share.outputs)) +
  geom_line() +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_datetime(date_breaks = "day", guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Spam share of outputs") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Daily share of outputs owned by suspected spammer")   +
  theme(plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15,  margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))

dev.off()



mean.spam.share.outputs <- all.output.volume.by.day[block_date %between%
    c(start.spam.date + 1, end.spam.date - 1), mean(spam.share.outputs)]
# Skip the first and last days because suspected spam started in the middle of the days

binom.ring.size <- rbind(
  data.table(x = 1:16, y = dbinom(0:15, size = 11, prob = 1 - 192/233),
    Model = paste0("Ring size: 11, Share of adversary outputs: ", round(100*192/233), "% (Chervinski et al. 2021)")),
  data.table(x = 1:16, y = dbinom(0:15, size = 16, prob = 1 - mean.spam.share.outputs),
    Model = paste0("Ring size: 16, Share of adversary outputs: ", round(100*mean.spam.share.outputs), "% (Estimated March 2024)")))

# "prob = 1 - 192/233" because:
# Chervinski et al. (2021)
# "Scenario II analyzes the impact of an attack where the malicious actor creates
# transactions with 2 inputs and 2 outputs, generating 96 transactions and 192
# malicious outputs in each block for a total of 233 outputs per block when
# adding the 41 user generated outputs."

print(binom.ring.size[, .(mean.eff.ring.size = sum(x*y)), by = "Model"])

png("effective-ring-size-binomial-pmf.png", width = 500, height = 600)

ggplot(binom.ring.size, aes(x = factor(x), y = y, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  geom_line(aes(x = factor(x), y = y, group = Model, colour = Model), linewidth = 1.25) +
  scale_y_continuous(labels = scales::label_percent()) +
  ggtitle("Long-term projected effective ring sizes, binomial assumption",
    subtitle = "Probability mass function of binomial(nominal_ring_size, 1 - adversary_outputs_share)") +
  xlab("                                       Effective ring size       github.com/Rucknium") +
  ylab("Share of rings")  +
  labs(colour = "Ring size") +
  theme(legend.position = "top", legend.text = element_text(size = 13), legend.title = element_blank(),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 11.5),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(fill = guide_legend(nrow = 2), colour = waiver())

dev.off()



# Last row of Table IV of
# Chervinski, J. O., Kreutz, D., & Yu, J. 2021, Analysis of transaction flooding attacks against Monero.
# Paper presented at 2021 IEEE International Conference on Blockchain and Cryptocurrency (ICBC).
Chervinski.2021 <- c(
  14.4701,
  30.8318,
  29.5862,
  16.8408,
  6.315,
  1.6359,
  0.2803,
  0.0366,
  0.0031,
  0.0002,
  0
)


Chervinski.ring.size <- rbind(
  data.table(x = 1:16, y = dbinom(0:15, size = 11, prob = 1 - 192/233),
    Model = paste0("Binomial assumption (n = 11, p = ", round(192/233, 2), ")")),
  data.table(x = 1:16, y = c(Chervinski.2021/100, rep(0, 16 - length(Chervinski.2021))),
    Model = "12 month spamming, with chain reaction analysis (Chervinski et al. 2021)"))


Chervinski.ring.size <- Chervinski.ring.size[x <= 11, ]

print(Chervinski.ring.size[, .(mean.eff.ring.size = sum(x*y)), by = "Model"])


png("chervinski-chain-reaction.png", width = 500, height = 600)

ggplot(Chervinski.ring.size, aes(x = factor(x), y = y, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  scale_y_continuous(labels = scales::label_percent()) +
  ggtitle("Long-term effective ring sizes, binomial and chain reaction",
    subtitle = "Probability mass function of binomial(nominal_ring_size, 1 - adversary_outputs_share)") +
  xlab("                                       Effective ring size       github.com/Rucknium") +
  ylab("Share of rings")  +
  labs(colour = "Ring size") +
  theme(legend.position = "top", legend.text = element_text(size = 13), legend.title = element_blank(),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 11.5),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(fill = guide_legend(nrow = 2), colour = waiver())

dev.off()



