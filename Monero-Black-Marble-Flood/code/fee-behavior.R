



all.tx.volume <- rbind(spam.results[[1]]$spam.fingerprint.tx, spam.results[[1]]$non.spam.fingerprint.tx, fill = TRUE)

all.tx.volume[, fee_per_byte_nanoneros := floor((tx_fee/tx_size_bytes)/1000)]

all.tx.volume.fees <- all.tx.volume[number_of_outputs == 2 & (
  fee_per_byte_nanoneros %between% c(18, 22) |
  fee_per_byte_nanoneros %between% c(78, 82) |
  fee_per_byte_nanoneros %between% c(315, 325) |
  fee_per_byte_nanoneros %between% c(3000, 4100)
), ]

all.tx.volume.fees[, fee_per_byte_nanoneros.cut := cut(fee_per_byte_nanoneros,
  breaks = c(0, 22, 82, 325, 4100), labels = c("20", "80", "320", "4000"))]


all.tx.volume.fees <- all.tx.volume.fees[, as.data.frame(prop.table(table(block_date, fee_per_byte_nanoneros.cut), margin = 1))]
all.tx.volume.fees$block_date <- as.Date(as.character(all.tx.volume.fees$block_date))


png("share-tx-in-fee-tier-all-txs.png", width = 500, height = 600)

ggplot(all.tx.volume.fees, aes(x = block_date,
  y = Freq,
  colour = factor(fee_per_byte_nanoneros.cut))) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = start.spam.date, linetype = 2) +
  scale_y_continuous( limits = c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_date(breaks = "3 day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Share of transactions by fee tier (all transactions)") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Share of transactions")  +
  labs(colour = "Fee tier (nanoneros/byte)") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()


non.spam.fingerprint.tx <- spam.results[[2]]$non.spam.fingerprint.tx


non.spam.fingerprint.tx[, fee_per_byte_nanoneros := floor((tx_fee/tx_size_bytes)/1000)]

non.spam.fingerprint.tx.fees <- non.spam.fingerprint.tx[number_of_outputs == 2 & (
  fee_per_byte_nanoneros %between% c(18, 22) |
  fee_per_byte_nanoneros %between% c(78, 82) |
  fee_per_byte_nanoneros %between% c(315, 325) |
  fee_per_byte_nanoneros %between% c(3000, 4100)
), ]

non.spam.fingerprint.tx.fees[, fee_per_byte_nanoneros.cut := cut(fee_per_byte_nanoneros,
  breaks = c(0, 22, 82, 325, 4100), labels = c("20", "80", "320", "4000"))]


non.spam.fingerprint.tx.fees <- non.spam.fingerprint.tx.fees[, as.data.frame(prop.table(table(block_date, fee_per_byte_nanoneros.cut), margin = 1))]
non.spam.fingerprint.tx.fees$block_date <- as.Date(as.character(non.spam.fingerprint.tx.fees$block_date))

png("share-tx-in-fee-tier-spam-removed.png", width = 500, height = 600)

ggplot(non.spam.fingerprint.tx.fees, aes(x = block_date,
  y = Freq,
  colour = factor(fee_per_byte_nanoneros.cut))) +
  geom_line(linewidth = 1.5)  +
  geom_vline(xintercept = start.spam.date, linetype = 2) +
  scale_y_continuous( limits = c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_date(breaks = "3 day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Share of transactions by fee tier (suspected spam removed)") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Share of transactions")  +
  labs(colour = "Fee tier (nanoneros/byte)") +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()








