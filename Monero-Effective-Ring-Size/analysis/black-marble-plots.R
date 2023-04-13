# Must install:
# install.packages(c("data.table", "ggplot2", "scales"))

library(data.table)
library(ggplot2)
# Must also have scales package installed



p2pool.upgrade <- as.Date("2023-03-18")
first.mordinal <- as.Date("2023-03-10")


black.marble.share <- melt(black.marble.share, id.vars = "block_date", variable.factor = FALSE)

black.marble.share[grepl("mordinal", variable), variable := "Mordinal"]
black.marble.share[grepl("coinbase", variable), variable := "Coinbase"]

black.marble.share <- black.marble.share[block_date >= as.Date("2023-02-01"), ]

black.marble.share[variable == "Coinbase" & block_date < as.Date("2023-03-18"), mean(value)]
black.marble.share[variable == "Coinbase" & block_date > as.Date("2023-03-18"), mean(value)]

png("Monero-Effective-Ring-Size/analysis/images/mordinal-coinbase-output-share.png", width = 800, height = 800)

ggplot(black.marble.share) +
  geom_line(aes(x = block_date, y = value, colour = variable), size = 1.5) +
  ggtitle("Percentage of Monero Transaction Outputs That Are Mordinals and Coinbases") +
  geom_vline( aes(xintercept = first.mordinal,
    colour = "First Mordinal minted"), size = 1.5, linetype = 2, key_glyph = "rect") +
  geom_vline( aes(xintercept = p2pool.upgrade,
    colour = "P2Pool payout efficiency upgrade"), size = 1.5, linetype = 2, key_glyph = "rect") +
  scale_colour_manual(name = NULL, aesthetics = c("colour", "fill"),
    values = c("Mordinal" = "black", "Coinbase" = "darkgoldenrod2", "First Mordinal minted" = "khaki4",
      "P2Pool payout efficiency upgrade" = "#FF6600FF")) +
  xlab("                                                                             Date                                          github.com/Rucknium") +
  ylab("Share of Transaction Outputs") +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_hline(aes(yintercept = 0), colour = "gray20") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
    breaks = seq(0, 100, by = 5), sec.axis = dup_axis(name = NULL)) 

dev.off()





eff.ring.size.stats.mean <- eff.ring.size.stats[, .(block_timestamp_ring.time.date,
  effective.ring.size.coinbase.mean, effective.ring.size.mordinal.mean,
  effective.ring.size.coinbase.mordinal.mean )]


eff.ring.size.stats.mean <- melt(eff.ring.size.stats.mean,
  id.vars = "block_timestamp_ring.time.date", variable.factor = FALSE)

eff.ring.size.stats.mean <- eff.ring.size.stats.mean[block_timestamp_ring.time.date >= as.Date("2023-02-01"), ]

eff.ring.size.stats.mean[grepl("coinbase.mordinal", variable), variable := "Combined Mordinal & Coinbase"]
eff.ring.size.stats.mean[grepl("mordinal", variable), variable := "Mordinal"]
eff.ring.size.stats.mean[grepl("coinbase", variable), variable := "Coinbase"]


png("Monero-Effective-Ring-Size/analysis/images/mean-effective-ring-size.png", width = 800, height = 800)

ggplot(eff.ring.size.stats.mean) +
  geom_line(aes(x = block_timestamp_ring.time.date , y = value, colour = variable), size = 1.5) +
  ggtitle("Average Empirical Effective Ring Size of Monero Transactions") +
  geom_vline( aes(xintercept = first.mordinal,
    colour = "First Mordinal"), size = 1.5, linetype = 2, key_glyph = "rect") +
  geom_vline( aes(xintercept = p2pool.upgrade,
    colour = "P2Pool payout efficiency upgrade"), size = 1.5, linetype = 2, key_glyph = "rect") +
  scale_color_manual(name = NULL,
    values = c("Mordinal" = "black", "Coinbase" = "darkgoldenrod1", "Combined Mordinal & Coinbase" = "purple",
      "First Mordinal" = "khaki4", "P2Pool payout efficiency upgrade" = "#FF6600FF")) +
  xlab("                                                                             Date                                          github.com/Rucknium") +
  ylab("Effective ring size (ring size minus number of Mordinal/coinbase outputs in the ring)") +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(breaks = 0:100, sec.axis = dup_axis(name = NULL))
# Have an axis tick at every integer

dev.off()





eff.ring.size.stats.5th.percentile <- eff.ring.size.stats[, .(block_timestamp_ring.time.date,
  effective.ring.size.coinbase.percentile.05, effective.ring.size.mordinal.percentile.05,
  effective.ring.size.coinbase.mordinal.percentile.05 )]



eff.ring.size.stats.5th.percentile <- melt(eff.ring.size.stats.5th.percentile,
  id.vars = "block_timestamp_ring.time.date", variable.factor = FALSE)

eff.ring.size.stats.5th.percentile <- eff.ring.size.stats.5th.percentile[
  block_timestamp_ring.time.date >= as.Date("2023-02-01"), ]

eff.ring.size.stats.5th.percentile[grepl("coinbase.mordinal", variable), variable := "Combined Mordinal & Coinbase"]
eff.ring.size.stats.5th.percentile[grepl("mordinal", variable), variable := "Mordinal"]
eff.ring.size.stats.5th.percentile[grepl("coinbase", variable), variable := "Coinbase"]


png("Monero-Effective-Ring-Size/analysis/images/5th-percentile-effective-ring-size.png", width = 800, height = 800)

ggplot(eff.ring.size.stats.5th.percentile) +
  geom_line(aes(x = block_timestamp_ring.time.date , y = value, colour = variable), size = 1.5) +
  ggtitle('Empirical Effective Ring Size for the "Unluckiest" 5 Percent of Monero Rings\n(i.e. 5th Percentile of Empirical Effective Ring Size)') +
  geom_vline( aes(xintercept = first.mordinal,
    colour = "First Mordinal"), size = 1.5, linetype = 2, key_glyph = "rect") +
  geom_vline( aes(xintercept = p2pool.upgrade,
    colour = "P2Pool payout efficiency upgrade"), size = 1.5, linetype = 2, key_glyph = "rect") +
  scale_color_manual(name = NULL,
    values = c("Mordinal" = "black", "Coinbase" = "darkgoldenrod1", "Combined Mordinal & Coinbase" = "purple",
      "First Mordinal" = "khaki4", "P2Pool payout efficiency upgrade" = "#FF6600FF")) +
  xlab("                                                                             Date                                          github.com/Rucknium") +
  ylab("Effective ring size (ring size minus number of Mordinal/coinbase outputs in the ring)") +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(breaks = 0:100, sec.axis = dup_axis(name = NULL))
# Have an axis tick at every integer

dev.off()


