
library(data.table)
library(ggplot2)
# Need lubridate package installed too:
# install.packages("lubridate")
require(Cairo)
# Cairo is optional. Better image quality with Cairo


num.times.referenced <- unique(xmr.rings[, .(output_index, num_times_referenced)])

round(prop.table(table(num.times.referenced$num_times_referenced))*100, 2)


coinbase.ring.members.stats <- coinbase.ring.members.stats[grepl("202[1-9]", block_timestamp_ring.date.isoweek)]
# Only get data for 2021 and after

coinbase.ring.members.stats[, isoweek := as.factor(block_timestamp_ring.date.isoweek)]

lubridate::isoweek(as.Date("2022-08-15"))
hardfork <- "2022-33"
# Hard fork happened Saturday Aug 13 (ISO week "2022-33"). ISO week starts on Mondays

lubridate::isoweek(as.Date("2021-08-29"))
first.p2pool <- "2021-34"
# ISO week "2021-34"
# "The first Monero block found by P2Pool was 2437679 on August 29th, 2021."
# https://www.reddit.com/r/Monero/comments/x0jdb3/p2pools_first_anniversary_is_today/

lubridate::isoweek(as.Date("2023-03-18"))
p2pool.upgrade <- "2023-11"
# ISO week "2023-11"
# P2Pool side chain hard fork to make payouts more efficient (fewer outputs per coinbase tx)
# https://www.reddit.com/r/MoneroMining/comments/11tln6z/psa_p2pool_miners_update_to_the_latest_version/


gg.boilerplate <- ggplot(coinbase.ring.members.stats) +
  geom_vline( aes(xintercept = which(unique(coinbase.ring.members.stats$isoweek) == first.p2pool),
    colour = "purple"), size = 1.5, linetype = 2) +
  geom_vline(aes(xintercept = which(unique(coinbase.ring.members.stats$isoweek) == hardfork),
    colour = "#FF6600FF"), size = 1.5, linetype = 2) +
  geom_vline( aes(xintercept = which(unique(coinbase.ring.members.stats$isoweek) == p2pool.upgrade),
    colour = "darkgreen"), size = 1.5, linetype = 2) +
  scale_color_manual(name = NULL,
    labels = c("1st block mined by P2Pool", "Monero hard fork", "P2Pool payout efficiency upgrade"),
    values = c("purple", "#FF6600FF", "darkgreen"),
    breaks = c("purple", "#FF6600FF", "darkgreen")) + 
  xlab("                                                                             ISO Week                                          github.com/Rucknium") +
  ylab ("Effective ring size (ring size minus number of coinbase outputs in the ring)") +
  scale_x_discrete(breaks = levels(coinbase.ring.members.stats$isoweek)[floor(seq(1, 
    nlevels(coinbase.ring.members.stats$isoweek), 
    length.out = 20))]) +
  theme(plot.title = element_text(size = 20), legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))



png("Monero-Effective_ring_size/analysis/images/mean-effective-ring-size.png", width = 800, height = 800)

gg.boilerplate +
  geom_line(aes(x = isoweek, y = effective.ring.size.mean, group = 1), size = 1.5) +
  ggtitle("Mean Empirical Effective Ring Size")

dev.off()


png("Monero-Effective-Ring-Size/analysis/images/median-effective-ring-size.png", width = 800, height = 800)

gg.boilerplate +
  geom_line(aes(x = isoweek, y = effective.ring.size.median, group = 1), size = 1.5) +
  ggtitle("Median Empirical Effective Ring Size")

dev.off()


png("Monero-Effective-Ring-Size/analysis/images/5th-percentile-effective-ring-size.png", width = 800, height = 800)

gg.boilerplate +
  geom_line(aes(x = isoweek, y = effective.ring.size.percentile.05, group = 1), size = 1.5) +
  ggtitle('Empirical Effective Ring Size for the "Unluckiest" 5 Percent of Rings\n(i.e. 5th Percentile of Empirical Effective Ring Size)')

dev.off()




