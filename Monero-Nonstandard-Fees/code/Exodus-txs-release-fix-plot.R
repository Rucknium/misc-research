
# install.packages("ggplot2")
# install.packages("lubridate")

library(ggplot2)

fee.clusters <- read.csv("Monero-Nonstandard-Fees/data/fee-clusters-by-day.csv", stringsAsFactors = FALSE)

exodus <- data.frame(block_timestamp_date = fee.clusters$block_timestamp_date, exodus.txs = fee.clusters$X24_34_44_fee, stringsAsFactors = FALSE)

exodus$Week <- factor(c(rep(NA, 3), paste0("Week starting ",
  rep(exodus$block_timestamp_date[-(1:3)][seq(1, nrow(exodus) - 3, by = 7)], each = 7))))
# Note: If data newer than 2023-10-17, then must change this

exodus$day.of.week <- lubridate::wday(as.Date(exodus$block_timestamp_date), label = TRUE, week_start = "Wednesday")

exodus <- exodus[(nrow(exodus) - 7*8 + 1):nrow(exodus), ]


png("Monero-Nonstandard-Fees/images/Exodus-txs-after-fix-release.png", width = 800, height = 800)

theme_set(theme_gray(base_size = 18))

ggplot(exodus, aes(x = day.of.week, y = exodus.txs, col = Week, group = Week)) +
  geom_line() + geom_point() + expand_limits(y = 0) +
  labs(title = "Number of Monero transactions with 0.00024060, 0.00034245, or\n0.00044430 XMR total fee (suspected Exodus Desktop wallet transactions)",
    x = "Day of week (by UTC time)", y = "Number of transactions per day",
    caption = "New version of Exodus Desktop with standard fee calculations was released 2023-10-10") +
  annotate("text", y = 0, x = 6, label = "github.com/Rucknium", size = 6) +
  theme(plot.caption = element_text(color = "red"))

dev.off()
