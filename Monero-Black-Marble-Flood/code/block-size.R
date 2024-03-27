

n.workers <- min(floor(parallelly::availableCores()/2), 32L)

future::plan(future::multicore(workers = n.workers))

system.time({
  block.data <- future.apply::future_lapply(sort(unlist(block.heights)), function(height) {
    
    block.data <- xmr.rpc(url.rpc = paste0(url.rpc, "/json_rpc"),
      method = "get_block",
      params = list(height = height ),
      keep.trying.rpc = TRUE)$result
    
    as.data.frame(block.data$block_header)
    
  })
})


block.data <- rbindlist(block.data)

block.data[, timestamp.POSIX := as.POSIXct(timestamp)]


block.data[, block_weight.rolling.max := zoo::rollapply(block.data$block_weight, width = 30, FUN = max, fill = NA)]


png("rolling-max-block-weight.png", width = 500, height = 600)

ggplot(block.data[timestamp.POSIX >= as.POSIXct((start.spam.date - 5)), ], aes(x = timestamp.POSIX, y = block_weight.rolling.max / 1000)) +
  geom_line() +
  scale_y_continuous(limit = c(0, NA), expand = c(0, 0)) +
  scale_x_datetime(date_breaks = "day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Monero empirical block weight (maximum peaks)",
    subtitle = "To smooth the line, the 30 block (1 hr) rolling maximum is displayed") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Block weight in kB (bytes/10^3)")   +
  theme(plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))

dev.off()

block.data[, block_weight.100.block.median := zoo::rollapply(block.data$block_weight, width = 101, FUN = median, fill = NA, align = "right")]

png("rolling-median-block-weight.png", width = 500, height = 600)

ggplot(block.data[timestamp.POSIX >= as.POSIXct((start.spam.date - 5)), ], aes(x = timestamp.POSIX, y = block_weight.100.block.median / 1000)) +
  geom_line() +
  scale_y_continuous(limit = c(0, max(block.data$block_weight.rolling.max / 1000, na.rm = TRUE)), expand = c(0, 0)) +
  scale_x_datetime(date_breaks = "day", expand = c(0, 0), guide = guide_axis(angle = 90)) +
  ggtitle("Monero empirical block weight",
    subtitle = "100 block rolling median") +
  xlab("                                                    Date              github.com/Rucknium") +
  ylab("Block weight in kB (bytes/10^3)")   +
  theme(plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))

dev.off()


