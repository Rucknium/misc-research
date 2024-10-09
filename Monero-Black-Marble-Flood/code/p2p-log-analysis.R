
# Turn on the p2p transaction receipt logging with `set_log net.p2p.msg:INFO`
# in monerod

# First, prepare the log files for analysis
# Uncomment these lines and run them:
# install.packages("remotes")
# remotes::install_github("Rucknium/xmrpeers")
# And install hese if not already installed:
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("gt")
# install.packages("quantreg")
# install.packages("circlize")
# install.packages("skellam")

# If logs are in the standard location on the machine, do this to collect and compress the logs:
xmrpeers::compress.log()

# Move the file to its final location (i.e. if the log files are in a machine
# different from the one that will do the analysis, move them) and put it in
# its own directory.

# Then the convert the log files into a data frame.
# It can be useful to restart R between each file processed.

n.files <- NA
log.file.directories <- paste0("", 1:n.files, "/")
# Trailing slash here

for (i in log.file.directories) {
  output <- xmrpeers::get.p2p.log(paste0(i, "extracted-xmr-log"))
  saveRDS(output, paste0(i, "p2p-gossip.rds"))
}


# In a new R session, start loading the log data

library(data.table)

p2p.gossip <- list()

n.files <- NA
log.file.directories <- paste0("", 1:n.files, "/")
# Trailing slash here

for (i in log.file.directories) {
  p2p.gossip[[as.character(i)]] <- readRDS(paste0(i, "/p2p-gossip.rds"))
}

p2p.gossip <- data.table::rbindlist(p2p.gossip, idcol = "file")

setorder(p2p.gossip, file, gossip.msg.id, time.log)

p2p.gossip <- p2p.gossip[time.p2p %between% c(as.POSIXct("2024-04-14"), as.POSIXct("2024-05-24")), ]

# Load ping data

peer.pings <- list()

for (i in log.file.directories) {
  peer.pings[[as.character(i)]] <- read.csv(paste0(i, "monero_peer_pings.csv"),
    header = FALSE, stringsAsFactors = FALSE)
  names(peer.pings[[as.character(i)]]) <- c("ip", "port", "direction", "ping1", "ping2", "ping3", "ping4", "ping5")
}

peer.pings <- data.table::rbindlist(peer.pings, idcol = "file")

stopifnot(sum(duplicated(peer.pings[, .(file, ip, port, direction)]) == 0))

peer.pings[, port := as.character(port)]

peer.pings$median.ping <- apply(
  peer.pings[, .(ping1, ping2, ping3, ping4, ping5)], 1, FUN = median, na.rm = TRUE)


# ****************************
# Number of unique IP addresses
# ****************************


p2p.gossip[, uniqueN(ip)]
# ^ Number of unique IP addresses in the dataset


# ****************************
# Duration of peer connections
# ****************************

hour.seq <- seq(as.POSIXct("2023-01-01"), as.POSIXct("2024-06-01"), by = "1 hour")

p2p.gossip[, hour := cut(time.p2p, hour.seq)]

p2p.gossip.hour <- p2p.gossip[, .(n.hours = uniqueN(hour)), by = c("file", "ip", "direction")]

summary(p2p.gossip.hour$n.hours)

p2p.gossip.hour[, as.list(summary(n.hours)), by = "direction"]

setorder(p2p.gossip, time.p2p)

diff.hour <- p2p.gossip[, .(diff.hour = diff(as.numeric(hour))), by = c("file", "ip", "direction")]

diff.hour.summary <- diff.hour[, .(diff.hour = 1 + sum(diff.hour > 1)), by = c("file", "ip", "direction")]
diff.hour.summary[is.na(diff.hour), diff.hour := 1] # Because if there was a missing for diff(), then it only appeared once.

diff.hour.summary[, as.list(summary(diff.hour)), by = "direction"]
# ^ Number of distinct intervals it appears in the data


conn.period <- function(x) {
  # https://stats.stackexchange.com/questions/107515/grouping-sequential-values-in-r
  y <- sort(x)
  conn.period <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > 1))
  conn.period[match(y, x)]
}


p2p.gossip[, conn.period := conn.period(as.integer(hour)), by = c("file", "ip", "direction")]

diff.time <- p2p.gossip[, .(diff.time = diff(range(as.numeric(time.p2p)))), by = c("file", "ip", "direction", "conn.period")]

files.for.duration <- c()

diff.time[file %in% files.for.duration, as.list(summary(as.numeric(diff.time)/ 60))]

diff.time[file %in% files.for.duration, as.list(summary(as.numeric(diff.time)/ 60)), by = "direction"]
# ^ Median duration of connections


diff.time[file %in% files.for.duration & direction == "OUT", 100 * prop.table(table(as.numeric(diff.time)/ 60 > 6*60))]
# ^ Share of outgoing connections lasting longer than 6 hours

diff.time[file %in% files.for.duration & direction == "OUT", 100 * prop.table(table(as.numeric(diff.time)/ 60 > 24*60))]
# ^ Share of outgoing connections lasting longer than 24 hours

diff.time[file %in% files.for.duration & direction == "INC", 100 * prop.table(table(as.numeric(diff.time)/ 60 > 6*60))]
# ^ Share of incoming connections lasting longer than 6 hours

diff.time[file %in% files.for.duration & direction == "INC", 100 * prop.table(table(as.numeric(diff.time)/ 60 > 24*60))]
# ^ Share of incoming connections lasting longer than 24 hours



library(ggplot2)

png("Monero-Black-Marble-Flood/pdf/images/p2p-connection-duration.png")

ggplot(diff.time[file %in% files.for.duration & as.numeric(diff.time)/ 60 <= 200, ],
  aes(as.numeric(diff.time)/ 60, color = direction)) +
  labs(title = "Kernel density estimate of peer connection duration", x = "Connection duration (minutes)") +
  geom_density(bw = 1) +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(nrow = 1, byrow = FALSE, override.aes = list(linewidth = 5)))

dev.off()

# ****************************
# Transaction sent by peers multiple times
# ****************************

same.tx.sent <- p2p.gossip[, .(n.same.tx.sent = .N), by = c("tx.hash", "file", "ip", "port", "direction")]

round(100 * prop.table(table(same.tx.sent$n.same.tx.sent)), 2)
# ^ Number of times each tx was received from the same peer

setorder(p2p.gossip, time.p2p)

time.diff.same.tx.sent <- p2p.gossip[tx.hash %chin% same.tx.sent[n.same.tx.sent >= 2, tx.hash],
  .(time.diff = diff(time.p2p), n.times.sent = .N), by = c("tx.hash", "file", "ip", "port", "direction")]

summary(as.numeric(time.diff.same.tx.sent$time.diff))


n.send.times.max <- 16

median.send.delta.data <- median.send.delta.data.n <- c()

for (i in 1:n.send.times.max) {
  median.send.delta <- time.diff.same.tx.sent[n.times.sent >= (i + 1),
    .(temp = as.numeric(time.diff)[i]), by = "tx.hash"][, median(temp)]
  median.send.delta.data <- c(median.send.delta.data, median.send.delta)
  median.send.delta.data.n <- c(median.send.delta.data.n, sum(time.diff.same.tx.sent$n.times.sent >= (i + 1)))
}

library(gt)

table.title <- "Time between duplicate transaction receipts"
label <- "table-multiple-send-p2p"

multiple.send.tex <- gt(data.table(`Number of times received` = scales::label_ordinal()(1:n.send.times.max + 1),
  `Median minutes elapsed since previous time tx received` = round(median.send.delta.data, 2),
  `Number of txs (rounded to 10)` = prettyNum(round(median.send.delta.data.n, digits = -1), big.mark = ","))) |>
  tab_header(title = table.title) |>
  as_latex()


latex.output <- multiple.send.tex |> as.character()

col.widths <- c(2.25, 3, 2.25)

latex.output <- gsub("begin[{]longtable[}][{][^\n]+",
  paste0("begin{longtable}{", paste0("Rp{", col.widths, "cm}", collapse = ""), "}"),
  latex.output)

latex.output <- gsub("caption*", "caption", latex.output, fixed = TRUE)
# Removing the "*" means that the table is numbered in the final PDF output

latex.output <- gsub("\\end{longtable}",
  paste0("\\label{", label, "}\n\\end{longtable}"), latex.output, fixed = TRUE)


cat(latex.output, file = "Monero-Black-Marble-Flood/pdf/tables/multiple-send-p2p.tex")





# ****************************
# Clumping
# ****************************

clumping <- p2p.gossip[, .(temp = .N), by = c("file", "gossip.msg.id")][, round(100 * prop.table(table(temp)), 2)]

clumping <- c(clumping) # Convert from table to vector
clumping <- c(clumping[as.numeric(names(clumping)) <= 10], `> 10` = sum(clumping[as.numeric(names(clumping)) > 10]))


table.title <- "Transactions clumping in gossip messages"
label <- "table-tx-clumping-p2p"


clumping.tex <- gt(data.table(`Number of txs in message` = names(clumping),
  `Share of messages (percentage)` = round(clumping, 2))) |>
  tab_header(title = table.title) |>
  cols_align(align = "right")  |>
  as_latex()

latex.output <- clumping.tex |> as.character()

col.widths <- c(2.25, 3)

latex.output <- gsub("begin[{]longtable[}][{][^\n]+",
  paste0("begin{longtable}{", paste0("Rp{", col.widths, "cm}", collapse = ""), "}"),
  latex.output)

latex.output <- gsub("caption*", "caption", latex.output, fixed = TRUE)
# Removing the "*" means that the table is numbered in the final PDF output

latex.output <- gsub("\\end{longtable}",
  paste0("\\label{", label, "}\n\\end{longtable}"), latex.output, fixed = TRUE)


cat(latex.output, file = "Monero-Black-Marble-Flood/pdf/tables/tx-clumping-p2p.tex")



# ****************************
# How does ping time affect time to receive tx?
# ****************************

library(quantreg)

time.since.first.receipt.pings <- p2p.gossip[ file %in% unique(peer.pings$file), .(time.since.first.receipt = time.p2p - min(time.p2p),
  ip = ip, port = port, direction = direction), by = c("file", "tx.hash")]


time.since.first.receipt.pings <- merge(time.since.first.receipt.pings, peer.pings)


summary(lm(as.numeric(time.since.first.receipt)*1000 ~ I(median.ping/2),
  data = time.since.first.receipt.pings[time.since.first.receipt %between% c(0, 60),]))
# *1000 is to convert to millisecond units. /2 is to make round-trip ping into single-leg ping
#

summary(rq(as.numeric(time.since.first.receipt)*1000 ~ I(median.ping/2),
  tau = 0.5, data = time.since.first.receipt.pings, method = "fn"))
#


# ****************************
# Tx arrival times when two logging nodes are connected to the same peer
# ****************************


unique.conn.hours <- unique(p2p.gossip[, .(file, ip, port, direction, hour)])
our.nodes.connected <- unique.conn.hours[, .(n.our.nodes.connected = .N), by = c("ip", "port", "hour")]
our.nodes.connected <- merge(our.nodes.connected[n.our.nodes.connected == 2, .(ip, hour)], unique.conn.hours)
# A few have our.nodes.connected == 3, but it is very rare and harder to analyze, so skip
our.nodes.connected <- merge(our.nodes.connected, p2p.gossip, by = c("ip", "hour", "file", "port"))

setorder(our.nodes.connected, file, time.p2p)
# Set order by file so the "first" and "second" nodes are in consistent order.
# Set next order priority by time.p2p so that the next step works properly

our.nodes.connected <- unique(our.nodes.connected, by = c("file", "ip", "port", "tx.hash"))
# Sometimes a peer sends the same transaction more than once, so eliminate the later duplicate

our.nodes.connected <- our.nodes.connected[, .(tx.hash.time.diff = diff(time.p2p),
  gossip.msg.id.1 = gossip.msg.id[1], gossip.msg.id.2 = gossip.msg.id[2],
  file.1 = file[1], file.2 = file[2]), by = c("ip", "port", "hour", "tx.hash")]


our.nodes.connected[, tx.hash.time.diff := as.numeric(tx.hash.time.diff)]

pair.in.time.sync <- c()



library(circlize)

# Circular density


simul.connection.data <- our.nodes.connected[tx.hash.time.diff <= 60 &
    (file.1 %in% pair.in.time.sync & file.2 %in% pair.in.time.sync),
  tx.hash.time.diff]

circ.data <- ifelse(simul.connection.data >= 0, simul.connection.data %% 1,
  abs(simul.connection.data %% -1))
# Compute modulo

circ.data <- c(circ.data - 1, circ.data, circ.data + 1)
# This gives us a "circular" support so we do not have the
# kernel density boundary issue

density.data <- density(circ.data, bw = 0.01, n = 512 * 3)

density.data$y <- density.data$y[density.data$x %between% c(-0.005, 1.005) ]
density.data$x <- density.data$x[density.data$x %between% c(-0.005, 1.005) ]


png("Monero-Black-Marble-Flood/pdf/images/one-second-period-tx-p2p-msg.png")

circos.par(start.degree = 90, gap.degree = 0, circle.margin = 0.15)

circos.initialize(sectors = rep("A", length(circ.data)), x = circ.data, xlim = c(0, 1))

circos.trackPlotRegion(ylim = c(0, max(density.data$y)), track.height = .9)

circos.trackLines(sectors = rep("A", length(density.data$x)), density.data$x, density.data$y,
  track.index = 1, area = TRUE, col = "#999999", border = "black" )

circos.xaxis(major.at = c(0, 0.25, 0.50, 0.75),
  labels = c(0, expression(frac(1, 4)), expression(1/2), expression(frac(3, 4))),
  labels.facing = "downward",  labels.col = "darkred", labels.pos.adjust = FALSE)

axis.marks <- c(0.5, 1, 1.5)

circos.yaxis(at = axis.marks)

for (i in axis.marks) {
  circos.trackLines(sectors = rep("A", 2), c(0, 1), rep(i, 2), lty = 2,
    track.index = 1 )
}



circos.clear()

title(main = "One-second cycle of time difference between same\ntx received from two different nodes")
title(sub = "Fractional seconds")

dev.off()



# Histogram

n.subsecond.intervals <- 8
hist.range <- c(-5 - (1/2)/n.subsecond.intervals, 5 + (1/2)/n.subsecond.intervals)

skellam.points <- n.subsecond.intervals * skellam::dskellam(-20:20, lambda1 = 20, lambda2 = 20)

png("Monero-Black-Marble-Flood/pdf/images/skellam-histogram-tx-p2p-msg.png")

hist(simul.connection.data[simul.connection.data %between% hist.range],
  breaks = seq(hist.range[1], hist.range[2], by = 1/n.subsecond.intervals), probability = TRUE,
  main = "Time difference between same tx received from two different nodes",
  xlab = "Time difference (seconds)")

points(-20:20/4, skellam.points, col = "red")
segments(-20:20/4, 0, -20:20/4, skellam.points, col = "red")
legend("topleft", legend = c("Histogram", "Theoretical Skellam\ndistribution"),
  fill = c("lightgray", NA), border = c("black", NA), pch = c(NA, 21),
  col = c(NA, "red"), bty = "n")

dev.off()



