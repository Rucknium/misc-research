
# install.packages(c("data.table", "ggplot2", "treemapify", "future", "future.apply"))

library(data.table)


unique.outbound.ips <- readLines("good_peers.txt")
# Run this for an hour to get good_peers:
# https://gist.github.com/Boog900/5e9fe91197fbbf5f5214df77de0c8cd8

unique.outbound.ips <- stringr::str_extract(unique.outbound.ips, "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
unique.outbound.ips <- unique(unique.outbound.ips)

# wget https://github.com/Boog900/monero-ban-list/raw/refs/heads/main/ban_list.txt
ban_list <- readLines("ban_list.txt")


convert.subnet.16 <- function(x) {
  gsub("[.][0-9]{1,3}[.][0-9]{1,3}$", "", x)
}


convert.subnet.24 <- function(x) {
  gsub("[.][0-9]{1,3}$", "", x)
}


unique.outbound.ips <- data.table(
  ip = unique.outbound.ips,
  subnet.16 = convert.subnet.16(unique.outbound.ips),
  subnet.24 = convert.subnet.24(unique.outbound.ips))

nrow(unique.outbound.ips)
uniqueN(unique.outbound.ips$subnet.16)
uniqueN(unique.outbound.ips$subnet.24)

ban_list.singletons <- ban_list[! grepl("/", ban_list)]
ban_list.ranges <- ban_list[grepl("/", ban_list)]

uniqueN(convert.subnet.16(ban_list.singletons))

malicious.ips <- unique.outbound.ips[ip %in% ban_list.singletons, ip]

for (i in ban_list.ranges) {
  for (j in seq_along(unique.outbound.ips$ip)) {
    if ( ! is.na(IP::ip.match(IP::ipv4(unique.outbound.ips$ip[j]), IP::ipv4r( i )))) {
      malicious.ips <- c(malicious.ips, unique.outbound.ips$ip[j])
    }
  }
}

h_d <- unique.outbound.ips[ ! ip %in% malicious.ips, uniqueN(ip)]
h_s <- unique.outbound.ips[ ! ip %in% malicious.ips, uniqueN(subnet.16)]
h_d / h_s
# Condition for p_ss > p_dd



unique.outbound.ips[, type := ifelse(ip %in% malicious.ips, "spy", "honest")]
unique.outbound.ips[, y := 1]

library(ggplot2)
library(treemapify)


png("pdf/images/treemap-status-quo.png", width = 1000, height = 1000)

ggplot(unique.outbound.ips, aes(area = y, fill = type,
  subgroup = subnet.16, subgroup2 = subnet.24)) +
  labs(title = "Subnet treemap of honest and spy nodes",
    subtitle = "Black perimeters indicate /16 subnet groupings. Yellow indicates /24 subnets.") +
  geom_treemap() +
  geom_treemap_subgroup2_border(colour = "yellow", size = 1.5) +
  geom_treemap_subgroup_border(color = "black", size = 2) +
  scale_fill_manual(name = "Node type:",
    values = c(scales::muted("blue", l = 40), scales::muted("red", l = 60))) +
  geom_treemap_subgroup_text(colour = "white", place = "centre", grow = TRUE, min.size = 8) +
  theme(plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = "top")

dev.off()


unique.outbound.ips.deduplicated <- unique.outbound.ips[,
  .(spy.share = mean(type == "spy")), by = "subnet.16"]

unique.outbound.ips.deduplicated[, type := "mixed"]
unique.outbound.ips.deduplicated[spy.share == 1, type := "spy"]
unique.outbound.ips.deduplicated[spy.share == 0, type := "honest"]

unique.outbound.ips.deduplicated[, y := 1]
setorder(unique.outbound.ips.deduplicated, type)
setorder(unique.outbound.ips.deduplicated, spy.share)



png("pdf/images/treemap-16-subnet-deduplication.png", width = 1000, height = 1000)

ggplot(unique.outbound.ips.deduplicated, aes(area = y, fill = spy.share)) +
  labs(title = "Subnet treemap of honest and spy nodes after /16 subnet deduplication") +
  geom_treemap(start = "topright") + # layout = "fixed"
  # start: The corner in which to start placing the tiles. One of
  # 'bottomleft' (the default), 'topleft', 'topright' or 'bottomright'.
  scale_fill_gradient2(name = "Spy share:    ", midpoint = 0.5,
    low = scales::muted("blue", l = 40), high = scales::muted("red", l = 60)) +
  # guides(fill = guide_legend(title = "Spy share")) +
  guides(fill = guide_colorbar(barwidth = 20)) +
  theme(plot.title = element_text(size = 25),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = "top")

dev.off()





threads <- 4
future::plan(future::multicore, workers = threads)
# Change to future::multisession if on Windows or in RStudio


n.default.out <- 12
n.nodes <- 10000


choose.peers <- function(unique.outbound.ips, connections, n.default.out, method) {

  while(length(connections) < n.default.out) {

    if (method == "status quo") {
      candidates <- unique.outbound.ips[! subnet.16 %in% convert.subnet.16(connections), ip]
    }
    if (method == "subnet deduplication") {
      candidates <- unique.outbound.ips[! subnet.16 %in% convert.subnet.16(connections), ]
      candidates <- candidates[sample.int(.N), ]
      candidates <- candidates[!duplicated(subnet.16), ip]
    }

    connections[length(connections) + 1] <- sample(candidates, 1)

  }

  connections
}



set.seed(314)



status.quo <- future.apply::future_replicate(n.nodes,
  choose.peers(unique.outbound.ips, c(), n.default.out, "status quo"),
  future.packages = "data.table")

deduplicated <- future.apply::future_replicate(n.nodes,
  choose.peers(unique.outbound.ips, c(), n.default.out, "subnet deduplication"),
  future.packages = "data.table")


mean(c(status.quo) %in% malicious.ips)
mean(c(deduplicated) %in% malicious.ips)

churn.peers <- function(x, churns, method) {
  for (i in seq_len(churns)) {
    x <- sample(x, length(x) - 1)
    x <- choose.peers(unique.outbound.ips, x, n.default.out, method)
  }
  x
}

set.seed(314)

status.quo.churned <- future.apply::future_apply(status.quo, MARGIN = 2,
  function(x) { churn.peers(x, 100, "status quo") },
  future.seed = TRUE, future.packages = "data.table")

deduplicated.churned <- future.apply::future_apply(deduplicated, MARGIN = 2,
  function(x) { churn.peers(x, 100, "subnet deduplication") },
  future.seed = TRUE, future.packages = "data.table")

mean(c(status.quo.churned) %in% malicious.ips)
mean(c(deduplicated.churned) %in% malicious.ips)



