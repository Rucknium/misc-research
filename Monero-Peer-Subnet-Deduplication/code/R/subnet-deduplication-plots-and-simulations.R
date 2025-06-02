
# Install packages:
# install.packages(c("data.table", "ggplot2", "treemapify", "future", "future.apply", "gt", "remotes"))
# remotes::install_github("Rucknium/xmrpeers", upgrade = FALSE)

library(data.table)
library(ggplot2)
library(treemapify)
library(gt)
library(xmrpeers)


data(good_peers)

good_peers <- stringr::str_extract(good_peers,
  "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
good_peers <- unique(good_peers)
good_peers <- na.omit(good_peers)
# Clean IP addresses

data(ban_list)
suspected.malicious.ips <- ban_list



unique.outbound.ips <- data.table(
  ip = good_peers,
  subnet.16 = as.subnet(good_peers, 16),
  subnet.24 = as.subnet(good_peers, 24))

nrow(unique.outbound.ips)
uniqueN(unique.outbound.ips$subnet.16)
uniqueN(unique.outbound.ips$subnet.24)


suspected.malicious.ips.exact <- good_peers[xmrpeers::in.ip.set(good_peers, suspected.malicious.ips)]




unique.outbound.ips[, type := ifelse(ip %in% suspected.malicious.ips.exact, "spy", "honest")]
unique.outbound.ips[, y := 1]



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
  labs(title = "Subnet treemap of honest and spy nodes after /24 subnet deduplication") +
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




unique.outbound.ips.deduplicated <- unique.outbound.ips[,
  .(spy.share = mean(type == "spy")), by = "subnet.24"]

unique.outbound.ips.deduplicated[, type := "mixed"]
unique.outbound.ips.deduplicated[spy.share == 1, type := "spy"]
unique.outbound.ips.deduplicated[spy.share == 0, type := "honest"]

unique.outbound.ips.deduplicated[, y := 1]
setorder(unique.outbound.ips.deduplicated, type)
setorder(unique.outbound.ips.deduplicated, spy.share)



png("pdf/images/treemap-24-subnet-deduplication.png", width = 1000, height = 1000)

ggplot(unique.outbound.ips.deduplicated, aes(area = y, fill = spy.share)) +
  labs(title = "Subnet treemap of honest and spy nodes after /24 subnet deduplication") +
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







data(good_peers)

good_peers <- stringr::str_extract(good_peers,
  "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
good_peers <- unique(good_peers)
good_peers <- na.omit(good_peers)
# Clean IP addresses

data(ban_list)
suspected.malicious.ips <- ban_list

future::plan(future::multisession,
  workers = max(c(1, floor(parallelly::availableCores()/6))))
# Multi-threaded is recommended


scenarios <- data.table(name = c(
  "status_quo_80_percent_unreachable",
  "deduplication_80_percent_unreachable",
  "status_quo_90_percent_unreachable",
  "deduplication_90_percent_unreachable"
),
  deduplication = c(FALSE, TRUE, FALSE, TRUE),
  share.reachable = c(0.20, 0.20, 0.10, 0.10)
)

simulated.networks <- list()

for (i in seq_len(nrow(scenarios))) {
  
  share.reachable <- scenarios[i, share.reachable]
  
  n.assumed.unreachable <- floor(length(good_peers) *
      ((1 - share.reachable) / share.reachable))
  
  do.deduplication <- scenarios[i, deduplication]
  
  set.seed(314)
  
  simulated.networks[[ scenarios[i, name] ]] <-
    xmrpeers::gen.network(outbound.ips = good_peers,
      malicious.ips = suspected.malicious.ips,
      n.unreachable = n.assumed.unreachable,
      already.connected.subnet.level = 24,
      deduplication.subnet.level = 24,
      do.deduplication = do.deduplication)
  
}






malicious.connection.summary <- sapply(simulated.networks, FUN = function(x) {
  x <- copy(x$nodes)
  x[malicious == FALSE, c(`N honest nodes` = .N, summary(n.malicious.outbound))]
}) |> t() |> data.frame(check.names = FALSE)


malicious.connection.summary <- cbind(
  Scenario = rownames(malicious.connection.summary), malicious.connection.summary)


fix.latex.table <- function(latex.output, label) {
  
  latex.output <- gsub("caption*", "caption", latex.output, fixed = TRUE)
  # Removing the "*" means that the table is numbered in the final PDF output
  latex.output <- gsub("\\large ", "", latex.output, fixed = TRUE)
  # Remove font size in the caption
  
  latex.output <- gsub("\\end{table}",
    paste0("\\label{table-", label, "}\n\\end{table}"), latex.output, fixed = TRUE)
  
  cat(latex.output, file = paste0("pdf/tables/", label, ".tex"))
  
  invisible(NULL)
  
}


latex.output <- gt(malicious.connection.summary) |>
  tab_header(title = "Summary statistics: Number of outbound connections, out of 12, to suspected malicious nodes") |>
  fmt_number(drop_trailing_zeros = TRUE) |>
  #  cols_align_decimal() |>
  tab_options(table.font.size = "") |>
  # latex.use_longtable = TRUE, 
  # https://cran.r-project.org/web/packages/gt/news/news.html
  # https://github.com/rstudio/gt/issues/1852
  as_latex() |>
  as.character()

fix.latex.table(latex.output, "malicious-connection-summary")



inbound.summary <- sapply(simulated.networks, FUN = function(x) {
  x <- copy(x$nodes)
  x[malicious == FALSE & reachable == TRUE, c(`N honest reachable nodes` = .N, summary(n.inbound))]
}) |> t() |> data.frame(check.names = FALSE)


inbound.summary <- cbind(
  Scenario = rownames(inbound.summary), inbound.summary)

latex.output <- gt(inbound.summary) |>
  tab_header(title = "Summary statistics: Number of inbound connections to honest reachable nodes") |>
  fmt_number(drop_trailing_zeros = TRUE) |>
  # cols_align_decimal() |>
  tab_options(table.font.size = "") |>
  as_latex() |>
  as.character()

fix.latex.table(latex.output, "inbound-summary")




network.stats.summary <- sapply(simulated.networks, FUN = function(x) {
  sapply(x$network.stats, function(x) {x$centralization})
}) |> t() |> data.frame(check.names = FALSE)

network.stats.summary <- cbind(
  Scenario = rownames(network.stats.summary), network.stats.summary)

setDT(network.stats.summary)

setnames(network.stats.summary, c("centr_betw", "centr_clo", "centr_degree", "centr_eigen"),
  c("Betweenness", "Closeness", "Degree", "Eigenvector"))


latex.output <- gt(network.stats.summary) |>
  tab_header(title = "Centrality statistics of the network") |>
  fmt_scientific() |>
  tab_options(latex.use_longtable = TRUE, table.font.size = "") |>
  as_latex() |>
  as.character()

fix.latex.table(latex.output, "network-stats")





n.inbound <- list()

for (i in seq_along(simulated.networks)) {
  n.inbound[[i]] <- data.table(
    algorithm = scenarios[i, ifelse(deduplication, "Deduplication", "Status quo")],
    unreachable = scenarios[i, paste0( 100 * (1 - share.reachable), "% unreachable nodes")],
    n.inbound = simulated.networks[[i]]$nodes[malicious == FALSE & reachable == TRUE, n.inbound]
  )
}

n.inbound <- rbindlist(n.inbound)

n.inbound[, algorithm := factor(algorithm, c("Status quo", "Deduplication"))]
# Fix order


png("pdf/images/inbound-histogram.png", width = 800, height = 800)

ggplot(n.inbound, aes(n.inbound, fill = I("#00aebf"))) +
  # Color from https://www.getmonero.org/press-kit/logos/mrl-logo.svg
  geom_histogram(binwidth = 2) +
  facet_grid(vars(algorithm), vars(unreachable)) +
  ggtitle("Histograms of inbound connections of honest reachable nodes") +
  xlab("Number of inbound connections") +
  ylab("Number of nodes")  +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15))

dev.off()






