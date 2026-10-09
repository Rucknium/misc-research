

# Must be in the `data` working directory

# Must have these packages installed
# install.packages(c("data.table", "ggplot2", "patchwork", "yaml", "RJSONIO", "tibble", "plyr"))


library(data.table)
library(ggplot2)
library(patchwork)



sim.run.defense <- "nyx-defense-50-hours"
sim.run.status.quo <- "status-quo-50-hours"
relay.id.unreachable <- "monero-relay-4000"
relay.id.reachable <- "monero-relay-2028"



extract.metrics <- function(sim.run, relay.id) {

  input_config.path <- file.path(sim.run, "input_config.yaml")

  shadow_agents.path <- file.path(sim.run, "shadow_agents.yaml")

  eclipse_metrics.path <- file.path(sim.run, "eclipse_metrics.jsonl")

  peerlist_dump.path <- file.path(sim.run, "daemon_logs", relay.id, "peerlist_dump.jsonl")


  input_config <- yaml::read_yaml(input_config.path)

  relays <- input_config$agents[grepl("relay", names(input_config$agents))]

  relays.roles <- sapply(relays, FUN = function(x) x$attributes$eclipse_role)
  relays.adversary <- names(relays)[relays.roles %in% c("attacker", "injector")]
  relays.honest <- names(relays)[ ! relays.roles %in% c("attacker", "injector")]

  # Check which one is the target
  names(relays[relays.roles == "target"])

  shadow_agents <- yaml::read_yaml(shadow_agents.path)

  shadow_agents <- data.table(agent = names(shadow_agents$hosts), ip = sapply(shadow_agents$hosts, FUN = function(x) {x$ip_addr}))

  adversary.ips <- shadow_agents[agent %in% relays.adversary, ip]
  honest.ips <- shadow_agents[agent %in% relays.honest, ip]

  peerlist.dump <- readLines(peerlist_dump.path)

  peerlist.dump <- lapply(peerlist.dump, FUN = RJSONIO::fromJSON)

  ####

  honest.adversary.whitelist.n <- lapply(peerlist.dump, FUN = function(x) {
    white_list <- data.table::rbindlist(x$white)
    data.table::setnames(white_list, c("V1", "V2"), c("peer", "last_seen"))
    data.table::setorder(white_list, -last_seen)
    white_list[, ip := gsub(":.*", "", peer)]
    honest.peers <- sum( white_list$ip %in% honest.ips)
    adversary.peers <- sum( white_list$ip %in% adversary.ips)
    data.table(time = as.POSIXct(x$t), honest.peers = honest.peers, adversary.peers = adversary.peers)
  })

  honest.adversary.whitelist.n <- data.table::rbindlist(honest.adversary.whitelist.n)

  honest.adversary.whitelist.n <- data.table::melt(honest.adversary.whitelist.n, id.vars = "time")

  honest.adversary.graylist.n <- lapply(peerlist.dump, FUN = function(x) {
    if (length(x$gray) == 0) {
      return( data.table(time = as.POSIXct(x$t), honest.peers = 0, adversary.peers = 0) )
    }
    gray_list <- data.table::rbindlist(x$gray)
    data.table::setnames(gray_list, c("V1", "V2"), c("peer", "last_seen"))
    data.table::setorder(gray_list, -last_seen)
    gray_list[, ip := gsub(":.*", "", peer)]
    honest.peers <- sum( gray_list$ip %in% honest.ips)
    adversary.peers <- sum( gray_list$ip %in% adversary.ips)
    data.table(time = as.POSIXct(x$t), honest.peers = honest.peers, adversary.peers = adversary.peers)
  })

  honest.adversary.graylist.n <- data.table::rbindlist(honest.adversary.graylist.n)

  honest.adversary.graylist.n <- data.table::melt(honest.adversary.graylist.n, id.vars = "time")

  # TODO: Trash peers




  eclipse.metrics <- readLines(eclipse_metrics.path)

  # https://stackoverflow.com/questions/76622247/read-a-jsonl-json-lines-file

  eclipse.metrics <- lapply(eclipse.metrics, FUN = RJSONIO::fromJSON)


  eclipse.metrics <- lapply(eclipse.metrics, FUN = function(x) {
    out_attacker <- x$targets[[1]]$out_attacker
    time <- x$sim_t + as.POSIXct("2000-01-01 00:00:00")
    data.table(time = time, out_attacker = out_attacker)
  })

  eclipse.metrics <- data.table::rbindlist(eclipse.metrics)


  list(
    honest.adversary.whitelist.n = honest.adversary.whitelist.n,
    honest.adversary.graylist.n = honest.adversary.graylist.n,
    eclipse.metrics = eclipse.metrics
  )

}


message(base::date(), " Starting extract.metrics()")


defense.unreachable.metrics <- extract.metrics(sim.run.defense, relay.id.unreachable)
defense.reachable.metrics <- extract.metrics(sim.run.defense, relay.id.reachable)

statusquo.unreachable.metrics <- extract.metrics(sim.run.status.quo, relay.id.unreachable)
statusquo.reachable.metrics <- extract.metrics(sim.run.status.quo, relay.id.reachable)

message(base::date(), " Finished extract.metrics()")


reachable.whitelists <- rbindlist(list(
  Proposed_defense = defense.reachable.metrics$honest.adversary.whitelist.n,
  Status_quo = statusquo.reachable.metrics$honest.adversary.whitelist.n), idcol = "sim.run")

reachable.whitelists[, sim.run := factor(sim.run, levels = c("Status_quo", "Proposed_defense"))]
reachable.whitelists[, variable := factor(variable, levels = c("adversary.peers", "honest.peers"))]

if (! dir.exists("images")) {
  dir.create("images")
}


png("../images/white-list-reachable.png", width = 800, height = 800)

p <- ggplot(reachable.whitelists, aes(time, value, colour = variable)) +
  geom_line(linewidth = 2) +
  ggtitle("white_list composition, reachable node")

p + facet_grid(rows = vars(sim.run)) +
  xlab("") + ylab("Peers") +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 20),
    legend.position = "top",
    legend.text = element_text(size = 22),
    panel.border = element_rect(linewidth = 2, colour = "black"),
    strip.text.y = element_text(size = 22),
    legend.title = element_blank())

dev.off()





unreachable.whitelists <- rbindlist(list(
  Proposed_defense = defense.unreachable.metrics$honest.adversary.whitelist.n,
  Status_quo = statusquo.unreachable.metrics$honest.adversary.whitelist.n), idcol = "sim.run")

unreachable.whitelists[, sim.run := factor(sim.run, levels = c("Status_quo", "Proposed_defense"))]
unreachable.whitelists[, variable := factor(variable, levels = c("adversary.peers", "honest.peers"))]


png("../imageswhite-list-unreachable.png", width = 800, height = 800)

p <- ggplot(unreachable.whitelists, aes(time, value, colour = variable)) +
  geom_line(linewidth = 2) +
  ggtitle("white_list composition, unreachable node")

p + facet_grid(rows = vars(sim.run)) +
  xlab("") + ylab("Peers") +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 20),
    legend.position = "top",
    legend.text = element_text(size = 22),
    panel.border = element_rect(linewidth = 2, colour = "black"),
    strip.text.y = element_text(size = 22),
    legend.title = element_blank())

dev.off()






unreachable.outbound.adversaries <- rbindlist(list(
  Proposed_defense = defense.unreachable.metrics$eclipse.metrics,
  Status_quo = statusquo.unreachable.metrics$eclipse.metrics), idcol = "sim.run")

data.table::setnames(unreachable.outbound.adversaries,
  "out_attacker", "Outbound_connections_to_adversary_peers")

unreachable.outbound.adversaries[, sim.run := factor(sim.run, levels = c("Status_quo", "Proposed_defense"))]

png("../imagesoutbound-adversaries-unreachable.png", width = 800, height = 800)

p <- ggplot(unreachable.outbound.adversaries, aes(time, Outbound_connections_to_adversary_peers)) +
  geom_line() +
  ggtitle("Outbound connections to adversary peers\nby unreachable node")

p + facet_grid(rows = vars(sim.run)) +
  xlab("") + ylab("Connections") +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    legend.position = "top",
    legend.text = element_text(size = 22),
    panel.border = element_rect(linewidth = 2, colour = "black"),
    strip.text.y = element_text(size = 22),
    legend.title = element_blank())

dev.off()

message(base::date(), " Finished creating images/white-list-reachable.png, images/white-list-unreachable.png, and images/outbound-adversaries-unreachable.png")





get.connection.data <- function(sim.run, relay.id) {

  input_config.path <- file.path(sim.run, "input_config.yaml")

  shadow_agents.path <- file.path(sim.run, "shadow_agents.yaml")

  eclipse_metrics.path <- file.path(sim.run, "eclipse_metrics.jsonl")

  peerlist_dump.path <- file.path(sim.run, "daemon_logs", relay.id, "peerlist_dump.jsonl")


  input_config <- yaml::read_yaml(input_config.path)

  relays <- input_config$agents[grepl("relay", names(input_config$agents))]

  relays.roles <- sapply(relays, FUN = function(x) x$attributes$eclipse_role)
  relays.adversary <- names(relays)[relays.roles %in% c("attacker", "injector")]
  relays.honest <- names(relays)[ ! relays.roles %in% c("attacker", "injector")]

  # Check which one is the target
  names(relays[relays.roles == "target"])

  shadow_agents <- yaml::read_yaml(shadow_agents.path)

  shadow_agents <- data.table(agent = names(shadow_agents$hosts),
    ip = sapply(shadow_agents$hosts, FUN = function(x) {x$ip_addr}))

  adversary.ips <- shadow_agents[agent %in% relays.adversary, ip]
  honest.ips <- shadow_agents[agent %in% relays.honest, ip]

  peerlist.dump <- readLines(peerlist_dump.path)

  peerlist.dump <- lapply(peerlist.dump, FUN = RJSONIO::fromJSON)



  whitelists <- lapply(peerlist.dump, FUN = function(x) {
    white_list <- data.table::rbindlist(x$white)
    data.table::setnames(white_list, c("V1", "V2"), c("peer", "last_seen"))
    data.table::setorder(white_list, -last_seen)
    white_list[, ip := gsub(":.*", "", peer)]
    white_list[, is.adversary := ip %in% adversary.ips]
    white_list[, time := as.POSIXct(x$t)]
    white_list[, .(time, peer, ip, is.adversary)]
  })

  whitelists <- data.table::rbindlist(whitelists)


  graylists <- lapply(peerlist.dump, FUN = function(x) {
    if (length(x$gray) == 0) {
      return( NULL )
    }
    gray_list <- data.table::rbindlist(x$gray)
    data.table::setnames(gray_list, c("V1", "V2"), c("peer", "last_seen"))
    data.table::setorder(gray_list, -last_seen)
    gray_list[, ip := gsub(":.*", "", peer)]
    gray_list[, is.adversary := ip %in% adversary.ips]
    gray_list[, time := as.POSIXct(x$t)]
    gray_list[, .(time, peer, ip, is.adversary)]
  })

  graylists <- data.table::rbindlist(graylists)




  ##################################


  connection.probe.path <- file.path(sim.run, "shared", "raw_probe",
    paste0("raw_", gsub("monero-", "", relay.id), ".jsonl.gz") )

  connection.probe <- readLines(gzfile(connection.probe.path))

  connection.probe <- lapply(connection.probe, FUN = RJSONIO::fromJSON)

  # connection.probe[[1]]$data$connections[[1]]$address

  connection.probe <- connection.probe[sapply(connection.probe, FUN = function(x) {
    x$kind == "connections"
  })]
  # "info" is mixed with "connections"

  outbound.connections <- lapply(connection.probe, FUN = function(x) {
    connections <- data.table::rbindlist(x$data$connections)
    if (nrow(connections) == 0) {
      return(list(time = x$sim_t, connections = NULL))
    }
    connections <- connections[(! incoming), address]
    list(time = x$sim_t, connections = connections)
  })

  new.outbound.connections <- list()

  for (i in 2:length(outbound.connections)) {
    new.outbound.connections[[i - 1]] <- list(
      time = outbound.connections[[i]]$time + as.POSIXct("2000-01-01 00:00:00"),
      prev.time = outbound.connections[[i - 1]]$time + as.POSIXct("2000-01-01 00:00:00"),
      connections = setdiff(outbound.connections[[i]]$connections, outbound.connections[[i - 1]]$connections)
    )
  }

  poll.times <- whitelists[, unique(time)]
  # Poll times for white and gray list will be the same since whole peer list is probed
  # at once

  peerlist.draws <- lapply(new.outbound.connections, FUN = function(x) {
    most.recent.poll.time <- max(poll.times[poll.times < x$prev.time])
    if (!is.finite(most.recent.poll.time)) { return(NULL) }
    # Want to get the peerlists from the time before the current time, since the
    # new connection could have been drawn any time between the current and previous poll
    # time.
    whitelist.draw.positions.honest <- whitelists[
      time == most.recent.poll.time & (! is.adversary), which(peer %in% x$connections)]
    whitelist.draws.n.honest <- length(whitelist.draw.positions.honest)
    graylist.draws.n.honest <- graylists[
      time == most.recent.poll.time & (! is.adversary), sum(peer %in% x$connections)]

    whitelist.draw.positions.adversary <- whitelists[
      time == most.recent.poll.time & (is.adversary), which(peer %in% x$connections)]
    whitelist.draws.n.adversary <- length(whitelist.draw.positions.adversary)
    graylist.draws.n.adversary <- graylists[
      time == most.recent.poll.time & (is.adversary), sum(peer %in% x$connections)]

    # use tibble::lst so we don't have to do `name = name` in list syntax
    tibble::lst(time = x$time, whitelist.draw.positions.honest,
      whitelist.draws.n.honest, graylist.draws.n.honest,
      whitelist.draw.positions.adversary, whitelist.draws.n.adversary, graylist.draws.n.adversary)

  })
  # This takes a while to complete

  peerlist.draws <- peerlist.draws[lengths(peerlist.draws) > 0]
  # get rid of NULL elements


  hour.categories <- cut.POSIXt(as.POSIXct(sapply(peerlist.draws, FUN = function(x) x$time)), breaks = "3 hour")
  levels(hour.categories) <- gsub(":00$", "", levels(hour.categories))
  levels(hour.categories) <- gsub("^2000-01-", "", levels(hour.categories))

  violin.data <- list()

  for (i in seq_len(uniqueN(hour.categories))) {

    peerlist.draws.section <- peerlist.draws[which(as.numeric(hour.categories) == i)]

    whitelist.draw.positions.honest <- unlist(lapply(peerlist.draws.section,
      FUN = function(x) {x$whitelist.draw.positions.honest} ))
    whitelist.draw.positions.adversary <- unlist(lapply(peerlist.draws.section,
      FUN = function(x) {x$whitelist.draw.positions.adversary} ))
    graylist.draws.n.honest <- sum(sapply(peerlist.draws.section,
      FUN = function(x) {x$graylist.draws.n.honest} ))
    graylist.draws.n.adversary <- sum(sapply(peerlist.draws.section,
      FUN = function(x) {x$graylist.draws.n.adversary} ))

    if (length(whitelist.draw.positions.honest) == 0) {
      whitelist.draw.positions.honest <- NA
    }
    if (length(whitelist.draw.positions.adversary) == 0) {
      whitelist.draw.positions.adversary <- NA
    }
    # Need to have the NA or else the violin density plots won;t
    # be on a consistent side due to missing data
    violin.data[[i]] <- rbind(
      data.table(time = unique(hour.categories)[i], pos = whitelist.draw.positions.honest,
        whitelist.draws.n = length(whitelist.draw.positions.honest),
        graylist.draws.n = graylist.draws.n.honest, type = "honest"),
      data.table(time = unique(hour.categories)[i], pos = whitelist.draw.positions.adversary,
        whitelist.draws.n = length(whitelist.draw.positions.adversary),
        graylist.draws.n = graylist.draws.n.adversary, type = "adversary")
    )

  }

  violin.data <- data.table::rbindlist(violin.data)

  violin.data[, type := factor(type)]

  tibble::lst(violin.data, outbound.connections, new.outbound.connections)

}




# tidysdm package also has a split violin function, but it's based on the
# StackOverflow answer and it has heavy dependencies, including these
# system packages: https://r-spatial.github.io/sf/#linux
# https://evolecolgroup.github.io/tidysdm/reference/geom_split_violin.html
# https://cran.r-project.org/web/packages/tidysdm/index.html


# These functions are from https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2
# with the "nudge" addition.


GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin",
  ggplot2::GeomViolin,
  draw_group = function(self,
    data,
    ...,
    # add the nudge here
    nudge = 0,
    draw_quantiles = NULL) {
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1, "group"]
    newdata <- plyr::arrange(transform(data,
      x = if (grp %% 2 == 1) xminv else xmaxv),
      if (grp %% 2 == 1) y else -y)
    newdata <- rbind(newdata[1, ],
      newdata,
      newdata[nrow(newdata), ],
      newdata[1, ])
    newdata[c(1, nrow(newdata)-1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

    # now nudge them apart
    newdata$x <- ifelse(newdata$group %% 2 == 1,
      newdata$x - nudge,
      newdata$x + nudge)

    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {

      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))

      quantiles <- ggplot2:::create_quantile_segment_frame(data,
        draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y")),
        drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin",
        grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...),
          quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin",
        ggplot2::GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function(mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "identity",
  # nudge param here
  nudge = 0,
  ...,
  draw_quantiles = NULL,
  trim = TRUE,
  scale = "area",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  ggplot2::layer(data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSplitViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim,
      scale = scale,
      # don't forget the nudge
      nudge = nudge,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...))
}




message(base::date(), " Starting get.connection.data()")

unreachable.connection.data.defense <- get.connection.data(
  sim.run = sim.run.defense, relay.id = relay.id.unreachable)

unreachable.connection.data.status.quo <- get.connection.data(
  sim.run = sim.run.status.quo, relay.id = relay.id.unreachable)

reachable.connection.data.defense <- get.connection.data(
  sim.run = sim.run.defense, relay.id = relay.id.reachable)

reachable.connection.data.status.quo <- get.connection.data(
  sim.run = sim.run.status.quo, relay.id = relay.id.reachable)

message(base::date(), " Finished get.connection.data()")

max.position <- max(c(
  unreachable.connection.data.defense$violin.data$pos,
  unreachable.connection.data.status.quo$violin.data$pos,
  reachable.connection.data.defense$violin.data$pos,
  reachable.connection.data.status.quo$violin.data$pos
), na.rm = TRUE)


p1.theme <- theme(axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  legend.position = "top",
  axis.text = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  plot.title = element_text(size = 30),
  plot.subtitle = element_text(size = 20),
  legend.text = element_text(size = 22),
  legend.title = element_blank())

p2.theme <- theme(axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(size = 22),
    legend.title = element_blank())

p3.theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 18),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(size = 22),
    legend.title = element_blank())



png("../imagespeerlist-draws-unreachable-defense.png", width = 800, height = 800)


connection.data <- unreachable.connection.data.defense

p1 <- ggplot(connection.data$violin.data, aes(x = time, y = pos, fill = type)) +
  geom_split_violin(scale = "width", nudge = 0.04, drop = FALSE) +
  ggtitle("Peers drawn as new outbound connections by\nunreachable node (proposed defense scenario)") +
  ylab("Position of white_list draws (log scale)") +
  scale_y_continuous(trans = c("log10", "reverse"), limits = c(1, max.position)) +
  p1.theme


max.bar.height <- connection.data$violin.data[, max(c(whitelist.draws.n, graylist.draws.n))]

p2.specific <- ggplot(connection.data$violin.data, aes(x = time, y = whitelist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("white_list") + ylim(c(0, max.bar.height)) + p2.theme

p3.specific <- ggplot(connection.data$violin.data, aes(x = time, y = graylist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("gray_list") + ylim(c(0, max.bar.height)) + p3.theme


p1 + p2.specific + p3.specific + plot_layout(nrow = 3, heights = c(0.6, 0.2, 0.2))

dev.off()




png("../imagespeerlist-draws-unreachable-status-quo.png", width = 800, height = 800)


connection.data <- unreachable.connection.data.status.quo

p1 <- ggplot(connection.data$violin.data, aes(x = time, y = pos, fill = type)) +
  geom_split_violin(scale = "width", nudge = 0.04, drop = FALSE) +
  ggtitle("Peers drawn as new outbound connections by\nunreachable node (status quo scenario)") +
  ylab("Position of white_list draws (log scale)") +
  scale_y_continuous(trans = c("log10", "reverse"), limits = c(1, max.position)) +
  p1.theme


max.bar.height <- connection.data$violin.data[, max(c(whitelist.draws.n, graylist.draws.n))]

p2.specific <- ggplot(connection.data$violin.data, aes(x = time, y = whitelist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("white_list") + ylim(c(0, max.bar.height)) + p2.theme

p3.specific <- ggplot(connection.data$violin.data, aes(x = time, y = graylist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("gray_list") + ylim(c(0, max.bar.height)) + p3.theme


p1 + p2.specific + p3.specific + plot_layout(nrow = 3, heights = c(0.6, 0.2, 0.2))

dev.off()





png("../imagespeerlist-draws-reachable-defense.png", width = 800, height = 800)


connection.data <- reachable.connection.data.defense

p1 <- ggplot(connection.data$violin.data, aes(x = time, y = pos, fill = type)) +
  geom_split_violin(scale = "width", nudge = 0.04, drop = FALSE) +
  ggtitle("Peers drawn as new outbound connections by\nreachable node (proposed defense scenario)") +
  ylab("Position of white_list draws (log scale)") +
  scale_y_continuous(trans = c("log10", "reverse"), limits = c(1, max.position)) +
  p1.theme


max.bar.height <- connection.data$violin.data[, max(c(whitelist.draws.n, graylist.draws.n))]

p2.specific <- ggplot(connection.data$violin.data, aes(x = time, y = whitelist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("white_list") + ylim(c(0, max.bar.height)) + p2.theme

p3.specific <- ggplot(connection.data$violin.data, aes(x = time, y = graylist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("gray_list") + ylim(c(0, max.bar.height)) + p3.theme


p1 + p2.specific + p3.specific + plot_layout(nrow = 3, heights = c(0.6, 0.2, 0.2))

dev.off()






png("../imagespeerlist-draws-reachable-status-quo.png", width = 800, height = 800)


connection.data <- reachable.connection.data.status.quo

p1 <- ggplot(connection.data$violin.data, aes(x = time, y = pos, fill = type)) +
  geom_split_violin(scale = "width", nudge = 0.04, drop = FALSE) +
  ggtitle("Peers drawn as new outbound connections by\nreachable node (status quo scenario)") +
  ylab("Position of white_list draws (log scale)") +
  scale_y_continuous(trans = c("log10", "reverse"), limits = c(1, max.position)) +
  p1.theme


max.bar.height <- connection.data$violin.data[, max(c(whitelist.draws.n, graylist.draws.n))]

p2.specific <- ggplot(connection.data$violin.data, aes(x = time, y = whitelist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("white_list") + ylim(c(0, max.bar.height)) + p2.theme

p3.specific <- ggplot(connection.data$violin.data, aes(x = time, y = graylist.draws.n, fill = type)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  ylab("gray_list") + ylim(c(0, max.bar.height)) + p3.theme


p1 + p2.specific + p3.specific + plot_layout(nrow = 3, heights = c(0.6, 0.2, 0.2))

dev.off()


message(base::date(), " Finished creating images/peerlist-draws-unreachable-defense.png, images/peerlist-draws-unreachable-status-quo.png, images/peerlist-draws-reachable-defense.png, and images/peerlist-draws-reachable-status-quo.png")





