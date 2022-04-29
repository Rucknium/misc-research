
library(data.table)
library(ggplot2)
library(scales)
library(Cairo)
# NOTE: Also need lubridate package installed, but not loading it due to 
# it masking functions



sum.value.pre.fork <- sum(spent.status.by.day[1, 
  .(value.btc.unspent.bch.unspent, value.btc.spent.bch.unspent,
    value.btc.unspent.bch.spent, value.btc.spent.bch.spent)])

sum.outputs.pre.fork <- sum(spent.status.by.day[1, 
  .(outputs.btc.unspent.bch.unspent, outputs.btc.spent.bch.unspent,
    outputs.btc.unspent.bch.spent, outputs.btc.spent.bch.spent)])


current.status <- spent.status.by.day[block_time.date == "2022-03-31", ]

current.status[, 
  .(value.btc.unspent.bch.unspent, value.btc.spent.bch.unspent,
    value.btc.unspent.bch.spent, value.btc.spent.bch.spent)]

100 * current.status[, 
  .(value.btc.unspent.bch.unspent, value.btc.spent.bch.unspent,
    value.btc.unspent.bch.spent, value.btc.spent.bch.spent)] / sum.value.pre.fork


current.status[, 
  .(outputs.btc.unspent.bch.unspent, outputs.btc.spent.bch.unspent,
    outputs.btc.unspent.bch.spent, outputs.btc.spent.bch.spent)]

100 * current.status[, 
  .(outputs.btc.unspent.bch.unspent, outputs.btc.spent.bch.unspent,
    outputs.btc.unspent.bch.spent, outputs.btc.spent.bch.spent)] / sum.outputs.pre.fork






spent.status.by.day.value.reshaped <- melt(spent.status.by.day[, 
  .(block_time.date, value.btc.unspent.bch.unspent, value.btc.spent.bch.unspent,
    value.btc.unspent.bch.spent, value.btc.spent.bch.spent)], id.vars = c("block_time.date"),
  measure.vars = c("value.btc.unspent.bch.unspent", "value.btc.spent.bch.unspent",
    "value.btc.unspent.bch.spent", "value.btc.spent.bch.spent"))


spent.status.by.day.value.reshaped[, block_time.date := as.POSIXct(block_time.date)]
spent.status.by.day.value.reshaped[, variable := 
    factor(variable, levels = c("value.btc.unspent.bch.unspent", "value.btc.unspent.bch.spent",
      "value.btc.spent.bch.unspent", "value.btc.spent.bch.spent"))]


spent.status.by.day.outputs.reshaped <- melt(spent.status.by.day[, 
  .(block_time.date, outputs.btc.unspent.bch.unspent, outputs.btc.spent.bch.unspent,
    outputs.btc.unspent.bch.spent, outputs.btc.spent.bch.spent)], id.vars = c("block_time.date"),
  measure.vars = c("outputs.btc.unspent.bch.unspent", "outputs.btc.spent.bch.unspent",
    "outputs.btc.unspent.bch.spent", "outputs.btc.spent.bch.spent"))


spent.status.by.day.outputs.reshaped[, block_time.date := as.POSIXct(block_time.date)]
spent.status.by.day.outputs.reshaped[, variable := 
    factor(variable, levels = c("outputs.btc.unspent.bch.unspent", "outputs.btc.spent.bch.unspent",
      "outputs.btc.unspent.bch.spent", "outputs.btc.spent.bch.spent"))]




trans.matrix.prep.value.reshaped <- melt(trans.matrix.prep[, 
  .(block_time.date, value.uu.to.su, value.uu.to.us, value.uu.to.ss,
    value.su.to.ss, value.us.to.ss)], id.vars = c("block_time.date"),
  measure.vars = c("value.uu.to.su", "value.uu.to.us", "value.uu.to.ss",
    "value.su.to.ss", "value.us.to.ss"))


trans.matrix.prep.value.reshaped[, block_time.date := as.POSIXct(block_time.date)]
trans.matrix.prep.value.reshaped[, variable := 
    factor(variable, levels = c("value.uu.to.su", "value.uu.to.us", "value.uu.to.ss",
      "value.su.to.ss", "value.us.to.ss"))]





trans.matrix.prep.outputs.reshaped <- melt(trans.matrix.prep[, 
  .(block_time.date, outputs.uu.to.su, outputs.uu.to.us, outputs.uu.to.ss,
    outputs.su.to.ss, outputs.us.to.ss)], id.vars = c("block_time.date"),
  measure.vars = c("outputs.uu.to.su", "outputs.uu.to.us", "outputs.uu.to.ss",
    "outputs.su.to.ss", "outputs.us.to.ss"))


trans.matrix.prep.outputs.reshaped[, block_time.date := as.POSIXct(block_time.date)]
trans.matrix.prep.outputs.reshaped[, variable := 
    factor(variable, levels = c("outputs.uu.to.su", "outputs.uu.to.us", "outputs.uu.to.ss",
      "outputs.su.to.ss", "outputs.us.to.ss"))]



date.breaks <- seq.POSIXt(min(spent.status.by.day.value.reshaped$block_time.date), 
  max(spent.status.by.day.value.reshaped$block_time.date), by = "month")



c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- scales::as.trans(a)
  b <- scales::as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
# Thanks to https://stackoverflow.com/questions/59542697/reverse-datetime-posixct-data-axis-in-ggplot-version-3

rev_date <- c_trans("reverse", "time")

# #FF9900 BTC color
# https://gist.github.com/paladini/ef383fce1b782d919898
# #0AC18E BCH color
# https://bitcoincashstandards.org/

png(paste0(bch.data.dir, "preliminary-pre-fork-BTC-BCH-spent-status-by-value.png"), width = 800, height = 2000)

print(
  ggplot(spent.status.by.day.value.reshaped, aes(x = block_time.date, y = value, fill = variable)) + 
    ggtitle("Spent status of Pre-fork BTC and BCH by Bitcoin Value") +
    geom_area(alpha = 0.6 , size = 0, colour = "black") + coord_flip() + 
    scale_x_continuous(trans = rev_date, breaks = date.breaks, labels = date_format("%b-%Y"), expand = c(0, 0)) + 
    scale_y_continuous(breaks = sum.value.pre.fork * seq(0, 1, by = 0.1), 
      labels = scales::percent_format(scale = 100 * 1/sum.value.pre.fork, accuracy = 1), expand = c(0, 0)) +
    scale_fill_manual(
      labels = c("BTC & BCH spent", "BTC spent & BCH unspent", "BTC unspent & BCH spent", "BTC & BCH unspent"),
      values = c("purple", "#FF9900", "#0AC18E", "darkgrey"), 
      breaks =  c("value.btc.spent.bch.spent", "value.btc.spent.bch.unspent", 
        "value.btc.unspent.bch.spent", "value.btc.unspent.bch.unspent")) +
    ylab("Percentage of Pre-fork Bitcoin Value") +
    theme(legend.position = "top", axis.title.y = element_blank(), 
      plot.title = element_text(size = 27),
      axis.text = element_text(size = 20), axis.title.x = element_text(size = 20),
      legend.title = element_blank(), legend.text = element_text(size = 14)) + 
    geom_vline(xintercept = as.POSIXct("2017-11-12"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-11-12"), label = "Max BCH/BTC Exchange Rate",
      y = 0.75 * sum.value.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2017-12-20"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-12-20"), label = "Max BCH/USD Exchange Rate",
      y = 0.75 * sum.value.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2018-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2018-11-15"), label = "BSV Hard Fork",
      y = 0.25 * sum.value.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2020-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2020-11-15"), label = "BCHABC Hard Fork",
      y = 0.25 * sum.value.pre.fork), colour = "black", size = 6, check_overlap = TRUE) +
    geom_text(aes(x = as.POSIXct("2022-03-15"), label = "github.com/Rucknium",
      y = 0.87 * sum.value.pre.fork), colour = "black", size = 6, check_overlap = TRUE)
)
# https://en.wikipedia.org/wiki/List_of_bitcoin_forks
# expand = c(0, 0) due to:
# https://stackoverflow.com/questions/48611719/remove-inner-padding-in-ggplot
dev.off()



png(paste0(bch.data.dir, "preliminary-pre-fork-BTC-BCH-spent-status-by-outputs.png"), width = 800, height = 2000)

print(
  ggplot(spent.status.by.day.outputs.reshaped, aes(x = block_time.date, y = value, fill = variable)) + 
    ggtitle("Spent status of Pre-fork BTC and BCH by Number of Outputs") +
    geom_area(alpha = 0.6 , size = 0, colour = "black") + coord_flip() + 
    scale_x_continuous(trans = rev_date, breaks = date.breaks, labels = date_format("%b-%Y"), expand = c(0, 0)) + 
    scale_y_continuous(breaks = sum.outputs.pre.fork * seq(0, 1, by = 0.1), 
      labels = scales::percent_format(scale = 100 * 1/sum.outputs.pre.fork, accuracy = 1), expand = c(0, 0)) +
    scale_fill_manual(
      labels = c("BTC & BCH spent", "BTC spent & BCH unspent", "BTC unspent & BCH spent", "BTC & BCH unspent"),
      values = c("purple", "#FF9900", "#0AC18E", "darkgrey"), 
      breaks =  c("outputs.btc.spent.bch.spent", "outputs.btc.spent.bch.unspent", 
        "outputs.btc.unspent.bch.spent", "outputs.btc.unspent.bch.unspent")) +
    ylab("Percentage of Pre-fork Outputs") +
    theme(legend.position = "top", axis.title.y = element_blank(), 
      plot.title = element_text(size = 26),
      axis.text = element_text(size = 20), axis.title.x = element_text(size = 20),
      legend.title = element_blank(), legend.text = element_text(size = 14)) + 
    geom_vline(xintercept = as.POSIXct("2017-11-12"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-11-12"), label = "Max BCH/BTC Exchange Rate",
      y = 0.75 * sum.outputs.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2017-12-20"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-12-20"), label = "Max BCH/USD Exchange Rate",
      y = 0.75 * sum.outputs.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2018-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2018-11-15"), label = "BSV Hard Fork",
      y = 0.10 * sum.outputs.pre.fork), colour = "black", size = 6, check_overlap = TRUE) + 
    geom_vline(xintercept = as.POSIXct("2020-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2020-11-15"), label = "BCHABC Hard Fork",
      y = 0.15 * sum.outputs.pre.fork), colour = "black", size = 6, check_overlap = TRUE) +
    geom_text(aes(x = as.POSIXct("2022-03-15"), label = "github.com/Rucknium",
      y = 0.87 * sum.outputs.pre.fork), colour = "black", size = 6, check_overlap = TRUE)
)
dev.off()



ann_text.value <- data.frame(
  block_time.date = as.POSIXct("2022-03-15"),
  value = 300000,
  variable = factor("value.us.to.ss", levels = levels(trans.matrix.prep.value.reshaped$variable)))
# Due to
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2


png(paste0(bch.data.dir, "preliminary-pre-fork-BTC-BCH-trans-matrix-by-value.png"), width = 800, height = 2000)

print(
  ggplot(trans.matrix.prep.value.reshaped, aes(x = block_time.date, y = value, fill = variable)) + 
    ggtitle("State Transition of Pre-fork BTC and BCH by Bitcoin Value\nKEY: {BTC Spent}{BCH Spent} to {BTC Spent}{BCH Spent}") +
    geom_line(aes(color = variable)) + coord_flip() + 
    scale_x_continuous(trans = rev_date, labels = date_format("%b-%Y"), expand = c(0, 0),
      breaks = date.breaks) +
    scale_y_continuous(labels = scales::comma) +
    facet_grid(. ~ variable, labeller = labeller(variable = 
        c(value.uu.to.su = "FF to TF", value.uu.to.us = "FF to FT", value.uu.to.ss = "FF to TT",
          value.su.to.ss = "TF to TT", value.us.to.ss = "FT to TT" ))) +
    ylab("Quantity of Bitcoin Value Transitioned per Day") +
    theme(legend.position = "none", axis.title.y = element_blank(),
      strip.text.x = element_text(size = 20), plot.title = element_text(size = 24),
      axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
      axis.text.x = element_text(angle = 270)) + 
    geom_vline(xintercept = as.POSIXct("2017-11-12"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2017-12-20"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2018-11-15"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2020-11-15"), linetype = 3) + 
    geom_text(data = ann_text.value, label = "github.com/Rucknium",
      colour = "black", size = 4.5, check_overlap = TRUE)
)
dev.off()



ann_text.output <- data.frame(
  block_time.date = as.POSIXct("2022-03-15"),
  value = 280000,
  variable = factor("outputs.us.to.ss", levels = levels(trans.matrix.prep.outputs.reshaped$variable)))

png(paste0(bch.data.dir, "preliminary-pre-fork-BTC-BCH-trans-matrix-by-outputs.png"), width = 800, height = 2000)

print(
  ggplot(trans.matrix.prep.outputs.reshaped, aes(x = block_time.date, y = value, fill = variable)) + 
    ggtitle("State Transition of Pre-fork BTC and BCH by Number of Outputs\nKEY: {BTC Spent}{BCH Spent} to {BTC Spent}{BCH Spent}") +
    geom_line(aes(color = variable)) + coord_flip() + 
    scale_x_continuous(trans = rev_date, labels = date_format("%b-%Y"), expand = c(0, 0),
      breaks = date.breaks) +
    scale_y_continuous(labels = scales::comma) +
    facet_grid(. ~ variable, labeller = labeller(variable = 
        c(outputs.uu.to.su = "FF to TF", outputs.uu.to.us = "FF to FT", outputs.uu.to.ss = "FF to TT",
          outputs.su.to.ss = "TF to TT", outputs.us.to.ss = "FT to TT" ))) +
    ylab("Number of Transitioned Outputs per Day") +
    theme(legend.position = "none", axis.title.y = element_blank(),
      strip.text.x = element_text(size = 20), plot.title = element_text(size = 24),
      axis.text = element_text(size = 15), axis.title.x = element_text(size = 15),
      axis.text.x = element_text(angle = 270)) + 
    geom_vline(xintercept = as.POSIXct("2017-11-12"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2017-12-20"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2018-11-15"), linetype = 3) +
    geom_vline(xintercept = as.POSIXct("2020-11-15"), linetype = 3) + 
    geom_text(data = ann_text.output, label = "github.com/Rucknium",
      colour = "black", size = 4.5, check_overlap = TRUE)
)
dev.off()







