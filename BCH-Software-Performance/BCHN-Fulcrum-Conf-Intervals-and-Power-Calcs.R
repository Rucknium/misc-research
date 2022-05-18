
# Download ODS file from here:
# https://docs.google.com/spreadsheets/d/1zR5XsKws_bKFFVGbf5FMaw0vcjLwFpUTveBCz4__bJg

# MUST add filepath to downloaded .ods file here:
P2P.Results.filepath <- ""

# MUST Install these packages below once by un-commenting these lines:
# install.packages("data.table")
# install.packages("readODS")
# install.packages("boot")
# install.packages("presize")
# install.packages("knitr")

library(data.table)
library(readODS)
library(boot)
library(presize)
library(knitr)


bchn.90p <- read_ods(P2P.Results.filepath, sheet = "bchn-90p")
bchn.90p.start <- read_ods(P2P.Results.filepath, sheet = "bchn_start-90p")
bchn.0p <- read_ods(P2P.Results.filepath, sheet = "bchn-0p")
bchn.0p.start <- read_ods(P2P.Results.filepath, sheet = "bchn-start-0p")

fulcrum.90p <- read_ods(P2P.Results.filepath, sheet = "fulcrum-90p")
fulcrum.0p <- read_ods(P2P.Results.filepath, sheet = "fulcrum-0p")

for (i in c("bchn.90p", "bchn.90p.start", "bchn.0p", "bchn.0p.start", "fulcrum.90p", "fulcrum.0p")) {
  setDT(get(i))
  setnames(get(i), colnames(get(i)), gsub("[^0-9a-zA-Z]", "", colnames(get(i))) )
}

block.phases <- data.table(BlockHeight = 0:max(bchn.90p$BlockHeight), phase = "warmup", stringsAsFactors = FALSE)
block.phases[BlockHeight %in% 145:244, phase := "empty"]
block.phases[BlockHeight %in% 245:254, phase := "fan-out"]
block.phases[BlockHeight %in% 255:259, phase := "steady state 1"]
block.phases[BlockHeight %in% 260:261, phase := "fan-in"]
block.phases[BlockHeight %in% 262:266, phase := "steady state 2"]

bchn.90p <- merge(bchn.90p, bchn.90p.start)
bchn.90p[, bchn.90p.elasped.time := as.numeric(as.POSIXlt(CompletedTimestamp) - as.POSIXlt(StartTimestamp))]

bchn.0p <- merge(bchn.0p, bchn.0p.start)
bchn.0p[, bchn.0p.elasped.time := as.numeric(as.POSIXlt(CompletedTimestamp) - as.POSIXlt(StartTimestamp))]

fulcrum.90p[, fulcrum.90p.elasped.time := ProcessingTimemsec]
fulcrum.0p[, fulcrum.0p.elasped.time := ProcessingTimemsec]

timing.data <- merge(block.phases, bchn.0p[, .(BlockHeight, bchn.0p.elasped.time)])
timing.data <- merge(timing.data, bchn.90p[, .(BlockHeight, bchn.90p.elasped.time)], all.x = TRUE)
timing.data <- merge(timing.data, fulcrum.0p[, .(BlockHeight, fulcrum.0p.elasped.time)], all.x = TRUE)
timing.data <- merge(timing.data, fulcrum.90p[, .(BlockHeight, fulcrum.90p.elasped.time)], all.x = TRUE)


timing.data <- timing.data[ ! phase %in% c("warmup", "empty", "fan-in"), ]

mean.statistic <- function(x, w) {mean(x[w])}

ci.table <- list()
power.table <- list()

set.seed(314)
# bootstrapping involves some randomization

for (i in setdiff(names(timing.data), c("BlockHeight", "phase"))) {
  
  by.output <- by(unlist(timing.data[, ..i]), timing.data$phase, FUN = function(x) {
    
    if ( any(is.na(x))) {
      return(list(boot.ci = c(NA, NA), prec_mean.10 = NA, prec_mean.25 = NA, prec_mean.50 = NA))
    }
    
    list(
      boot.ci = round(boot.ci(boot(x, mean.statistic, R = 10000, stype = "i"), conf = 0.90, type = "perc")$percent[4:5]),
      prec_mean.10 = ceiling(prec_mean(mean(x), sd(x), conf.width = 0.10 * mean(x), conf.level = 0.90)$n),
      prec_mean.25 = ceiling(prec_mean(mean(x), sd(x), conf.width = 0.25 * mean(x), conf.level = 0.90)$n),
      prec_mean.50 = ceiling(prec_mean(mean(x), sd(x), conf.width = 0.50 * mean(x), conf.level = 0.90)$n)
    )
  })
  
  ci.lower <- sapply(by.output, FUN = function(x) {x$boot.ci[1]})
  ci.upper <- sapply(by.output, FUN = function(x) {x$boot.ci[2]})
  
  ci.table[[i]] <- data.table(processing.type = gsub(".elasped.time", "", i), 
    block.type = names(by.output), ci.lower, ci.upper)
  
  prec_mean.10 <- sapply(by.output, FUN = function(x) {x$prec_mean.10})
  prec_mean.25 <- sapply(by.output, FUN = function(x) {x$prec_mean.25})
  prec_mean.50 <- sapply(by.output, FUN = function(x) {x$prec_mean.50})
  
  power.table[[i]] <- data.table(processing.type = gsub(".elasped.time", "", i), 
    block.type = names(by.output), prec_mean.10, prec_mean.25, prec_mean.50)
  
}

ci.table <- data.table::rbindlist(ci.table)
power.table <- data.table::rbindlist(power.table)

colnames(ci.table) <- c("Processing Type", "Block Type", 
  "Lower 90% Confidence Interval", "Upper 90% Confidence Interval")
colnames(power.table) <- c("Processing Type", "Block Type", 
  "N for C.I. width < 10% of mean", "25%", "50%")


knitr::kable(ci.table, format = "pipe")

knitr::kable(power.table, format = "pipe")




