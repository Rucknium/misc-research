library(data.table)
library(ggplot2)
library(scales)
# NOTE: Also need lubridate package installed, but not loading it due to 
# it masking functions


spent.status.by.date <- spent.status.by.date[ ! is.na(block_time.date), ]

spent.status.by.date.reshaped <- melt(spent.status.by.date, id.vars = c("block_time.date"),
  measure.vars = c("perc.value.cumsum", "unspent.perc.value.cumsum"))


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

spent.status.by.date.reshaped[, block_time.date := as.POSIXct(block_time.date)]
spent.status.by.date.reshaped[, variable := 
    factor(variable, levels = c("perc.value.cumsum", "unspent.perc.value.cumsum"))]

# #FF9900 BTC color
# https://gist.github.com/paladini/ef383fce1b782d919898
# #0AC18E BCH color
# https://bitcoincashstandards.org/

png(paste0(bch.data.dir, "preliminary-pre-fork-BCH-spent-status.png"), width = 800, height = 2000)

print(
  ggplot(spent.status.by.date.reshaped, aes(x = block_time.date, y=value, fill=variable)) + 
    geom_area(alpha = 0.6 , size = 0, colour = "black") + coord_flip() + 
    scale_x_continuous(trans = rev_date) + 
    scale_fill_manual(values = c("#0AC18E", "purple"), breaks =  rev(c("perc.value.cumsum", "unspent.perc.value.cumsum"))) +
    ylab("\t\t\t\t\t\tPercent          github.com/Rucknium") +
    theme(legend.position = "top", axis.title.y = element_blank(), 
      axis.text = element_text(size = 20), axis.title.x = element_text(size = 20),
      legend.title = element_blank(), legend.text = element_text(size = 15)) + 
    geom_vline(xintercept = as.POSIXct("2017-11-12"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-11-12"), label = "Max BCH/BTC Exchange Rate", y = 25), colour = "white", size = 7.5) + 
    geom_vline(xintercept = as.POSIXct("2017-12-20"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2017-12-20"), label = "Max BCH/USD Exchange Rate", y = 25), colour = "white", size = 7.5) + 
    geom_vline(xintercept = as.POSIXct("2018-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2018-11-15"), label = "BSV Hard Fork", y = 12), colour = "white", size = 7.5) + 
    geom_vline(xintercept = as.POSIXct("2020-11-15"), linetype = 3) +
    geom_text(aes(x = as.POSIXct("2020-11-15"), label = "BCHABC Hard Fork", y = 15), colour = "white", size = 7.5)
)
# https://en.wikipedia.org/wiki/List_of_bitcoin_forks
dev.off()


