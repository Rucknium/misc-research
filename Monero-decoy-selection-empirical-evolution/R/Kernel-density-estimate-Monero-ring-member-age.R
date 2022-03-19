
library(RPostgreSQL)
library(data.table)
library(lubridate)
library(viridis)
library(animation)
library(Cairo)

dbDriver("PostgreSQL")

drv <- dbDriver("PostgreSQL")

dbname.arg   <- ""
host.arg     <- ""
port.arg     <- NA # numeric
user.arg     <- ""
password.arg <- ""

con <- dbConnect(drv, dbname = dbname.arg,
  host = host.arg, port = port.arg,
  user = user.arg, password = password.arg)



xmr.rings <- dbGetQuery(con, "SELECT tx_block_height,tx_block_timestamp,tx_block_tx_index,tx_vin_index,tx_vin_amount,tx_vin_ringmember_index,ringmember_block_height,ringmember_block_timestamp,ringmember_block_tx_index,ringmember_tx_txo_index,ringmember_txo_amount_index FROM tx_ringmember_list")



setDT(xmr.rings)

xmr.rings[, tx_block_timestamp.POSIXct := as.POSIXct(tx_block_timestamp, origin = "1970-01-01")]

xmr.rings[, tx_block_timestamp.year.week := 
    paste0(lubridate::isoyear(tx_block_timestamp.POSIXct), "-",
      formatC(lubridate::isoweek(tx_block_timestamp.POSIXct), width = 2, flag = "0"))]

xmr.rings[, tx_block_timestamp.year.month := 
    paste0(lubridate::year(tx_block_timestamp.POSIXct), "-",
      formatC(lubridate::month(tx_block_timestamp.POSIXct), width = 2, flag = "0"))]


xmr.rings[, log.ring.member.age.seconds := log(tx_block_timestamp - ringmember_block_timestamp)]

kde.weekly.log.ring.member.age.seconds <- xmr.rings[, .(log.ring.member.age.seconds.kde =
    density(log.ring.member.age.seconds[is.finite(log.ring.member.age.seconds)], bw = "SJ")[c("x", "y")]),
  by = tx_block_timestamp.year.week]

kde.monthly.log.ring.member.age.seconds <- xmr.rings[, .(log.ring.member.age.seconds.kde =
    density(log.ring.member.age.seconds[is.finite(log.ring.member.age.seconds)], bw = "SJ")[c("x", "y")]),
  by = tx_block_timestamp.year.month]

max.log.ring.member.age.seconds <- max(xmr.rings$log.ring.member.age.seconds, na.rm = TRUE)


year.week.range <- sort(unique(xmr.rings$tx_block_timestamp.year.week))

year.month.range <- sort(unique(xmr.rings$tx_block_timestamp.year.month))



monthly.historical.length <- 12

col.palette <- viridis::magma(monthly.historical.length)

ani.options(interval = 1, ani.height = 700, ani.width = 700)

saveGIF({
  
  for (i in seq_len(length(year.month.range)) ) {
    
    subset.seq <- 1:i
    subset.seq <- subset.seq[max(1, i - monthly.historical.length + 1):max(subset.seq)]
    col.palette.subset <- col.palette[max(1, monthly.historical.length - i + 1):monthly.historical.length]
    col.palette.subset[length(col.palette.subset)] <- "green"
    
    year.month.range.subset <- year.month.range[subset.seq]
    
    par(bg = 'black', fg = 'white') 
    
    plot(1, 
      main = "Distribution of Monero Ring Member Age, by Month of Transaction",
      sub = year.month.range[i],
      ylab = "Kernel density estimate", xlab = "ln(age in seconds)",
      col.lab = "white", col.axis = "white", col.main = "white", col.sub = "white",
      col = "transparent",
      xlim = c(5, max.log.ring.member.age.seconds), ylim = c(0, 0.45))
    
    for ( j in seq_along(subset.seq)) {
      
      lines(
        kde.monthly.log.ring.member.age.seconds[
          tx_block_timestamp.year.month %in% year.month.range.subset[j], log.ring.member.age.seconds.kde][[1]],
        kde.monthly.log.ring.member.age.seconds[
          tx_block_timestamp.year.month %in% year.month.range.subset[j], log.ring.member.age.seconds.kde][[2]],
        col = col.palette.subset[j],
        lwd = ifelse(j == length(subset.seq), 4, 1)
      )
      
    }
    
  }
  
}, movie.name = "Kernel-density-estimate-Monero-ring-member-age-monthly.gif")



weekly.historical.length <- 52/2

col.palette <- viridis::magma(weekly.historical.length)

ani.options(interval = 0.5, ani.height = 700, ani.width = 700)

saveGIF({
  
  for (i in seq_len(length(year.week.range)) ) {
    
    subset.seq <- 1:i
    subset.seq <- subset.seq[max(1, i - weekly.historical.length + 1):max(subset.seq)]
    col.palette.subset <- col.palette[max(1, weekly.historical.length - i + 1):weekly.historical.length]
    col.palette.subset[length(col.palette.subset)] <- "green"
    
    year.week.range.subset <- year.week.range[subset.seq]
    
    par(bg = 'black', fg = 'white') 
    
    plot(1,
      main = "Distribution of Monero Ring Member Age, by Week of Transaction",
      sub = year.week.range[i],
      ylab = "Kernel density estimate", xlab = "ln(age in seconds)",
      col.lab = "white", col.axis = "white", col.main = "white", col.sub = "white",
      col = "transparent",
      xlim = c(5, max.log.ring.member.age.seconds), ylim = c(0, 0.45))
    
    for ( j in seq_along(subset.seq)) {
      
      lines(
        kde.weekly.log.ring.member.age.seconds[
          tx_block_timestamp.year.week %in% year.week.range.subset[j], log.ring.member.age.seconds.kde][[1]],
        kde.weekly.log.ring.member.age.seconds[
          tx_block_timestamp.year.week %in% year.week.range.subset[j], log.ring.member.age.seconds.kde][[2]],
        col = col.palette.subset[j],
        lwd = ifelse(j == length(subset.seq), 4, 1)
      )
      
    }
    
  }
  
}, movie.name = "Kernel-density-estimate-Monero-ring-member-age-weekly.gif")


