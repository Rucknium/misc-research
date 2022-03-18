# install.packages("RPostgreSQL")
# install.packages("data.table")
 
library(RPostgreSQL)
library(data.table)

dbDriver("PostgreSQL")

drv <- dbDriver("PostgreSQL")

dbname.arg   <- ""
host.arg     <- ""
port.arg     <-   # numeric
user.arg     <- ""
password.arg <- ""

con <- dbConnect(drv, dbname = dbname.arg,
                 host = host.arg, port = port.arg,
                 user = user.arg, password = password.arg)

dbListTables(con)


xmr.tx <- dbGetQuery(con, "SELECT * FROM tx_input_list")

setDT(xmr.tx)

xmr.tx[, unique.tx := paste(vin_amount,  amount_index, sep = "-")]

xmr.tx.collisions <- xmr.tx[, .(
  collision.index = anyDuplicated(unique.tx),
  n.inputs = uniqueN(vin_index)), 
  by = .(block_height, tx_index)]

setorder(xmr.tx.collisions, block_height, tx_index)  

prop.table(table(xmr.tx.collisions$collision.index != 0))


xmr.tx.collisions[, block.interval := cut(block_height, 100)]

xmr.tx.collisions[, .(prop.collisions = 100 * mean(collision.index != 0)), by = .(block.interval)]


xmr.tx.collisions[n.inputs <= 2, .(prop.collisions = 100 * mean(collision.index != 0)), by = .(block.interval)]

prop.table(table(xmr.tx.collisions$collision.index[xmr.tx.collisions$n.inputs <= 2] != 0))


xmr.tx.collisions[n.inputs == 2, .(prop.collisions = 100 * mean(collision.index != 0)), by = .(block.interval)]

prop.table(table(xmr.tx.collisions$collision.index[xmr.tx.collisions$n.inputs == 2] != 0))


xmr.tx.collisions[n.inputs == 1, .(prop.collisions = 100 * mean(collision.index != 0)), by = .(block.interval)]

prop.table(table(xmr.tx.collisions$collision.index[xmr.tx.collisions$n.inputs == 1] != 0))


