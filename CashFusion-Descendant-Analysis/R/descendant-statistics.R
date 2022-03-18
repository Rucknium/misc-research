# install.packages("data.table")
# install.packages("RSQLite")
# install.packages("DBI")

library(data.table)
library(RSQLite)
library(DBI)


data.dir <- ""
# Input data directory here, with trailing "/"


load(paste0(data.dir, "touched-UTXO-intermediate-COMPLETE.Rdata"), verbose = TRUE)

con <- DBI::dbConnect(RSQLite::SQLite(), paste0(data.dir, "tx-graph-node-indices.db"))

DBI::dbWriteTable(con, "utxovalue", 
  data.frame(destination = character(0), value = numeric(0), stringsAsFactors = FALSE),
  overwrite = TRUE)


tx.graph.files <- list.files(data.dir)
tx.graph.files <- tx.graph.files[grepl("^tx_graph.+rds$", tx.graph.files)]

for (file.iter in tx.graph.files) {
  
  tx.graph.chunk <- readRDS(paste0(data.dir, file.iter))
  
  tx.graph.chunk <- data.frame(destination = paste0(tx.graph.chunk$outgoing$txid, "-", 
    formatC(tx.graph.chunk$outgoing$position, width = 4, format = "f", flag = "0", digits = 0)), 
    value = tx.graph.chunk$outgoing$value, stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(con, "utxovalue", 
    tx.graph.chunk, append = TRUE)
  
  cat(base::date(), file.iter, "\n")
  
}



master.edgelist <- DBI::dbGetQuery(con, 
  "SELECT origin_index, destination_index FROM edgelist_intermediate_2")

str(master.edgelist)

master.edgelist <- as.matrix(master.edgelist)

data.table::uniqueN(c(master.edgelist))


utxo.set <- setdiff(master.edgelist[, 2], master.edgelist[, 1])

rm(master.edgelist)


DBI::dbWriteTable(con, "utxoset", 
  data.frame(destination_index = utxo.set, stringsAsFactors = FALSE),
  overwrite = TRUE)

DBI::dbWriteTable(con, "utxoset_intermediate_1", 
  data.frame(destination = character(0), destination_index = integer(0), stringsAsFactors = FALSE),
  overwrite = TRUE)

base::date()
DBI::dbExecute(con, "INSERT INTO utxoset_intermediate_1 SELECT 
  destination, utxoset.destination_index AS destination_index FROM
  utxoset INNER JOIN edgelist_intermediate_2 ON utxoset.destination_index = edgelist_intermediate_2.destination_index")
base::date()


DBI::dbWriteTable(con, "utxoset_intermediate_2", 
  data.frame(destination = character(0), destination_index = integer(0),
    value = numeric(0), stringsAsFactors = FALSE),
  overwrite = TRUE)

base::date()
DBI::dbExecute(con, "INSERT INTO utxoset_intermediate_2 SELECT 
  utxoset_intermediate_1.destination AS destination, destination_index, value FROM
  utxoset_intermediate_1 INNER JOIN utxovalue ON utxoset_intermediate_1.destination = utxovalue.destination")
base::date()


utxoset.value.extended <- DBI::dbGetQuery(con, 
  'SELECT * FROM utxoset_intermediate_2')

colnames(utxoset.value.extended) <- c("txid_position", "tx_graph_index", "value")

utxoset.value.extended$is_cashfusion_descendant <- ifelse(
  utxoset.value.extended$tx_graph_index %in% touched.UTXO, 1L, 0L)


unspent.coinbases <- readRDS(paste0(data.dir, "unspent_coinbases.rds"))

nrow(unspent.coinbases)


unspent.coinbases$tx_graph_index <- NA_integer_
unspent.coinbases$is_coinbase <- 1L
unspent.coinbases$is_cashfusion_descendant <- 0L

utxoset.value.extended$is_coinbase <- 0L

utxoset.value.extended <- rbind(
  utxoset.value.extended[, c("txid_position", "tx_graph_index", "value", "is_cashfusion_descendant", "is_coinbase")],
  unspent.coinbases[, c("txid_position", "tx_graph_index", "value", "is_cashfusion_descendant", "is_coinbase")]
  )



100 * prop.table(table(utxoset.value.extended$is_cashfusion_descendant[
  utxoset.value.extended$value > 0 ]))

utxoset.value.aggregated <- aggregate(utxoset.value.extended$value[
  utxoset.value.extended$value > 0 ], 
  by = list(utxoset.value.extended$is_cashfusion_descendant[
    utxoset.value.extended$value > 0 ]), FUN = sum)

utxoset.value.aggregated$percent <- 100 * utxoset.value.aggregated$x / sum(utxoset.value.aggregated$x)

utxoset.value.aggregated


saveRDS(utxoset.value.extended, file = paste0(data.dir, "CashFusion-Descendants.rds"))

write.csv(utxoset.value.extended, file = paste0(data.dir, "CashFusion-Descendants.csv"), row.names = FALSE)


