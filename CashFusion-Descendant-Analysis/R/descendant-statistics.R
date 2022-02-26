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
  data.frame(destination = character(0), value = numeric(0), stringsAsFactors = FALSE))


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

master.edgelist <- as.matrix(master.edgelist)

utxo.set <- setdiff(master.edgelist[, 2], master.edgelist[, 1])

rm(master.edgelist)


DBI::dbWriteTable(con, "utxoset", 
  data.frame(destination_index = utxo.set, stringsAsFactors = FALSE))

DBI::dbWriteTable(con, "utxoset_intermediate_1", 
  data.frame(destination = character(0), destination_index = integer(0), stringsAsFactors = FALSE))

base::date()
DBI::dbExecute(con, "INSERT INTO utxoset_intermediate_1 SELECT 
  destination, utxoset.destination_index AS destination_index FROM
  utxoset INNER JOIN edgelist_intermediate_2 ON utxoset.destination_index = edgelist_intermediate_2.destination_index")
base::date()


DBI::dbWriteTable(con, "utxoset_intermediate_2", 
  data.frame(destination = character(0), destination_index = integer(0),
    value = numeric(0), stringsAsFactors = FALSE))

base::date()
DBI::dbExecute(con, "INSERT INTO utxoset_intermediate_2 SELECT 
  utxoset_intermediate_1.destination AS destination, destination_index, value FROM
  utxoset_intermediate_1 INNER JOIN utxovalue ON utxoset_intermediate_1.destination = utxovalue.destination")
base::date()


utxoset.value.extended <- DBI::dbGetQuery(con, 
  'SELECT * FROM utxoset_intermediate_2')

utxoset.value.extended$is_cashfusion_descendant <- ifelse(
  utxoset.value.extended$destination_index %in% touched.UTXO, 1L, 0L)

100 * prop.table(table(utxoset.value.extended$is_cashfusion_descendant))

utxoset.value.aggregated <- aggregate(utxoset.value.extended$value, 
  by = list(utxoset.value.extended$is_cashfusion_descendant), FUN = sum)

100 * utxoset.value.aggregated$x / sum(utxoset.value.aggregated$x)


write.csv(utxoset.value.extended, file = paste0(data.dir, "CashFusion-Descendants.csv"), row.names = FALSE)


