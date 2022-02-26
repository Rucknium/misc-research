# install.packages("data.table")
# install.packages("RSQLite")
# install.packages("DBI")
# install.packages("igraph")

library(data.table)
library(RSQLite)
library(DBI)
library(igraph)

data.dir <- ""
# Input data directory here, with trailing "/"

fusions.df <- readRDS("https://github.com/Rucknium/CashFusionStats/raw/main/data/fusions_df.rds")
# Get the list of transactions that are the fusion transactions


con <- DBI::dbConnect(RSQLite::SQLite(), paste0(data.dir, "tx-graph-node-indices.db"))

master.edgelist <- DBI::dbGetQuery(con, 
  "SELECT origin_index, destination_index FROM edgelist_intermediate_2")


master.edgelist <- as.matrix(master.edgelist)

bch.graph <- igraph::graph_from_edgelist(master.edgelist)


fusioned.nodes <- DBI::dbGetQuery(con, paste0('SELECT * FROM nodes WHERE node IN ("',
  paste0(fusions.df$txid, collapse = '", "'), '")'))

fusioned.nodes <- base::intersect(unique(c(master.edgelist)), fusioned.nodes$node_index)

utxo.set <- setdiff(master.edgelist[, 2], master.edgelist[, 1])

fusioned.nodes <- sort(fusioned.nodes, decreasing = FALSE)

fusioned.nodes.to.process <- split(fusioned.nodes, 
  cut(fusioned.nodes, fusioned.nodes[seq(4, length(fusioned.nodes) - 4, by = 4)]))

fusioned.nodes.to.process <- rev(fusioned.nodes.to.process)


touched.UTXO <-  c()

counter.i <- 1

rm(master.edgelist)
gc()


for ( i in fusioned.nodes.to.process[counter.i:length(fusioned.nodes.to.process)]) {

  to.set <- setdiff(utxo.set, touched.UTXO)
  if (counter.i %% 10 == 0 ) {
    save(i, counter.i, touched.UTXO, 
      file = paste0(data.dir, "touched-UTXO-intermediate-", 
        counter.i, ".Rdata"), compress = FALSE)
  }
  
  bch.paths <- distances(bch.graph, 
    v = i, 
    to = to.set, mode = "out")
  
  touched.UTXO <- c(touched.UTXO, to.set[colSums(is.finite(bch.paths)) > 0 ] )
  
  cat(base::date(), " | ",
    round(100 * length(touched.UTXO) / length(utxo.set), 3), "% | ",
    counter.i,
    " of ", length(fusioned.nodes.to.process), "\n")
  
  counter.i <- counter.i + 1
  
}

save(i, counter.i, touched.UTXO, 
  file = paste0(data.dir, "touched-UTXO-intermediate-", 
    "COMPLETE", ".Rdata"), compress = FALSE)


# How to restart the analysis if interrupted:

# Run this script up until the "gc()".
# Note that you should be using the version of fusions_df.rds that you used 
# in the initial script run.

# Find the most recent save state, i.e. highest value of 
# touched-UTXO-intermediate-xxxx.Rdata
# Then load it:
# load(paste0(data.dir, "touched-UTXO-intermediate-xxxx.Rdata"))

# Then start the for loop

