# CashFusion Descendant Analysis

![CashFusion-Red-Team-Logo](https://github.com/Rucknium/CashFusionStats/raw/beta/www/images/logos/CashFusion-Red-Team-logo-1869-by-478.png)

## How to reproduce the analysis

Be advised that the analysis takes several weeks of computing time and upwards of 50 GB of available RAM to execute.

### Prerequisites

The main analysis is done with the R statistical programming language. R itself can be downloaded [here](https://cloud.r-project.org/). [RStudio](https://www.rstudio.com/products/rstudio/download/#download) is a good IDE for R. Install the necessary R packages with:

```R
install.packages("rbch")
install.packages("data.table")
install.packages("future.apply") 
install.packages("RSQLite")
install.packages("DBI")
install.packages("igraph")
install.packages("stringr")
```

You must have a Bitcoin Cash (BCH) full node synced with the transaction index enabled with the `-txindex` flag. As of now, the analysis has been tested with the [Bitcoin Unlimited](https://www.bitcoinunlimited.info/) node implementation. 

## `extract-tx-graphs.R`

The [R/extract-tx-graphs.R](R/extract-tx-graphs.R) script file issues JSON-RPC queries to `bitcoind`, the Bitcoin Cash node daemon. Make sure `bitcoind` is running before initiating this script.

In the R script file you must set `bitcoin.conf.file` to the filepath of your bitcoin.conf file and `data.dir` to the directory where you want files to be stored.

The script spawns multiple R process threads to accelerate queries to `bitcoind` and will take several hours to execute. In the specified data directory, a set of files named `tx_graph_height_BEGIN _to_END.rds` will be created.

The R object format of these files is:
```
List of 2
 $ incoming: Classes 'data.table and 'data.frame': N obs. of 3 variables:
  ..$ txid : chr [txid of the transaction]
  ..$ origin.txid : chr [txid of the output that is being spent by the transaction]
  ..$ origin.position : num [the position (of the output being spent) within the output's transaction, indexed from one]
 $ outgoing: Classes 'data.table and 'data.frame': N obs. of 4 variables:
  ..$ txid : chr [txid of the transaction]
  ..$ address : chr [address that the transaction is being spent to]
  ..$ position : int [position of the output being produced by the transaction, indexed from one]
  ..$ value : num [quantity of BCH being spent to each address] 
```

Note that, unlike the underlying blockchain data, the position of outputs is indexed from one, not from zero.

## `construct-edgelist.R`

The [R/construct-edgelist.R](R/construct-edgelist.R) script file produces a SQL database that contains the [edge list](https://en.wikipedia.org/wiki/Edge_list) of the BCH transaction [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)). Set `data.dir` to the same directory same as you used in  `extract-tx-graphs.R`.

The script assigns an integer index to every output. Converting the transaction ID + output position to integer indixes is necessary so that the transaction graph can be stored in RAM. This script is single-threaded so as to avoid conflicts when writing to the database. It should take a few hours to complete.

## `determine-descendants.R`

The [R/determine-descendants.R](R/determine-descendants.R) script file uses the `igraph` package to determine which outputs in the unspent transaction output (UTXO) set can be traced back to an earlier CashFusion transaction. The dataset produced by the [CashFusionStats](https://github.com/Rucknium/CashFusionStats) repository is used to identify the CashFusion transactions.

Set `data.dir` as before. These operations are time-consuming and may take weeks. There is an option to interrupt the process and restart it.

## `descendant-statistics.R`

The [R/descendant-statistics.R](R/descendant-statistics.R) script file merges the CashFusion descendants, which has been identified by integer indices, with their transaction ID identifiers. It then calculates a simple total of proportion of the UTXO set that is a CashFusion descendant as well as the value in BCH terms. A `"CashFusion-Descendants.csv` csv file is output with the following columns:

```
destination: An idenifier of an unspect output, in the form of TXID-position
destination_index: An integer index for the unspent output that was used in the transaction graph analysis
value: The value, in BCH, of the output
is_cashfusion_descendant: Takes value of 1 if output is a descendant of a CashFusion transaction and 0 otherwise
```

Once again note that, unlike the underlying blockchain data, the position of outputs is indexed from one, not from zero.
