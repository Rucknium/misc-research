# Pre-fork BCH/BTC Spending Analysis

### See [this article](https://rucknium.me/posts/pre-fork-BTC-BCH-spending/) for commentary on the results

## How to reproduce the analysis

Be advised that the analysis takes several days of computing time, upwards of 50 GB of available RAM, and about 2.5 Terabytes of storage space to execute. The scripts call [this](https://gist.githubusercontent.com/jeffwong/5925000/raw/bf02ed0dd2963169a91664be02fb18e45c4d1e20/sqlitewritetable.R) external script file.

### Prerequisites

The main analysis is done with the R statistical programming language. R itself can be downloaded [here](https://cloud.r-project.org/). [RStudio](https://www.rstudio.com/products/rstudio/download/#download) is a good IDE for R. Install the necessary R packages with:

```R
install.packages(c("rbch","data.table","future.apply","RSQLite","DBI","igraph","stringr","curl","ggplot2","scales","Cairo","lubridate"))
```

Linux users may have difficulties installing the required packages due to external dependencies (as they must be compiled from source). Ubuntu users will require ```build-essential```, ```libcurl4-openssl-dev``` and  ```libgmp3-dev```. Please raise an issue in this GitHub if you encounter any further issues. 

You must have a Bitcoin Cash (BCH) full node synced with the transaction index enabled with the `-txindex` flag. As of now, the analysis has been tested with the [Bitcoin Unlimited](https://www.bitcoinunlimited.info/) node implementation. You must also have a Bitcoin Core (BTC) full node synced with the transaction index enabled with the `-txindex` flag.

It is best to run the following script files successively in separate R sessions for better RAM management.

## `extract-tx-graphs.R`

The [extract-tx-graphs.R](extract-tx-graphs.R) script file issues JSON-RPC queries to `bitcoind`, the BTC or BCH node daemon. 

In the R script file you must set `bitcoin.conf.file` to the filepath of your bitcoin.conf file and `data.dir` to the directory where you want files to be stored. Note that the `data.dir` in the R file should be a different directory than the `bitcoind` data dir where the BCH blockchain is stored. 

`extract-tx-graphs.R` should be run twice: once for BCH and once for BTC. `data.dir` should be different for BCH and BTC. `current.block.height` should be set to 733867 for BCH and 729896 for BTC to perform the analysis up to March 31, 2022. The `bitcoind` for BCH (i.e. Bitcoin Unlimited) should be running when the R script is running for BCH. The same goes for BTC. The same bitcoin.conf file can be used for both blockchains.

Your bitcoin.conf file should contain these lines at a minimum:

```
testnet=0
rpcuser=<USER>
rpcpassword=<PASSWORD>
rpcport=8332
txindex=1
rpcallowip=0.0.0.0/0
rpcbind=localhost
server=1
```

Input \<USER\> and \<PASSWORD\> of your choice. Perform a quick sanity check to ensure you can access your node via JSON-RPC. The following should return data, and not refuse your connection: (user being the value of rpcuser / pass the value of rpcpass)

```
curl --data-binary '{"jsonrpc":"1.0","id":"curltext","method":"getblockchaininfo","params":[]}' -H 'content-type:text/plain;' http://user:pass@localhost:8332/
```

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

The [construct-edgelist.R](construct-edgelist.R) script file produces a SQL database that contains the [edge list](https://en.wikipedia.org/wiki/Edge_list) of the transaction [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)). `extract-tx-graphs.R` should be run twice: once for BCH and once for BTC. `data.dir` should be different for BCH and BTC. Set `data.dir` to the same directories as you used in `extract-tx-graphs.R`.

The script assigns an integer index to every output. Converting the transaction ID + output position to integer indices is necessary so that the transaction graph can be stored in RAM. This script is single-threaded so as to avoid conflicts when writing to the database. It should take a few hours to complete.

## `get-block-times.R`

The [get-block-times.R](get-block-times.R) script file produces a `.rds` file that contains the time that each block was produced. The script should be run twice: once for BCH and once for BTC. `bitcoind` must be running for the corresponding blockchain. `data.dir` should be different for BCH and BTC. Set `data.dir` to the same directories as you used in `extract-tx-graphs.R`. `current.block.height` should be set to 733867 for BCH and 729896 for BTC to perform the analysis up to March 31, 2022.

## `aggregate-spent-status-data.R`

The [aggregate-spent-status-data.R](determine-descendants.R) merges and aggregates the BCH and BTC data that is produced by earlier scripts. `bch.data.dir` and `btc.data.dir` should be set to the same directories as you used in `extract-tx-graphs.R`.

Three data files are produced in the `bch.data.dir` directory:

`pre-fork-BTC-BCH-spent_status.csv` with data structure:

```
53658348 obs. of  6 variables
btc.spent.block_height: int  Block height that the output was spent on the BTC blockchain, if it was spent
bch.spent.block_height: int  Block height that the output was spent on the BCH blockchain, if it was spent
destination_index     : int  Unique integer index of the output that was created by the R script
value                 : num  Value of the output, in bitcoin units
bch.block_time        : POSIXct, format: "YYYY-MM-DD HH:MM:SS" Block time of the BCH block height
btc.block_time        : POSIXct, format: "YYYY-MM-DD HH:MM:SS" Block time of the BTC block height
```

`spent_status_by_day.csv` with data structure:

```
1704 obs. of  9 variables:
block_time.date                : Date, format: "YYYY-MM-DD"
value.btc.unspent.bch.unspent  : num  
outputs.btc.unspent.bch.unspent: int  
value.btc.spent.bch.unspent    : num  
outputs.btc.spent.bch.unspent  : int  
value.btc.unspent.bch.spent    : num  
outputs.btc.unspent.bch.spent  : int  
value.btc.spent.bch.spent      : num  
outputs.btc.spent.bch.spent    : int  
```

`state_trans_by_day.csv` with data structure:

```
1704 obs. of  9 variables:
block_time.date : Date, format: "YYYY-MM-DD"
value.ff.to.tf  : num  
outputs.ff.to.tf: int  
value.ff.to.ft  : num  
outputs.ff.to.ft: int  
value.ff.to.tt  : num  
outputs.ff.to.tt: int  
value.ft.to.tt  : num  
outputs.ft.to.tt: num  
value.tf.to.tt  : num  
outputs.tf.to.tt: num  
```

See the article linked at the top of this README for information about how to interpret these variables.

## `create-plots.R`

Creates four PNG plots in the `Pre-fork-BCH-BTC-Spending/images` directory. You must specify `bch.data.dir` as before.

These plots are:

`pre-fork-BTC-BCH-spent-status-by-value.png`
`pre-fork-BTC-BCH-spent-status-by-outputs.png`
`pre-fork-BTC-BCH-state-transition-by-value.png`
`pre-fork-BTC-BCH-state-transition-by-outputs.png`



