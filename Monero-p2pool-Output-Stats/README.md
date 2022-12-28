#  Monero-p2pool-Output-Stats

The p2pool-output-stats.R script 

## Steps to run:

1) [Install R](https://cloud.r-project.org/)

2) Install the `data.table`, `RJSONIO`, and `RCurl` R Packages. The easiest way to do this is to start an R session by typing `R` in your terminal and then input `install.packages(c("data.table", "RJSONIO", "RCurl"))` into the R prompt. R may ask you to confirm that you want to create a user-level directory to store the packages. The R packages will be compiled from source code by default if you are using Linux. Then exit the R session by typing `quit("no")`.

3) You must have a Monero node fully synced and running. By default, the R script will assume that `monerod`'s RPC interface is available at `http://127.0.0.1:18081`.

4) You must have the `monero-blockchain-stats` utility somewhere on your computer. The easiest way to get this is to [download the Monero CLI Wallet](https://www.getmonero.org/downloads/#cli). The `monero-blockchain-stats` utility is bundled with the CLI Wallet.

5) To run the script, in your terminal input `Rscript <path/to/p2pool-output-stats.R> <initial_block_height> <end_block_height> "<path/to/monero-blockchain-stats>"`. For example, if the path to p2pool-output-stats.R is ~/misc-research/Monero-p2pool-Output-Stats/p2pool-output-stats.R, the initial block height is 2721396, the end block height is 2786779, and the path to monero-blockchain-stats is ~/monero/monero-blockchain-stats, then the input into terminal would be `Rscript ~/misc-research/Monero-p2pool-Output-Stats/p2pool-output-stats.R 2721396 2786779 "~/monero/monero-blockchain-stats"`.

