## March 2024 Suspected Black Marble Flooding Against Monero: Privacy, User Experience, and Countermeasures

In the `pdf` directory is a rapid preliminary analysis of the suspected black marble flooding incident against Monero in March 2024.

To run the R analysis in the `code` directory, you must have `monerod` running on your local machine. If `monerod` does not have its RPC port at http://127.0.0.1:18081, you must change the `url.rpc` variable in `output-index.R` to the correct URL and port.

The analysis requires about 32 GB of RAM. The code should complete in a few hours.

Several R packages must be installed. These can be installed with:

```R
install.packages(c("ggplot2", "data.table", "RJSONIO", "RCurl", "parallelly", "future", "future.apply", "zoo", "scales", "actuar"))
```

The R files should be run in this order:
- `output-index.R`
- `spam-assumptions.R`
- `block-size.R`
- `empirical-effective-ring-size.R`
- `effective-ring-size-projections.R`
- `fee-behavior.R`
- `mempool-tx-confirmation.R`

The data analyzed in `mempool-tx-confirmation.R` was actively collected in real time at a few Monero nodes and is not yet publicly available.
