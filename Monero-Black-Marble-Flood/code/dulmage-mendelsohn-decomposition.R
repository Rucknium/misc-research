

### INSTRUCTIONS
# Run the R script below (Need more than 100GB of RAM)
# Install Rust
# Clone https://github.com/Rucknium/cryptonote-analysis
# Then
# cd cryptonote-analysis
# cargo build --release
# cd scripts/monero
# g++ -O2 create_csparse_edges.cpp
# ./a.out 3114046
# # 3114046 should be same as current.height from R script
# cargo run --release --bin dmdec csparse-edges-3114046.txt rings-before-dm-3114046.txt rings-after-dm-3114046.txt blocksizes-3114046.txt fine-decomp-3114046.txt
# The output will say how many "ring size 1" rings exist after the DM decomposition




# Need these packages:
# install.packages("data.table")
# install.packages("igraph")
# install.packages("RCurl")
# install.packages("RJSONIO")
# install.packages("parallelly")
# install.packages("future")
# install.packages("future.apply")


library(data.table)


dm.decompsition.dir <- ""
# Should be the parent directory, followed by cryptonote-analysis/scripts/monero/
# Must have trailing "/"


current.height <- 3114046 # 2024-03-27 06:30:37 UTC
# Same as end.spam.height

edgelist.output.file <- paste0(dm.decompsition.dir, "edges-", current.height, ".txt")

start.height <- 1220516 # First RingCT outputs

url.rpc <- "http://127.0.0.1:18081"
# Set the IP address and port of your node. Should usually be "http://127.0.0.1:18081"


stopifnot(!is.na(current.height))

block.heights <- start.height:current.height




# Modified from TownforgeR::tf_rpc_curl function
xmr.rpc <- function(
    url.rpc = "http://127.0.0.1:18081/json_rpc",
  method = "",
  params = list(),
  userpwd = "",
  num.as.string = FALSE,
  nonce.as.string = FALSE,
  keep.trying.rpc = FALSE,
  curl = RCurl::getCurlHandle(),
  ...
){

  json.ret <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = method,
      params = params
    ), digits = 50
  )

  rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
    .opts = list(
      userpwd = userpwd,
      postfields = json.ret,
      httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
    ),
    curl = curl
  ), error = function(e) {NULL})

  if (keep.trying.rpc && length(rcp.ret) == 0) {
    while (length(rcp.ret) == 0) {
      rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
        .opts = list(
          userpwd = userpwd,
          postfields = json.ret,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
          # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
        ),
        curl = curl
      ), error = function(e) {NULL})
    }
  }

  if (is.null(rcp.ret)) {
    stop("Cannot connect to monerod. Is monerod running?")
  }

  if (num.as.string) {
    rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  if (nonce.as.string & ! num.as.string) {
    rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
  }

  RJSONIO::fromJSON(rcp.ret, asText = TRUE) # , simplify = FALSE
}



system.time({

  n.workers <- min(floor(parallelly::availableCores()/2), 32L)

  future::plan(future::multisession(workers = n.workers))
  options(future.globals.maxSize= 8000*1024^2)

  set.seed(314)

  # Randomize block heights to make processing time more uniform between parallel processes
  block.heights <- split(block.heights, sample(cut(block.heights, n.workers)))
  # First randomly put heights into list elements (split() will sort them ascendingly in each list element)
  block.heights <- lapply(block.heights, sample)
  # Then order the heights randomly within each list element
  block.heights <- unname(block.heights)

  returned <- future.apply::future_lapply(block.heights, function(block.heights) {

    handle <- RCurl::getCurlHandle()

    return.data <- vector("list", length(block.heights))

    for (height.iter in seq_along(block.heights)) {

      height <- block.heights[height.iter]

      block.data <- xmr.rpc(url.rpc = paste0(url.rpc, "/json_rpc"),
        method = "get_block",
        params = list(height = height ),
        keep.trying.rpc = TRUE,
        curl = handle)$result

      txs.to.collect <- c(block.data$miner_tx_hash, block.data$tx_hashes)

      rcp.ret <- 	tryCatch(RCurl::postForm(paste0(url.rpc, "/get_transactions"),
        .opts = list(
          postfields = paste0('{"txs_hashes":["', paste0(txs.to.collect, collapse = '","'), '"],"decode_as_json":true}'),
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        ),
        curl = handle
      ), error = function(e) {NULL})

      if (length(rcp.ret) == 0) {
        while (length(rcp.ret) == 0) {
          rcp.ret <- tryCatch(RCurl::postForm(paste0(url.rpc, "/get_transactions"),
            .opts = list(
              postfields = paste0('{"txs_hashes":["', paste0(txs.to.collect, collapse = '","'), '"],"decode_as_json":true}'),
              httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
            ),
            curl = handle
          ), error = function(e) {NULL})
        }
      }

      rcp.ret <- RJSONIO::fromJSON(rcp.ret, asText = TRUE)

      output.index.collected <- vector("list", length(txs.to.collect))
      rings.collected <- vector("list", length(txs.to.collect) - 1)

      for (i in seq_along(txs.to.collect)) {

        tx.json <- tryCatch(
          RJSONIO::fromJSON(rcp.ret$txs[[i]]$as_json, asText = TRUE),
          error = function(e) {NULL} )

        if (is.null(tx.json)) {
          # stop()
          cat(paste0("tx: ", i, " block: ", height, "\n"), file = "~/RingCT-problems.txt", append = TRUE)
          next
        }

        output.amounts <- sapply(tx.json$vout, FUN = function(x) {x$amount})

        tx_size_bytes <- ifelse(i == 1,
          nchar(rcp.ret$txs[[i]]$pruned_as_hex) / 2,
          nchar(rcp.ret$txs[[i]]$as_hex) / 2)
        # Coinbase has special structure
        # Reference:
        # https://libera.monerologs.net/monero-dev/20221231
        # https://github.com/monero-project/monero/pull/8691
        # https://github.com/monero-project/monero/issues/8311

        calc.tx.weight.clawback <- function(p) {
          pow.of.two <- 2^(1:4)
          pow.of.two.index <- findInterval(p, pow.of.two, left.open = TRUE) + 1
          num_dummy_outs <- pow.of.two[pow.of.two.index] - p
          transaction_clawback <- 0.8 * ( (23 * (p + num_dummy_outs)/2) * 32 - (2 * ceiling(log2(64 * p)) + 9) * 32 )
          # Equation from page 63 of Zero to Monero 2.0
          transaction_clawback
        }

        if (length(tx.json$vout) == 2 && i > 1) {
          # i > 1 means not the first tx, which is the coinbase tx
          tx_weight_bytes <- tx_size_bytes
        } else {
          tx_weight_bytes <- tx_size_bytes + calc.tx.weight.clawback(length(tx.json$vout))
        }


        tx_fee <- ifelse(i == 1 || is.null(tx.json$rct_signatures), NA, tx.json$rct_signatures$txnFee)
        # missing non-RingCT tx fee

        is.mordinal <-
          height >= 2838965 &&
          length(tx.json$vout) == 2 &&
          i > 1 && # not the first tx, which is the coinbase tx
          length(tx.json$extra) > 44 &&
          tx.json$extra[45] == 16
        # With "&&", evaluates each expression sequentially until it is false (if ever). Then stops.
        # If all are TRUE, then returns true.

        is.mordinal.transfer <-
          height >= 2838965 &&
          length(tx.json$vout) == 2 &&
          i > 1 && # not the first tx, which is the coinbase tx
          length(tx.json$extra) > 44 &&
          tx.json$extra[45] == 17

        output.index.collected[[i]] <- data.table(
          block_height = height,
          block_timestamp = block.data$block_header$timestamp,
          block_size = block.data$block_size,
          block_reward = block.data$reward,
          tx_num = i,
          tx_hash = txs.to.collect[i],
          tx_version = tx.json$version,
          tx_fee = tx_fee,
          tx_size_bytes = tx_size_bytes,
          tx_weight_bytes = tx_weight_bytes,
          number_of_inputs = length(tx.json$vin),
          number_of_outputs = length(tx.json$vout),
          output_num = seq_along(rcp.ret$txs[[i]]$output_indices),
          output_index = rcp.ret$txs[[i]]$output_indices,
          output_amount = output.amounts,
          output_unlock_time = tx.json$unlock_time,
          is_mordinal = is.mordinal,
          is_mordinal_transfer = is.mordinal.transfer)


        if (i == 1L) { next }
        # Skip first tx since it is the coinbase and has no inputs

        tx_hash <- txs.to.collect[i]

        rings <- vector("list", length(tx.json$vin))

        for (j in seq_along(tx.json$vin)) {
          rings[[j]] <- data.table(
            tx_hash = tx_hash,
            input_num = j,
            input_amount = tx.json$vin[[j]]$key$amount,
            key_offset_num = seq_along(tx.json$vin[[j]]$key$key_offsets),
            key_offsets = tx.json$vin[[j]]$key$key_offsets
          )
        }

        rings.collected[[i-1]] <- rbindlist(rings)

      }

      output.index.collected <- data.table::rbindlist(output.index.collected)
      rings.collected <- rbindlist(rings.collected)

      return.data[[height.iter]] <- list(
        output.index.collected = output.index.collected,
        rings.collected = rings.collected)

    }

    return.data

  } )
})

future::plan(future::sequential)
# Shut down workers to free RAM


returned.temp <- vector("list", length(returned))

for (i in seq_along(returned)) {
  returned.temp[[i]] <- list(
    output.index.collected = rbindlist(lapply(returned[[i]],
      FUN = function(y) { y$output.index.collected })),
    rings.collected = rbindlist(lapply(returned[[i]],
      FUN = function(y) { y$rings.collected }))
  )
}

returned.temp <- list(
  output.index.collected = rbindlist(lapply(returned.temp,
    FUN = function(y) { y$output.index.collected })),
  rings.collected = rbindlist(lapply(returned.temp,
    FUN = function(y) { y$rings.collected }))
)



output.index <- returned.temp$output.index.collected
returned.temp$output.index.collected <- NULL
rings <- returned.temp$rings.collected
rm(returned.temp)

setorder(rings, tx_hash, input_num, key_offset_num)

rings[, output_index := cumsum(key_offsets), by = c("tx_hash", "input_num")]

rings <- merge(rings, unique(output.index[, .(tx_hash, block_height, block_timestamp, tx_fee, tx_size_bytes)]), by = "tx_hash")

setnames(rings, c("block_height", "block_timestamp", "tx_fee", "tx_size_bytes"),
  c("block_height_ring", "block_timestamp_ring", "tx_fee_ring", "tx_size_bytes_ring"))

output.index[, output_amount_for_index := ifelse(tx_num == 1, 0, output_amount)]

output.index <- output.index[ !(tx_num == 1 & tx_version == 1), ]
# Remove coinbase outputs that are ineligible for use in a RingCT ring
# See https://libera.monerologs.net/monero-dev/20230323#c224570



xmr.rings <- merge(rings, output.index[, .(block_height, block_timestamp, tx_num, output_num,
  output_index, output_amount, output_amount_for_index, output_unlock_time, number_of_inputs,
  number_of_outputs, is_mordinal, is_mordinal_transfer, tx_fee, tx_size_bytes)],
  # only dont need tx_hash column from output.index
  by.x = c("input_amount", "output_index"),
  by.y = c("output_amount_for_index", "output_index")) #, all = TRUE)


xmr.rings <- xmr.rings[input_amount == 0, ]
# Remove non-RingCT rings

xmr.rings[, num_times_referenced := .N, by = "output_index"]

output.index.date <- unique(output.index[, .(block_timestamp = block_timestamp)])

output.index.date[, block_date := as.Date(as.POSIXct(block_timestamp, origin = "1970-01-01"))]

output.index <- merge(output.index, output.index.date)
# speed improvement by splitting and then merging

gc()



# End ring gathering

# Start spam-era analysis



start.spam.height <- 3097764 # 2024-03-04 15:21:24
start.spam.date <- as.Date("2024-03-04")


end.spam.height <- 3114046 # 2024-03-27 06:30:37 UTC
end.spam.date <- as.Date("2024-03-27")




output.index[, block_date.week.day := weekdays(block_date)]


spam.types <- list(list(
  fingerprint.text = "1in/2out 20 nanoneros/byte",
  fingerprint.crieria = substitute(
    floor((tx_fee/tx_size_bytes)/1000) %between% c(18, 22) &
      number_of_inputs == 1 &
      number_of_outputs == 2)),
  list(
    fingerprint.text = "1in/2out 20 or 320 nanoneros/byte",
    fingerprint.crieria = substitute(
      floor((tx_fee/tx_size_bytes)/1000) %between% c(315, 325) &
        number_of_inputs == 1 &
        number_of_outputs == 2)))


spam.results <- list()

for (spam.type in seq_along(spam.types)) {

  spam.fingerprint.all <- list()
  spam.fingerprint.tx.all <- list()

  for (spam.type.sub in 1:spam.type) {

    pre.spam.level.week.day <- output.index[
      # block_height < start.spam.height &
      block_date < start.spam.date &
        tx_num != 1 &
        eval(spam.types[[spam.type.sub]]$fingerprint.crieria),
      .(txs.rm.from.spam.set = round(uniqueN(tx_hash)/4)),
      # NOTE: /4 assumes number of pre-spam weeks in data is 4.
      by = "block_date.week.day"]

    spam.fingerprint <- output.index[
      block_height %between% c(start.spam.height, end.spam.height) &
        tx_num != 1 &
        eval(spam.types[[spam.type.sub]]$fingerprint.crieria),  ]

    spam.fingerprint[, fingerprint := spam.types[[spam.type.sub]]$fingerprint.text]

    spam.fingerprint.tx <- spam.fingerprint[!duplicated(tx_hash), ]

    spam.fingerprint.tx <- merge(spam.fingerprint.tx,
      pre.spam.level.week.day[, .(block_date.week.day, txs.rm.from.spam.set)], by = "block_date.week.day")

    set.seed(314)


    tx_hash.to.rm <- spam.fingerprint.tx[, .(tx_hash.to.rm = sample(tx_hash,
      min(c(unique(txs.rm.from.spam.set), length(tx_hash))), replace = FALSE)), by = "block_date"]
    spam.fingerprint.tx[, txs.rm.from.spam.set := NULL]
    spam.fingerprint.tx <- spam.fingerprint.tx[ ! tx_hash %chin% tx_hash.to.rm$tx_hash.to.rm, ]

    spam.fingerprint.all[[spam.type.sub]] <- spam.fingerprint
    spam.fingerprint.tx.all[[spam.type.sub]] <- spam.fingerprint.tx

  }

  spam.fingerprint <- rbindlist(spam.fingerprint.all)
  spam.fingerprint.tx <- rbindlist(spam.fingerprint.tx.all)

  non.spam.fingerprint <- output.index[ tx_num != 1 &
      (
        (! block_height %between% c(start.spam.height, end.spam.height)) |
          (block_height %between% c(start.spam.height, end.spam.height)  &
              ! (tx_hash %chin% spam.fingerprint.tx$tx_hash))
      ), ]

  non.spam.fingerprint.tx <- non.spam.fingerprint[!duplicated(tx_hash), ]

  spam.results[[spam.type]] <- list(
    spam.fingerprint = spam.fingerprint, spam.fingerprint.tx = spam.fingerprint.tx,
    non.spam.fingerprint = non.spam.fingerprint, non.spam.fingerprint.tx = non.spam.fingerprint.tx
  )

}



# End spam analysis

# Start edgelist creation



xmr.rings[, c("is_mordinal", "is_mordinal_transfer", "tx_fee",
  "tx_size_bytes", "output_unlock_time", "tx_fee_ring", "tx_size_bytes_ring") := NULL]

gc()

set.seed(314)

xmr.rings.trimmed <- xmr.rings[
  block_height_ring >= start.spam.height,
  .(output_index, real.spend = seq_len(.N) == sample(.N, 1)),
  by = c("tx_hash", "input_num")
]


xmr.rings.trimmed <- xmr.rings.trimmed[
  (real.spend | ! output_index %in% spam.results[[2]]$spam.fingerprint$output_index) &
    (! tx_hash %in% spam.results[[2]]$spam.fingerprint.tx$tx_hash),
  .(output_index, tx_hash.input_num = paste0(tx_hash, "-", input_num))
]





gc()

# xmr.rings has:
# ringmember/input --> tx hash relation (output_index --> tx_hash)

# output.index has:
# tx hash --> output relation (tx_hash --> output_index)

all.factor.levels <- c(unique(formatC(xmr.rings.trimmed$output_index, format = "d")),
  unique(xmr.rings.trimmed$tx_hash.input_num))
# Must use formatC() so that the output_index integers are
# converted to character correctly.


edgelist <- unname(as.matrix(xmr.rings.trimmed[,
  .(formatC(output_index, format = "d"), tx_hash.input_num)]))


gc()

edgelist <- structure(factor(edgelist, levels = all.factor.levels),
  dim = dim(edgelist), class = c('matrix', 'factor'))

rm(all.factor.levels)

class(edgelist) <- "matrix"
attr(edgelist, "levels") <- NULL

stopifnot( ! any(is.na(c(edgelist))))

stopifnot(typeof(edgelist) == "integer")

gc()

table(duplicated(edgelist))

stopifnot( ! any(duplicated(edgelist)))

igraph.graph <- igraph::graph_from_edgelist(edgelist, directed = TRUE)

igraph.is.bipartite <- igraph::bipartite_mapping(igraph.graph)

str(igraph.is.bipartite)

stopifnot(igraph.is.bipartite$res)

rm(igraph.graph, igraph.is.bipartite)

edgelist <- edgelist[, c(2, 1)]

# colnames(edgelist) <- c("keyimage_id", "output_id")

data.table::fwrite(edgelist, file = edgelist.output.file,
  quote = FALSE, sep = " ", col.names = FALSE)


# Then run the DM decomposition instructions

# This is the data to compare to:

effective.rings.before.DM <- xmr.rings.trimmed[, .(temp = .N), by = "tx_hash.input_num"][, table(temp)]

effective.rings.before.DM

100 * prop.table(effective.rings.before.DM)

# Then take the number from the displayed output:
# "Singletons (traceable keyimages): XXXXX"

# And plug it in here:

DM.decomp.singletons <- NA

100 * DM.decomp.singletons / sum(effective.rings.before.DM)












