

# First, run tradeoff-output-index.R
# Make sure to install the packages listed at the top there.



setwd("Monero-Black-Marble-Flood/pdf")
# Set location of where images and tables should be saved




num.blocks.before.spam <- uniqueN(output.index$block_height)



mean.non.spam.output.per.block <- nrow(output.index)/num.blocks.before.spam
# 105

r <- mean.non.spam.output.per.block

non.spam.fingerprint.tx.sim <- copy(output.index[ ! duplicated(tx_hash) & tx_num != 1, ])
# tx_num != 1 to remove coinbase outputs


non.spam.fingerprint.tx.sim.tabulated.weight <- non.spam.fingerprint.tx.sim[, as.data.table(table(tx_weight_bytes, number_of_inputs))]
non.spam.fingerprint.tx.sim.tabulated.weight <- non.spam.fingerprint.tx.sim.tabulated.weight[N != 0, ]
non.spam.fingerprint.tx.sim.tabulated.weight[, tx_weight_bytes := as.numeric(tx_weight_bytes)]
non.spam.fingerprint.tx.sim.tabulated.weight[, number_of_inputs := as.numeric(number_of_inputs)]
non.spam.fingerprint.tx.sim.tabulated.weight[, N := as.numeric(N)]


non.spam.fingerprint.tx.sim.tabulated.size <- non.spam.fingerprint.tx.sim[, as.data.table(table(tx_size_bytes, number_of_inputs))]
non.spam.fingerprint.tx.sim.tabulated.size <- non.spam.fingerprint.tx.sim.tabulated.size[N != 0, ]
non.spam.fingerprint.tx.sim.tabulated.size[, tx_size_bytes := as.numeric(tx_size_bytes)]
non.spam.fingerprint.tx.sim.tabulated.size[, number_of_inputs := as.numeric(number_of_inputs)]
non.spam.fingerprint.tx.sim.tabulated.size[, N := as.numeric(N)]

non.spam.fingerprint.tx.sim.tabulated.in.out <- non.spam.fingerprint.tx.sim[, as.data.table(table(number_of_inputs, number_of_outputs))]
non.spam.fingerprint.tx.sim.tabulated.in.out <- non.spam.fingerprint.tx.sim.tabulated.in.out[N != 0, ]
non.spam.fingerprint.tx.sim.tabulated.in.out[, number_of_inputs := as.numeric(number_of_inputs)]
non.spam.fingerprint.tx.sim.tabulated.in.out[, number_of_outputs := as.numeric(number_of_outputs)]
non.spam.fingerprint.tx.sim.tabulated.in.out[, N := as.numeric(N)]




mean.coinbase.tx.size <- output.index[!duplicated(tx_hash) & tx_num == 1, mean(tx_size_bytes)]


mean.size.1in.2out <- non.spam.fingerprint.tx.sim[number_of_inputs == 1 & number_of_outputs == 2, mean(tx_size_bytes)]
mean.size.2in.2out <- non.spam.fingerprint.tx.sim[number_of_inputs == 2 & number_of_outputs == 2, mean(tx_size_bytes)]
mean.size.16.ring.input <- mean.size.2in.2out - mean.size.1in.2out

mean.size.zero.ring.tx.size <- mean.size.1in.2out - mean.size.16.ring.input + 32 * 4
# This "size zero ring" is so that the variable ring size can be added later
# See below for why 32 * 4 is added

mean.size.16.ring.input <- mean.size.16.ring.input - 32 * 4
# We subtract 32 bytes times 4 because
# 1) The key image in the vin[[1]]$key$k_image JSON object is 32 bytes (64 characters
# of hexcode, divided by two). There is one key image per ring.
# 2-4) There are 3 other 32 byte strings in
# rctsig_prunable$CLSAGs[[1]]$c1
# rctsig_prunable$CLSAGs[[1]]$D
# rctsig_prunable$pseudoOuts
# that appear once per ring and do not seem to scale up with ring size.
# The "mean.size.16.ring.input" will be one 32 byte string for the ring signature
# string per each ring member, plus some amount (about 3) bytes for
# the average storage space per integer output index in vin[[1]]$key$key_offsets
# I think the integer output indices are stored as variable-length (in bytes)
# data objects.
mean.size.one.ring.member <- mean.size.16.ring.input / 16



prunable.bpp <- function(x) {
  x <- cut(x, breaks = c(0, 2, 4, 8, 16), labels = c("7", "8", "9", "10"))
  x <- 32 * 2 * as.numeric(as.character(x))
  # For L and R
  # Two outputs: 32 * 2 * 7
  # 3-4 outputs: 32 * 2 * 8
  # 5-8 outputs: 32 * 2 * 9
  # 9-16 outputs: 32 * 2 * 10
  # (Times 2 since there is one 32-byte string each for L and R)
  x + 32 * 6
  # 32 * 6 because there are 32 bytes each for A, A1, B, r1, s1, and d1
}




C_u <- function(f, n) {

  ring.size.sim <- n

  non.spam.fingerprint.tx.sim.tabulated.weight[, tx_weight_bytes.sim :=
      N * (tx_weight_bytes - mean.size.16.ring.input * number_of_inputs +
            number_of_inputs * mean.size.one.ring.member * ring.size.sim)]

  mean.non.spam.kb.per.block <-
    (sum(non.spam.fingerprint.tx.sim.tabulated.weight[, tx_weight_bytes.sim])/num.blocks.before.spam)
  # Coinbase txs are excluded

  mean.non.spam.kb.per.block * f

}




usd.xmr.rate <- 120 # April 2024 exchange rate
n.nodes <- 22537 # From monero.fail/map April 2024
ssd.1.tb.usd <- 114.50 # Median price of 1 TB SATA SSD April 2024 https://ssd.userbenchmark.com/

C_d <- function(n, m) {

  tb <- 1e+12

  ring.size.sim <- n

  non.spam.fingerprint.tx.sim.tabulated.size[, tx_size_bytes.sim :=
      N * (tx_size_bytes - mean.size.16.ring.input * number_of_inputs + number_of_inputs * mean.size.one.ring.member * ring.size.sim)]

  mean.non.spam.kb.per.block <- mean.coinbase.tx.size +
    (sum(non.spam.fingerprint.tx.sim.tabulated.size[, tx_size_bytes.sim])/num.blocks.before.spam)
  # Keep coinbase tx since that has to be stored

  bytes <- mean.non.spam.kb.per.block

  m * bytes * n.nodes * (ssd.1.tb.usd / usd.xmr.rate) / tb

}

C_u <- Vectorize(C_u)
C_d <- Vectorize(C_d)
# Have to Vectorize() these because otherwise the arguments will be
# expanded as vectors incorrectly when working on the blockchain dataset


ref.tx.size.cost <- function(f, n) {

  stopifnot(length(f) == length(n))

  tx.size <- non.spam.fingerprint.tx.sim[number_of_inputs == 2 & number_of_outputs == 2,
    mean(tx_size_bytes) - mean.size.16.ring.input * 2 + 2 * mean.size.one.ring.member * n]

  tx.weight <- non.spam.fingerprint.tx.sim[number_of_inputs == 2 & number_of_outputs == 2,
    mean(tx_weight_bytes) - mean.size.16.ring.input * 2 + 2 * mean.size.one.ring.member * n]

  list(size = tx.size, cost = tx.weight * f * usd.xmr.rate)

}

# No need to Vectorize() ref.tx.size.cost()



stopifnot(ceiling(mean.size.zero.ring.tx.size) == 975)
stopifnot(ceiling(mean.size.one.ring.member) == 35)

adversary.owned.outputs <- function(b, f, n) {
  2 * b / (f * (975 + 35 * n))
}

eff.ring.size <- function(b, f, n) {
  1 + (n - 1) * r /
    (r + adversary.owned.outputs(b, f, n) )
}

adversary.share.outputs <- function(b, f, n) {
  1 - r /
    (r + adversary.owned.outputs(b, f, n) )
}


CE <- function(f, n, m, b) {
  (C_u(f, n) + C_d(n, m)) /
    eff.ring.size(b, f, n)
}
# Cost effectiveness




f.range <- seq(0.000000010, 0.000000020 * 20, length.out = 40)
n.range <- seq(11, 60, by = 1)
f.n.range <- expand.grid(f = f.range, n = n.range)

min.acceptable.eff.ring.size <- 5


status.quo.no.attack.block.size <- C_u(1, 16)
status.quo.no.attack.block.size.one.year <-
  status.quo.no.attack.block.size * 30 * 24 * 365 / 10^9

non.spam.fingerprint.tx.sim.tabulated.in.out[, prunable.data :=
    N * (number_of_inputs * (32 * 3 + 32 * 16) + prunable.bpp(number_of_outputs) ) ]

prunable.data.per.block <- sum(non.spam.fingerprint.tx.sim.tabulated.in.out$prunable.data)/num.blocks.before.spam

status.quo.no.attack.block.size.one.year.pruned <-
  (status.quo.no.attack.block.size - (7/8) * prunable.data.per.block)  * 30 * 24 * 365 / 10^9






perf.tests <- readLines("performance-test/CLSAG-performance.txt")

skip <- grep("CLSAG", perf.tests)[1] - 1

perf.tests <- read.csv("performance-test/CLSAG-performance.txt", skip = skip, header = FALSE, stringsAsFactors = FALSE)

header.names <- c("CLSAG", "tx_size", "batch_size", "rangeproof_splits", "inputs", "outputs", "decomp_n", "decomp_m", "ring_size",
  "test_name",
  "posix_time", "iterations",
  "min", "max", "mean", "med", "stddev", "npskew")

perf.tests <- perf.tests[, 1:length(header.names)]
# Remove quantile data
colnames(perf.tests) <- header.names

stopifnot(all(perf.tests$ring_size == perf.tests$decomp_n ^ perf.tests$decomp_m))


perf.tests$mean <- 10^-6 * perf.tests$mean  # Convert from nanoseconds to milliseconds

pow.of.two <- 2^(1:4)


summary(perf.model.short <- lm(mean ~ ring_size * inputs + factor(ceiling(log2(outputs))), data = perf.tests))
summary(perf.model.long <- lm(mean ~ ring_size * inputs + log2(ring_size) * log2(inputs) + factor(ceiling(log2(outputs))), data = perf.tests))

nested.model.test <- anova(perf.model.short, perf.model.long, test = "F")

print(nested.model.test)

stopifnot(nested.model.test[["Pr(>F)"]][2] == 0)
# nested.model.test[["Pr(>F)"]][2] == 0 means p value is < 2.2e-16




alice.best.response <- function(b, f.n.range, m = 2, min.acceptable.eff.ring.size = 5, force.eval.point = NULL, plot = TRUE) {

  b <- b / (24*30)
  # Convert to budget per block


  if ( ! is.null(force.eval.point)) {

    if (plot == TRUE) {
      stop("Cannot force.eval.point and plot at the same time.")
    }

    stopifnot(is.data.frame(force.eval.point))
    stopifnot(all(colnames(force.eval.point) == c("f", "n")))


    contour.contents <- CE(force.eval.point$f, force.eval.point$n, m, b)
    contour.contents <- contour.contents * 1000 # Convert to millineros
    cost.effectiveness.at.optimum <- contour.contents

    unrestricted.min <- force.eval.point
    restricted.min <- force.eval.point
    eff.ring.size.at.optimum <- eff.ring.size(b, force.eval.point$f, force.eval.point$n)



  } else {


    contour.contents <- CE(f.n.range$f, f.n.range$n, m, b)
    contour.contents <- contour.contents * 1000 # Convert to millineros
    contour.matrix <- matrix(contour.contents, nrow = length(f.range), byrow = FALSE)

    eff.ring.size.evaluated <- eff.ring.size(b, f.n.range$f, f.n.range$n)

    unrestricted.min <- f.n.range[which.min(contour.contents), ]

    restricted.min <- f.n.range[
      eff.ring.size.evaluated >= min.acceptable.eff.ring.size, ][
        which.min(contour.contents[eff.ring.size.evaluated >= min.acceptable.eff.ring.size]), ]

    eff.ring.size.at.optimum <- eff.ring.size.evaluated[
      eff.ring.size.evaluated >= min.acceptable.eff.ring.size][
        which.min(contour.contents[eff.ring.size.evaluated >= min.acceptable.eff.ring.size])]

    cost.effectiveness.at.optimum <- contour.contents[f.n.range$f == restricted.min$f & f.n.range$n == restricted.min$n]

  }

  if (nrow(restricted.min) == 0) {
    stop("Constraint set is empty. This means no values in f.n.range satisfied the min.acceptable.eff.ring.size.")
  }


  attack.block.size.at.restricted.min <-
    C_u(restricted.min[, "f"], restricted.min[, "n"]) / restricted.min[, "f"] + # Data contributed by normal users
    b / restricted.min[, "f"] # Data contributed by adversary

  no.attack.block.size.at.restricted.min <-
    C_u(restricted.min[, "f"], restricted.min[, "n"]) / restricted.min[, "f"]

  no.attack.block.size.at.restricted.min.one.year <-
    no.attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9


  non.spam.fingerprint.tx.sim.tabulated.in.out[, prunable.data :=
      N * (number_of_inputs * (32 * 3 + 32 * restricted.min[, "n"]) + prunable.bpp(number_of_outputs) ) ]
  # TODO: explain this computation

  prunable.data.per.block <- sum(non.spam.fingerprint.tx.sim.tabulated.in.out$prunable.data)/num.blocks.before.spam


  no.attack.block.size.at.restricted.min.one.year <- no.attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9
  no.attack.block.size.at.restricted.min.one.year.pruned <-
    (no.attack.block.size.at.restricted.min - (7/8) * prunable.data.per.block) * 30 * 24 * 365 / 10^9
  # TODO: explain this computation

  attack.block.size.at.restricted.min.one.year <- attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9


  n.adversary.tx.per.block <- adversary.owned.outputs(b, restricted.min[, "f"], restricted.min[, "n"]) / 2

  prunable.data.per.block.adversary <- n.adversary.tx.per.block * (1 * (32 * 3 + 32 * restricted.min[, "n"]) + prunable.bpp(2) )
  # TODO: explain this computation

  attack.block.size.at.restricted.min.one.year.pruned <-
    (attack.block.size.at.restricted.min - (7/8) * (prunable.data.per.block + prunable.data.per.block.adversary)) * 30 * 24 * 365 / 10^9
  # TODO: explain this computation

  ref.tx.at.optimum <- ref.tx.size.cost(restricted.min[, "f"], restricted.min[, "n"])

  predicted.verif.time.ref.tx <- predict(perf.model.long,
    newdata = data.frame(inputs = 2, outputs = 2, ring_size = restricted.min[, "n"]))


  predicted.verif.time.normal.block <- predict(perf.model.long,
    newdata = non.spam.fingerprint.tx.sim.tabulated.in.out[,
      .(inputs = number_of_inputs, outputs = number_of_outputs, ring_size = restricted.min[, "n"])])

  predicted.verif.time.normal.block <- sum(predicted.verif.time.normal.block *
      non.spam.fingerprint.tx.sim.tabulated.in.out$N) / num.blocks.before.spam


  predicted.verif.time.spam.part.block <- n.adversary.tx.per.block * predict(perf.model.long,
    newdata = data.frame(inputs = 1, outputs = 2, ring_size = restricted.min[, "n"]))


  # Adversary budget, ring_size, fee_per_byte, effective_ring_size,
  # User's cost for 2in/2out tx,, User's tx size for 2in/2out tx,
  # Block size (no attack), Block size (attack),
  # One year blockchain growth (unpruned, no attack), One year blockchain growth (unpruned, attack),
  # One year blockchain growth (pruned, no attack), One year blockchain growth (pruned, attack)

  results.table <- data.table(
    b = b, b_per_day = b * 30 * 24,
    ring_size = restricted.min[, "n"], fee_per_byte = restricted.min[, "f"] * 10^9,
    cost_effectiveness = cost.effectiveness.at.optimum,
    effective_ring_size = eff.ring.size(b, restricted.min[, "f"], restricted.min[, "n"]),
    user_tx_cost_2in_2out = ref.tx.at.optimum$cost, user_tx_size_2in_2out = ref.tx.at.optimum$size,
    block_size_no_attack = no.attack.block.size.at.restricted.min,
    block_size_attack = attack.block.size.at.restricted.min,
    one_year_blockchain_growth_unpruned_no_attack = no.attack.block.size.at.restricted.min.one.year,
    one_year_blockchain_growth_unpruned_attack = attack.block.size.at.restricted.min.one.year,
    one_year_blockchain_growth_pruned_no_attack = no.attack.block.size.at.restricted.min.one.year.pruned,
    one_year_blockchain_growth_pruned_attack = attack.block.size.at.restricted.min.one.year.pruned,
    verif_time_2in_2out = predicted.verif.time.ref.tx * 10^-3, # Times 10^-3 converts to seconds
    verif_time_no_attack = predicted.verif.time.normal.block * 10^-3,
    verif_time_attack = (predicted.verif.time.normal.block + predicted.verif.time.spam.part.block)* 10^-3
  )



  if (! plot) {
    return(results.table)
  }



  par(mar = c(10, 4, 4, 2) + 0.1)

  filled.contour(x = f.range, y = n.range, z = contour.matrix,
    main = paste0("Most cost-effective minimum fee and ring size, given adversary budget of ",
      round(b * 24*30, 2), " XMR/day"),
    # TODO list adversary budget in title
    ylab = "Nominal ring size",
    key.title = title(main = "Cost\nEffectiveness\n(lower is better)",
      cex.main = 1, font.main = 1, xpd = NA),
    # non-bold font. xpd = NA means that title isn't clipped.
    ylim = c(min(n.range) - 1, max(n.range) + 1),
    xlim = c(min(f.range) - mean(f.range) * 0.05, max(f.range) + mean(f.range) * 0.05),
    plot.axes = {
      mtext(side = 1, line = 8.5, adj = 0,
        text =
          paste0("Adversary XMR budget per day: ", round(b * 24*30, 2),
            "\nEffective ring size: ", round(eff.ring.size.at.optimum, 2),
            ", Adversary share of outputs: ", round(adversary.share.outputs(b, restricted.min[, "f"], restricted.min[, "n"]), 2),
            "\nAggregate XMR cost to transacting users per block: " , round(C_u(restricted.min[, "f"], restricted.min[, "n"]), 3),
            "\nAggregate XMR cost to node operators per block with normal tx volume: ", round(C_d(restricted.min[, "n"], m), 3),
            "\n2in/2out tx size at optimum: ", round(ref.tx.at.optimum$size, 0),
            " bytes, 2in/2out tx cost at optimum: ", round(ref.tx.at.optimum$cost * 100, 2), " USD cents",
            "\nOne year blockchain growth with normal tx volume unpruned: ",
            round(no.attack.block.size.at.restricted.min.one.year), " GB, pruned: ",
            round(no.attack.block.size.at.restricted.min.one.year.pruned), " GB",
            "                github.com/Rucknium"))
      legend("bottomright",
        legend = c("Optimal fee and ring size, unrestricted", "Optimal fee and ring size, restricted",
          "Minimum acceptable effective ring size"), xpd = NA, inset = c(-0.175, -0.15),
        pch = c(17, 16, NA), lty = c(NA, NA, 1),
        col = c("blue", "green4", "black"), pt.cex = c(2, 4, NA), lwd = 2, cex = 1.1)
      title(xlab = "Minimum fee (nanoneros per byte)", line = 2.25)
      x.axis.ticks <- seq(min(f.range), max(f.range), by = 2e-08)
      axis(1, at = x.axis.ticks, labels = x.axis.ticks * 10^9)
      axis(2)
      contour.contents.2 <- eff.ring.size(b, f.n.range$f, f.n.range$n)
      contour.matrix.2 <- matrix(contour.contents.2, nrow = length(f.range), byrow = FALSE)
      contour(x = f.range, y = n.range, z = contour.matrix.2, method = "edge",
        levels = min.acceptable.eff.ring.size, vfont = c("sans serif", "bold"),
        labcex = 1.75, add = TRUE)

      points(restricted.min, pch = 16, col = "green4", cex = 4)
      points(unrestricted.min, pch = 17, col = "blue", cex = 2)

    }
  )

  return(results.table)

}







library(gt)



latex.table <- function(x, title = NULL, col.widths, output = "", label = "", source.note = NULL, add.row.color = NULL) {

  stopifnot(ncol(x) == length(col.widths))

  latex.output <- as.data.frame(x) |>
    gt() |>
    cols_label(
      b_per_day = "Adversary XMR budget per day",
      ring_size = "Nominal ring size",
      fee_per_byte = "Min fee (nanoneros per byte)",
      cost_effectiveness = "CE",
      effective_ring_size = "Effective ring size",
      user_tx_size_2in_2out = "Size of 2in/2out (bytes)",
      user_tx_cost_2in_2out = "User's cost to send 2in/2out (USD cents)",
      block_size_no_attack = "Block size (KB)",
      block_size_attack = "Block size (KB)",
      one_year_blockchain_growth_unpruned_no_attack = "Unpruned",
      one_year_blockchain_growth_unpruned_attack = "Unpruned",
      one_year_blockchain_growth_pruned_no_attack = "Pruned",
      one_year_blockchain_growth_pruned_attack = "Pruned",
      verif_time_2in_2out = "Seconds to verify 2in/2out",
      verif_time_no_attack = "Seconds to verify txs in block",
      verif_time_attack = "Seconds to verify txs in block"
    ) |>
    fmt_number(columns = matches("(blockchain)|(fee_per_byte)|(block_size)|(user_tx_size_2in_2out)"), decimals = 0) |>
    fmt_number(columns = matches("(b_per_day)|(effective_ring_size)|(user_tx_cost)|(verif_time_no_attack)|(verif_time_attack)|(cost_effectiveness)"), decimals = 2) |>
    fmt_number(columns = matches("(verif_time_2in_2out)"), decimals = 3) |>
    tab_header(title = title) |>
    tab_spanner(label = "One year blockchain growth (GB)", columns = starts_with("one_year_blockchain_growth")) |>
    tab_spanner(label = "Normal tx volume", columns = matches("no_attack")) |>
    tab_spanner(label = "Normal tx volume + black marble flooding", columns = matches("([^n][^o]_attack)|(effective_ring_size)")) |>
    # "we build spanners from the bottom up" https://gt.rstudio.com/reference/tab_spanner.html
    tab_source_note(source_note = source.note)

  if (! is.null(add.row.color)) {
    latex.output <- latex.output |>
      data_color(columns = everything(), rows = add.row.color$row, fn = function(x) add.row.color$color)
  }

  latex.output <- latex.output |>
    as_latex() |>
    as.character()

  latex.output <- gsub("begin[{]longtable[}][{][^\n]+",
    paste0("begin{longtable}{", paste0("Rp{", col.widths, "cm}", collapse = ""), "}"),
    latex.output)

  latex.output <- gsub("caption*", "caption", latex.output, fixed = TRUE)
  # Removing the "*" means that the table is numbered in the final PDF output

  latex.output <- gsub("\\end{longtable}",
    paste0("\\label{", label, "}\n\\end{longtable}"), latex.output, fixed = TRUE)


  cat(latex.output, file = output)

  invisible(NULL)

}



permuted.options <- expand.grid(f = c(10, 20, 40, 100, 200) * 10^-9, n = c(16, 30, 45, 60))

results.table <- list()

for (i in seq_len(nrow(permuted.options))) {
  results.table[[i]] <- alice.best.response(b = 2.5, f.n.range = f.n.range,
    force.eval.point = permuted.options[i, ], plot = FALSE)
}

results.table <- data.table::rbindlist(results.table)




latex.table(results.table[, .(
  b_per_day, ring_size, fee_per_byte, cost_effectiveness,
  user_tx_size_2in_2out = user_tx_size_2in_2out,
  user_tx_cost_2in_2out = user_tx_cost_2in_2out * 100,
  verif_time_2in_2out,
  block_size_no_attack = block_size_no_attack / 1000,
  verif_time_no_attack,
  one_year_blockchain_growth_unpruned_no_attack,
  one_year_blockchain_growth_pruned_no_attack,
  one_year_blockchain_growth_unpruned_attack,
  one_year_blockchain_growth_pruned_attack,
  effective_ring_size,
  block_size_attack = block_size_attack / 1000,
  verif_time_attack
)],
  title = "Cost effectiveness of minimum fee and ring size options when adversary budget is 2.5 XMR per day",
  output = "tables/permuted-cost-effectiveness-2_5-budget.tex",
  label = "table-2_5-budget",
  col.widths = c(1.75, 1, 1.5, 0.75, 1, 2, 1.5, 0.75, 1.4, 1, 1, 1, 1, 1, 0.75, 1.4),
  source.note = "Row in green is the status quo. Row in orange is the best cost effectiveness.",
  add.row.color =
    data.frame(row = c(2, which.min(results.table$cost_effectiveness)), color = c("green", "orange")))




b.options <- 2.5 * c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200)

results.table <- list()

results.table[[1]] <- alice.best.response(b = 2.5, f.n.range = f.n.range,
  force.eval.point = data.frame(f = 20 * 10^-9, n = 16), plot = FALSE)

for (i in seq_along(b.options)) {
  results.table[[i + 1]] <- alice.best.response(b = b.options[i], f.n.range = f.n.range, plot = FALSE)
}

results.table <- data.table::rbindlist(results.table)



latex.table(results.table[, .(
  b_per_day, ring_size, fee_per_byte, cost_effectiveness,
  user_tx_size_2in_2out = user_tx_size_2in_2out,
  user_tx_cost_2in_2out = user_tx_cost_2in_2out * 100,
  verif_time_2in_2out,
  block_size_no_attack = block_size_no_attack / 1000,
  verif_time_no_attack,
  one_year_blockchain_growth_unpruned_no_attack,
  one_year_blockchain_growth_pruned_no_attack,
  one_year_blockchain_growth_unpruned_attack,
  one_year_blockchain_growth_pruned_attack,
  effective_ring_size,
  block_size_attack = block_size_attack / 1000,
  verif_time_attack
)],
  title = "Minimum fee and ring size at optimal cost effectiveness, adversary budget scenarios",
  output = "tables/cost-effectiveness-budget-scenarios.tex",
  label = "table-scenarios-budget",
  col.widths = c(1.75, 1, 1.5, 0.75, 1, 2, 1.5, 0.75, 1.4, 1, 1, 1, 1, 1, 0.75, 1.4),
  source.note = "Row in green is the status quo",
  add.row.color = data.frame(row = 1, color = "green"))







# make plot
png(paste0("images/cost-effective-contour-plot-50-budget.png"), width = 800, height = 900)
alice.best.response(b = 50, f.n.range = f.n.range, plot = TRUE)
dev.off()


# make animation
library(animation)
library(Cairo)

# If Cairo is installed, use:
ani.options(interval = 2, ani.dev = "CairoPNG",
  ani.width = 800, ani.height = 900)
# Otherwise, use:
# ani.options(interval = 2, ani.width = 800, ani.height = 900)

saveGIF({
  for (i in seq_along(b.options)) {
    alice.best.response(b = b.options[i], f.n.range = f.n.range, plot = TRUE)
  }
}, movie.name = "cost-effectiveness-animation.gif")






library(huxtable)

# report_latex_dependencies()

replacement.names <- names(coef(perf.model.long))
names(replacement.names) <- replacement.names
names(replacement.names) <- gsub(":", " $\\times$ ", names(replacement.names), fixed = TRUE)
names(replacement.names) <- gsub("factor(ceiling(log2(outputs)))",
  "$\\lceil\\log_{2}(\\mathrm{outputs})\\rceil = $ ", names(replacement.names), fixed = TRUE)

names(replacement.names) <- gsub("log2",
  "$\\log_{2}$", names(replacement.names), fixed = TRUE)


names(replacement.names) <- gsub("ring_", "ring ", names(replacement.names), fixed = TRUE)


perf.model.long.tex <- huxreg(perf.model.long, error_pos = "same",
  statistics = c(N = "nobs", `Adjusted R-squared` = "adj.r.squared"),
  coefs = replacement.names,
  note = "Standard errors in parentheses. {stars}.")
# generics::glance(perf.model.long)

perf.model.long.tex.n.rows <- nrow(number_format(perf.model.long.tex))
number_format(perf.model.long.tex)[perf.model.long.tex.n.rows - 1, 2] <- "%.4f"
# number_format = "%.4f" so that the R^2 does not display as just rounded "1"
perf.model.long.tex <- set_escape_contents(perf.model.long.tex,
  row = seq_len(length(replacement.names) + 1), value = FALSE)
# Don't excape the Latex statements

perf.model.long.tex <- to_latex(perf.model.long.tex, tabular_only = TRUE)

cat(perf.model.long.tex, file = "tables/verification-time-regression.tex")







# P2Pool calculations

coinbase.outputs.per.block <- output.index[tx_num == 1, .N / num.blocks.before.spam]
# output.index[tx_num != 1, .N / num.blocks.before.spam]

print(coinbase.outputs.per.block * 30 * 24 * 356 * mean.size.one.ring.member * 59 / 1e+9)
# If coinbase consolations are ring size 1, then we have 2.7 GB less data in a year

# "Coinbase Consolidation Tx Type"
# https://github.com/monero-project/research-lab/issues/108
# "Avoid selecting coinbase outputs as decoys"
# https://github.com/monero-project/research-lab/issues/109



tenth.percentile.p2pool.output.amounts <- output.index[
  tx_num == 1 & 10^-12 * output_amount < 0.6, 10^-12 * quantile(output_amount, probs = 0.1)]

print( ( (32 * 4 + mean.size.one.ring.member * 16) * 20 * 1e-09) / tenth.percentile.p2pool.output.amounts )
# status quo: A ring consumes 5% of the coinbase output in fees

print( ( (32 * 4 + mean.size.one.ring.member * 60) * 70 * 1e-09) / tenth.percentile.p2pool.output.amounts )
# Ring size 60 and 60 nanoneros / byte fee: A ring consumes 49% of the coinbase output in fees

print( ( (32 * 4 + mean.size.one.ring.member * 1) * 70 * 1e-09) / tenth.percentile.p2pool.output.amounts )
# Ring size 1 and 60 nanoneros / byte fee: A ring consumes 3.6% of the coinbase output in fees



output.index[
  tx_num == 1 & 10^-12 * output_amount < 0.6, 10^-12 * quantile(output_amount, probs = 0.1)]


