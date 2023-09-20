
# install.packages("gt")

library(gt)

PPV <- function(n, beta, mu_C) {
  d <- 1:n
  (1/n)*(1-beta)^n*(1-mu_C) +
    sum( (1/d) * dbinom(d-1, n-1, beta) * (mu_C+beta*(1-mu_C)) )
}
# Formula for PPV estimator


PPV.matrix <- function(n, beta.set, mu_c.set) {

  results <- matrix(0, nrow = length(beta.set), ncol = length(mu_c.set),
    dimnames = list(beta.set*100, mu_c.set*100))

  for (i in beta.set) {
    for (j in mu_c.set) {
      results[i == beta.set, j == mu_c.set] <- PPV(n, i, j)
    }
  }

  results

}


latex.table <- function(x, title) {

  latex.output <- as.data.frame(x) |>
    gt(rownames_to_stub = TRUE) |>
    tab_header(title = title ) |>
    tab_stubhead(label = "% blockchain outputs with defect") |>
    tab_spanner(label = "% rings of defective txs with change output as the real spend", columns = colnames(x) ) |>
  #  tab_style(style = cell_text(align = "center"), locations = cells_stubhead()) |> # Not properly implemented for LaTeX
    as_latex() |>
    as.character()

  cat(gsub("begin{longtable}{l", "begin{longtable}{r", latex.output, fixed = TRUE))

  invisible(NULL)

}



beta.set <- c(0.001, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50)
mu_c.set <- seq(0.3, 0.7, by = 0.05)

PPV.16 <- PPV.matrix(16, beta.set, mu_c.set)

latex.table(round(PPV.16*100, 2), "Positive Predictive Value when ring size is 16")


PPV.128 <- PPV.matrix(128, beta.set, mu_c.set)

latex.table(round(PPV.128*100, 2), "Positive Predictive Value when ring size is 128")


beta.set.multiplier <- matrix(beta.set, nrow = length(beta.set), ncol = length(mu_c.set),
  dimnames = list(beta.set*100, mu_c.set*100))

latex.table(round((PPV.16 - 1/16)*beta.set.multiplier*100, 2), "(PPV - 1/ring_size)*(share of rings affected) when ring size is 16")

latex.table(round((PPV.128 - 1/128)*beta.set.multiplier*100, 2), "(PPV - 1/ring_size)*(share of rings affected) when ring size is 128")

