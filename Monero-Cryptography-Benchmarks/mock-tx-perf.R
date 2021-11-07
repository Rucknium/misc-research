
perf.tests.txt <- readLines("Monero-Cryptography-Benchmarks/raw-data/mock_tx_perf_z2a_1.txt")

perf.tests.txt <- perf.tests.txt[(-1) * 1:46]
perf.tests.txt <- perf.tests.txt[perf.tests.txt != ""]
perf.tests.txt <- perf.tests.txt[substr(perf.tests.txt, 1, 4) != "2021"]


i <- 2

while (i <= length(perf.tests.txt)) {
  if ( substr(perf.tests.txt[i], 1, 12) != "test_mock_tx") {
    perf.tests.txt <- perf.tests.txt[ (-1) * i ]
  }
  i <- i + 2
}


test_mock_tx.data <- perf.tests.txt[grepl("^test_mock_tx", perf.tests.txt)]
params.data <- perf.tests.txt[ ! grepl("^test_mock_tx", perf.tests.txt)]

stopifnot(length(test_mock_tx.data) == length(params.data))

params.data <- strsplit(params.data, " || ", fixed = TRUE)
params.data <- lapply(params.data, FUN = function(x) {
  gsub(".+: ", "", x)
})

stopifnot(all(lengths(params.data) == 7))

params.data <- as.data.frame(matrix(unlist(params.data), ncol = 7, byrow = TRUE))
for (i in 2:7) {
  params.data[, i] <- as.numeric(params.data[, i])
}

colnames(params.data) <- c("type", "size.bytes", "batch.size", "inputs", 
  "outputs", "rangeproof.split", "ref.set.size")

test_mock_tx.data <- gsub(".+> +", "", test_mock_tx.data)

test_mock_tx.data <- strsplit(test_mock_tx.data, " +")

stopifnot(all(lengths(test_mock_tx.data) == 19))

test_mock_tx.data <- as.data.frame(matrix(as.numeric(unlist(test_mock_tx.data)), 
  ncol = 19, byrow = TRUE))

# https://github.com/monero-project/monero/blob/master/src/common/timings.h
colnames(test_mock_tx.data) <- c(
  "time_t",
  "size_t",
  "min",
  "max",
  "mean",
  "median",
  "stdev",
  "npskew",
  paste0("percentile.", seq(0, 100, by = 10))
)

combined.data <- cbind(params.data, test_mock_tx.data)

write.csv(combined.data, file = "Monero-Cryptography-Benchmarks/processed-data/mock_tx_perf_z2a_1.csv", row.names = FALSE)

# Very preliminary analysis starts below




table(combined.data$type)

summary(combined.data$stdev/combined.data$mean)


summary(lm(mean ~ type + size.bytes + batch.size + inputs + 
    outputs + rangeproof.split + ref.set.size, data = combined.data))


combined.data.inflated <- combined.data[rep(1:nrow(combined.data), 50), ]


summary(lm(mean ~ type + size.bytes + batch.size + inputs + 
    outputs + rangeproof.split + ref.set.size, data = combined.data.inflated))

summary(lm(log(mean) ~ type + size.bytes + batch.size + inputs + 
    outputs + rangeproof.split + ref.set.size, data = combined.data.inflated))

summary(lm(log(mean) ~ type + log(size.bytes) + log(batch.size) + log(inputs) + 
    log(outputs) + log(I(rangeproof.split + 1)) + log(ref.set.size), data = combined.data.inflated))
