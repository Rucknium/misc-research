


# Install gmp and ggplot2 packages:
# install.packages(c("gmp", "ggplot2"))
# May have to install this first: https://gmplib.org/

library(ggplot2)


# Use equation 2.1 of
# Holst, L. (1986). On Birthday, Collectors’, Occupancy and Other Classical Urn Problems.
# International Statistical Review / Revue Internationale de Statistique, 54(1), 15–27. https://doi.org/10.2307/1403255

# "In this paper we will consider problems connected with drawing with replacement from
# an urn with r balls of different colours."
# "The inverse of the occupancy problem is sometimes called the coupon collector's
# problem. It reads: how many draws are necessary for obtaining k different balls?"

# Our problem is a simplification where r = k. Then, r_(k) = r!


total.nodes <- 1000L
k <- 8L
# k is number of distinct pruning slices


coupon.pmf <- sapply(k:total.nodes, FUN = function(n) {
  r <- k
  gmp::div.bigq(
    gmp::mul.bigq(gmp::factorialZ(r), gmp::Stirling2(n = n - 1L, k = k - 1L)),
    gmp::pow.bigq(r, n)
  )
}
)

one.div.by.coupon.cdf <- vector("numeric", length(coupon.pmf))

for (i in seq_along(one.div.by.coupon.cdf)) {
  one.div.by.coupon.cdf[i] <- gmp::asNumeric(gmp::sub.bigq(1L, Reduce(gmp::add.bigz, coupon.pmf[1:i])))
}



png("images/pruned-node-collectors-problem-to-100.png", width = 600, height = 600)

ggplot(data.frame(x = k:total.nodes, y = one.div.by.coupon.cdf), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Probability of not having all 8 distinct pruned\nslices on the Monero network (8-100 nodes)") +
  ylab("Probability (log scale)") +
  xlab("Nodes on network (log scale)      github.com/Rucknium") +
  scale_x_log10(limits = c(8, 100), breaks = c(8, 16, 32, 64, 100)) +
  scale_y_log10(limits = c(1e-05, 1), breaks = 10^(0:-5)) +
  theme_set(theme_gray(base_size = 20))

dev.off()


security.128.bit <- gmp::asNumeric(gmp::div.bigq(1L, gmp::pow.bigq(2L, 128L)))
# https://bitcoin.stackexchange.com/questions/38512/is-12-word-seed-phrase-safe-enough

png("images/pruned-node-collectors-problem-to-1000.png", width = 600, height = 600)

ggplot(data.frame(x = k:total.nodes, y = one.div.by.coupon.cdf), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Probability of not having all 8 distinct pruned\nslices on the Monero network (8-1000 nodes)") +
  ylab("Probability (log scale)") +
  xlab("Nodes on network (log scale)      github.com/Rucknium") +
  scale_x_log10(limits = c(8, 1000), breaks = c(10, 50, 100, 500, 1000)) +
  scale_y_log10(limits = c(1e-60, 1), breaks = 10^(c(0, -10, -20, -30, -40, -50, -60))) +
  theme_set(theme_gray(base_size = 20)) +
  geom_hline(yintercept = security.128.bit, linetype = 2) +
  annotate("text", x = 15, y = security.128.bit * 200, size = 6,
    label = "2^-128", parse = TRUE)

dev.off()


n.nodes.lower.prob.than.128.bit.security.guessibility <-
  (k:total.nodes)[one.div.by.coupon.cdf <= security.128.bit][1]

print(n.nodes.lower.prob.than.128.bit.security.guessibility)


# Probability of missing at least one of the pruning slices at least one
# of the m times if all of the nodes re-draw the pruning seeds m times:

q <- 1000

# Use a network size of 100 nodes:

i <- which(k:total.nodes == 100)

1 - gmp::asNumeric(Reduce(gmp::add.bigz, coupon.pmf[1:i]))
# Drawn once

1 - gmp::asNumeric(gmp::pow.bigq(Reduce(gmp::add.bigz, coupon.pmf[1:i]), q))
# Re-drawn m times

