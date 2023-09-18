
n <- 16
beta <- 0.05
C <- 0.4
n.rings <- 10000000
# 10 million rings

set.seed(314)
# Set the random seed so the results are reproducible

non.change.real.spend <- sample(c(1L, 0L), size = n.rings, replace = TRUE, prob = c(beta, 1 - beta))
# Draw n.rings elements with replacement from the set of {1, 0} with probability beta of drawing 1 and
# probability 1-beta of drawing 0. The 1 is a real spend output that has the defect.

real.spend <- ifelse(
  sample(c(TRUE, FALSE), size = n.rings, replace = TRUE, prob = c(C, 1 - C)),
  1L,
  non.change.real.spend)
# With probability C, the ring spends change. The change has the defect by assumption. With probability
# 1-C, the user spend a non-change output that has beta probability (above) of having the defect.

rings <- matrix(c(
  sample(c(1L, 0L), size = n.rings * (n - 1), replace = TRUE, prob = c(beta, 1 - beta)),
  real.spend),
  nrow = n.rings, ncol = n)
# Create a matrix n.rings x n in size. The first n-1 columns are filled with decoys. With probability
# beta these decoys have the defect. The last column is the real spend, created above.

n.defects.by.ring  <- rowSums(rings)
# Compute the number of ring members with defects in each ring
prob.correct <- (1/n) * sum(n.defects.by.ring == 0L) + sum( ( (1/n.defects.by.ring) * real.spend )[n.defects.by.ring > 0L])
100 * prob.correct/n.rings
# Result: 31.73675
# This calculates the probability of the classifier correctly guessing the real spend. We use a formula
# for 1/n and 1/n.defects.by.ring to "distribute" the guesses between the ring members with the defects.

# The operation below explicitly follows the rules of the classiffier to select
# one of the ring members as teh guessed real spend. It is much slower than the
# formula above
guesses <- apply(rings, 1, FUN = function(x) {
  if (sum(x) == 0L) {
    return(sample(1:n, size = 1))
  }
  # If none of the ring members have the defect, randomly guess that one of them
  # is the real spend, with equal probability.
  if (sum(x) == 1L) {
    return(which(x == 1L))
  }
  # If just one of the ring members has the defect, guess that the ring member
  # with the defect is the real spend
  return(sample(which(x == 1L), size = 1))
  # If more than one ring members have the defect, randomly guess that one of the
  # ring members with the defect is the real spend, with equal probability.
} )

100 * mean(guesses == n)
# Result: 31.7407
# If the guess was the n'th ring member, it is correct. The mean of the correct
# number of guesses is the empirical probability of correctly guessing the real spend.


mu_D0.hat <- sum(n.defects.by.ring == 0L)/n.rings
# Use the formula for mu_D=0 estimator

mu_C.hat <- 1 - mu_D0.hat/(1-beta)^n
# Use the formula for mu_C estimator

100 * mu_C.hat
# Result: 39.92552
# Very close to the true value of 40%

PPV <- function(n, beta, mu_C) {
  d <- 1:n
  (1/n)*(1-beta)^n*(1-mu_C) +
    sum( (1/d) * dbinom(d-1, n-1, beta) * (mu_C+beta*(1-mu_C)) )
}
# Formula for PPV estimator

100 * PPV(n = n, beta = beta, mu_C = mu_C.hat)
# Result: 31.6962
# The estimated PPV is very close to the values of 31.73675 and 31.7407

