
# Install packages:
# install.packages(c("data.table", "remotes"))
# remotes::install_github("Rucknium/xmrpeers", upgrade = FALSE)

library(data.table)
library(xmrpeers)

data(good_peers)


good_peers <- stringr::str_extract(good_peers,
  "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
good_peers <- unique(good_peers)
good_peers <- na.omit(good_peers)
# Clean IP addresses

data(ban_list)
suspected.malicious.ips <- ban_list


suspected.malicious.ips.exact <- good_peers[xmrpeers::in.ip.set(good_peers, suspected.malicious.ips)]

honest.reachable.peers <- setdiff(good_peers, suspected.malicious.ips.exact)


h_s <- uniqueN(honest.reachable.peers)
h_d <- uniqueN(as.subnet(honest.reachable.peers, 24))

h_s / h_d

adversary.subnet.tabulation <- table(as.subnet(suspected.malicious.ips.exact, 24))

approx.subnet.distinct.adversary <- 
  sum(adversary.subnet.tabulation[adversary.subnet.tabulation <= 5])
# If less than 5, assume that the adversary did not rent the whole subnet

approx.whole.subnet.adversary <- length(adversary.subnet.tabulation[adversary.subnet.tabulation > 5])

w_s <- 0.59
w_d <- 1

approx.expenditure <- w_d * approx.subnet.distinct.adversary + w_s * 254 * approx.whole.subnet.adversary


b <- approx.expenditure

message("h_s: ", h_s)
message("h_d: ", h_d)
message("w_s: ", w_s)
message("w_d: ", w_d)
message("b: ", b)
message("h_s/h_d price premium is: ", h_s / h_d)



w_d / w_s
h_s / h_d



p_ss <- b/(w_s * h_s + b)
p_dd <- b/(w_d * h_d + b)

p_ss
p_dd

p_ds <- b/(254 * w_s * h_d + b)
p_sd <- b/(w_d * h_s + b)

p_ds
p_sd

# p_ss   |   p_sd
# p_ds   |   p_dd

p_ss
p_sd
p_ds
p_dd


# For strict dominance, need: 
# p_sd > p_dd
w_d * h_s < w_d * h_d
h_s < h_d
# Which are false


# So, instead, check the conditions (17) and (18) of Theorem 1.2 of Sun (2022):
stopifnot((p_ss - p_sd) * (p_dd - p_ds) > 0)
stopifnot((p_ss - p_ds) * (p_dd - p_sd) > 0)


P_protocol_status_quo <- (p_dd - p_ds) /
  (p_ss - p_sd - p_ds + p_dd)

message("Protocol uses status quo strategy with probability: ", P_protocol_status_quo)
message("Protocol uses subnet deduplication strategy with probability: ", 1 - P_protocol_status_quo)

P_adversary_whole_subnets <- (p_dd - p_sd) /
  (p_ss - p_sd - p_ds + p_dd)

message("Adversary uses whole subnets strategy with probability: ", P_adversary_whole_subnets)
message("Adversary uses subnet-distinct strategy with probability: ", 1 - P_adversary_whole_subnets)


u_adversary <- (p_ss * p_dd - p_sd * p_ds) /
  (p_ss - p_sd - p_ds + p_dd)

u_adversary_check <- 
  p_ss * P_protocol_status_quo * P_adversary_whole_subnets +
  p_ds * (1 - P_protocol_status_quo) * P_adversary_whole_subnets +
  p_sd * P_protocol_status_quo * (1 - P_adversary_whole_subnets) +
  p_dd * (1 - P_protocol_status_quo) * (1 - P_adversary_whole_subnets)
# Check with direct computation

stopifnot(all.equal(u_adversary, u_adversary_check))

message("Probability that an honest connection is established to an adversary: ", u_adversary)





