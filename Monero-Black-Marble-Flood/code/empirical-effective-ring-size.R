



CRYPTONOTE_DEFAULT_TX_SPENDABLE_AGE = 10
DIFFICULTY_TARGET_V2 = 120
DEFAULT_UNLOCK_TIME = CRYPTONOTE_DEFAULT_TX_SPENDABLE_AGE * DIFFICULTY_TARGET_V2
RECENT_SPEND_WINDOW = 15 * DIFFICULTY_TARGET_V2

SECONDS_IN_A_YEAR =  60 * 60 * 24 * 365
BLOCKS_IN_A_YEAR = SECONDS_IN_A_YEAR / DIFFICULTY_TARGET_V2




calculate_average_output_flow <- function(crod) {
  # 1
  num_blocks_to_consider_for_flow = min(c(length(crod), BLOCKS_IN_A_YEAR))
  
  # 2
  if (length(crod) > num_blocks_to_consider_for_flow) {
    num_outputs_to_consider_for_flow = crod[length(crod)] - crod[ length(crod) - num_blocks_to_consider_for_flow ]
    # R indexes from 1
  } else {
    num_outputs_to_consider_for_flow = crod[length(crod)] # R indexes from 1
  }
  
  # 3
  average_output_flow = DIFFICULTY_TARGET_V2 * num_blocks_to_consider_for_flow / num_outputs_to_consider_for_flow
  
  return(average_output_flow)
}

calculate_num_usable_rct_outputs <- function(crod) {
  # 1
  num_usable_crod_blocks = length(crod) - (CRYPTONOTE_DEFAULT_TX_SPENDABLE_AGE - 1)
  
  # 2
  num_usable_rct_outputs = crod[num_usable_crod_blocks] # R indexes from 1
  
  return(num_usable_rct_outputs)
}




GAMMA_SHAPE = 19.28
GAMMA_RATE = 1.61
# GAMMA_SCALE = 1 / GAMMA_RATE


G <- function(x) {
  actuar::plgamma(x, shapelog = GAMMA_SHAPE, ratelog = GAMMA_RATE)
}




crod <- xmr.rpc(url.rpc = paste0(url.rpc, "/json_rpc"), method = "get_output_distribution",
  params = list(amounts = list(0), from_height = 0, to_height = current.height, binary = FALSE, cumulative = TRUE))



start_height <- crod$result$distributions[[1]]$start_height
crod <- crod$result$distributions[[1]]$distribution
crod.full <- crod


spam.output_index <- list()

for (i in seq_along(spam.results)) {
  spam.output_index[[i]] <- list(name = spam.types[[i]]$fingerprint.text,
    output_index = spam.results[[i]]$spam.fingerprint$output_index)
}


n.workers <- min(floor(parallelly::availableCores()/2), 32L)

future::plan(future::multisession(workers = n.workers))


adversary.owned.dsa.mass <- future.apply::future_lapply((start.spam.height:current.height), function(ring.construction.height) {
  
  crod <- crod.full[1:(ring.construction.height - start_height + 1)]
  
  average_output_flow <- calculate_average_output_flow(crod)
  
  num_usable_rct_outputs <- calculate_num_usable_rct_outputs(crod)
  
  
  v <- average_output_flow
  z <- num_usable_rct_outputs
  
  
  G_star <- function(x) {
    (0 <= x*v & x*v <= 1800) *
      (G(x*v + 1200) - G(1200) +
          ( (x*v)/(1800) ) * G(1200)
      )/G(z*v + 1200) +
      (x*v > 1800) * G(x*v + 1200)/G(z*v + 1200)
  }
  
  
  usable.outputs <- 1:num_usable_rct_outputs
  
  crod.reversed <- cumsum(abs(diff(rev(crod)))[-(1:9)])
  # Remove first 9 blocks before cumsum() since cant spend from those outputs
  
  crod.reversed <- c(0, crod.reversed)
  
  y_0 <- crod.reversed[-length(crod.reversed)] + 1
  y_1 <- crod.reversed[-1]
  pmf.decoy.crod <- (G_star(y_1 + 1) - G_star(y_0)) / (y_1 + 1 - y_0)
  
  
  pmf.decoy <- rep(pmf.decoy.crod, times = diff(crod.reversed))
  
  
  
  pmf.decoy.reversed <- rev(pmf.decoy)
  
  result <- list()
  
  for (i in seq_along(spam.output_index)) {
    
    estimated.adversary.owned.share <- sum(pmf.decoy.reversed[
      spam.output_index[[i]]$output_index[ spam.output_index[[i]]$output_index <= length(pmf.decoy.reversed)]  ])
    
    result[[i]] <- data.table(ring.construction.height = ring.construction.height,
      estimated.adversary.owned.share = estimated.adversary.owned.share,
      type = spam.output_index[[i]]$name)
    
  }
  
  rbindlist(result)
  
})



adversary.owned.dsa.mass <- rbindlist(adversary.owned.dsa.mass)

adversary.owned.dsa.mass <- merge(adversary.owned.dsa.mass, block.data[, .(height, timestamp.POSIX)],
  by.x = "ring.construction.height", by.y = "height")

setorder(adversary.owned.dsa.mass, timestamp.POSIX)

adversary.owned.dsa.mass[, effective.ring.size := 1 + (1 - estimated.adversary.owned.share) * 15]


png("empirical-effective-ring-size.png", width = 800, height = 800)

ggplot(adversary.owned.dsa.mass, aes(x = timestamp.POSIX, y = effective.ring.size, colour = type)) +
  geom_line() +
  scale_y_continuous(breaks = 1:16, limits = c(0, NA), expand = c(0, 0)) +
  scale_x_datetime(date_breaks = "day", guide = guide_axis(angle = 90)) +
  ggtitle("Estimated mean effective ring size") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Mean effective ring size") +
  labs(colour = "Spam type") +
  theme(legend.position = "top", legend.text = element_text(size = 15), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))

dev.off()




guess.prob <- function(effective.ring.size, nominal.ring.size) {
  decoys <- nominal.ring.size - 1
  sapply(effective.ring.size, FUN = function(x) {
    weighted.mean(1/(1 + 0:decoys),
      w = dbinom(0:decoys, size = decoys, prob = (x - 1)/decoys))
  })
}



adversary.owned.dsa.mass[, guess.prob := guess.prob(effective.ring.size, nominal.ring.size = 16)]

png("empirical-guessing-probability.png", width = 800, height = 800)

ggplot(adversary.owned.dsa.mass, aes(x = timestamp.POSIX, y = guess.prob, colour = type)) +
  geom_line() +
  scale_y_continuous( limits = c(0, NA), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_datetime(date_breaks = "day", guide = guide_axis(angle = 90)) +
  ggtitle("Estimated probability of correctly guessing the real spend") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Probability") +
  labs(colour = "Spam type") +
  theme(legend.position = "top", legend.text = element_text(size = 15), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))

dev.off()



adversary.owned.dsa.mass[, effective.ring.size.one := dbinom(0, size = 15, prob = 1 - estimated.adversary.owned.share)]

png("empirical-ring-size-one.png", width = 800, height = 800)

ggplot(adversary.owned.dsa.mass, aes(x = timestamp.POSIX, y = effective.ring.size.one, colour = type)) +
  geom_line() +
  scale_y_continuous( limits = c(0, NA), expand = c(0, 0), labels = scales::label_percent()) +
  scale_x_datetime(date_breaks = "day", guide = guide_axis(angle = 90)) +
  ggtitle("Estimated share of rings with effective ring size of one") +
  xlab("                                                    Date                      github.com/Rucknium") +
  ylab("Share of rings") +
  labs(colour = "Spam type") +
  theme(legend.position = "top", legend.text = element_text(size = 15), legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15), strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 5)))

dev.off()

future::plan(future::sequential)
# Reset to remove threaded R sessions to get back RAM




