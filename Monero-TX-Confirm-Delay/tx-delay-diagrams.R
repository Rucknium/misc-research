

library(data.table)

set.seed(314)

txs <- cumsum(rexp(50))
# Transaction arrival can be modeled as a Poisson process. The time interval
# between arrival of transactions are independent exponential random variables
txs <- txs/txs[length(txs)]

# ideal
blk.geom.ideal <- list(
  list(
    col = "red",
    text = "Block mined\nby red pool",
    tx0 = 0,
    tx1 = 0.4,
    block = 0.4
  ),
  list(
    col = "blue",
    text = "Block mined\nby blue pool",
    tx0 = 0.4,
    tx1 = 0.7,
    block = 0.7
  ),
  list(
    col = "darkgreen",
    text = "Block mined\nby green pool",
    tx0 = 0.7,
    tx1 = 0.9,
    block = 0.9
  ),
  list(
    col = "purple",
    text = "Block mined\nby purple pool",
    tx0 = 0.9,
    tx1 = 1.2,
    block = 1.2
  ),
  list(
    col = "brown",
    text = "Block mined\nby brown pool",
    tx0 = 0.9,
    tx1 = 1.2,
    block = 1.5
  )
)

# centralized pool
blk.geom.pool <- list(
  list(
    col = "red",
    text = "Block mined\nby red pool",
    tx0 = 0,
    tx1 = 0.25,
    block = 0.4
  ),
  list(
    col = "blue",
    text = "Block mined\nby blue pool",
    tx0 = 0.25,
    tx1 = 0.4,
    block = 0.7
  ),
  list(
    col = "darkgreen",
    text = "Block mined\nby green pool",
    tx0 = 0.4,
    tx1 = 0.7,
    block = 0.9
  ),
  list(
    col = "purple",
    text = "Block mined\nby purple pool",
    tx0 = 0.7,
    tx1 = 0.9,
    block = 1.2
  ),
  list(
    col = "brown",
    text = "Block mined\nby brown pool",
    tx0 = 0.9,
    tx1 = 1.2,
    block = 1.5
  )
)

# p2pool
blk.geom.p2pool <- list(
  list(
    col = "red",
    text = "Block mined\nby red pool",
    tx0 = 0,
    tx1 = 0.25,
    block = 0.4
  ),
  list(
    col = "#FF6600FF", # Monero orange
    text = "Block mined\nby p2pool",
    tx0 = 0.25,
    tx1 = 0.67,
    block = 0.7
  ),
  list(
    col = "darkgreen",
    text = "Block mined\nby green pool",
    tx0 = 0.67,
    tx1 = 0.7,
    block = 0.9
  ),
  list(
    col = "purple",
    text = "Block mined\nby purple pool",
    tx0 = 0.7,
    tx1 = 0.9,
    block = 1.2
  ),
  list(
    col = "brown",
    text = "Block mined\nby brown pool",
    tx0 = 0.9,
    tx1 = 1.2,
    block = 1.5
  )
)

blk.geoms <- list(blk.geom.ideal, blk.geom.pool, blk.geom.p2pool)


filenames <- c(
  "Monero-TX-Confirm-Delay/images/xmr-pool-ideal.png",
  "Monero-TX-Confirm-Delay/images/xmr-pool-delay.png",
  "Monero-TX-Confirm-Delay/images/xmr-pool-p2pool-mix.png")

plot.titles <- c(
  "Ideal Case",
  "Pool Delay Case",
  "Mixed P2Pool and Pool Delay Case"
)

for (diagram in 1:3) {
  
  blk.geom <- blk.geoms[[diagram]]
  
  png(filenames[diagram], width = 600, height = 200)
  
  par(mar = c(0, 0, 0, 0))
  
  plot(txs, rep(0, length(txs)),
    pch = 20, cex = 0.25,
    bty = "n", axes = FALSE,
    frame.plot = FALSE, xaxt = "n", ann = FALSE, yaxt = "n",
    ylim = c(-0.35, 0.20), xlim = c(0, 1))
  
  abline(h = -0.1)
  text(0.5, 0.18, labels = paste0("Monero Transactions Included in a Block: ",
    plot.titles[diagram]), cex = 1.2)
  text(0.5, 0.1, labels = "Time")
  lines(c(0.15, 0.47), c(0.1, 0.1))
  arrows(0.53, 0.1, 0.85, 0.1)
  text(0.1, -0.3, labels = "github.com/Rucknium")
  
  ellipse.bounds <- function(x0, x1) {
    list(center = mean(c(x0, x1)), width = 0.5 * diff(c(x0, x1)))
  }
  
  for ( i in seq_along(blk.geom)) {
    
    ellipse.bds <- ellipse.bounds(blk.geom[[i]]$tx0, blk.geom[[i]]$tx1)
    
    plotrix::draw.ellipse(ellipse.bds$center, 0, 
      ellipse.bds$width, c(0.025),
      border = blk.geom[[i]]$col)
    
    txs.in.block <- txs[txs %between% c(blk.geom[[i]]$tx0, blk.geom[[i]]$tx1)]
    
    points(blk.geom[[i]]$block, -0.1, pch = 15, col = blk.geom[[i]]$col)
    text(blk.geom[[i]]$block, -0.15, col = blk.geom[[i]]$col, cex = 1, pos = 1,
      labels = paste0(blk.geom[[i]]$text, ".\n\nAverage tx\nconfirm delay: ", 
        round(10 * mean(blk.geom[[i]]$block - txs.in.block), 1)))
    lines(c(ellipse.bds$center, blk.geom[[i]]$block), c(-0.025, -0.1), col = blk.geom[[i]]$col)
    lines(c(blk.geom[[i]]$tx1, blk.geom[[i]]$block), c(0, -0.1), col = blk.geom[[i]]$col)
    
  }
  
  dev.off()
  
}





