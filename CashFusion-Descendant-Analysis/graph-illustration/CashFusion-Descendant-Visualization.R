# install.packages("igraph")

library(igraph)

edgelist <- structure(list(origin = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
  9L, 10L, 10L, 11L, 12L, 12L, 15L, 16L, 17L, 18L, 19L, 19L, 20L, 
  20L, 20L, 22L, 22L, 25L, 25L, 26L, 27L, 27L, 30L, 30L, 29L, 37L, 
  37L, 39L), destination = c(10L, 10L, 10L, 10L, 22L, 12L, 12L, 
    12L, 13L, 19L, 14L, 15L, 20L, 16L, 17L, 20L, 22L, 36L, 30L, 23L, 
    31L, 25L, 26L, 27L, 21L, 28L, 24L, 32L, 33L, 34L, 35L, 29L, 37L, 
    36L, 38L, 39L, 40L)), class = "data.frame", row.names = c(NA, 
      -37L))

vertex.colors <- structure(list(name = 1:40, color = c("blue", "purple", "purple", 
  "purple", "blue", "purple", "blue", "blue", "blue", "red", "blue", 
  "red", "green", "orange", "purple", "purple", "purple", "blue", 
  "blue", "purple", "orange", "purple", "green", "orange", "purple", 
  "purple", "purple", "purple", "blue", "blue", "green", "orange", 
  "orange", "orange", "orange", "orange", "blue", "green", "blue", 
  "green")), class = "data.frame", row.names = c(NA, -40L))

illustrative.graph <- igraph::graph_from_data_frame(edgelist, vertices = vertex.colors)

V(illustrative.graph)$color <- V(illustrative.graph)$color
V(illustrative.graph)$label <- NA

png("CashFusion-Descendant-Analysis/graph-illustration/CashFusion-Descendant-Visualization.png",
  width = 480 * 2, height = 480 * 2, type ="cairo")

plot(illustrative.graph, 
  layout = layout_with_kk, 
  edge.arrow.size = 1, vertex.size = 7, vertex.frame.color = "transparent",
  edge.arrow.width = 0.75, edge.color = "black")

legend("topleft", 
  legend = c("CashFusion", 
    "Spent Cashfusion Descendant", "Spent Non-Cashfusion Descendant", 
    "Unspent Cashfusion Descendant", "Unspent non-Cashfusion Descendant"),
  fill = c("red", "purple", "blue", "orange", "green"),
  cex = 1.8, bty = "n")

dev.off()
