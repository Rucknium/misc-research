


get_key_images_ring_member_indices <- function(x) {
  if (length(x$vin[[1]]$key$k_image) > 0) {
    ring_member_indices <- lapply(x$vin, FUN = function(x) {
      key_image = x$key$k_image
      result <- list(cumsum(as.integer(x$key$key_offsets)))
      names(result) <- key_image
      result
    })
  } else {
    ring_member_indices <- "MINER_TX"
  }
  unlist(ring_member_indices, recursive = FALSE)
}

invalidated.txs.key_images_ring_member_indices <- lapply(invalidated.txs,
  FUN = get_key_images_ring_member_indices)
names(invalidated.txs.key_images_ring_member_indices) <- invalidated.tx_hashes

valid.txs <- get_transactions(url.rpc, unique(merged.key_images$tx_hash.VALID), handle = handle)

valid.txs.key_images_ring_member_indices <- lapply(valid.txs,
  FUN = get_key_images_ring_member_indices)
names(valid.txs.key_images_ring_member_indices) <- unique(merged.key_images$tx_hash.VALID)

unlisted.invalidated.txs.key_images_ring_member_indices <-
  unlist(unname(invalidated.txs.key_images_ring_member_indices), recursive = FALSE)

intersection.ring_member_indices <- lapply(valid.txs.key_images_ring_member_indices,
  FUN = function(x) {
    intersection.ring_member_indices <- list()
    for (key_image in names(x)) {
      intersection.ring_member_indices[[key_image]] <- intersect(
        x[[key_image]],
        unlisted.invalidated.txs.key_images_ring_member_indices[[key_image]]
      )
    }
    intersection.ring_member_indices
})


table(lengths(unlist(intersection.ring_member_indices, recursive = FALSE)))


