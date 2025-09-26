


case.study.tx_hash <- "d3e574c05b01f5d74815e3894f55e1645f402cdbf0415877f49e752a71b0d376"

case.study.tx <- get_transactions(url.rpc, case.study.tx_hash, handle = handle)[[1]]

case.study.tx.ring.member.indices <- cumsum(case.study.tx$vin[[1]]$key$key_offsets)

orphaned.blocks[orphaned.blocks$tx_hash == case.study.tx_hash, ]


t(orphaned.blocks[orphaned.blocks$global_output_index %in% case.study.tx.ring.member.indices, ])

# block_height        "3499660"                                                         
# block_hash          "385e28b4945f04c4f4593f9baf4d76e6a676c189d499a6c71f8db9b6a5179411"
# tx_hash             "ca774d41eba6e5174f54340af1ef48f5eff5253963f8f0fba4a7fe383af97821"
# global_output_index "139283631"                                                       
# output_public_key   "b2de1e442b60b70ac681e9044581bb2dcbf66c97865b340d9a8ae4e3bb8e3d61"

t(mainchain.blocks[mainchain.blocks$global_output_index %in% case.study.tx.ring.member.indices, ])

# block_height        "3499665"                                                         
# block_hash          "dab9f76a5462c058c92a8d08f87cb22a28aa40ed88aed759da3430ffac495e60"
# tx_hash             "4125d5bf7c2c6df3d3aec3f5567f641fb2ee1786a212b68a499f133acd0a98d6"
# global_output_index "139283631"                                                       
# output_public_key   "61b08cd8a250fd2558cdd1a63ebfa38aeea0c9d4395c9223d315439fbc2e0e68"



case.study.tx_hash.VALID <- merged.key_images[tx_hash.INVALIDATED == case.study.tx_hash, tx_hash.VALID]


intersection.ring_member_indices[case.study.tx_hash.VALID]
# Discovers real spend
# Real spend has index number 139282277


