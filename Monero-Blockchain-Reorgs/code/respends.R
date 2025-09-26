


library(data.table)

handle <- RCurl::getCurlHandle()


# From https://github.com/WeebDataHoarder/Monero-Timeline-Sep14/blob/master/data/transactions/invalidated.txt
invalidated.tx_hashes.hardcoded <- c(
  "c64ed94341b181ad6420e749e92bd6ab31733c45de4ef6f9c73f737a10afffc1",
  "428917da7c9c55154ac6ffcb1fd9d514ef5f8bce2b3cc911c62a3beafee12d95",
  "afb3634976b16bc2797edaa2d45d83e7d34be6a6bc54a5e9b6bdbab4660e2bd1",
  "9184561df39a420fad62f39351827c8910ad7699d840f1ed953f15cca374152f",
  "f1de1f0c1540720e236fb7982cd25a8a1f9ea305e2b13bdcd19bd1dd40c8ed61",
  "45a48c3aca8e76dca4fcbc2dbc2703696dca648123895fc91cf632a3c746b153",
  "9edc0c1266b50bab292d23dc9476f2c74e629f85e385c951b2c38a1a3d8a082c",
  "7e49f939266eded6194f64a6a596dd01db8a8daa04c1158af1bab41e34eacc26",
  "f72f9d69e7545c199ab9a72f16578f690b2a5efefbc19630c544fc1b6a4b1c76",
  "eca24321c38106c2d88fe732675a78bd916bf8e6a012b5c445a315aa5a348886",
  "b9c17d37ab4901ffeb5d927e1467965e0af9675cc11ec5197950ec4acda6636e",
  "a51b5ac9964ae41ba4de0101fc667261055710b2f0512b1166b74bc89be77935",
  "b8c746f9d446242fb204e205219f445adc0d6b991b585ec63aed2c562692865d",
  "9f4590ce23aa5d829f717cde0ab8b0eedf53b4245eeb8c036c64e39fc0c33f33",
  "02b3504e7fc80ef680f3bb1e1d7d11e103bbb43fbee9cfb5c2a9651aaf1b0aa1",
  "46b957d6430b7d2e4fb6540fb369f0820a5743dd123330c2725fe96f35942624",
  "d8b374d11fea032f502c2d0519dad08c90e6e49cbb9d2d5a7356575aa99594bd",
  "094ff165e386cf207a538eb748e0494f45eb8592bf84de7601406aa6a2161488",
  "5f2661988119ce087d82850265f664ffecf7465e2f923f4e4f4b208b8db1e966",
  "dde2119a2c69b23f95b5e632288ae9fb20b4ba98e285e45017c009a846ddd861",
  "b301ddac6edb0d1438a0c45ab7823887274e53df19c55c57b0a76f9e43fcf9f7",
  "d411a2ba93f484bdf627d68354857794e7c6fec38d58a3c9f89a53b5f12371ab",
  "6d4ddfc9712e1ebc1d037b4cb44a31e6933f6587f56f542da98b181c7ebb78dd",
  "8fb220c4045424b10512818a5d8f004fc54862849f620000a67209a86da9ae7d",
  "1b807eb59dbf276aee8b321f53bcac4dfff99c1f01b76208ec9cdfba612afb5e",
  "05acc65204712a9a7ba1ff1b9eeb432cdd754de9f41a9cb42c41fcd82e0dd19b",
  "65a3c4bde5fc7bc69598f740958af52305cc61b5744d0ddd1d125e1be452e6fa",
  "7b433ef786287db48a0ea59dad27a21311a70930e887d6fce3c33e8c6b149049",
  "9adf3e46a3b3abe54c181617fa0874b1f96e1c7a3ee0c7e555c31021198d15ab",
  "ab1db5e6442db6c8c6522fa59d2661755897a697285fb5071234665fcf449499",
  "b2b7bdf4ddcd63d458797c3bb84bd3429fb6a2444f61e1fc6c432095409982ca",
  "2a15be8f65cb19a72d00b059bdd413a1d0a6c77f4cfa5a191977194ed5fa3880",
  "d455088f1285989b0f2e5aa7b795c5b4deb3f1c4f5f555ae4d74a6ed892a96ae",
  "d5eb089c938a39a1c43677a2e6fa315f8eb33e369fcc578810c5b7014af096a7",
  "ac2b400ccef23e33837d2a6f02e80de21bc19b07384a530ecfbdf8940ff2612e",
  "14526812fa30bb0c80936895f1520dc11d74cd495d3250f09786092b7f5ab482",
  "717d5c1a0921ac6f41276c1d916abc8d96bc36aa44def356df747bf0a01f1da8",
  "1593b9784d6cda1c4386069039095fefacabc32abbbbe6980a51c11997f439da",
  "b5acd124068f28b4f0a29f9df6f784e61f964f9b07559d36c316d88ca03c1940",
  "6a3d975581a499af48416109788dd0e5762e09af61a809cc240ead100127cb29",
  "7998a18e7e6f217d3c7b02bc0cbad679036fdf554c6090f758fa46b0aae79a57",
  "69d60ef6fdba2300271ed04f03fdd4dedbc3e459036af060f7cde5be0a2907c5",
  "9869dc8cc4776a617e91cc75802477d2e20cdeeaed879bee2739654d4bab43be",
  "a883543d9300b87bed342da13e7a378878a2720b48d1df969d81c464353b3697",
  "2a065be8fc565ef4a0a786779ea0671fcc9347ef602222fea33914d1cf3008f6",
  "bcd08c86011a5337e9c9fd66d65c025dd97edba4045147e626d680eb0e2c57bf",
  "32f2d6baa8569a123a68b08403c4106a907327f151e32f1b5304bd0aa2967eda",
  "8dc0690698e1796661abee2a139721f1356736bdb3a1d6f1d32cb1a662e54313",
  "c713262e5fc1d9908d8bb5163588cbba15c811f5a1c83b4bdeef0a32400cb464",
  "f79b2d16bda2f2cc2d65513bb998bade4b070b1cd900c643f59dff4fdac011a9",
  "4178d7483b687cefe91a56047bd3d0d28856c79888cc478ef588fc6e44625d06",
  "974593c1db6d5bc8c8f79cf562447d5ad032a4aedfec7e21bafc9477e45bf657",
  "0f10e24a541c749d0a6c671e753f77099b38b11b53ba3ffd222c4ef7272e531e",
  "f009dd8a3a1e99aacd8e57cd431e99b07d4c6fcc9d06f8a251a1106bf594b9d0",
  "eda5c60ff2a461edcd1a511745b812fd84e2e8c836f764caa569b19e3fa49c2e",
  "8e4c6cc0a3688eeab7f7c0d44a61a76953c9759703fe479f2d5a8e491c4d14ee",
  "37d206e9f1478e4bad5019759175cf976c5c0804b06579d911ded4bd148b0bf2",
  "e4abfb9d0135aa777de931976419ecf97824fb7ffd041584f1110069982c5a7c",
  "002e49ec807bd0f19eef14e459e156f46c63e268d124b0189b29394acae8d518",
  "1bc78f2dbb5e2521e676840983bf09b5d2073326a6ee1a6b2c321f128ed1a6ee",
  "cb559fa19ab075e6e9e7e0220289da4fa1f77914e9af661e3369d574c4fff8f9",
  "cb06efc25a912d0f40eed60bba5f19abef06c6c4a53e5a6d3483cac80ecdb17b",
  "e6dfae64599e5925283aac4a7cd2fca75dc334a1df7071ef134a7b57211e5755",
  "245e998945430c7f6f1f19fc6ad2d974bc7abb1c0e5a937906fcc5db9c5f125a",
  "82a7b3b86523a8322fa5b1f67a9cee92187c0e2e90f9dff5ca09fde89b258bb9",
  "37b24712dbe39c1e47c046a05de7ad96c8615531512c83fa1bdfe192a75ab186",
  "7e049a44d3d57da4d1e18033134548941055a0a4989ac64110e2de7d1f224cab",
  "683c5806184882d92a70c4595a8c9ffc380d663e8f6fe954f2619d3cf0f67709",
  "87147b78614853867be937e51aff26f2b74f619345433b65e2798517b5556884",
  "c7eab13efc495d0c481d0484fa8702cbe577407af8f253e4a36860649864f7c5",
  "5b663e42808900f37c9cbc1ac4dfd2ee0c4d0a93e6fdbcf0f1c18fc77fc2cf80",
  "36a05a9febc2be16a9a9b4b08b21d5d4d82b1032828f50c4fdf5ed1a1e0e2e20",
  "49af54c0b77064841617373cfa8f56ba8e8be2c0ec9afb22fd256cfd51388bb5",
  "da1f09d852224b1487e4dcc608a61e40be81aeaf7d556d7f876cc13917c14884",
  "13414b5224ab8d1d0ad1047dc7c84c62fc3b6b94a35364affe9c4a56ffe597b1",
  "d3e574c05b01f5d74815e3894f55e1645f402cdbf0415877f49e752a71b0d376",
  "7fa68fad6210bdfd13728b052aadf914260596403ad8a46916b17284e0fce89f",
  "4dede28888f871dd69c47d371367bbdf308a609fbc24cab3ba628fa3ba6d8ed9",
  "1cd0a65bfa6ad0b0475305600a7be3dae3900e416c0774bae0580be3da79fec8",
  "c2451856773f3a17a3bfad55bf33acba0fdfcaf49630c7eb917fbc83046b7d02",
  "8768259ed4126182b229380deefdc8597e7facb945431f7da23fe67e9d4c4e60",
  "f9142b64239a084b9e7f692e4ba1e3f9ba13acba61d38219964f8e7ffcbfbfc4",
  "e8fba8940bceabd19ca5d576be0b3833b457d2ebcbdacb465e2cc0da34168763",
  "8b4a387f4dc1a0cfbf9f129108cd9a1a02dd51c7b699feb02c921251b4cc904f",
  "72bb973375b4c3c917e980263fc4cbfc988f8b694d614da6fca20266357967af",
  "991cb2db3e6d79e30f60c5811da53133cc7acd169f85741d69343c102f1827b0",
  "63d28995b3008c0bc90b6a6d03ab96862f0baf5607b756d6ed4e9334dd599d74",
  "dbb7cffcad490a8be9f90f65223164e131e975a80565730cf79846cf73236304",
  "2c08c5f8e0698323119048e5662f219edc7981dedbe3ddb9d6c3f539dd6acf06",
  "2bb596c72a3685f0ebbc5f8f16f4ffe4b9c04740333c1104dcf8bdf2e0c93532",
  "72c08885e0646d99ab38225373a0b43fbefcfdda074c8c6f36b6b69f837cb87f",
  "3ffb5e6378f7eec281b5ed64d203a83c0979124d7bc8e03863e8cd24a597d901",
  "6898abe2589e8b7db76fc7536e96402c4315489c1f6877548b379b36a7b0a73b",
  "9e3a051cc41af870ba821c84b619019adca014ce9fd8f84f1cb5d3da29f4e2b8",
  "e93c6acf3e1c6c6afd99e88a225b608d00e69a8631d08a5f56449f7b888af33b",
  "97466ad19fce7f268a56317ec6e095752b6ec18f1e8cf1904a1cb123032baa63",
  "95256143cde0b9c3b37bf92601f1919ab15dab201220b165036de16486a8c143",
  "fd02f5958575ea5a11d11be645ddcddd99eabee664659b3f119a25686a8c89a7",
  "b383199502fcd2c2d4500b3dc29400db84336b8dc480171274fbca4937d8f19c",
  "f6d94f843fb6e803647294086587df46e87cb872a0afe49f504480bf0aca5849",
  "0686f5c1ee8c68ee04f171ec18547f2dce34856e9cb4b21310c36d91b15b5cdf",
  "157fae945c021c2833d71811895bcc14957821d4dcb2c0095740dce597becbb4",
  "953ff5e5bbeb9cf47818b63bb19435c39bbe3416070d3c1e7d27e40f8e8b62ae",
  "f07b0261e56acb27b0613876815d0f68ee07af4b352553a3f246d3997e70c1e2",
  "3e917a5c5df21d5f7b671c269257afae8ba28cc3d6700a3c1bb1f6717aa2e808",
  "f01996ee2144700786b3420705499901e886b0d2858a2761a0819879b697f03b",
  "b4c89d7a8ccee4961a46fb5148e6c8039a9e1460affbfc70bdefc067c43f3f96",
  "776e029928532c85eac5e63d6960874c63745005914bcef7bc60a6a53c73de6c",
  "04a17a0b0f301e618e9fb41a5582f7202f64ff0a1f8c91cc9e2e1e8353501322",
  "b4599cce74b9acaf9b1980fee3cf26e77e5417ab6eecee867bdbc0a53cd245ef",
  "375b9b98d106acf2a80227b2e98acd1dde0bd902de6a3ad67f9683938ee38d44",
  "4d31e2f6dc8508a5dec86858f25538c2303ade8e5f2a5948105392a8fd2a1e34",
  "cfb9a8ff9a5228d9035e764c459f946e20c3b0b5dfff14cd20507cbffb14561d",
  "130722b353cc0d1ff723067350af593272cbc0f226c707899445889a3603cc6a",
  "1ddccf341edd64f58e669fc17568f07cec84b04a7f6be7d455b4ca206a99ec7f"
)


# Assume that if a tx has not been confirmed for 720 blocks (about 24 hours),
# then it is invalid
valid.txs.after.reorg <- lapply(
  (last.common.block_height + 1):(last.common.block_height + block.reorg.depth + 720),
  FUN = function(x) {
    txs <- get_block_txs(url.rpc, block_height = x, full.txs = FALSE, handle = handle)
    unique(txs$tx_hash)
  })

invalidated.tx_hashes <- setdiff(orphaned.blocks$tx_hash, unlist(valid.txs.after.reorg) )

stopifnot(setequal( invalidated.tx_hashes, invalidated.tx_hashes.hardcoded))
# Check that the programmatically-generated list of invalid txs
# matches the hardcoded list.


invalidated.txs <- get_transactions(url.rpc, invalidated.tx_hashes, handle = handle)


get_key_images <- function(x) {
  if (length(x$vin[[1]]$key$k_image) > 0) {
    key_images <- sapply(x$vin, FUN = function(x) {x$key$k_image })
  } else {
    key_images <- "MINER_TX"
  }
  data.frame(
    tx_hash = x$tx_hash,
    key_image = key_images,
    stringsAsFactors = FALSE)
}

invalidated.txs.key_images <- lapply(invalidated.txs, FUN = get_key_images)

invalidated.txs.key_images <- do.call(rbind, invalidated.txs.key_images)


current.height <- monerod.rpc.req(url.rpc, method = "get_last_block_header",
  params = list(height = block_height), handle = handle)$result$block_header$height


valid.txs.key_images <- lapply((last.common.block_height + 1):current.height, FUN = function(x) {
  txs <- get_block_txs(url.rpc, block_height = x, full.txs = TRUE, handle = handle)
  txs.key_images <- lapply(txs, FUN = get_key_images)
  txs.key_images <- do.call(rbind, txs.key_images)
  txs.key_images$block_height <- x
  txs.key_images
  
})


valid.txs.key_images <- do.call(rbind, valid.txs.key_images)

valid.txs.key_images[valid.txs.key_images$key_image %in% invalidated.txs.key_images$key_image, ]

invalidated.txs.key_images[invalidated.txs.key_images$key_image %in% valid.txs.key_images$key_image, ]$tx_hash


merged.key_images <- merge(invalidated.txs.key_images, valid.txs.key_images, by = "key_image",
  suffixes = c(".INVALIDATED",".VALID"))

colnames(merged.key_images)[colnames(merged.key_images) == "block_height"] <- "block_height.VALID"

merged.key_images <- merged.key_images[order(merged.key_images$tx_hash.INVALIDATED, merged.key_images$tx_hash.VALID), ]

write.csv(merged.key_images, file = "", row.names = FALSE)


block.timestamps <- lapply(unique(merged.key_images$block_height.VALID), FUN = function(x) {
  timestamp <- monerod.rpc.req(url.rpc, method = "get_block_header_by_height",
    params = list(height = x), handle = handle)$result$block_header$timestamp
  data.frame(block_height.VALID = x, timestamp = timestamp)
})

block.timestamps <- do.call(rbind, block.timestamps)

merged.key_images <- merge(merged.key_images, block.timestamps)

data.table::setDT(merged.key_images)

timing.spent.key_image <- merged.key_images[, .(earliest.time.spent.key_image = min(timestamp)), by = "tx_hash.INVALIDATED"]

data.table::setorder(timing.spent.key_image, earliest.time.spent.key_image)

timing.spent.key_image <- timing.spent.key_image[, 
  .(cumulative.share.spent = sum(.N)), by = "earliest.time.spent.key_image"]

timing.spent.key_image[, cumulative.share.spent := cumsum(cumulative.share.spent) / length(invalidated.tx_hashes)]

timing.spent.key_image[, earliest.time.spent.key_image := as.POSIXct(earliest.time.spent.key_image)]


reorg.timestamp <- monerod.rpc.req(url.rpc, method = "get_block_header_by_hash",
  params = list(hash = reorg.block_hashes[length(reorg.block_hashes)]), handle = handle)$result$block_header$timestamp

stepfun.timing.spent.key_image <- stepfun(timing.spent.key_image$earliest.time.spent.key_image,
  c(0, timing.spent.key_image$cumulative.share.spent))


png("sep-14-respends-UPDATE-sep-24.png", width = 600, height = 600)

xlim <- c(reorg.timestamp, max(timing.spent.key_image$earliest.time.spent.key_image))

par(mar = c(7, 4, 4, 2) + 0.1, cex = 1.4)
plot(stepfun.timing.spent.key_image,
  main = "Cumulative share of invalidated txs that have had at least\none key image confirmed in another tx after invalidation",
  # cex.main = 0.8,
  cex = 0.8,
  ylim = c(0, 1), yaxs = "i", xaxt = "n", xlab = "", ylab = "Cumulative share",
  xlim = xlim)

text(x = xlim[1], y = 0.9, pos = 4, label = "github.com/Rucknium")

xlim <- as.POSIXct(xlim)
xlim <- round(xlim, units = "hours")

axis(side = 4)
axis.Date(1, at = seq(xlim[1], xlim[2], by = 60^2 * 6))
axis.POSIXct(1, at = seq(xlim[1], xlim[2], by = 60^2 * 6), las = 3)

one.week.in.seconds <- 60^2 * 24 * 7

abline(v = reorg.timestamp, col = "red", lty = "dashed")
abline(v = reorg.timestamp + one.week.in.seconds, col = "blue", lty = "dashed")
reorg.timestamp

# Using https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r
labels <- "Reorg incident"

boxes <- sapply(nchar(labels), FUN = function(n) {
  paste(rep("\U2588", n), collapse = "")
})

text(x = reorg.timestamp, y = 0.5, labels = boxes, col = "white", srt = 90, family = "mono")
text(x = reorg.timestamp, y = 0.5, labels = labels, col = "red", srt = 90, family = "mono")

labels <- "txpool cleared"

boxes <- sapply(nchar(labels), FUN = function(n) {
  paste(rep("\U2588", n), collapse = "")
})

text(x = reorg.timestamp + one.week.in.seconds, y = 0.25, labels = boxes,
  col = "white", srt = 90, family = "mono")
text(x = reorg.timestamp + one.week.in.seconds, y = 0.25, labels = labels,
  col = "blue", srt = 90, family = "mono")

dev.off()



orphaned.blocks.extended <- orphaned.blocks

orphaned.blocks.extended$invalidated <- orphaned.blocks.extended$tx_hash %in% invalidated.tx_hashes

orphaned.blocks.extended <- unique(orphaned.blocks.extended[, c("block_height", "tx_hash", "invalidated")] )
orphaned.blocks.extended <- merge(orphaned.blocks.extended,
  data.frame(block_height = 1:block.reorg.depth + last.common.block_height), all = TRUE)
# Don't miss any blocks with zero txs
n.invalidated <- table(orphaned.blocks.extended$block_height, orphaned.blocks.extended$invalidated)

n.invalidated <- as.data.frame(unclass(n.invalidated))
n.invalidated$height <- as.integer(rownames(n.invalidated))

knitr::kable(n.invalidated[, c("height", "FALSE", "TRUE")],
  col.names = c("Orphan chain block height", "Valid", "Invalidated"),
  format = "pipe", row.names = FALSE, format.args = list(big.mark = ","))









