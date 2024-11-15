

library(data.table)

S_DCPC.2023 <- readRDS("raw-data/S-DCPC_2023/dcpc_2023_indlevel_public_rds.rds")
S_DCPC.2022 <- readRDS("raw-data/S-DCPC_2022/dcpc_2022_indlevel_public_rds.rds")
S_DCPC.2021 <- readRDS("raw-data/S-DCPC_2021/dcpc_2021_indlevel_public_rds.rds")
S_DCPC.2020 <- readRDS("raw-data/S-DCPC_2020/scpc_2020_public_rds.rds")
S_DCPC.2019 <- readRDS("raw-data/S-DCPC_2019/scpc_2019_public_rds.rds")
S_DCPC.2018 <- readRDS("raw-data/S-DCPC_2018/scpc_2018_rds.rds")
S_DCPC.2017 <- readRDS("raw-data/S-DCPC_2017/scpc_2017_rds.rds")
S_DCPC.2016 <- readRDS("raw-data/S-DCPC_2016/scpc_2016_rds.rds")
S_DCPC.2015 <- readRDS("raw-data/S-DCPC_2015/scpc_2015_rds.rds")


for (i in 2015:2023) {
  print(nrow(get(paste0("S_DCPC.", i))))
}



reason <- do.call(rbind, list(
  data.frame(year = 2023, reason = S_DCPC.2023$pa126_a, weight = S_DCPC.2023$ind_weight_all),
  data.frame(year = 2022, reason = S_DCPC.2022$pa126_a, weight = S_DCPC.2022$ind_weight_all),
  data.frame(year = 2021, reason = S_DCPC.2021$pa126_a, weight = S_DCPC.2021$ind_weight_all),
  data.frame(year = 2020, reason = S_DCPC.2020$pa126_a, weight = S_DCPC.2020$r_weight),
  data.frame(year = 2019, reason = S_DCPC.2019$pa126_a, weight = S_DCPC.2019$r_weight),
  data.frame(year = 2018, reason = S_DCPC.2018$pa126_a, weight = S_DCPC.2018$r_weight),
  data.frame(year = 2017, reason = S_DCPC.2017$pa126_a, weight = S_DCPC.2017$r_weight),
  data.frame(year = 2016, reason = S_DCPC.2016$pa126_a, weight = S_DCPC.2016$r_weight),
  data.frame(year = 2015, reason = S_DCPC.2015$pa126_a, weight = S_DCPC.2015$r_weight)
))

setDT(reason)

owns.crypto <- reason
owns.crypto[, owns.crypto := ! is.na(reason)]

reason <- reason[ ! is.na(reason), ]
reason[, reason := as.character(reason)]

reason[reason %in% c(1, 2, 4), reason := "Payments"]
reason[reason %in% c(3), reason := "Investment"]
reason[reason %in% c(5, 9), reason := "Other"]
reason[reason %in% c(6), reason := "Interest in new tech"]
reason[reason %in% c(7, 8), reason := "Don't trust banks/government/USD"]

reason[, reason := factor(reason, levels =
    c("Investment", "Payments", "Interest in new tech", "Don't trust banks/government/USD", "Other"))]

reason <- aggregate(weight ~ reason + year, data = reason, FUN = sum, drop = FALSE)

setDT(reason)

reason[is.na(weight), weight := 0]

reason <- reason[, .(reason = reason, share = 100 * weight / sum(weight)), by = "year"]


owns.crypto <- aggregate(weight ~ owns.crypto + year, data = owns.crypto, FUN = sum, drop = FALSE)
setDT(owns.crypto)
owns.crypto[is.na(weight), weight := 0]
owns.crypto <- owns.crypto[, .(owns.crypto = owns.crypto, share = 100 * weight / sum(weight)), by = "year"]

owns.crypto <- owns.crypto[ (owns.crypto), ]
setnames(owns.crypto, "owns.crypto", "reason")
owns.crypto$reason <- NULL
owns.crypto$reason <- "Owns crypto"

owns.crypto[, panel := "Owns crypto"]

reason[, panel := "Primary reason"]

reason.owns.crypto <- rbind(reason, owns.crypto)

reason.owns.crypto[, panel := factor(panel, levels = c("Owns crypto", "Primary reason"))]


reason.owns.crypto[, year := factor(year)]


saveRDS(reason.owns.crypto, file = "processed-data/reason-owns-crypto.rds")



