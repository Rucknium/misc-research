

library(data.table)
library(readstata13)
library(survey)


SHED <- list()

SHED[["2021"]] <- readstata13::read.dta13("raw-data/SHED_2021/public2021.dta", generate.factors = TRUE, nonint.factors = TRUE)
SHED[["2022"]] <- readstata13::read.dta13("raw-data/SHED_2022/public2022.dta", generate.factors = TRUE, nonint.factors = TRUE)
SHED[["2023"]] <- readstata13::read.dta13("raw-data/SHED_2023/public2023.dta", generate.factors = TRUE, nonint.factors = TRUE)

crypto.used.as.payment_annual.trend <- c()

for (i in names(SHED)) {
  SHED[[i]]$crypto.used.as.payment <- SHED[[i]]$S16_b  == "Yes"
  SHED[[i]] <- svydesign(ids = ~0, data = SHED[[i]], weights = SHED[[i]]$weight)
  crypto.used.as.payment_annual.trend[[i]] <-
    round(100 * as.data.frame(svymean(~ crypto.used.as.payment, design = SHED[[i]], na.rm = TRUE))[2, 1], 1)
}

saveRDS(crypto.used.as.payment_annual.trend, file = "processed-data/SHED-annual-trend.rds")


# Then re-do it for the pooled dataset


SHED <- list()

SHED[["2021"]] <- readstata13::read.dta13("raw-data/SHED_2021/public2021.dta", generate.factors = TRUE, nonint.factors = TRUE)
SHED[["2022"]] <- readstata13::read.dta13("raw-data/SHED_2022/public2022.dta", generate.factors = TRUE, nonint.factors = TRUE)
SHED[["2023"]] <- readstata13::read.dta13("raw-data/SHED_2023/public2023.dta", generate.factors = TRUE, nonint.factors = TRUE)


colnames(SHED[["2021"]]) <- paste0(colnames(SHED[["2021"]]), ".2021")
colnames(SHED[["2022"]]) <- paste0(colnames(SHED[["2022"]]), ".2022")
colnames(SHED[["2023"]]) <- paste0(colnames(SHED[["2023"]]), ".2023")

SHED.panel <- merge(SHED[["2021"]], SHED[["2022"]], by.x = "CaseID.2021", by.y = "caseid2021.2022",
  all = TRUE)

SHED.panel <- merge(SHED.panel, SHED[["2023"]], by.x = "CaseID.2022", by.y = "caseid2022.2023",
  all = TRUE, incomparables = NA)
# NOTE: merge.data.frame() and merge.data.table() have different behaviors with incomparables = NA




pooled.vars <- c(
  "S16_a",
  "S16_b",
  "S16_c",
  "S18", "S19_a", "S19_b", "S19_c", "S20_a", "S20_b", "S20_c", "S20_d",
  "BK1",
  "C2A",
  "ppgender",
  "race_5cat",
  "ED0",
  "ED1",
  "pppa_lgb",
  "ppp20197",
  "I40",
  "B2",
  "I41_b",
  "FL0",
  "B10",
  "ppage",
  "ppcmdate",
  "ind1",
  "ppcm0160",
  "ppcm1301",
  "ppmsacat",
  "ppfs0596",
  "A1_a",
  "BK2_a",
  "BK2_b",
  "BK2_c",
  "BK2_d",
  "BK2_e",
  "BNPL1",
  "ppfs1482",
  "GE2A",
  "GE1A",
  "E7",
  "E8_b",
  "xlaptop",
  "devicetype2",
  "S21"
)

for (pooled.var in pooled.vars) {

  # if (pooled.var == "S19_b") {stop()}

  SHED.panel[, paste0(pooled.var, ".pooled")] <- NA

  valid.var.col.name <- intersect(colnames(SHED.panel), paste0(pooled.var, ".", 2021:2023))[1]

  var.transform <- ifelse(is.factor(SHED.panel[, valid.var.col.name]), as.character, identity)

  for (i in as.character(2021:2023)) {

    if ( ! (paste0(pooled.var, ".", i) %in% colnames(SHED.panel) ) ) {
      next
    }

    SHED.panel[is.na(SHED.panel[, paste0(pooled.var, ".pooled")]), paste0(pooled.var, ".pooled")] <-
      var.transform(SHED.panel[is.na(SHED.panel[, paste0(pooled.var, ".pooled")]), paste0(pooled.var, ".", i)])
  }

  if (is.factor(SHED.panel[, valid.var.col.name])) {
    SHED.panel[, paste0(pooled.var, ".pooled")] <- factor(SHED.panel[, paste0(pooled.var, ".pooled")])
  }

}





SHED.panel$panel_weight.common <- apply(
  SHED.panel[, c("panel_weight.2021", "panel_weight.2022", "panel_weight.2023")], MARGIN = 1,
  FUN = function(x) {
    y <- unique(na.omit(x))
    if (length(y) == 0) { return(0) }
    return(mean(y))
  })

SHED.panel$panel_weight.common[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2021)] <-
  SHED.panel$weight.2021[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2021)]

SHED.panel$panel_weight.common[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2022)] <-
  SHED.panel$weight.2022[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2022)]

SHED.panel$panel_weight.common[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2023)] <-
  SHED.panel$weight.2023[SHED.panel$panel_weight.common == 0 & !is.na(SHED.panel$weight.2023)]
# Weighting is a little improvised





# S16_a
# Bought cryptocurrency or held as an
# investment - In the past year, have you done
# the following with cryptocurrency, such as
# Bitcoin or Ethereum?
SHED.panel$crypto.used.as.investment <- SHED.panel$S16_a.pooled == "Yes"


# S16_b
# In the past year, have you done the following
# with cryptocurrency, such as Bitcoin or Ethereum?
# - Used to buy something or make a payment
SHED.panel$crypto.used.as.payment <- SHED.panel$S16_b.pooled == "Yes"

# S16_c
# Used cryptocurrency to send money to friends
# or family - In the past year, have you done
# the following with cryptocurrency, such as
# Bitcoin or Ethereum?
SHED.panel$crypto.used.to.send.money <- SHED.panel$S16_c.pooled == "Yes"

# S18
# Were any of the family or friends you sent
# cryptocurrency to living outside of the
# United States?
SHED.panel$crypto.used.to.send.money.outside.us <- SHED.panel$S18.pooled == "Yes"

# S19_a
# Buy something in-person - Have you used
# cryptocurrency to do each of the following?
SHED.panel$crypto.used.as.payment.in.person <- SHED.panel$S19_a.pooled == "Yes"


# S19_b
# Buy something online - Have you used
# cryptocurrency to do each of the following?
SHED.panel$crypto.used.as.payment.online <- SHED.panel$S19_b.pooled == "Yes"

# S19_c
# Pay a bill online - Have you used
# cryptocurrency to do each of the following?
SHED.panel$crypto.used.as.payment.online.bill.pay <- SHED.panel$S19_c.pooled == "Yes"


# S20_a
# Convert cash into cryptocurrency - Have you
# used a cryptocurrency ATM/kiosk to do each of
# the following?
SHED.panel$crypto.atm.convert.cash.to.crypto <- SHED.panel$S20_a.pooled == "Yes"

# S20_b
# Withdraw cash - Have you used a cryptocurrency
# ATM/kiosk to do each of the following?
SHED.panel$crypto.atm.withdraw.cash <- SHED.panel$S20_b.pooled == "Yes"

# S20_c
# Make a payment - Have you used a
# cryptocurrency ATM/kiosk to do each of the
# following?
SHED.panel$crypto.atm.make.payment <- SHED.panel$S20_c.pooled == "Yes"

# S20_d
# Send money to family or friends - Have you
# used a cryptocurrency ATM/kiosk to do each of
# the following?
SHED.panel$crypto.atm.send.money <- SHED.panel$S20_d.pooled == "Yes"





# BK1
# Do you and/or your spouse or partner
# currently have a checking, savings or money
# market account?
SHED.panel$lacks.bank.account <- SHED.panel$BK1.pooled == "No"

# C2A
# Do you currently have at least one credit card?
SHED.panel$lacks.credit.card <- SHED.panel$C2A.pooled == "No"

# ppgender
# Gender [Ipsos source]
SHED.panel$is.male <- SHED.panel$ppgender.pooled == "Male"

# race_5cat Race 5 categories
SHED.panel$race <- SHED.panel$race_5cat.pooled

# ED0
# What is the highest level of school you have
# completed or the highest degree you have
# received?
SHED.panel$education.level <- relevel(SHED.panel$ED0.pooled, "High school degree or GED")

# ED1
# Which one of the following broad categories
# best describes your (current/most recent)
# educational program?
SHED.panel$education.subject <- relevel(SHED.panel$ED1.pooled, "Business/management")

# pppa_lgb [Ipsos source]
# Q230: Which of the following best describes how you think of yourself?
SHED.panel$lgbtq <- relevel(SHED.panel$pppa_lgb.pooled, "Straight, that is, not gay")

# ppp20197 [Ipsos source]
# QEG22: Are you a citizen of the United States?
SHED.panel$is.noncitizen <- SHED.panel$ppp20197.pooled == "No"

# I40
# Which of the following categories best
# describes the total income that you and/or
# your spouse or partner received from all
# sources, before taxes and deductions, in the
# past 12 months?
SHED.panel$income.category <- SHED.panel$I40.pooled

# B2
# Overall, which one of the following best
# describes how well you are managing
# financially these days?
SHED.panel$overall.financial.wellbeing <- SHED.panel$B2.pooled

# I41_b
# Supplemental Nutrition Assistance Program
# (SNAP or food stamps) - In the past 12
# months, have you received any of the
# following?
SHED.panel$received.food.stamps <- SHED.panel$I41_b.pooled

# FL0
# On a scale from zero to ten, where zero is
# not at all willing to take risks and ten is
# very willing to take risks, what number would
# you be on the scale?

SHED.panel$risk.tolerance <- as.numeric(SHED.panel$FL0.pooled) - 1

# B10
# Overall, on a scale from zero to ten, where
# zero is not at all satisfied and ten is
# completely satisfied, how satisfied are you
# with life as a whole these days?
SHED.panel$life.satisfaction <- as.numeric(SHED.panel$B10.pooled) - 1

# ppage
# Age [Ipsos source]
# ppcmdate
# Date member completed Core survey
# Must correct age variable for time of initial Ipsos survey
SHED.panel$age <- SHED.panel$ppage.pooled + (2021 - as.numeric(substr(SHED.panel$ppcmdate.pooled, 1, 4)))

# ind1
# IND1: Industry (tight scale) in current or main job
SHED.panel$job.industry <- relevel(SHED.panel$ind1.pooled, "Retail/Stores/Shopping (including Online Retail)")

# ppcm0160 [Ipsos source]
# Q26: Occupation (detailed) in current or main job
SHED.panel$job.occupation <- relevel(SHED.panel$ppcm0160.pooled, "Food Preparation and Serving")

# ppcm1301 [Ipsos source]
# GOVEMP1: Employer type
SHED.panel$employer.type <- relevel(SHED.panel$ppcm1301.pooled, "Private-for-profit company")

# ppmsacat
# MSA Status [Ipsos source]
SHED.panel$resides.in.metro.statistical.area <- SHED.panel$ppmsacat.pooled == "Metro"

# ppfs0596 [Ipsos source]
# Q22: What is the approximate total amount of
# your household's savings and investments?
SHED.panel$total.household.savings <- relevel(SHED.panel$ppfs0596.pooled, "$100,000 - $249,999")

# A1_a
# In the past 12 months, has each of the following happened to you:
# - Turned down for credit
SHED.panel$rejected.for.credit <- SHED.panel$A1_a.pooled == "Yes"

# BK2_a
# In the past 12 months, did you and/or spouse or partner:
# - Purchase a money order from a place other than a bank
SHED.panel$purchase.non.bank.money.order <- SHED.panel$BK2_a.pooled == "Yes"

# BK2_b
# In the past 12 months, did you and/or spouse or partner:
# - Cash a check at a place other than a bank
SHED.panel$cash.check.non.bank <- SHED.panel$BK2_b.pooled == "Yes"

# BK2_c
# In the past 12 months, did you and/or spouse or partner:
# - Take out a payday loan or payday advance
SHED.panel$take.payday.loan <- SHED.panel$BK2_c.pooled == "Yes"

# BK2_d
# In the past 12 months, did you and/or spouse or partner:
# - Take out a pawn shop loan or an auto title loan
SHED.panel$take.auto.or.pawn.shop.loan <- SHED.panel$BK2_d.pooled == "Yes"

# BK2_e
# In the past 12 months, did you and/or spouse or partner:
# - Obtain a tax refund advance to receive your refund faster
SHED.panel$take.tax.refund.advance <- SHED.panel$BK2_e.pooled == "Yes"

# BNPL1
# In the past year, have you used a “Buy Now
# Pay Later” service to buy something?
SHED.panel$used.buy.now.pay.later <- SHED.panel$BNPL1.pooled == "Yes"

# ppfs1482 [Ipsos source]
# Q108: Where do you think your credit score falls
SHED.panel$perceived.credit.score <- factor(SHED.panel$ppfs1482.pooled, levels = c("Fair", "Excellent", "Good", "Poor", "Very poor", "Don’t know"))
# Note "Don’t know" has a curved apostrophe ’

# GE2A
# Some people earn money by selling items at
# places like flea markets and garage sales or
# through online marketplaces like eBay or
# Etsy. In the past month, have you made money
# by selling items in any of these ways?

SHED.panel$informal.selling.of.goods <- SHED.panel$GE2A.pooled == "Yes"

# GE1A
# In the past month, have you done any
# freelance or gig-work, either to supplement
# your income or as your main job?

SHED.panel$freelance.or.gig.work <- SHED.panel$GE1A.pooled == "Yes"

# E7
# During the past 12 months, have you
# personally experienced discrimination or
# unfair treatment because of your race,
# ethnicity, age, religion, disability status,
# sexual orientation, gender, or gender
# identity?
SHED.panel$experienced.discrimination <- SHED.panel$E7.pooled == "Yes"

# E8_b
# In the past 12 months, did you personally experience
# discrimination or unfair treatment while
# doing any of the following?
# - Banking or applying for a loan
SHED.panel$experienced.discrimination.in.banking <- SHED.panel$E8_b.pooled == "Yes"

# xlaptop
# Is R a KP laptop user?
SHED.panel$is.kp.laptop.user <- SHED.panel$xlaptop.pooled == "Yes"

# devicetype2
# DOV: Device Type - at the end of survey
SHED.panel$respondent.device.type <- relevel(SHED.panel$devicetype2.pooled, "WinPC")

# S21
# What was the main reason you used
# cryptocurrency to buy something, make a
# payment, or send money?
SHED.panel$why.used.crypto.as.payment <- SHED.panel$S21.pooled



SHED.panel.survey <- svydesign(ids = ~0, data = SHED.panel, weights = SHED.panel$panel_weight.common)
SHED.survey <- SHED.panel.survey


saveRDS(SHED.survey, file = "processed-data/SHED.rds")





