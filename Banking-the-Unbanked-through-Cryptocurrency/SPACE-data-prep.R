

library(data.table)
library(survey)
library(stargazer)
library(huxtable)
library(rvest)


SPACE <- read.csv("raw-data/SPACE_2022/SPACE_wide_2022.csv", stringsAsFactors = FALSE)

country.names <- matrix(c(
  "CY", "Cyprus",
  "MT", "Malta",
  "BE", "Belgium",
  "IT", "Italy",
  "ES", "Spain",
  "GR", "Greece",
  "AT", "Austria",
  "EE", "Estonia",
  "FI", "Finland",
  "IE", "Ireland",
  "LT", "Lithuania",
  "LU", "Luxembourg",
  "LV", "Latvia",
  "PT", "Portugal",
  "SI", "Slovenia",
  "SK", "Slovakia",
  "FR", "France"
), ncol = 2, byrow = TRUE)

colnames(country.names) <- c("COUNTRY", "COUNTRY.name")
country.names <- as.data.frame(country.names)

SPACE <- merge(SPACE, country.names)

continuous.age <- matrix(c(
  1,	median(c(18, 24)),
  2,	median(c(25, 29)),
  3,	median(c(30, 34)),
  4,	median(c(35, 39)),
  5,	median(c(40, 44)),
  6,	median(c(45, 49)),
  7,	median(c(50, 54)),
  8,	median(c(55, 59)),
  9,	median(c(60, 64)),
  10,	median(c(64, 69)),
  11,	median(c(70, 74)),
  12,	85),
  ncol = 2, byrow = TRUE)

colnames(continuous.age) <- c("AGE", "Age")
continuous.age <- as.data.frame(continuous.age)

SPACE <- merge(SPACE, continuous.age)

colnames(continuous.age) <- c("AGE", "Age")
continuous.age <- as.data.frame(continuous.age)

SPACE <- merge(SPACE, continuous.age)

SPACE$Gender <- factor(SPACE$D1, level = 1:3, labels = c("Male", "Female", "Other, non-binary"))
SPACE$Gender <- relevel(SPACE$Gender, "Female")


cash.reserves.amount <- c(
  "Less than €100",
  "€100 - €250",
  "€250 - €500",
  "€500 - €1,000",
  "€1,000 - €5,000",
  "€5,000 - €10,000",
  "More than €10,000"
)

SPACE$cash.reserves.amount <- factor(SPACE$QQ10, level = 1:7, labels = cash.reserves.amount)

SPACE$cash.reserves.any <- factor(SPACE$QQ9, level = 0:1, labels = c("No", "Yes"))

SPACE$cash.reserves <- ifelse(SPACE$cash.reserves.any == "Yes",
  as.character(SPACE$cash.reserves.amount), "€0")

SPACE$cash.reserves <- as.factor(SPACE$cash.reserves)

setDT(SPACE)

SPACE[, has.crypto := factor(QQ1A_3, levels = 0:1, labels = c("No", "Yes"))]

SPACE[, crypto.use := factor(QQ1B, levels = 0:3, labels = c("None", "Investment", "Payment", "Both"))]
SPACE[is.na(crypto.use), crypto.use := "None"]

table(SPACE$crypto.use)
table(SPACE$has.crypto)


SPACE[, has.payment.account := factor(QQ1A_1, labels = c("No", "Yes"))]
SPACE[, has.card := factor(QQ1A_2, labels = c("No", "Yes"))]

SPACE[, has.financial.investments := factor(D10_6, labels = c("No", "Yes"))]

SPACE[, has.savings.account := factor(D10_3, labels = c("No", "Yes"))]

SPACE[, PaymentPreference := factor(QQ3, levels = c(3, 1:2), labels = c("No preference", "Cash", "Cashless"))]
# Put no preference first so it is the contrast level

SPACE[, "cash.wide.acceptance" := factor(QQ13A_1, labels = c("No", "Yes"))]
SPACE[, "cash.faster" := factor(QQ13A_2, labels = c("No", "Yes"))]
SPACE[, "cash.privacy" := factor(QQ13A_3, labels = c("No", "Yes"))]
SPACE[, "cash.easier" := factor(QQ13A_4, labels = c("No", "Yes"))]
SPACE[, "cash.safer" := factor(QQ13A_5, labels = c("No", "Yes"))]
SPACE[, "cash.immediately.settled" := factor(QQ13A_6, labels = c("No", "Yes"))]
SPACE[, "cash.spending.awareness" := factor(QQ13A_7, labels = c("No", "Yes"))]
SPACE[, "cash.other" := factor(QQ13A_8, labels = c("No", "Yes"))]
SPACE[, "cash.no.use" := factor(QQ13A_9, labels = c("No", "Yes"))]
SPACE[, "cash.none" := factor(QQ13A_10, labels = c("No", "Yes"))]



SPACE.survey <- svydesign(ids = ~ID, data = SPACE, weights = SPACE$Gross_weight)

saveRDS(SPACE.survey, file = "processed-data/SPACE.rds")


# Load the data again to get the diary data

space <- read.csv("raw-data/SPACE_2022/SPACE_wide_2022.csv", stringsAsFactors = FALSE)


setDT(space)

space.diary <- melt(space[, c("ID", "D1", "AGE", "WEEKDAY", "COUNTRY", "Gross_weight", colnames(space)[grepl("^QB", colnames(space))]), with = FALSE],
  measure.vars = patterns(purpose = "^QB1", value = "^QB3", instrument = "^QB4"))



SPACE.diary <- melt(SPACE[, c("ID", "D1", "AGE", "WEEKDAY", "COUNTRY", "Gross_weight", colnames(SPACE)[grepl("^QB", colnames(SPACE))]), with = FALSE],
  measure.vars = patterns(purpose = "^QB1", value = "^QB3", instrument = "^QB4"))


purpose.label <- c("Clothes/sportswear",
  "Electronic goods/household appliances",
  "Food/daily supplies",
  "Medicine/cosmetics/drugstore products",
  "Media/games/entertainment",
  "Charitable donations",
  "Travel/accommodation",
  "Furniture/other household items",
  "Tickets for events/attractions",
  "Luxury goods",
  "Financial: insurance, investment, crypto-assets",
  "Household related services",
  "Other")



SPACE.diary <- SPACE.diary[complete.cases(SPACE.diary), ]
SPACE.diary[purpose == 999999, purpose := NA]
SPACE.diary[value == 999999, value := NA]
SPACE.diary[instrument == 999999, instrument := NA]

SPACE.diary[, D1 := factor(D1)]
SPACE.diary[, AGE := factor(AGE)]
SPACE.diary[, WEEKDAY := factor(WEEKDAY)]
SPACE.diary[, COUNTRY := factor(COUNTRY)]
SPACE.diary[, purpose := factor(purpose, levels = seq_along(purpose.label), labels = purpose.label)]
SPACE.diary[, instrument := factor(instrument)]

SPACE.diary[, `Paid with crypto` := instrument == 10]


SPACE.diary.survey <- svydesign(ids = ~ID, data = SPACE.diary, weights = SPACE.diary$Gross_weight)


saveRDS(SPACE.diary.survey, file = "processed-data/SPACE_diary.rds")


# This computation takes a while

aggregate.crypto.spending.clustered <- as.data.frame(svyby(~I(365*value/1e+09), ~`Paid with crypto`,
  design = as.svrepdesign(subset(SPACE.diary.survey,
    purpose != "Financial: insurance, investment, crypto-assets")), svytotal))

aggregate.crypto.spending.clustered.CI <- aggregate.crypto.spending.clustered[2, 3] * 1.645
aggregate.crypto.spending.clustered[2, 2]
aggregate.crypto.spending.clustered[2, 2] + c(-1 * aggregate.crypto.spending.clustered.CI, aggregate.crypto.spending.clustered.CI)

aggregate.crypto.spending.clustered <- list(
  point.estimate = aggregate.crypto.spending.clustered[2, 2],
  CI.90.percent = aggregate.crypto.spending.clustered[2, 2] +
    c(-1 * aggregate.crypto.spending.clustered.CI, aggregate.crypto.spending.clustered.CI)
)

saveRDS(aggregate.crypto.spending.clustered, file = "processed-data/EU-aggregate-crypto-spending.rds")




