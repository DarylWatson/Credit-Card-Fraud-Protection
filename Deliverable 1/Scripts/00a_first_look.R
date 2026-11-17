# 00a_first_look.R
# First look at the raw data: size, column types, missing values, duplicates,
# class balance, skew, and whether train and test look alike.
# Run from the folder that holds train.csv and test.csv. Shows results only.

library(tidyverse)
library(moments)   # skewness()

raw_train <- read.csv("train.csv")
raw_test  <- read.csv("test.csv")

# ---- Size and column types ----

dim(raw_train)   # 699,960 x 26
dim(raw_test)    # 299,984 x 25 (no is_fraud)

# is_fraud is numeric, the six flags are "True"/"False" text, and five
# categorical columns are text. 01_load_clean.R fixes all three.
glimpse(raw_train)

# ---- Missing values, blanks and duplicates ----

sapply(raw_train, function(column) sum(is.na(column)))
sapply(raw_test,  function(column) sum(is.na(column)))

# is.na() misses empty strings, so check the text columns separately.
text_cols <- c("merchant_category", "card_type", "auth_method",
               "channel", "device_type")
sapply(text_cols, function(v) sum(raw_train[[v]] == ""))

# Drop id first, since every id is unique.
nrow(raw_train) - nrow(distinct(select(raw_train, -id)))

# Result: no missing values, blanks or duplicates in either file.

# ---- The target ----

table(raw_train$is_fraud)   # 688,271 legitimate, 11,689 fraud
mean(raw_train$is_fraud)    # 0.0167

# ---- Numeric columns: ranges, skew and extreme values ----

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score",
              "cvv_retry_count", "prior_disputes")

summary(raw_train[, num_cols])

data.frame(
  skewness = sapply(num_cols, function(v) round(skewness(raw_train[[v]]), 2)),
  p99.9    = sapply(num_cols, function(v) quantile(raw_train[[v]], 0.999)),
  max      = sapply(num_cols, function(v) max(raw_train[[v]]))
)
# Result: account_balance_usd (skew ~14) and amount_usd (~7) have long right
# tails, so 01_load_clean.R adds log versions. cvv_retry_count and
# prior_disputes are skewed too, but they are small counts, so left as is.

# ---- Do train and test look alike? ----

data.frame(
  train = sapply(num_cols, function(v) round(mean(raw_train[[v]]), 3)),
  test  = sapply(num_cols, function(v) round(mean(raw_test[[v]]), 3))
)
# Result: nearly identical means. The test file is not used for modeling
# because it has no labels.