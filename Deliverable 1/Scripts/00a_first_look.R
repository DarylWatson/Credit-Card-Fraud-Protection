# =============================================================================
# 00a_first_look.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   The first look at the raw data, done before any cleaning. It answers:
#     - How big are the files, and what type is each column?
#     - Is anything missing or duplicated?
#     - How imbalanced is the target?
#     - What do the numeric columns look like (ranges, skew, extreme values)?
#     - Do the training and test files look alike?
#   What it found decided the cleaning steps in 01_load_clean.R.
#
# How to run it
#   1. Put train.csv and test.csv in the same folder as this script.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Run it line by line with Ctrl+Enter, or all at once with
#      Ctrl+Shift+Enter (Source with Echo). Either way, each result appears
#      in the Console under the line that made it.
#   It only shows results. It does not change or save anything.
#
# Packages used (install once with install.packages(c("tidyverse", "moments")))
#   tidyverse  For dplyr's glimpse(), select() and distinct().
#   moments    For skewness().
# =============================================================================

library(tidyverse)
library(moments)

# ---- Load the raw files -----------------------------------------------------

# read.csv() reads a CSV file into a dataframe. The training
# file is about 98 MB, so this takes several seconds.
raw_train <- read.csv("train.csv")
raw_test  <- read.csv("test.csv")

# ---- Size and column types --------------------------------------------------

# dim() gives rows and columns.
# Train: 699,960 rows x 26 columns. Test: 299,984 rows x 25 columns,
# because the test file has no is_fraud column.
dim(raw_train)
dim(raw_test)

# glimpse() lists every column with its type and first values.
# It shows three things 01_load_clean.R has to fix:
#   - is_fraud is a decimal number (dbl), not a category
#   - the six risk flags are text ("True"/"False"), not numbers
#   - five more columns are text (chr) that should be categories
glimpse(raw_train)

# ---- Missing values, blanks and duplicates ----------------------------------

# is.na() marks each missing value, and sum() counts them.
# sapply() repeats that for every column.
sapply(raw_train, function(column) sum(is.na(column)))
sapply(raw_test,  function(column) sum(is.na(column)))

# A text column can also be "missing" as an empty string "", which is.na()
# does not catch, so count those in the five text columns.
text_cols <- c("merchant_category", "card_type", "auth_method",
               "channel", "device_type")
sapply(text_cols, function(v) sum(raw_train[[v]] == ""))

# Duplicates: distinct() keeps one copy of each different row.
# We drop the id column first with select(-id), because every id is unique.
# If no rows were duplicates, distinct() keeps them all and this is 0.
nrow(raw_train) - nrow(distinct(select(raw_train, -id)))

# Result: no missing values, no blanks and no duplicates in either file.

# ---- The target -------------------------------------------------------------

# table() counts each value. mean() of a 0/1 column is the
# share of 1s, which here is the fraud rate.
table(raw_train$is_fraud)   # 688,271 legitimate, 11,689 fraud
mean(raw_train$is_fraud)    # 0.0167, about 1 in 60

# ---- Numeric columns: ranges, skew and extreme values -----------------------

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score",
              "cvv_retry_count", "prior_disputes")

# summary() gives the minimum, quartiles, median, mean and maximum.
# A mean far above the median is the first sign of a long
# right tail: amount_usd has a median of $27 but a mean of $80.
summary(raw_train[, num_cols])

# Skewness measures how lopsided a distribution is: 0 means
# symmetric, and a large positive value means a long right tail.
# quantile(x, 0.999) is the value that only 1 row in 1,000 is above.
# Comparing it with max() shows how far the most extreme values reach.
data.frame(
  skewness = sapply(num_cols, function(v) round(skewness(raw_train[[v]]), 2)),
  p99.9    = sapply(num_cols, function(v) quantile(raw_train[[v]], 0.999)),
  max      = sapply(num_cols, function(v) max(raw_train[[v]]))
)
# Result: account_balance_usd (skewness about 14) and amount_usd (about 7)
# are by far the most skewed, with maximums far beyond their 99.9th
# percentiles. That is why 01_load_clean.R adds log versions of those two.
# cvv_retry_count and prior_disputes are skewed too (about 3 and 2), but
# they are small counts that are mostly 0, not long tails, so they were
# left as they are. The 1.5 x IQR outlier counts are in 01_load_clean.R.

# ---- Do the training and test files look alike? -----------------------------

# If the test file came from a different distribution, patterns learned on
# the training file might not carry over. Comparing column means is a quick
# first check.
data.frame(
  train = sapply(num_cols, function(v) round(mean(raw_train[[v]]), 3)),
  test  = sapply(num_cols, function(v) round(mean(raw_test[[v]]), 3))
)
# Result: the means are nearly identical. We later decided not to use the
# test file at all, because it has no labels, but this check still shows
# the two files were generated the same way.
