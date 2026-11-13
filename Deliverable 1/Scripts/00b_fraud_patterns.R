# =============================================================================
# 00b_fraud_patterns.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   The first pass at which variables relate to fraud, run on the raw data
#   before cleaning. For every predictor it shows how the fraud rate changes
#   across its values. This pass picked the variables the report focuses on.
#
#   02_eda_figures.R repeats some of these tables on the cleaned data, for
#   the figures and the numbers quoted in the report. This script is broader:
#   it covers every variable, including the ones that turned out to show
#   little signal.
#
# How to run it
#   Same as 00a_first_look.R: set the working directory to the folder with
#   train.csv, then run line by line (Ctrl+Enter) or with Source with Echo
#   (Ctrl+Shift+Enter). It only shows results.
#
# Packages used
#   None beyond base R. The main tool is tapply().
# =============================================================================

raw_train <- read.csv("train.csv")

overall_rate <- mean(raw_train$is_fraud)
overall_rate   # 0.0167

# rate_by() makes a small table for one column: how many rows have each
# value (n), and what share of them are fraud (fraud_rate).
#   tapply(x, groups, f) splits x into groups and applies f to each group.
#   length() counts the rows; mean() of the 0/1 target gives
#   the fraud rate.
rate_by <- function(column) {
  data.frame(
    n          = tapply(raw_train$is_fraud, column, length),
    fraud_rate = round(tapply(raw_train$is_fraud, column, mean), 4)
  )
}

# ---- Categorical variables --------------------------------------------------

# Result: auth_method and merchant_category vary a lot. card_type, channel,
# device_type and day_of_week all stay close to the overall rate.
rate_by(raw_train$merchant_category)
rate_by(raw_train$card_type)
rate_by(raw_train$auth_method)
rate_by(raw_train$channel)
rate_by(raw_train$device_type)
rate_by(raw_train$day_of_week)

# ---- True/false risk flags, and small counts --------------------------------

# Result: every flag raises the fraud rate when it is True. cvv_retry_count
# has the strongest effect: 2 failed attempts give a fraud rate of about 42%.
rate_by(raw_train$is_foreign_transaction)
rate_by(raw_train$is_new_merchant)
rate_by(raw_train$used_vpn)
rate_by(raw_train$ip_country_mismatch)
rate_by(raw_train$billing_shipping_mismatch)
rate_by(raw_train$is_ai_generated_scam_attempt)
rate_by(raw_train$cvv_retry_count)
rate_by(raw_train$prior_disputes)

# ---- Hour of day ------------------------------------------------------------

# Result: every hour stays between about 1.4% and 1.8%, so time of day shows
# little signal on its own.
rate_by(raw_train$time_of_day_hour)

# ---- Transaction amount, in ranges ------------------------------------------

# Nested ifelse() puts each amount into a range. Each test only
# runs if the ones before it were FALSE, so "x <= 25" here means
# "above $10 and up to $25".
x <- raw_train$amount_usd
amount_range <- ifelse(x <= 10,   "$0 to $10",
                ifelse(x <= 25,   "$10 to $25",
                ifelse(x <= 50,   "$25 to $50",
                ifelse(x <= 100,  "$50 to $100",
                ifelse(x <= 250,  "$100 to $250",
                ifelse(x <= 500,  "$250 to $500",
                ifelse(x <= 1000, "$500 to $1,000", "Over $1,000")))))))

# factor() with levels sets the order the ranges are listed in. Without it,
# they would be sorted alphabetically ("$10 ..." before "$25 ...").
amount_range <- factor(amount_range,
                       levels = c("$0 to $10", "$10 to $25", "$25 to $50",
                                  "$50 to $100", "$100 to $250", "$250 to $500",
                                  "$500 to $1,000", "Over $1,000"))
rate_by(amount_range)
# Result: fraud is more common above $100, highest in the $100 to $250 range.

# ---- Continuous variables: medians by class and correlations ----------------

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score")

# Median of each variable for legitimate (row 0) and fraud (row 1)
# transactions. tapply() gives the two medians for one variable, and
# sapply() repeats that for each variable. A median is used rather than a
# mean because several columns are skewed.
sapply(num_cols, function(v) tapply(raw_train[[v]], raw_train$is_fraud, median))

# cor() between each variable and the 0/1 target. Values near 0
# mean the variable on its own says little about fraud. Correlation only
# measures a straight-line relationship, so a variable can still matter in
# other ways.
cor_cols <- c(num_cols, "cvv_retry_count", "prior_disputes")
correlations <- sapply(cor_cols, function(v) cor(raw_train[[v]], raw_train$is_fraud))

# order(-correlations) lists the largest first.
round(correlations[order(-correlations)], 3)
# Result: cvv_retry_count (0.21), merchant_risk_score (0.13) and
# velocity_score (0.10) are highest. Distance, card age and balance are all
# below 0.01.
