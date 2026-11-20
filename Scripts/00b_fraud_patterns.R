# 00b_fraud_patterns.R
# How the fraud rate changes across every predictor, on the raw data.
# Broader than 02_eda_figures.R: it includes variables with little signal.
# Run from the folder that holds train.csv. Shows results only.

raw_train <- read.csv("train.csv")

overall_rate <- mean(raw_train$is_fraud)
overall_rate   # 0.0167

# Row count and fraud rate for each value of a column.
rate_by <- function(column) {
  data.frame(
    n          = tapply(raw_train$is_fraud, column, length),
    fraud_rate = round(tapply(raw_train$is_fraud, column, mean), 4)
  )
}

# ---- Categorical variables ----

# Result: auth_method and merchant_category vary a lot; the rest stay near
# the overall rate.
rate_by(raw_train$merchant_category)
rate_by(raw_train$card_type)
rate_by(raw_train$auth_method)
rate_by(raw_train$channel)
rate_by(raw_train$device_type)
rate_by(raw_train$day_of_week)

# ---- Risk flags and small counts ----

# Result: every flag raises the fraud rate. cvv_retry_count is strongest:
# 2 failed attempts give about 42% fraud.
rate_by(raw_train$is_foreign_transaction)
rate_by(raw_train$is_new_merchant)
rate_by(raw_train$used_vpn)
rate_by(raw_train$ip_country_mismatch)
rate_by(raw_train$billing_shipping_mismatch)
rate_by(raw_train$is_ai_generated_scam_attempt)
rate_by(raw_train$cvv_retry_count)
rate_by(raw_train$prior_disputes)

# ---- Hour of day ----

# Result: every hour stays between about 1.4% and 1.8%.
rate_by(raw_train$time_of_day_hour)

# ---- Transaction amount, in ranges ----

x <- raw_train$amount_usd
amount_range <- ifelse(x <= 10,   "$0 to $10",
                ifelse(x <= 25,   "$10 to $25",
                ifelse(x <= 50,   "$25 to $50",
                ifelse(x <= 100,  "$50 to $100",
                ifelse(x <= 250,  "$100 to $250",
                ifelse(x <= 500,  "$250 to $500",
                ifelse(x <= 1000, "$500 to $1,000", "Over $1,000")))))))

# Set the order explicitly; otherwise the ranges sort alphabetically.
amount_range <- factor(amount_range,
                       levels = c("$0 to $10", "$10 to $25", "$25 to $50",
                                  "$50 to $100", "$100 to $250", "$250 to $500",
                                  "$500 to $1,000", "Over $1,000"))
rate_by(amount_range)
# Result: fraud is more common above $100, highest from $100 to $250.

# ---- Continuous variables: medians by class and correlations ----

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score")

# Medians, since several columns are skewed. Row 0 = legitimate, 1 = fraud.
sapply(num_cols, function(v) tapply(raw_train[[v]], raw_train$is_fraud, median))

cor_cols <- c(num_cols, "cvv_retry_count", "prior_disputes")
correlations <- sapply(cor_cols, function(v) cor(raw_train[[v]], raw_train$is_fraud))

round(correlations[order(-correlations)], 3)
# Result: cvv_retry_count (0.21), merchant_risk_score (0.13) and
# velocity_score (0.10) are highest. Distance, card age and balance are
# below 0.01.