# 01_load_clean.R
# Loads the labeled training data, checks it, and cleans it: fixes data
# types, adds log money columns, and flags logically odd rows.
# Run from the folder that holds train.csv and test.csv. Leaves a cleaned
# dataframe `train` in the session for 02_eda_figures.R, and ends with a
# short cleaning report.

library(tidyverse)

# ---- Load ----

train <- read.csv("train.csv")

# The test file has no labels, so it is read only to count its rows.
n_test <- nrow(read.csv("test.csv"))

n_train    <- nrow(train)
n_fraud    <- sum(train$is_fraud)
fraud_rate <- mean(train$is_fraud)

# ---- Step 1: missing values and duplicates ----

n_missing <- sum(is.na(train))

# is.na() misses empty strings, so check the text columns separately.
text_cols <- c("merchant_category", "card_type", "auth_method",
               "channel", "device_type")
n_blank <- sum(sapply(text_cols, function(v) sum(train[[v]] == "")))

n_duplicates <- nrow(train) - nrow(distinct(select(train, -id)))

# Result: all three are 0, so nothing is removed or imputed.

# ---- Step 2: data types ----

# Flags become 0/1, categorical columns become factors (so glm() makes dummy
# variables), and `fraud` is a labeled copy of is_fraud for plots and
# classifiers that expect categories.
train <- train |>
  mutate(
    is_foreign_transaction       = ifelse(is_foreign_transaction == "True", 1, 0),
    is_new_merchant              = ifelse(is_new_merchant == "True", 1, 0),
    used_vpn                     = ifelse(used_vpn == "True", 1, 0),
    ip_country_mismatch          = ifelse(ip_country_mismatch == "True", 1, 0),
    billing_shipping_mismatch    = ifelse(billing_shipping_mismatch == "True", 1, 0),
    is_ai_generated_scam_attempt = ifelse(is_ai_generated_scam_attempt == "True", 1, 0),
    merchant_category = factor(merchant_category),
    card_type         = factor(card_type),
    auth_method       = factor(auth_method),
    channel           = factor(channel),
    device_type       = factor(device_type),
    fraud = factor(ifelse(is_fraud == 1, "Fraud", "Legitimate"),
                   levels = c("Legitimate", "Fraud"))
  )

# ---- Step 3: outliers and log transforms ----

# 1.5 x IQR rule.
count_iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  sum(x < q1 - 1.5 * IQR(x) | x > q3 + 1.5 * IQR(x))
}
n_outlier_amount  <- count_iqr_outliers(train$amount_usd)
n_outlier_balance <- count_iqr_outliers(train$account_balance_usd)

# Large values look like real purchases, so they are kept. Log copies reduce
# the skew for plots and linear models; log(x + 1) handles zeros.
train <- train |>
  mutate(
    log_amount  = log(amount_usd + 1),
    log_balance = log(account_balance_usd + 1)
  )

# ---- Step 4: flag logically odd rows ----

# See 00c_consistency_checks.R. Rows are kept so models can learn whether
# these matter.
train <- train |>
  mutate(
    atm_mismatch = ifelse(channel == "ATM" & device_type != "ATM Machine", 1, 0),
    over_balance = ifelse(amount_usd > account_balance_usd, 1, 0)
  )

n_atm_mismatch <- sum(train$atm_mismatch)
n_over_balance <- sum(train$over_balance)

# ---- Cleaning report ----

# as.character() keeps whole counts from printing with decimals.
data.frame(
  check = c("Training rows", "Test rows (not used)", "Fraud rows",
            "Fraud rate (%)", "Missing (NA) values", "Blank text values",
            "Duplicate rows", "IQR outliers, amount", "IQR outliers, balance",
            "ATM channel, other device", "Amount above balance"),
  result = as.character(c(n_train, n_test, n_fraud, round(100 * fraud_rate, 2),
                          n_missing, n_blank, n_duplicates, n_outlier_amount,
                          n_outlier_balance, n_atm_mismatch, n_over_balance))
)