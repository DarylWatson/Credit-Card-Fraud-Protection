# =============================================================================
# 01_load_clean.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   1. Loads the labeled training data.
#   2. Checks it for missing values, duplicates, outliers, and anomalies.
#   3. Cleans it: fixes data types, adds log-transformed money columns, and
#      adds 0/1 columns that mark logically odd rows.
#
# How to run it
#   1. Put train.csv and test.csv in the same folder as this script.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Run it line by line with Ctrl+Enter, or all at once with
#      Ctrl+Shift+Enter (Source with Echo).
#   It leaves a cleaned dataframe called `train` in your R session, which
#   02_eda_figures.R uses. The last line shows a short cleaning report.
#
# Package used (install once with install.packages("tidyverse"))
#   tidyverse  For dplyr's mutate(), select() and distinct() and the |> pipe.
#              mutate() adds or changes columns.
#              The pipe |> passes the dataframe on its left into the
#              function on its right, so steps can be chained.
# =============================================================================

library(tidyverse)

# ---- Load -------------------------------------------------------------------

# read.csv() reads a CSV file into a dataframe. The training
# file is about 98 MB, so this takes several seconds.
train <- read.csv("train.csv")

# The competition's test file has no is_fraud column, so we cannot score a
# model on it ourselves and we do not use it for modeling. We read it only to
# report how many rows it has.
n_test <- nrow(read.csv("test.csv"))

n_train    <- nrow(train)            # number of training transactions
n_fraud    <- sum(train$is_fraud)    # number of fraudulent transactions
fraud_rate <- mean(train$is_fraud)   # share of transactions that are fraud

# ---- Step 1: missing values and duplicates ----------------------------------

# is.na() marks each missing value, and sum() counts them
# across the whole dataframe.
n_missing <- sum(is.na(train))

# A text column can also be "missing" as an empty string "", which is.na()
# does not catch, so count those in the five text columns.
text_cols <- c("merchant_category", "card_type", "auth_method",
               "channel", "device_type")
n_blank <- sum(sapply(text_cols, function(v) sum(train[[v]] == "")))

# distinct() keeps one copy of each different row. Without the
# id column, any duplicated transaction would be dropped, so the difference
# in row counts is the number of duplicates.
n_duplicates <- nrow(train) - nrow(distinct(select(train, -id)))

# Result: all three counts are 0, so nothing is removed or imputed.

# ---- Step 2: data types -----------------------------------------------------

# read.csv() loads the six risk flags as the text "True" or "False", and the
# five categorical variables as text. mutate() replaces each column with a
# better type:
#   - Flags become 1/0 with ifelse(). Numbers work in every
#     model, and the mean of a 0/1 column is the share of rows where the
#     flag is set.
#   - Categorical variables become factors with factor(). A
#     factor tells R the column is a fixed set of categories, so models such
#     as glm() make one dummy variable per category automatically.
#   - is_fraud is stored as the decimals 0 and 1. We keep it as it is (handy
#     for mean() and cor()) and add a labeled copy, `fraud`, for plots and
#     for classification functions that expect categories.
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

# ---- Step 3: outliers and log transforms ------------------------------------

# Count outliers with the 1.5 x IQR rule: a value is an outlier
# if it is more than 1.5 * IQR below the first quartile or above the third
# quartile. quantile() gives the quartiles, IQR() gives the
# interquartile range, and | means "or".
count_iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  sum(x < q1 - 1.5 * IQR(x) | x > q3 + 1.5 * IQR(x))
}
n_outlier_amount  <- count_iqr_outliers(train$amount_usd)
n_outlier_balance <- count_iqr_outliers(train$account_balance_usd)

# Both money columns are strongly right-skewed: most values are small and a
# few are very large (the median amount is about $27, the maximum about
# $6,900). We do NOT delete the large values, because they look like real
# large purchases and large purchases are more often fraud.
#
# Instead we add log-transformed copies. A log scale squeezes
# the long right tail, so $10 to $100 is the same step as $100 to $1,000.
# This helps plots and linear models such as logistic regression; tree
# models do not need it. We take log(x + 1) rather than log(x) so that a
# value of 0 still gives 0 instead of an error. The original columns stay.
train <- train |>
  mutate(
    log_amount  = log(amount_usd + 1),
    log_balance = log(account_balance_usd + 1)
  )

# ---- Step 4: flag logically odd rows ----------------------------------------

# Two patterns in the data do not make real-world sense (see
# 00c_consistency_checks.R). We keep these rows and mark them with 0/1
# columns, so that a model can learn whether they matter.
#   atm_mismatch: the channel is ATM, but the device is not an ATM machine.
#   over_balance: the purchase is larger than the account balance.
train <- train |>
  mutate(
    atm_mismatch = ifelse(channel == "ATM" & device_type != "ATM Machine", 1, 0),
    over_balance = ifelse(amount_usd > account_balance_usd, 1, 0)
  )

n_atm_mismatch <- sum(train$atm_mismatch)
n_over_balance <- sum(train$over_balance)

# ---- Cleaning report --------------------------------------------------------

# as.character() turns the numbers into text, so whole counts print without
# the decimals that the fraud rate would otherwise add to every row.
data.frame(
  check = c("Training rows", "Test rows (not used)", "Fraud rows",
            "Fraud rate (%)", "Missing (NA) values", "Blank text values",
            "Duplicate rows", "IQR outliers, amount", "IQR outliers, balance",
            "ATM channel, other device", "Amount above balance"),
  result = as.character(c(n_train, n_test, n_fraud, round(100 * fraud_rate, 2),
                          n_missing, n_blank, n_duplicates, n_outlier_amount,
                          n_outlier_balance, n_atm_mismatch, n_over_balance))
)
