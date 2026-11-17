# 00c_consistency_checks.R
# Checks pairs of columns that should agree in real data. This is where the
# report's two anomalies come from (flagged in 01_load_clean.R as
# atm_mismatch and over_balance).
# Run from the folder that holds train.csv. Shows results only.

raw_train <- read.csv("train.csv")

# ---- 1. channel vs device_type ----

channel_device <- table(raw_train$channel, raw_train$device_type)
channel_device

# Row percentages: how each channel splits across device types.
channel_totals <- c(table(raw_train$channel))
round(100 * channel_device / channel_totals, 1)
# In real data these rows would differ a lot. Here they are nearly the same.

# ATM machines are about 3% of every channel, even the ATM channel.
round(100 * channel_device[, "ATM Machine"] / channel_totals, 1)

c(
  atm_channel_not_atm_device = sum(raw_train$channel == "ATM" &
                                   raw_train$device_type != "ATM Machine"),
  atm_device_not_atm_channel = sum(raw_train$channel != "ATM" &
                                   raw_train$device_type == "ATM Machine"),
  online_on_pos_terminal     = sum(raw_train$channel == "Online" &
                                   raw_train$device_type == "POS Terminal")
)
# Result: 29,385, 20,042 and 45,270. Channel and device look independent,
# which fits synthetic data. Only the first pair is flagged in
# 01_load_clean.R.

# ---- 2. amount vs account balance ----

sum(raw_train$amount_usd > raw_train$account_balance_usd)
# Result: 7,470. On a credit card, spending past the balance can be normal,
# and the data does not define account_balance_usd, so this may not be an
# error.