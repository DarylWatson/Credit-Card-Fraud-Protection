# =============================================================================
# 00c_consistency_checks.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   Checks pairs of columns that should agree with each other in real data.
#   This is how we found the rows the report calls anomalies:
#     1. channel vs device_type. An ATM transaction should come from an ATM
#        machine, and an in-app purchase should come from a phone or tablet.
#     2. amount_usd vs account_balance_usd. Is a purchase ever larger than
#        the balance?
#   01_load_clean.R turns the two findings into 0/1 flag columns
#   (atm_mismatch and over_balance). This script shows the evidence behind
#   them.
#
# How to run it
#   Same as 00a_first_look.R: set the working directory to the folder with
#   train.csv, then run line by line (Ctrl+Enter) or with Source with Echo
#   (Ctrl+Shift+Enter). It only shows results.
#
# Packages used
#   None beyond base R.
# =============================================================================

raw_train <- read.csv("train.csv")

# ---- 1. channel vs device_type ----------------------------------------------

# table() with two columns is a cross tabulation: one row
# per channel, one column per device type, and each cell counts the
# transactions with that pair.
channel_device <- table(raw_train$channel, raw_train$device_type)
channel_device

# Row percentages: how each channel's transactions split across device types.
# channel_totals holds the number of transactions in each channel; c() turns
# the table into a plain vector of those totals. Dividing the cross table by
# it divides every cell in a row by that row's total, because R lines the
# vector up with the rows.
channel_totals <- c(table(raw_train$channel))
round(100 * channel_device / channel_totals, 1)
# If channel and device were related, as in real data, these rows would look
# very different from each other. Here they look almost the same.

# The clearest case: ATM machines are about 3% of every channel, including
# the ATM channel itself.
round(100 * channel_device[, "ATM Machine"] / channel_totals, 1)

# Counts of some pairs that make no real-world sense. A comparison like
# raw_train$channel == "ATM" gives TRUE/FALSE for every row, & requires both
# to be TRUE, and sum() counts the TRUEs.
c(
  # This is what 01_load_clean.R flags as atm_mismatch.
  atm_channel_not_atm_device = sum(raw_train$channel == "ATM" &
                                   raw_train$device_type != "ATM Machine"),
  # The reverse: an ATM machine used on a channel other than ATM.
  atm_device_not_atm_channel = sum(raw_train$channel != "ATM" &
                                   raw_train$device_type == "ATM Machine"),
  # A store's card terminal used for an online purchase.
  online_on_pos_terminal     = sum(raw_train$channel == "Online" &
                                   raw_train$device_type == "POS Terminal")
)
# Result: 29,385, 20,042 and 45,270. The channel and device columns look as
# if they were generated independently of each other, which fits Kaggle's
# statement that the data is synthetic. Only the first pair is flagged in
# 01_load_clean.R, so the flag covers just one part of the problem.

# ---- 2. amount vs account balance -------------------------------------------

sum(raw_train$amount_usd > raw_train$account_balance_usd)
# Result: 7,470. This is what 01_load_clean.R flags as over_balance.
# Caution: on a credit card, spending more than the balance is normal, and
# the data does not say what account_balance_usd measures. So this pattern
# may not be an error at all.
