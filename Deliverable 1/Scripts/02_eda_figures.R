# =============================================================================
# 02_eda_figures.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   1. Builds the summary tables behind every number quoted in Section 2 of
#      the report (fraud rates by group, medians, correlations).
#   2. Draws the three exploratory figures:
#        Figure 1  fraud rate with and without each risk flag (ggplot2)
#        Figure 2  fraud rate by merchant category, authentication method
#                  and transaction amount (ggplot2)
#        Figure 3  boxplots of six continuous variables (base R graphics)
#
# How to run it
#   Run 01_load_clean.R first, in the same R session. This script uses the
#   cleaned dataframe `train` and the numbers it made (such as fraud_rate).
#   Then run this script line by line (Ctrl+Enter) or with Source with Echo
#   (Ctrl+Shift+Enter). Each figure appears in RStudio's Plots pane.
#
# Saving a figure for the report
#   In the Plots pane, click Export > Save as Image, choose PNG, and set the
#   size in pixels: Figure 1 = 780 x 300, Figure 2 = 780 x 440,
#   Figure 3 = 780 x 480. In Word, right-click the old picture, choose
#   Change Picture > From a File, and make it 6.5 inches wide. At that width,
#   the text in the figure comes out at about the same size as the report's.
#
# Package used (install once with install.packages("tidyverse"))
#   tidyverse  dplyr's group_by(), summarize(), mutate(), select() and
#              full_join() for the tables, and ggplot2 for
#              Figures 1 and 2.
# =============================================================================

library(tidyverse)

# One color per group, used the same way in every figure. Blue, orange and
# green stay easy to tell apart for readers with the common forms of color
# blindness. Gray marks the overall rate.
blue   <- "#2a78d6"
orange <- "#eb6834"
green  <- "#1baf7a"
gray   <- "#52514e"

# =============================================================================
# Tables
# =============================================================================

# ---- Risk flags -------------------------------------------------------------

flag_cols <- c("is_foreign_transaction", "is_new_merchant", "used_vpn",
               "ip_country_mismatch", "billing_shipping_mismatch",
               "is_ai_generated_scam_attempt")

# For each flag: how common it is (the mean of a 0/1 column is the share of
# 1s), and the fraud rate in percent when the flag is 0 and when it is 1.
# train$is_fraud[train[[v]] == 1] keeps only the rows where flag v is 1.
flag_rates <- data.frame(
  label = c("Foreign transaction", "New merchant", "Used VPN",
            "IP country mismatch", "Billing/shipping mismatch",
            "AI-generated scam attempt"),
  share_flagged = sapply(flag_cols, function(v) mean(train[[v]])),
  pct_absent    = sapply(flag_cols, function(v) 100 * mean(train$is_fraud[train[[v]] == 0])),
  pct_present   = sapply(flag_cols, function(v) 100 * mean(train$is_fraud[train[[v]] == 1]))
)

# Fraud rate by number of failed CVV attempts.
cvv_rates <- tapply(train$is_fraud, train$cvv_retry_count, mean)

# ---- Merchant category, authentication method and amount --------------------

# group_by() + summarize() gives one row per category with its fraud rate
# in percent. mutate() then adds a plain-text `level` column and
# a `group` column naming the variable, and select() keeps those columns.
merchant_rates <- train |>
  group_by(merchant_category) |>
  summarize(pct = 100 * mean(is_fraud)) |>
  mutate(level = as.character(merchant_category), group = "Merchant category") |>
  select(level, pct, group)

auth_rates <- train |>
  group_by(auth_method) |>
  summarize(pct = 100 * mean(is_fraud)) |>
  mutate(level = as.character(auth_method), group = "Authentication method") |>
  select(level, pct, group)

# Amount ranges with nested ifelse(). Each test only runs if
# the ones before it were FALSE, so "amount_usd <= 50" means "above $25 and
# up to $50".
amount_rates <- train |>
  mutate(level = ifelse(amount_usd <= 25,   "$0 to $25",
                 ifelse(amount_usd <= 50,   "$25 to $50",
                 ifelse(amount_usd <= 100,  "$50 to $100",
                 ifelse(amount_usd <= 250,  "$100 to $250",
                 ifelse(amount_usd <= 1000, "$250 to $1,000", "Over $1,000")))))) |>
  group_by(level) |>
  summarize(pct = 100 * mean(is_fraud)) |>
  mutate(group = "Transaction amount")

# One row for the overall fraud rate, to compare every group against.
overall_row <- data.frame(level = "All transactions", pct = 100 * fraud_rate,
                          group = "All transactions")

# full_join() keeps every row from both tables. These tables
# share no rows, so joining them simply stacks them into one table.
group_rates <- overall_row |>
  full_join(merchant_rates, by = c("level", "pct", "group")) |>
  full_join(auth_rates,     by = c("level", "pct", "group")) |>
  full_join(amount_rates,   by = c("level", "pct", "group"))

# ---- Variables with little signal -------------------------------------------

# The lowest and highest fraud rate (in percent) across the values of each
# of these variables. range() gives the minimum and maximum.
# They all stay within a few tenths of a percentage point of 1.67%.
weak_vars <- c("card_type", "channel", "device_type", "day_of_week",
               "time_of_day_hour")
weak_ranges <- sapply(weak_vars,
                      function(v) round(100 * range(tapply(train$is_fraud, train[[v]], mean)), 2))

# ---- Continuous variables ---------------------------------------------------

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score")

# Median of each variable for legitimate and fraudulent transactions.
medians <- sapply(num_cols, function(v) tapply(train[[v]], train$fraud, median))

# Correlation of each numeric variable with the 0/1 fraud label,
# largest first.
cor_cols <- c(num_cols, "cvv_retry_count", "prior_disputes")
correlations <- sapply(cor_cols, function(v) cor(train[[v]], train$is_fraud))
correlations <- round(correlations[order(-correlations)], 3)

# ---- Show the tables --------------------------------------------------------

flag_rates     # e.g. IP country mismatch: about 16% fraud when present
cvv_rates      # 2 failed attempts: about 42% fraud
group_rates    # e.g. no authentication 6.4%, biometric 0.5%, gift cards 7.3%
weak_ranges    # each within about 1.4% to 1.9%
medians        # e.g. merchant risk score 34 vs 45.9, age 52 vs 61
correlations   # cvv 0.21, merchant risk 0.13, velocity 0.10

# =============================================================================
# Figure 1: fraud rate with and without each risk flag
# =============================================================================

# A dot plot: one row per flag, with a blue dot for the fraud rate when the
# flag is absent and an orange dot for when it is present.
# factor() with levels puts the flags in order of their "present" rate, so
# the highest ends up at the top (ggplot2 draws the first level at the
# bottom).
flag_rates$label <- factor(flag_rates$label,
                           levels = flag_rates$label[order(flag_rates$pct_present)])

# Writing color = "Flag absent" inside aes() gives each layer a legend entry,
# and scale_color_manual() picks the color for each entry.
fig_flags <- ggplot(flag_rates, aes(y = label)) +
  geom_point(aes(x = pct_absent,  color = "Flag absent")) +
  geom_point(aes(x = pct_present, color = "Flag present")) +
  scale_color_manual(values = c("Flag absent" = blue, "Flag present" = orange)) +
  labs(x = "Fraud rate (%)", y = NULL, color = NULL)

fig_flags

# =============================================================================
# Figure 2: fraud rate by merchant category, authentication method and amount
# =============================================================================

# All 24 rows share one axis, colored by the variable they belong to, with
# the overall rate in gray at the top for comparison.
# The level order (bottom to top): amount ranges from largest to smallest,
# then authentication methods and merchant categories each sorted by fraud
# rate, then the overall row.
level_order <- c("Over $1,000", "$250 to $1,000", "$100 to $250",
                 "$50 to $100", "$25 to $50", "$0 to $25",
                 auth_rates$level[order(auth_rates$pct)],
                 merchant_rates$level[order(merchant_rates$pct)],
                 "All transactions")
group_rates$level <- factor(group_rates$level, levels = level_order)
group_rates$group <- factor(group_rates$group,
                            levels = c("All transactions", "Merchant category",
                                       "Authentication method", "Transaction amount"))

fig_groups <- ggplot(group_rates, aes(x = pct, y = level, color = group)) +
  geom_point() +
  scale_color_manual(values = c("All transactions" = gray,
                                "Merchant category" = blue,
                                "Authentication method" = orange,
                                "Transaction amount" = green)) +
  labs(x = "Fraud rate (%)", y = NULL, color = NULL)

fig_groups

# =============================================================================
# Figure 3: boxplots of six continuous variables
# =============================================================================

# Base R graphics. par() sets plot-wide options:
#   mfrow = c(2, 3) splits the plot area into 2 rows and 3 columns, and
#   mar = c(bottom, left, top, right) sets the margins around each plot, in
#   lines of text. The default margins are wide, so these are smaller.
# boxplot(y ~ group, data = ...) draws one box per group, using the same
# formula style as plot(y ~ x, data = ...).
# Each box shows the median (thick line), the middle 50% (the box), and
# outliers past 1.5 x IQR (the circles).
# Transaction amount is left out here because Figure 2 already shows it.
draw_distributions <- function() {
  par(mfrow = c(2, 3), mar = c(2.5, 4, 2.5, 1))
  box_colors <- c(blue, orange)
  boxplot(merchant_risk_score ~ fraud, data = train, col = box_colors,
          main = "Merchant risk score", xlab = "", ylab = "Score")
  boxplot(velocity_score ~ fraud, data = train, col = box_colors,
          main = "Velocity score", xlab = "", ylab = "Score")
  boxplot(customer_age ~ fraud, data = train, col = box_colors,
          main = "Customer age", xlab = "", ylab = "Years")
  boxplot(hours_since_last_txn ~ fraud, data = train, col = box_colors,
          main = "Hours since last transaction", xlab = "", ylab = "Hours")
  boxplot(distance_from_home_km ~ fraud, data = train, col = box_colors,
          main = "Distance from home", xlab = "", ylab = "Kilometers")
  boxplot(card_age_months ~ fraud, data = train, col = box_colors,
          main = "Card age", xlab = "", ylab = "Months")
  par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))   # back to R's defaults
}

draw_distributions()
