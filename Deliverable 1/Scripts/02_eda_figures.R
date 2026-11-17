# 02_eda_figures.R
# Builds the summary tables behind the numbers in Section 2 of the report
# and draws Figures 1 to 3.
# Runs 01_load_clean.R first if the cleaned data is not loaded. Figures
# appear in the Plots pane; 03_save_figures.R saves them as PNGs.

library(tidyverse)

# fraud_rate exists only if 01_load_clean.R has run (the 00 scripts make
# raw_train, not train).
if (!exists("fraud_rate")) source("01_load_clean.R")

# Colorblind-friendly colors, used the same way in every figure.
blue   <- "#2a78d6"
orange <- "#eb6834"
green  <- "#1baf7a"
gray   <- "#52514e"

# ==== Tables ====

# ---- Risk flags ----

flag_cols <- c("is_foreign_transaction", "is_new_merchant", "used_vpn",
               "ip_country_mismatch", "billing_shipping_mismatch",
               "is_ai_generated_scam_attempt")

# Share of rows flagged, and fraud rate (%) with the flag off and on.
flag_rates <- data.frame(
  label = c("Foreign transaction", "New merchant", "Used VPN",
            "IP country mismatch", "Billing/shipping mismatch",
            "AI-generated scam attempt"),
  share_flagged = sapply(flag_cols, function(v) mean(train[[v]])),
  pct_absent    = sapply(flag_cols, function(v) 100 * mean(train$is_fraud[train[[v]] == 0])),
  pct_present   = sapply(flag_cols, function(v) 100 * mean(train$is_fraud[train[[v]] == 1]))
)

cvv_rates <- tapply(train$is_fraud, train$cvv_retry_count, mean)

# ---- Merchant category, authentication method and amount ----

# Each table gets the same three columns (level, pct, group) so they can be
# stacked for Figure 2.
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

amount_rates <- train |>
  mutate(level = ifelse(amount_usd <= 25,   "$0 to $25",
                 ifelse(amount_usd <= 50,   "$25 to $50",
                 ifelse(amount_usd <= 100,  "$50 to $100",
                 ifelse(amount_usd <= 250,  "$100 to $250",
                 ifelse(amount_usd <= 1000, "$250 to $1,000", "Over $1,000")))))) |>
  group_by(level) |>
  summarize(pct = 100 * mean(is_fraud)) |>
  mutate(group = "Transaction amount")

overall_row <- data.frame(level = "All transactions", pct = 100 * fraud_rate,
                          group = "All transactions")

# The tables share no rows, so full_join() just stacks them.
group_rates <- overall_row |>
  full_join(merchant_rates, by = c("level", "pct", "group")) |>
  full_join(auth_rates,     by = c("level", "pct", "group")) |>
  full_join(amount_rates,   by = c("level", "pct", "group"))

# ---- Variables with little signal ----

# Lowest and highest fraud rate (%) across each variable's values.
weak_vars <- c("card_type", "channel", "device_type", "day_of_week",
               "time_of_day_hour")
weak_ranges <- sapply(weak_vars,
                      function(v) round(100 * range(tapply(train$is_fraud, train[[v]], mean)), 2))

# ---- Continuous variables ----

num_cols <- c("amount_usd", "hours_since_last_txn", "txn_count_last_24h",
              "distance_from_home_km", "card_age_months", "customer_age",
              "account_balance_usd", "velocity_score", "merchant_risk_score")

medians <- sapply(num_cols, function(v) tapply(train[[v]], train$fraud, median))

cor_cols <- c(num_cols, "cvv_retry_count", "prior_disputes")
correlations <- sapply(cor_cols, function(v) cor(train[[v]], train$is_fraud))
correlations <- round(correlations[order(-correlations)], 3)

# ---- Show the tables ----

flag_rates     # IP country mismatch: about 16% fraud when present
cvv_rates      # 2 failed attempts: about 42% fraud
group_rates    # no authentication 6.4%, biometric 0.5%, gift cards 7.3%
weak_ranges    # each within about 1.4% to 1.9%
medians        # merchant risk score 34 vs 45.9, age 52 vs 61
correlations   # cvv 0.21, merchant risk 0.13, velocity 0.10

# ==== Figure 1: fraud rate with and without each risk flag ====

# Order flags by their "present" rate; ggplot2 draws the first level at the
# bottom, so the highest ends up on top.
flag_rates$label <- factor(flag_rates$label,
                           levels = flag_rates$label[order(flag_rates$pct_present)])

fig_flags <- ggplot(flag_rates, aes(y = label)) +
  geom_point(aes(x = pct_absent,  color = "Flag absent")) +
  geom_point(aes(x = pct_present, color = "Flag present")) +
  scale_color_manual(values = c("Flag absent" = blue, "Flag present" = orange)) +
  labs(x = "Fraud rate (%)", y = NULL, color = NULL)

fig_flags

# ==== Figure 2: fraud rate by merchant category, authentication and amount ====

# Bottom to top: amount ranges (largest first), authentication methods and
# merchant categories (each sorted by rate), then the overall row.
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

# ==== Figure 3: boxplots of six continuous variables ====

# Base R graphics in a 2 x 3 grid. Amount is left out because Figure 2
# covers it.
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
  par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))   # restore R's defaults
}

draw_distributions()