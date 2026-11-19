# =============================================================================
# 04_logistic_regression.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   1. Makes the train/validation split that EVERY model in the project uses,
#      and saves it so teammates load the same rows.
#   2. Fits the logistic regression model (the baseline model).
#   3. Scores it on the held-out validation rows: ROC-AUC, PR-AUC, and
#      precision/recall at two thresholds.
#   4. Runs a diagnostic fit that adds is_ai_generated_scam_attempt, to show
#      how much that possible leakage variable would change the results.
#
# How to run it
#   Run 01_load_clean.R first, in the same R session (this script uses the
#   cleaned dataframe `train`). Then run this script line by line
#   (Ctrl+Enter) or with Source with Echo (Ctrl+Shift+Enter).
#   It saves: results/split_idx.rds       (shared split, for all models)
#             results/logistic_results.rds (metrics, for Section 5)
#
# Packages used (install once with install.packages(c("tidyverse", "pROC")))
#   tidyverse  For the |> pipe, select() and mutate().
#   pROC       roc() and auc() compute ROC curves and the area under them.
# =============================================================================

library(tidyverse)
library(pROC)

dir.create("results", showWarnings = FALSE)

# ---- Shared split -----------------------------------------------------------

# Every model uses the same 80/20 split, so their scores can be compared.
# The split is stratified: fraud rows and legitimate rows are each split
# 80/20, so both sets keep the same 1.67% fraud rate. set.seed() makes the
# random choice repeatable.
set.seed(3220)
fraud_rows <- which(train$is_fraud == 1)
legit_rows <- which(train$is_fraud == 0)
train_idx  <- c(sample(fraud_rows, floor(0.8 * length(fraud_rows))),
                sample(legit_rows, floor(0.8 * length(legit_rows))))

# Teammates: load this file instead of making your own split.
#   train_idx <- readRDS("results/split_idx.rds")
saveRDS(train_idx, "results/split_idx.rds")

fit_data <- train[train_idx, ]     # used to fit the model
val_data <- train[-train_idx, ]    # held out: only used to score the model

# ---- Predictors -------------------------------------------------------------

# Left out on purpose:
#   id, is_fraud, fraud                 identifiers and the target itself.
#   amount_usd, account_balance_usd     replaced by log_amount, log_balance.
#                                       Logistic regression assumes a straight-
#                                       line effect, and the log versions are
#                                       far less skewed (see 01_load_clean.R).
#   is_ai_generated_scam_attempt        possible leakage: it may only be known
#                                       after an investigation, not at the
#                                       moment of the transaction. It is added
#                                       back only in the diagnostic fit below.
# Included: the two anomaly flags (atm_mismatch, over_balance), so the model
# can learn whether those odd rows matter.
# day_of_week and time_of_day_hour are treated as categories (factor), because
# the effect of an hour or a day need not be a straight line.
predictors <- c("merchant_category", "card_type", "auth_method", "channel",
                "device_type", "day_of_week", "time_of_day_hour",
                "is_foreign_transaction", "is_new_merchant", "used_vpn",
                "ip_country_mismatch", "billing_shipping_mismatch",
                "hours_since_last_txn", "txn_count_last_24h",
                "distance_from_home_km", "card_age_months", "customer_age",
                "velocity_score", "merchant_risk_score", "cvv_retry_count",
                "prior_disputes", "log_amount", "log_balance",
                "atm_mismatch", "over_balance")

make_formula <- function(vars) {
  vars <- ifelse(vars %in% c("day_of_week", "time_of_day_hour"),
                 paste0("factor(", vars, ")"), vars)
  reformulate(vars, response = "is_fraud")
}

# ---- Scoring helper ---------------------------------------------------------

# Fraud is rare, so accuracy is misleading (predicting "legitimate" every time
# is 98.3% accurate). ROC-AUC and PR-AUC do not depend on a threshold.
# Precision and recall need a threshold, so we report two:
#   0.5        the default cut-off.
#   best-F1    the cut-off with the best F1 score on the TRAINING rows, so the
#              validation rows are not used to pick it.
pr_auc <- function(actual, score) {
  # Area under the precision-recall curve, by the step-wise (average
  # precision) method.
  o <- order(score, decreasing = TRUE)
  a <- actual[o]
  precision <- cumsum(a) / seq_along(a)
  sum(precision * a) / sum(a)
}

at_threshold <- function(actual, score, cut) {
  pred <- as.integer(score >= cut)
  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)
  precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  recall    <- tp / (tp + fn)
  c(threshold = cut, precision = precision, recall = recall,
    f1 = 2 * precision * recall / (precision + recall))
}

best_f1_cut <- function(actual, score) {
  cuts <- seq(0.01, 0.9, by = 0.01)
  f1 <- sapply(cuts, function(k) at_threshold(actual, score, k)["f1"])
  cuts[which.max(replace(f1, is.na(f1), 0))]
}

fit_and_score <- function(vars, label) {
  start <- Sys.time()
  # glm() with family = binomial is logistic regression.
  model <- glm(make_formula(vars), data = fit_data, family = binomial)
  secs  <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # type = "response" gives predicted fraud probabilities. If a validation
  # row has a factor level that was not in the fit rows, predict() errors;
  # with 560,000 fit rows that should not happen.
  fit_score <- predict(model, fit_data, type = "response")
  val_score <- predict(model, val_data, type = "response")

  cut <- best_f1_cut(fit_data$is_fraud, fit_score)
  list(label = label, model = model, seconds = secs,
       roc_auc = as.numeric(auc(roc(val_data$is_fraud, val_score, quiet = TRUE))),
       pr_auc  = pr_auc(val_data$is_fraud, val_score),
       at_0.5  = at_threshold(val_data$is_fraud, val_score, 0.5),
       at_best = at_threshold(val_data$is_fraud, val_score, cut),
       val_score = val_score)
}

# ---- Models -----------------------------------------------------------------

# Primary logistic regression: no possible-leakage variable.
lr <- fit_and_score(predictors, "Logistic regression")

# Diagnostic only: same model plus the possible-leakage flag.
lr_leak <- fit_and_score(c(predictors, "is_ai_generated_scam_attempt"),
                         "Logistic regression + AI scam flag (diagnostic)")

# Reference line for Section 5: predicting the most common class gives an
# ROC-AUC of 0.5 by definition (no ranking of transactions).
results <- data.frame(
  model      = c("Majority class", lr$label, lr_leak$label),
  roc_auc    = c(0.5, lr$roc_auc, lr_leak$roc_auc),
  pr_auc     = c(mean(val_data$is_fraud), lr$pr_auc, lr_leak$pr_auc),
  precision  = c(NA, lr$at_best["precision"], lr_leak$at_best["precision"]),
  recall     = c(0, lr$at_best["recall"], lr_leak$at_best["recall"]),
  f1         = c(NA, lr$at_best["f1"], lr_leak$at_best["f1"]),
  fit_secs   = c(0, lr$seconds, lr_leak$seconds)
)
results[, -1] <- round(results[, -1], 3)
results   # a majority-class PR-AUC equals the fraud rate (the chance level)

# Precision and recall at the default 0.5 cut-off, for comparison.
rbind(lr$at_0.5, lr_leak$at_0.5) |> round(3)

# Which predictors matter? Odds ratios (exp of the coefficients) for the
# numeric and flag variables. An odds ratio above 1 raises the odds of fraud.
# The numeric variables are on different scales, so compare the flags with
# each other, not with the numeric variables.
coefs <- summary(lr$model)$coefficients
round(exp(coefs[!grepl("factor\\(|merchant_category|card_type|auth_method|channel|device_type",
                       rownames(coefs)), "Estimate"]), 3)

saveRDS(list(results = results, at_0.5 = lr$at_0.5, at_best = lr$at_best,
             coefficients = coefs),
        "results/logistic_results.rds")