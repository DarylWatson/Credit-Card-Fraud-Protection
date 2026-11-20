# 05_svm.R
# RBF SVM setup: training sample, recipe, model and tuning plan.
# Needs kernlab.

library(tidymodels)

if (!exists("train_set")) source("04_split.R")

# ---- Training sample ----

# Training time grows with roughly the square of the rows, so keep all fraud
# rows plus 4 legitimate rows per fraud row (20% fraud).
set.seed(3220)
fraud_part <- train_set |> filter(is_fraud == 1)
legit_part <- train_set |>
  filter(is_fraud == 0) |>
  slice_sample(n = 4 * nrow(fraud_part))
svm_sample <- bind_rows(fraud_part, legit_part)

count(svm_sample, fraud)   # 37,400 legitimate, 9,350 fraud

# ---- Recipe ----

# Log money columns replace the raw ones; the SVM needs scaled predictors.
svm_recipe <- recipe(fraud ~ ., data = svm_sample) |>
  step_rm(id, is_fraud, amount_usd, account_balance_usd) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

# ---- Model ----

svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")

svm_workflow <- workflow() |>
  add_recipe(svm_recipe) |>
  add_model(svm_spec)

svm_workflow

# ---- Tuning plan ----

# 3-fold CV on 10,000 sample rows; valid_set is kept out of tuning.
set.seed(3220)
tune_folds <- svm_sample |>
  slice_sample(n = 10000) |>
  vfold_cv(v = 3, strata = fraud)

svm_grid <- expand_grid(cost = c(0.1, 1, 10),
                        rbf_sigma = c(0.001, 0.005, 0.02, 0.08))

svm_metrics <- metric_set(roc_auc)