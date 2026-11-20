# 04_split.R
# Stratified 80/20 train/validation split. Leaves train_set and valid_set.

library(tidymodels)

if (!exists("fraud_rate")) source("01_load_clean.R")

set.seed(3220)

# Stratify on fraud so both sets keep the 1.67% fraud rate.
fraud_split <- initial_split(train, prop = 0.8, strata = fraud)
train_set   <- training(fraud_split)
valid_set   <- testing(fraud_split)

# ---- Check the split ----

bind_rows(Training = train_set, Validation = valid_set, .id = "set") |>
  group_by(set) |>
  summarize(rows = n(), fraud = sum(is_fraud),
            fraud_rate = round(100 * mean(is_fraud), 3))
# 559,968 training and 139,992 validation rows, both 1.67% fraud.

nrow(semi_join(train_set, valid_set, by = "id"))   # 0 shared rows