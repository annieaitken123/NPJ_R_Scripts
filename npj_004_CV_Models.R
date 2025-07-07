# Predictive Modeling Script: Stratified Walk-Forward Cross-Validation
# ---------------------------------------------------------------
# Title: npj_004_CV_Models.R
# Purpose: Estimate within- and between-person associations between
# morning biometrics (HR, HRV, RR) and evening symptoms (Crash, Fatigue, Brain Fog)
# using logistic mixed-effects models.
#
# Cross-validation strategy: 5-fold walk-forward within-person CV
# + 20% of users held out to evaluate cross-person generalization.
# Optional: toggle to exclude users with >30% missing data.
#
# Outputs: Cross-validated and holdout-set AUC, recall, precision, F1-score, etc.
# Figures for missingness and HRV variability.
# ---------------------------------------------------------------

# ==== Load Libraries ====
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(runner)
library(lme4)
library(recipes)
library(pROC)
library(yardstick)
library(naniar)

# ==== Set Seed for Reproducibility ====
set.seed(42)

# ==== Toggle: Exclude users with >30% missing days ====
apply_exclusion_filter <- TRUE

# ==== Define Helper Functions ====

# Compute baseline mean and SD per user for each biometric
compute_baseline_vars <- function(df) {
  df %>%
    group_by(user_id_pk) %>%
    mutate(
      mean_HR = mean(HR, na.rm = TRUE),
      mean_HRV = mean(HRV, na.rm = TRUE),
      mean_RR = mean(RR, na.rm = TRUE),
      sd_HR = sd(HR, na.rm = TRUE),
      sd_HRV = sd(HRV, na.rm = TRUE),
      sd_RR = sd(RR, na.rm = TRUE),
      cov_HR = sd_HR / mean_HR,
      cov_HRV = sd_HRV / mean_HRV,
      cov_RR = sd_RR / mean_RR
    ) %>%
    ungroup()
}

# Compute rolling 7-day window statistics + CWC variables
compute_change_vars <- function(df) {
  df <- as.data.table(df)
  df[, interval_days := as.numeric(difftime(observation_date_pk, shift(observation_date_pk, type = "lag"), units = "days")), by = user_id_pk]
  df[, missed_day := ifelse(interval_days == 1, 0, 1)]

  df[, `:=`(
    HR_7d_sd = runner(HR, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) sd(.x, na.rm = TRUE) else NA_real_),
    HRV_7d_sd = runner(HRV, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) sd(.x, na.rm = TRUE) else NA_real_),
    RR_7d_sd = runner(RR, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) sd(.x, na.rm = TRUE) else NA_real_),
    HR_7d_avg = runner(HR, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) mean(.x, na.rm = TRUE) else NA_real_),
    HRV_7d_avg = runner(HRV, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) mean(.x, na.rm = TRUE) else NA_real_),
    RR_7d_avg = runner(RR, k = 7, f = ~ if (sum(!is.na(.x)) >= 7) mean(.x, na.rm = TRUE) else NA_real_)
  ), by = user_id_pk]

  df[, `:=`(
    cov_HR_7d = HR_7d_sd / HR_7d_avg,
    cov_HRV_7d = HRV_7d_sd / HRV_7d_avg,
    cov_RR_7d = RR_7d_sd / RR_7d_avg
  )]

  df[, `:=`(
    HR_cwc = HR - mean_HR,
    HRV_cwc = HRV - mean_HRV,
    RR_cwc = RR - mean_RR,
    cov_HRV_7d_cwc = cov_HRV_7d - cov_HRV,
    cov_HR_7d_cwc = cov_HR_7d - cov_HR,
    cov_RR_7d_cwc = cov_RR_7d - cov_RR
  )]

  as_tibble(df)
}

# ==== Load and Preprocess Data ====
# (Assumes object `crash_ml` exists with appropriate columns)

# Prepare data splits: 80% train / 20% holdout based on users
unique_users <- unique(crash_ml$user_id_pk)
user_split <- sample(c("train", "holdout"), length(unique_users), replace = TRUE, prob = c(0.8, 0.2))
user_split_df <- data.frame(user_id_pk = unique_users, data_set_holdout = user_split)

crash_ml <- crash_ml %>%
  rename(RR = BreathRate, BrainFog = `Brain Fog`) %>%
  mutate(Crash = ifelse(Crash >= 2, NA, Crash)) %>%
  left_join(user_split_df, by = "user_id_pk") %>%
  arrange(user_id_pk, observation_date_pk) %>%
  mutate(
    Fatigue_Dicho = factor(ifelse(Fatigue >= 2, 1, ifelse(Fatigue <= 1, 0, NA))),
    BrainFog_Dicho = factor(ifelse(BrainFog >= 2, 1, ifelse(BrainFog <= 1, 0, NA))),
    Crash_1d_lag = lag(as.numeric(Crash)),
    Fatigue_1d_lag_Dicho = lag(as.numeric(Fatigue_Dicho)),
    BrainFog_1d_lag_Dicho = lag(as.numeric(BrainFog_Dicho))
  )

# Filter for training users only
fold_data <- crash_ml %>% filter(data_set_holdout == "train")

# Apply exclusion filter toggle
if (apply_exclusion_filter) {
  user_quality <- fold_data %>%
    group_by(user_id_pk) %>%
    summarise(
      start_date = min(observation_date_pk),
      end_date = max(observation_date_pk),
      n_expected_days = as.numeric(end_date - start_date) + 1,
      n_actual_days = n_distinct(observation_date_pk),
      pct_missing = 1 - (n_actual_days / n_expected_days)
    )

  users_to_exclude <- user_quality %>%
    filter(pct_missing > 0.30) %>%
    pull(user_id_pk)

  fold_data <- fold_data %>%
    filter(!user_id_pk %in% users_to_exclude)
}


# ==== Assign Walk-Forward Folds for CV ====
# Each user's data is sorted chronologically and split into 5 sequential folds
# ensuring that training always precedes validation in time.

num_folds <- 5

fold_data <- fold_data %>%
  group_by(user_id_pk) %>%
  arrange(observation_date_pk, .by_group = TRUE) %>%
  mutate(
    obs_index = row_number(),
    n_obs = n(),
    fold = ceiling(obs_index / (n_obs / num_folds)),  # sequential chunking
    fold = ifelse(fold > num_folds, num_folds, fold)  # cap fold index at 5
  ) %>%
  ungroup()

# ==== Prepare Holdout Set ====
# This set includes 20% of users excluded from training folds
holdout_data <- crash_ml %>%
  filter(data_set_holdout == "holdout") %>%
  arrange(user_id_pk, observation_date_pk) %>%
  mutate(fold = 10)  # Arbitrary fold index for clarity

# ==== Initialize CV Containers ====
all_results_strat <- list()      # Metrics per fold
holdout_results <- list()       # Metrics for final holdout set
trained_models <- list()        # Stores each model object

# ==== Define Outcomes and Model Types ====
outcomes <- c("Crash", "Fatigue_Dicho", "BrainFog_Dicho")
model_types <- c("all_vars_without_lag", "with_lag")

# ==== Start Outer Loop Over Outcomes ====
for (outcome in outcomes) {
  message(glue::glue("Running models for outcome: {outcome}"))

  # Filter rows with non-missing outcome
  fold_data_outcome <- fold_data %>%
    filter(!is.na(.data[[outcome]]))

  # Define appropriate lag variable based on outcome
  lag_var <- case_when(
    outcome == "Crash" ~ "Crash_1d_lag",
    outcome == "Fatigue_Dicho" ~ "Fatigue_1d_lag_Dicho",
    outcome == "BrainFog_Dicho" ~ "BrainFog_1d_lag_Dicho"
  )

  # Loop over CV folds (2 through 5) for walk-forward validation
  for (fold_idx in 2:5) {
    message(glue::glue("  Fold: {fold_idx}"))

    # Split into training and validation based on time
    train_fold <- fold_data_outcome %>% filter(fold < fold_idx)
    valid_fold <- fold_data_outcome %>% filter(fold == fold_idx)

    # Compute user-level baseline stats
    train_baseline <- compute_baseline_vars(train_fold)
    valid_baseline <- left_join(valid_fold, distinct(train_baseline, user_id_pk, starts_with("mean_"), starts_with("cov_")), by = "user_id_pk")

    # Add within-person change features
    train_processed <- compute_change_vars(train_baseline)
    valid_processed <- compute_change_vars(valid_baseline)


    # === Loop Over Model Types: With or Without Lag Predictors ===
    for (model_type in model_types) {
      message(glue::glue("    Model Type: {model_type}"))

      # ==== Preprocess Training and Validation Data ====

      rec <- recipe(as.formula(paste(outcome, "~ .")), data = fold_train_data) %>%
        step_select(all_of(c(lag_var, names(variable_names_ml))), all_outcomes(), all_of("user_id_pk"), everything()) %>%
        step_naomit(all_predictors()) %>%
        step_normalize(all_of(c(lag_var, names(variable_names_ml)))) %>%
        prep(training = fold_train_data)

      # Apply the preprocessing recipe
      fold_train_prepped <- bake(rec, new_data = fold_train_data)
      fold_valid_prepped <- bake(rec, new_data = fold_valid_data)

      # Optional: Check missingness in a key predictor (e.g., HRV 7-day CoV)
      prop_missing <- round(
        sum(is.na(fold_train_prepped$HRV_7d_CoV.cwc)) / nrow(fold_train_prepped),
        2
      )
      message(glue::glue("    Prop Missing HRV_7d_CoV.cwc in training: {prop_missing}"))

      # Define model formula based on model type
      predictors <- c(
        "HR.cm", "HR.cwc", "HR_CoV", "HR_7d_CoV.cwc",
        "HRV.cm", "HRV.cwc", "HRV_CoV", "HRV_7d_CoV.cwc",
        "RR.cm", "RR.cwc", "RR_CoV"
      )

      if (model_type == "with_lag") {
        predictors <- c(lag_var, predictors)
      }

      # ==== Fit Model ====

      # Final model formula
      model_formula <- as.formula(
        paste0(outcome, " ~ ", paste(predictors, collapse = " + "), " + (1 | user_id_pk)")
      )

      # ==== Save Trained Model ====
      trained_models[[outcome]][[model_type]][[i]] <- model

      # ==== Generate Predictions ====
      # Two variants:
      #  - Without random effects (fixed effects only)
      #  - With random effects (subject-specific intercepts)
      predictions_no_re <- predict(
        model,
        newdata = fold_valid_prepped,
        type = "response",
        allow.new.levels = TRUE,
        re.form = NA
      )

      predictions_re <- predict(
        model,
        newdata = fold_valid_prepped,
        type = "response",
        allow.new.levels = TRUE,
        re.form = ~(1 | user_id_pk)
      )

      # ==== Evaluate AUC ====
      auc_no_re <- auc(roc(fold_valid_prepped[[outcome]], predictions_no_re))
      auc_re <- auc(roc(fold_valid_prepped[[outcome]], predictions_re))

      message(glue::glue("    AUC (No RE): {round(auc_no_re, 3)}"))
      message(glue::glue("    AUC (With RE): {round(auc_re, 3)}"))

      # ==== Format Predictions for Metric Calculation ====
      # Yardstick expects 1 = positive class first
      valid_data_no_re <- tibble(
        truth = factor(fold_valid_prepped[[outcome]], levels = c(1, 0)),
        estimate = factor(ifelse(predictions_no_re > 0.5, 1, 0), levels = c(1, 0))
      )

      valid_data_re <- tibble(
        truth = factor(fold_valid_prepped[[outcome]], levels = c(1, 0)),
        estimate = factor(ifelse(predictions_re > 0.5, 1, 0), levels = c(1, 0))
      )

      # ==== Compute Classification Metrics ====
      # -- No Random Effects
      recall_score_no_re <- recall(valid_data_no_re, truth, estimate, na_rm = TRUE)
      precision_score_no_re <- precision(valid_data_no_re, truth, estimate, na_rm = TRUE)
      f1_score_no_re <- f_meas(valid_data_no_re, truth, estimate, na_rm = TRUE)
      specificity_no_re <- spec(valid_data_no_re, truth, estimate, na_rm = TRUE)
      fpr_no_re <- 1 - specificity_no_re$.estimate

      # -- With Random Effects
      recall_score_re <- recall(valid_data_re, truth, estimate, na_rm = TRUE)
      precision_score_re <- precision(valid_data_re, truth, estimate, na_rm = TRUE)
      f1_score_re <- f_meas(valid_data_re, truth, estimate, na_rm = TRUE)
      specificity_re <- spec(valid_data_re, truth, estimate, na_rm = TRUE)
      fpr_re <- 1 - specificity_re$.estimate

      # ==== Store Fold-Level Metrics ====
      all_results_strat[[outcome]][[model_type]]$fold_auc_no_re[i] <- auc_no_re
      all_results_strat[[outcome]][[model_type]]$fold_auc_re[i] <- auc_re

      all_results_strat[[outcome]][[model_type]]$recall_score_no_re[i] <- recall_score_no_re$.estimate
      all_results_strat[[outcome]][[model_type]]$recall_score_re[i]    <- recall_score_re$.estimate

      all_results_strat[[outcome]][[model_type]]$precision_score_no_re[i] <- precision_score_no_re$.estimate
      all_results_strat[[outcome]][[model_type]]$precision_score_re[i]    <- precision_score_re$.estimate

      all_results_strat[[outcome]][[model_type]]$f1_score_no_re[i] <- f1_score_no_re$.estimate
      all_results_strat[[outcome]][[model_type]]$f1_score_re[i]    <- f1_score_re$.estimate

      all_results_strat[[outcome]][[model_type]]$specificity_no_re[i] <- specificity_no_re$.estimate
      all_results_strat[[outcome]][[model_type]]$specificity_re[i]    <- specificity_re$.estimate

      all_results_strat[[outcome]][[model_type]]$fpr_no_re[i] <- fpr_no_re
      all_results_strat[[outcome]][[model_type]]$fpr_re[i]    <- fpr_re
    }
  }
}

# ==== Summarize Fold-Level CV Results ====
# Compute mean performance metrics across folds for each outcome/model type

mean_results_strat <- list()

for (outcome in names(all_results_strat)) {
  for (model_type in names(all_results_strat[[outcome]])) {

    results <- all_results_strat[[outcome]][[model_type]]

    mean_results_strat[[outcome]][[model_type]] <- list(
      mean_auc_no_re = mean(unlist(results$fold_auc_no_re), na.rm = TRUE),
      mean_auc_re    = mean(unlist(results$fold_auc_re), na.rm = TRUE),

      mean_recall_no_re    = mean(unlist(results$recall_score_no_re), na.rm = TRUE),
      mean_recall_re       = mean(unlist(results$recall_score_re), na.rm = TRUE),

      mean_precision_no_re = mean(unlist(results$precision_score_no_re), na.rm = TRUE),
      mean_precision_re    = mean(unlist(results$precision_score_re), na.rm = TRUE),

      mean_f1_no_re        = mean(unlist(results$f1_score_no_re), na.rm = TRUE),
      mean_f1_re           = mean(unlist(results$f1_score_re), na.rm = TRUE),

      mean_specificity_no_re = mean(unlist(results$specificity_no_re), na.rm = TRUE),
      mean_specificity_re    = mean(unlist(results$specificity_re), na.rm = TRUE),

      mean_fpr_no_re       = mean(unlist(results$fpr_no_re), na.rm = TRUE),
      mean_fpr_re          = mean(unlist(results$fpr_re), na.rm = TRUE)
    )
  }
}


# ==== Evaluate Trained Models on Holdout Set ====
# Purpose: Test final model (fold 5) on completely unseen users

holdout_results <- list()

for (outcome in c("Crash", "Fatigue_Dicho", "BrainFog_Dicho")) {
  for (model_type in c("all_vars_without_lag", "with_lag")) {
    message(glue::glue("Evaluating holdout performance for: {outcome} | {model_type}"))

    # Prepare holdout set: compute baseline and change features
    ho_base <- compute_baseline_vars(holdout_data)
    ho_processed <- compute_change_vars(ho_base)

    # Reuse lag_var logic
    lag_var <- case_when(
      outcome == "Crash" ~ "Crash_1d_lag",
      outcome == "Fatigue_Dicho" ~ "Fatigue_1d_lag_Dicho",
      outcome == "BrainFog_Dicho" ~ "BrainFog_1d_lag_Dicho"
    )

    # Preprocess holdout set using recipe from training
    rec_ho <- recipe(as.formula(paste(outcome, "~ .")), data = fold_data) %>%
      step_select(all_of(c(lag_var, names(variable_names_ml))), all_outcomes(), all_of("user_id_pk"), everything()) %>%
      step_naomit(all_predictors()) %>%
      step_normalize(all_of(c(lag_var, names(variable_names_ml)))) %>%
      prep(training = fold_data)

    ho_prepped <- bake(rec_ho, new_data = ho_processed)

    # Use final model from fold 5
    final_model <- trained_models[[outcome]][[model_type]][[5]]

    # Generate predictions (no RE for holdout)
    ho_preds <- predict(final_model, newdata = ho_prepped, type = "response", allow.new.levels = TRUE)

    # Calculate AUC
    auc_ho <- as.numeric(auc(roc(ho_prepped[[outcome]], ho_preds)))

    # Prepare data for yardstick metrics
    valid_ho <- tibble(
      truth = factor(ho_prepped[[outcome]], levels = c(1, 0)),
      estimate = factor(ifelse(ho_preds > 0.5, 1, 0), levels = c(1, 0))
    ) %>% drop_na(truth, estimate)

    # Compute metrics
    recall_ho      <- recall(valid_ho, truth, estimate, na_rm = TRUE)
    precision_ho   <- precision(valid_ho, truth, estimate, na_rm = TRUE)
    f1_ho          <- f_meas(valid_ho, truth, estimate, na_rm = TRUE)
    specificity_ho <- spec(valid_ho, truth, estimate, na_rm = TRUE)
    fpr_ho         <- 1 - specificity_ho$.estimate

    # Store results
    holdout_results[[outcome]][[model_type]] <- list(
      auc         = auc_ho,
      recall      = recall_ho$.estimate,
      precision   = precision_ho$.estimate,
      f1          = f1_ho$.estimate,
      specificity = specificity_ho$.estimate,
      fpr         = fpr_ho
    )
  }
}














