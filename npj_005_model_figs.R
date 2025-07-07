# ==== Summary and Visualization of Model Performance ====
# --------------------------------------------------------
# Title: npj_005_model_figs
# Purpose:This script combines cross-validation (CV) and holdout set results,
# computes mean/min/max for each metric, and generates visual comparisons.
# Outputs: CV vs Holdout comparison plots (AUC, Recall, Precision, etc.)
# --------------------------------------------------------

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# ==== Set Up Metric Labels ====
metric_label_map <- c(
  "recall_score" = "RECALL",
  "precision_score" = "PRECISION",
  "f1_score" = "F1 SCORE",
  "specificity" = "SPECIFICITY",
  "fpr" = "FPR"
)

# ==== Prepare Cross-Validation Metrics ====
cv_long <- list()

for (outcome in names(all_results_strat)) {
  for (model_type in c("all_vars_without_lag", "with_lag")) {
    for (metric_base in names(metric_label_map)) {
      for (type in c("no_re", "re")) {
        metric_key <- paste0(metric_base, "_", type)
        values <- all_results_strat[[outcome]][[model_type]][[metric_key]]

        if (is.null(values)) next
        values <- na.omit(unlist(values))
        if (length(values) == 0) next

        cv_long[[length(cv_long) + 1]] <- data.frame(
          Outcome = gsub("_Dicho", "", outcome),
          Model = ifelse(model_type == "all_vars_without_lag", "Biometrics Only", "Biometrics + Lagged Outcome"),
          Metric = metric_label_map[[metric_base]],
          RE = toupper(type),
          Mean = mean(values),
          Min = min(values),
          Max = max(values),
          Source = "CV",
          Alpha = 1.0
        )
      }
    }
  }
}

cv_df <- bind_rows(cv_long)

# ==== Prepare Holdout Set Metrics ====
metric_labels_ho <- c(
  "recall" = "RECALL",
  "precision" = "PRECISION",
  "f1" = "F1 SCORE",
  "specificity" = "SPECIFICITY",
  "fpr" = "FPR"
)

ho_long <- list()

for (outcome in names(ho_results)) {
  for (model_type in c("all_vars_without_lag", "with_lag")) {
    for (metric_base in names(metric_labels_ho)) {
      value <- ho_results[[outcome]][[model_type]][[metric_base]]
      if (is.null(value)) next

      ho_long[[length(ho_long) + 1]] <- data.frame(
        Outcome = gsub("_Dicho", "", outcome),
        Model = ifelse(model_type == "all_vars_without_lag", "Biometrics Only", "Biometrics + Lagged Outcome"),
        Metric = metric_labels_ho[[metric_base]],
        RE = NA,
        Mean = value,
        Min = value,
        Max = value,
        Source = "Holdout",
        Alpha = 0.5
      )
    }
  }
}

ho_df <- bind_rows(ho_long)

# ==== Clean and Prepare for Plotting ====
# Set consistent outcome order
outcome_levels <- c("Crash", "Fatigue", "BrainFog")
cv_df$Outcome <- factor(cv_df$Outcome, levels = outcome_levels)
ho_df$Outcome <- factor(ho_df$Outcome, levels = outcome_levels)

# Set metric order
metric_order <- c("RECALL", "PRECISION", "F1 SCORE", "SPECIFICITY", "FPR")
cv_df$Metric <- factor(cv_df$Metric, levels = metric_order)
ho_df$Metric <- factor(ho_df$Metric, levels = metric_order)

# Define color scheme
custom_colors <- c(
  "Biometrics Only" = "magenta",
  "Biometrics + Lagged Outcome" = "blue"
)
