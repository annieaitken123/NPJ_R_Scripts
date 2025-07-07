# ==========================================================
# Script Name: npj_002_SummaryTables.R
# Author: Annie Aitken
# Purpose: Generate per-user summary metrics for Crash, Fatigue, and Brain Fog
# Input: dataframe with cleaned and merged Visible data
# Output:
#   - Raw `summary_table` with per-user metrics
#   - Summary statistics table using `gtsummary::tbl_summary`
# Dependencies:
#   - tidyverse, lubridate, zoo, gtsummary
# Notes:
#   - Computes frequency and duration of symptom events (Crash, Fatigue, Brain Fog)
#   - Includes sequence length and days-between-event metrics
#   - Converts ordinal symptoms to binary indicators (e.g., Fatigue ≥ 2)
# ==========================================================

# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(gtsummary)

# Calculate dichotomous variables for elevated fatigue and brain fog
table_data <- crash_ml_dropna %>%
  mutate(
    elevated_fatigue = ifelse(Fatigue >= 2, 1, 0),
    elevated_brain_fog = ifelse(Brain.Fog >= 2, 1, 0)
  )

# Function to calculate longest sequence of 1s, ignoring NAs
calculate_sequential_length <- function(vec) {
  vec <- na.omit(as.numeric(vec))
  if (length(vec) == 0) return(0)
  rle_vec <- rle(vec)
  lengths <- rle_vec$lengths[rle_vec$values == 1]
  if (length(lengths) == 0) return(0)
  return(max(lengths))
}

# Function to calculate average days between events, ignoring NAs
calculate_avg_days_between_events <- function(dates) {
  dates <- na.omit(dates)
  if (length(dates) < 2) return(NA_real_)
  diffs <- diff(sort(dates))
  return(mean(diffs, na.rm = TRUE))
}

# Generate summary table
summary_table <- table_data %>%
  mutate(observation_date_pk = as.Date(observation_date_pk)) %>%
  group_by(user_id_pk) %>%
  mutate(
    date_diff = c(0, diff(observation_date_pk)),
    series_cluster_1d = cumsum(date_diff > 1) + 1
  ) %>%
  group_by(user_id_pk, series_cluster_1d) %>%
  mutate(
    sequence_num_1d = row_number(),
    seq_length_1d = max(sequence_num_1d)
  ) %>%
  ungroup() %>%
  group_by(user_id_pk) %>%
  summarise(
    days_with_data = n_distinct(observation_date_pk),
    total_biometric_observations = n(),
    avg_seq_length_1d = mean(seq_length_1d, na.rm = TRUE),
    total_crashes = sum(as.numeric(as.character(Crash)), na.rm = TRUE),
    total_elevated_fatigue = sum(elevated_fatigue, na.rm = TRUE),
    total_elevated_brain_fog = sum(elevated_brain_fog, na.rm = TRUE),
    longest_seq_crash = calculate_sequential_length(Crash),
    longest_seq_fatigue = calculate_sequential_length(elevated_fatigue),
    longest_seq_brain_fog = calculate_sequential_length(elevated_brain_fog),
    avg_days_between_crashes = calculate_avg_days_between_events(observation_date_pk[Crash == 1]),
    avg_days_between_fatigue = calculate_avg_days_between_events(observation_date_pk[elevated_fatigue == 1]),
    avg_days_between_brain_fog = calculate_avg_days_between_events(observation_date_pk[elevated_brain_fog == 1])
  ) %>%
  ungroup()

# Print raw per-user summary table
print(summary_table)

# Create formatted descriptive summary table
summary_table %>%
  select(-user_id_pk) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}–{max}]")
  )
