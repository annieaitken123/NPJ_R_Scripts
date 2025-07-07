# 001_DataPrep.R
# Purpose: Clean and prepare biometric + symptom data for Visible ML project
# Author: Annie Aitken
# Last updated: 2025-07-05

# ---- LIBRARIES ----
library(tidymodels)
library(tidyverse)
library(lubridate)
library(magrittr)
library(timetk)
library(forecast)
library(zoo)

# ---- DATA INPUT ----
# Load datasets
crash_ml <- read.csv("visible_dataset_crashPred.csv")[-1]
demographics <- read.csv("visible_demographics.csv")[-1]

# ---- INITIAL FILTERING ----
bf_df <- crash_ml %>% select(-Crash, -Fatigue)

user_summary <- bf_df %>%
  group_by(user_id_pk) %>%
  summarise(max_timept = max(timept), .groups = "drop")

# Criteria: users must have ≥14 observations
valid_users <- user_summary %>%
  filter(max_timept > 13) %>%
  pull(user_id_pk)

bf_df <- bf_df %>%
  filter(user_id_pk %in% valid_users)

# ---- VARIABLE CREATION ----
bf_df <- bf_df %>%
  mutate(
    gender = case_when(
      gender_id %in% c(1, 2) ~ as.character(gender_id),
      gender_id %in% c(3, 4, 5) ~ "3",
      TRUE ~ as.character(gender_id)
    ),
    source_device = ifelse(source_device == "polarVeritySense", 1, 0)
  ) %>%
  group_by(user_id_pk) %>%
  mutate(
    observation_date_pk = as.Date(observation_date_pk),
    date_diff = c(0, diff(observation_date_pk)),
    time_pt = row_number(),
    max_obs = max(timept),
    hr_na = is.na(HR),
    sum_hr_na_7d = rollsum(hr_na, 7, fill = NA, align = "right"),
    sum_hr_na_14d = rollsum(hr_na, 14, fill = NA, align = "right"),
    sum_br_na_7d = rollsum(is.na(BreathRate), 7, fill = NA, align = "right"),
    missed_day = ifelse(interval_days_new == "1", 0, 1),
    missed_days_7d = rollsum(missed_day, 7, fill = NA, align = "right"),
    missing_prop_14d = (14 - sum_hr_na_14d) / 14
  ) %>%
  ungroup()

# ---- DATA CLEANING & FEATURE ENGINEERING ----
bf_df_proc <- bf_df %>%
  rename(Brain.Fog = `Brain Fog`) %>%
  select(user_id_pk, observation_date_pk, Brain.Fog, HRV_samples_n,
         HRV, HR, BreathRate, gender, source_device,
         sum_hr_na_7d, sum_br_na_7d, max_obs, missed_days_7d, missing_prop_14d) %>%
  arrange(user_id_pk, observation_date_pk) %>%
  group_by(user_id_pk) %>%
  mutate(
    HR_sd = sd(HR, na.rm = TRUE),
    HRV_sd = sd(HRV, na.rm = TRUE),
    BR_sd = sd(BreathRate, na.rm = TRUE),

    # Conditional 7-day stats
    HR_7d_sd = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                      rollapply(HR, 7, sd, fill = NA, align = "right", na.rm = TRUE), NA),
    HRV_7d_sd = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                       rollapply(HRV, 7, sd, fill = NA, align = "right", na.rm = TRUE), NA),
    HR_7d_avg = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                       rollapply(HR, 7, mean, fill = NA, align = "right", na.rm = TRUE), NA),
    HRV_7d_avg = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                        rollapply(HRV, 7, mean, fill = NA, align = "right", na.rm = TRUE), NA),
    BR_7d_sd = ifelse(missed_days_7d <= 2 & sum_br_na_7d <= 2,
                      rollapply(BreathRate, 7, sd, fill = NA, align = "right", na.rm = TRUE), NA),
    BR_7d_avg = ifelse(missed_days_7d <= 2 & sum_br_na_7d <= 2,
                       rollapply(BreathRate, 7, mean, fill = NA, align = "right", na.rm = TRUE), NA),

    # Lag variables
    HR_1d_lag = lag(HR),
    HRV_1d_lag = lag(HRV),
    BrainFog_1d_lag = lag(Brain.Fog),

    # Coefficient of Variation
    HR_CoV = HR_sd / mean(HR, na.rm = TRUE),
    HRV_CoV = HRV_sd / mean(HRV, na.rm = TRUE),
    BR_CoV = BR_sd / mean(BreathRate, na.rm = TRUE),
    HR_CoV_7d = HR_7d_sd / HR_7d_avg,
    HRV_CoV_7d = HRV_7d_sd / HRV_7d_avg,
    BR_CoV_7d = BR_7d_sd / BR_7d_avg,

    # Dichotomize outcome
    BrainFog_Dicho = factor(ifelse(Brain.Fog >= 2, 1, ifelse(Brain.Fog <= 1, 0, NA))),
    BrainFog_Dicho_1d_lag = lag(BrainFog_Dicho),
    BrainFog_1st = ifelse(BrainFog_Dicho == 1 & lag(BrainFog_Dicho) == 0, 1, 0)
  ) %>%
  ungroup()

# ---- CENTERING ----
vars_to_center <- c("HRV", "HR", "BreathRate", "HR_7d_avg", "HRV_7d_avg", "BR_7d_avg")
bf_df_proc <- center_variables(bf_df_proc, vars_to_center)

bf_df_proc <- bf_df_proc %>%
  group_by(user_id_pk) %>%
  mutate(
    HRV_7d_sd.cwc = HRV_7d_sd - HRV_sd,
    HR_7d_sd.cwc = HR_7d_sd - HR_sd,
    BR_7d_sd.cwc = BR_7d_sd - BR_sd,
    HRV_7d_CoV.cwc = HRV_CoV_7d - HRV_CoV,
    HR_7d_CoV.cwc = HR_CoV_7d - HR_CoV,
    BR_7d_CoV.cwc = BR_CoV_7d - BR_CoV
  ) %>%
  ungroup()

# ---- DEMOGRAPHICS MERGE ----
bf_df_stats <- left_join(bf_df_proc, demographics, by = "user_id_pk") %>%
  mutate(
    gender = case_when(
      gender_id %in% c(1, 2) ~ as.character(gender_id),
      gender_id %in% c(3, 4, 5) ~ "3",
      TRUE ~ as.character(gender_id)
    ),
    source_device = replace_na(source_device, 0)
  ) %>%
  group_by(user_id_pk) %>%
  fill(gender, age, .direction = "updown") %>%
  ungroup()

# ---- ADHERENCE FILTERING (OPTIONAL) ----
# Optional: Filter to remove 14-day windows with >60% missing
bf_df_stats_filtered <- bf_df_stats %>%
  filter(missing_prop_14d > 0.60)

# ---- EXPORT CLEANED DATA ----
# saveRDS(bf_df_stats_filtered, "bf_df_stats_filtered.rds")
# write_csv(bf_df_stats_filtered, "visible_data_cleaned.csv")
