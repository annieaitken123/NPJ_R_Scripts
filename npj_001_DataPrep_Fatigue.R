# 001_DataPrep_Fatigue.R
# Purpose: Clean and prepare biometric + symptom data for Fatigue prediction (Visible ML project)
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
crash_ml <- read.csv("visible_dataset_crashPred.csv")[-1]
demographics <- read.csv("visible_demographics.csv")[-1]
device <- read.csv("visible_device.csv")

# ---- INITIAL FILTERING ----
fatigue_df <- crash_ml %>% select(-Crash, -`Brain Fog`)

user_summary <- fatigue_df %>%
  group_by(user_id_pk) %>%
  summarise(max_timept = max(timept), .groups = "drop")

valid_users <- user_summary %>%
  filter(max_timept > 13) %>%
  pull(user_id_pk)

fatigue_df <- fatigue_df %>%
  filter(user_id_pk %in% valid_users)

# ---- VARIABLE CREATION ----
fatigue_df <- fatigue_df %>%
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
fatigue_df_proc <- fatigue_df %>%
  select(user_id_pk, observation_date_pk, Fatigue, HRV_samples_n,
         HRV, HR, BreathRate, gender, source_device,
         sum_hr_na_7d, sum_br_na_7d, max_obs, missed_days_7d, missing_prop_14d) %>%
  arrange(user_id_pk, observation_date_pk) %>%
  group_by(user_id_pk) %>%
  mutate(
    HR_sd = sd(HR, na.rm = TRUE),
    HRV_sd = sd(HRV, na.rm = TRUE),
    BR_sd = sd(BreathRate, na.rm = TRUE),

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

    HR_1d_lag = lag(HR),
    HRV_1d_lag = lag(HRV),
    Fatigue_1d_lag = lag(Fatigue),

    HR_CoV = HR_sd / mean(HR, na.rm = TRUE),
    HRV_CoV = HRV_sd / mean(HRV, na.rm = TRUE),
    BR_CoV = BR_sd / mean(BreathRate, na.rm = TRUE),
    HR_CoV_7d = HR_7d_sd / HR_7d_avg,
    HRV_CoV_7d = HRV_7d_sd / HRV_7d_avg,
    BR_CoV_7d = BR_7d_sd / BR_7d_avg,

    Fatigue_Dicho = factor(ifelse(Fatigue >= 2, 1, ifelse(Fatigue <= 1, 0, NA))),
    Fatigue_Dicho_1d_lag = lag(Fatigue_Dicho),
    Fatigue_1st = ifelse(Fatigue_Dicho == 1 & lag(Fatigue_Dicho) == 0, 1, 0)
  ) %>%
  ungroup()

# ---- CENTERING ----
vars_to_center <- c("HRV", "HR", "BreathRate", "HR_7d_avg", "HRV_7d_avg", "BR_7d_avg")
fatigue_df_proc <- center_variables(fatigue_df_proc, vars_to_center)

fatigue_df_proc <- fatigue_df_proc %>%
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
fatigue_df_stats <- left_join(fatigue_df_proc, demographics, by = "user_id_pk") %>%
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
fatigue_df_stats_filtered <- fatigue_df_stats %>%
  filter(missing_prop_14d > 0.60)

# ---- EXPORT CLEANED DATA ----
# saveRDS(fatigue_df_stats_filtered, "fatigue_df_stats_filtered.rds")
# write_csv(fatigue_df_stats_filtered, "visible_data_fatigue_cleaned.csv")
