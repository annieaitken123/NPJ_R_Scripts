# LIBRARIES ----

library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(forecast)
library(magrittr)
library(zoo)

# DATA ----

crash_ml <- read.csv("visible_dataset_crashPred.csv")[-1]
demographics <- read.csv("/Users/annieaitken/Dropbox/Consulting/Visible/Visible_Crash_Prediction/visible_demographics.csv")[-1]

# FILTERED BASE DATA ----

crash_df <- crash_ml %>%
  select(-Fatigue, -`Brain Fog`)

user_summary <- crash_df %>%
  group_by(user_id_pk) %>%
  summarise(max_timept = max(timept), .groups = "drop")

valid_users <- user_summary %>%
  filter(max_timept > 13) %>%
  pull(user_id_pk)

crash_df <- crash_df %>%
  filter(user_id_pk %in% valid_users) %>%
  ungroup()

# VARIABLE CREATION ----

crash_df <- crash_df %>%
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
    observation_date_pk = as.Date(observation_date_pk, "%Y-%m-%d"),
    date_diff = c(0, diff(observation_date_pk)),
    max_crashes = sum(Crash, na.rm = TRUE),
    time_pt = row_number(),
    max_obs = max(timept, na.rm = TRUE),
    hr_na = is.na(HR),
    sum_hr_na_7d = rollsum(hr_na, k = 7, fill = NA, align = "right"),
    sum_hr_na_14d = rollsum(hr_na, k = 14, fill = NA, align = "right"),
    sum_br_na_7d = rollsum(is.na(BreathRate), k = 7, fill = NA, align = "right"),
    missed_day = ifelse(interval_days_new == "1", 0, 1),
    missed_days_7d = rollsum(missed_day, k = 7, fill = NA, align = "right"),
    missing_prop_14d = (14 - sum_hr_na_14d) / 14
  ) %>%
  ungroup()

# FINAL DATAFRAME FOR MODELING ----

crash_df_proc <- crash_df %>%
  mutate(source_device = replace_na(source_device, 2)) %>%
  select(
    user_id_pk, observation_date_pk, Crash, HRV_samples_n,
    HRV, HR, BreathRate,
    gender, source_device, sum_hr_na_7d, sum_br_na_7d, max_obs,
    missed_days_7d, missing_prop_14d
  ) %>%
  arrange(user_id_pk, observation_date_pk) %>%
  group_by(user_id_pk) %>%
  mutate(
    HR_sd = sd(HR, na.rm = TRUE),
    HRV_sd = sd(HRV, na.rm = TRUE),
    HR_7d_sd = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                      rollapply(HR, width = 7, FUN = sd, fill = NA, align = "right", na.rm = TRUE), NA),
    HRV_7d_sd = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                       rollapply(HRV, width = 7, FUN = sd, fill = NA, align = "right", na.rm = TRUE), NA),
    HR_7d_avg = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                       rollapply(HR, width = 7, FUN = mean, fill = NA, align = "right", na.rm = TRUE), NA),
    HRV_7d_avg = ifelse(missed_days_7d <= 2 & sum_hr_na_7d <= 2,
                        rollapply(HRV, width = 7, FUN = mean, fill = NA, align = "right", na.rm = TRUE), NA),
    BR_sd = sd(BreathRate, na.rm = TRUE),
    BR_7d_sd = ifelse(missed_days_7d <= 2 & sum_br_na_7d <= 2,
                      rollapply(BreathRate, width = 7, FUN = sd, fill = NA, align = "right", na.rm = TRUE), NA),
    BR_7d_avg = ifelse(missed_days_7d <= 2 & sum_br_na_7d <= 2,
                       rollapply(BreathRate, width = 7, FUN = mean, fill = NA, align = "right", na.rm = TRUE), NA),
    HR_CoV = HR_sd / mean(HR, na.rm = TRUE),
    HRV_CoV = HRV_sd / mean(HRV, na.rm = TRUE),
    BR_CoV = BR_sd / mean(BreathRate, na.rm = TRUE),
    HR_CoV_7d = HR_7d_sd / HR_7d_avg,
    HRV_CoV_7d = HRV_7d_sd / HRV_7d_avg,
    BR_CoV_7d = BR_7d_sd / BR_7d_avg,
    Crash_1d_lag = lag(Crash),
    Crash_1st = ifelse(Crash == 1 & lag(Crash) == 0, 1,
                       ifelse(Crash == 0, 0, NA))
  ) %>%
  ungroup()

# CWC CENTERING ----

vars_cwc <- c("HRV", "HR", "BreathRate", "HR_7d_avg", "HRV_7d_avg", "BR_7d_avg")
crash_df_proc <- center_variables(crash_df_proc, vars_cwc)

crash_df_proc <- crash_df_proc %>%
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

# MERGE + CLEANING ----

crash_df_stats <- merge(crash_df_proc, demographics, by = "user_id_pk", all.x = TRUE)

crash_df_stats <- crash_df_stats %>%
  mutate(
    gender = case_when(
      gender_id %in% c(1, 2) ~ as.character(gender_id),
      gender_id %in% c(3, 4, 5) ~ "3",
      TRUE ~ as.character(gender_id)
    )
  ) %>%
  group_by(user_id_pk) %>%
  fill(gender, age, .direction = "updown") %>%
  ungroup() %>%
  mutate(
    source_device = replace_na(source_device, 0),
    Crash = factor(ifelse(Crash %in% c(0, 1), Crash, NA)),
    Crash_1d_lag = factor(ifelse(Crash_1d_lag %in% c(0, 1), Crash_1d_lag, NA))
  )

# FINAL FILTER FOR ADHERENCE ----

crash_df_stats_2 <- crash_df_stats %>%
  filter(missing_prop_14d > 0.60) %>%
  ungroup()
