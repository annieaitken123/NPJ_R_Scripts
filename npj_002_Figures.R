# ------------------------------------------------------------------------------
# SCRIPT: npj_02_Figures.R
# PURPOSE: Create summary plots, tables, and QC visualizations
#          Includes: biometrics summary, outcome distributions, CV folds, device agreement
# ------------------------------------------------------------------------------

# Load Libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(kableExtra)
library(ggpubr)
library(reshape2)
library(ggplot2)

# ------------------------------------------------------------------------------
# STEP 1: SUMMARY TABLE OF BIOMETRICS
# ------------------------------------------------------------------------------

# Rename BreathRate to RR
crash_ml_dropna <- crash_ml_dropna %>% rename("RR" = "BreathRate")

# Select and summarize numeric variables
numeric_vars <- crash_ml_dropna %>%
  select(HR, HRV, RR) %>%
  filter(HR > 0 & HRV > 0) %>%
  names()

summary_stats <- crash_ml_dropna %>%
  filter(HR > 0 & HRV > 0) %>%
  summarise(across(all_of(numeric_vars), list(Mean = mean, SD = sd, Min = min, Max = max), na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Metric"), names_sep = "_") %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  mutate(Range = paste0(Min, " - ", Max)) %>%
  select(Variable, Mean, SD, Range)

summary_stats %>%
  kable(format = "html", digits = 2, col.names = c("Variable", "Mean", "SD", "Range")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

# ------------------------------------------------------------------------------
# STEP 2: DENSITY PLOTS & DOTPLOTS FOR HR, HRV, RR
# ------------------------------------------------------------------------------

long_data <- crash_ml_dropna %>%
  filter(HR > 0 & HRV > 0) %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "Variable", values_to = "Value")

density_plot <- ggplot(long_data, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free", ncol = 1) +
  labs(title = "Density Plots of Biometrics", x = "", y = "Density") +
  theme_minimal(base_family = "Times New Roman") +
  scale_fill_manual(values = c("#B2DFDB", "#5AB3D6", "#0288D1")) +
  theme(legend.position = "none")

dotplot <- ggplot(sample_frac(long_data, 0.1), aes(x = Variable, y = Value, color = Value)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.3)) +
  scale_color_gradient(low = "#B2DFDB", high = "#0288D1") +
  labs(title = "Scatter Plot of Biometrics", x = "", y = "Value") +
  theme_minimal(base_family = "Times New Roman") +
  theme(legend.position = "right")

# ------------------------------------------------------------------------------
# STEP 3: CORRELATION MATRIX FOR BIOMETRICS
# ------------------------------------------------------------------------------

cor_matrix <- crash_ml_dropna %>% select(RR, HR, HRV) %>% cor(use = "complete.obs")
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_melted <- melt(cor_matrix)

biometric_corrs <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0098a9", high = "#de425b", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Half-Correlation Matrix for Biometrics", x = "", y = "") +
  theme_minimal(base_family = "Times New Roman") +
  coord_fixed()

# ------------------------------------------------------------------------------
# STEP 4: OUTCOME DISTRIBUTION BARPLOT
# ------------------------------------------------------------------------------

df_long <- data_splits %>%
  select(Crash, Fatigue_Dicho, BrainFog_Dicho) %>%
  mutate(Crash = ifelse(Crash == 1, "Yes", "No"),
         Fatigue = case_when(Fatigue_Dicho == 1 ~ "Yes", TRUE ~ "No"),
         BrainFog = case_when(BrainFog_Dicho == 1 ~ "Yes", TRUE ~ "No")) %>%
  pivot_longer(cols = c(Crash, Fatigue, BrainFog), names_to = "Outcome", values_to = "Response")

df_summary <- df_long %>%
  group_by(Outcome, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Outcome) %>%
  mutate(percent = count / sum(count) * 100)

combined_plot <- ggplot(df_summary, aes(x = Outcome, y = percent, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("No" = "#83a3a0", "Yes" = "#d68084")) +
  labs(title = "Distribution of Outcome Responses", y = "Percentage (%)", fill = "Response") +
  theme_minimal(base_family = "Times New Roman")

# ------------------------------------------------------------------------------
# STEP 5: STRATIFIED CV ILLUSTRATION
# ------------------------------------------------------------------------------

set.seed(123)
df_cv <- data.frame(user_id = rep(LETTERS[1:3], each = 150)) %>%
  group_by(user_id) %>%
  mutate(fold = cut_number(row_number(), 5, labels = FALSE)) %>%
  group_by(user_id, fold) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  mutate(split = case_when(
    fold == 5 & row_num <= 30 & row_num > 25 ~ "test",
    fold == 5 & row_num > 30 ~ "excluded",
    fold == 4 & row_num <= 25 & row_num > 20 ~ "test",
    fold == 4 & row_num > 25 ~ "excluded",
    fold == 3 & row_num <= 20 & row_num > 15 ~ "test",
    fold == 3 & row_num > 20 ~ "excluded",
    fold == 2 & row_num <= 15 & row_num > 10 ~ "test",
    fold == 2 & row_num > 15 ~ "excluded",
    fold == 1 & row_num <= 10 & row_num > 5 ~ "test",
    fold == 1 & row_num > 10 ~ "excluded",
    TRUE ~ "train"
  ))

strat_fig <- ggplot(df_cv, aes(x = row_num, y = as.factor(fold), fill = split)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("test" = "#E69F00", "train" = "#56B4E9", "excluded" = "grey")) +
  facet_wrap(~user_id) +
  labs(title = "Dataset Stratification for CV", x = "Observation", y = "Fold", fill = "Set") +
  theme_classic(base_family = "Times New Roman")

# ------------------------------------------------------------------------------
# STEP 6: DEVICE AGREEMENT (BLAND-ALTMAN)
# ------------------------------------------------------------------------------

df_ba <- crash_df_proc %>%
  group_by(user_id_pk, source_device) %>%
  mutate(source_device = recode(source_device, `1` = "Polar", `2` = "Phone")) %>%
  summarise(hr_mean = mean(HR, na.rm = TRUE), hrv_mean = mean(HRV, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = source_device, values_from = c(hr_mean, hrv_mean), names_glue = "{.value}_{source_device}") %>%
  mutate(hr_diff = hr_mean_Polar - hr_mean_Phone,
         hrv_diff = hrv_mean_Polar - hrv_mean_Phone,
         hr_avg = (hr_mean_Polar + hr_mean_Phone) / 2,
         hrv_avg = (hrv_mean_Polar + hrv_mean_Phone) / 2)

BA_HR <- ggplot(df_ba, aes(x = hr_avg, y = hr_diff)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = mean(df_ba$hr_diff, na.rm = TRUE), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean(df_ba$hr_diff, na.rm = TRUE) + 1.96 * sd(df_ba$hr_diff, na.rm = TRUE), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(df_ba$hr_diff, na.rm = TRUE) - 1.96 * sd(df_ba$hr_diff, na.rm = TRUE), color = "red", linetype = "dashed") +
  labs(x = "Mean HR Between Devices", y = "Difference (Phone - Polar)") +
  theme_minimal()

BA_HRV <- ggplot(df_ba, aes(x = hrv_avg, y = hrv_diff)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = mean(df_ba$hrv_diff, na.rm = TRUE), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean(df_ba$hrv_diff, na.rm = TRUE) + 1.96 * sd(df_ba$hrv_diff, na.rm = TRUE), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(df_ba$hrv_diff, na.rm = TRUE) - 1.96 * sd(df_ba$hrv_diff, na.rm = TRUE), color = "red", linetype = "dashed") +
  labs(x = "Mean HRV Between Devices", y = "Difference (Phone - Polar)") +
  theme_minimal()

# ------------------------------------------------------------------------------
# STEP 7: COMBINE & EXPORT
# ------------------------------------------------------------------------------

fig1_arranged <- ggarrange(
  dotplot, combined_plot, biometric_corrs, NULL, BA_HR, BA_HRV,
  ncol = 2, nrow = 4,
  labels = c("a", "b", "c", "", "d", ""),
  label.x = 0.02, label.y = 1,
  label.font = list(size = 14, face = "bold")
)

ggsave("Figure1.png", fig1_arranged, width = 8, height = 10)
ggsave("Figure6.png", strat_fig, dpi = 300, width = 10, height = 6)
