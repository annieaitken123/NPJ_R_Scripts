# ============================================================================
# Title: npj_003_MLM_Models.R
# Purpose: Final Multilevel Linear Mixed Models for Crash, Fatigue, and Brain Fog
# Author: Annie Aitken
# Description:
#   - Fits multilevel logistic regression (Crash) and linear mixed models (Fatigue, Brain Fog)
#   - Separates within-person (WP) and between-person (BP) biometric predictors
#   - Controls for covariates including age, gender, prior-day symptoms, observation date,
#     sensor modality, and data availability
#   - Extracts standardized coefficients and confidence intervals
#   - Produces faceted forest plots of results for manuscript figures
# Output:
#   - Model summaries (via tab_model)
#   - Forest plots for each outcome (ggplot2, saved as .png)
# Dependencies:
#   - lme4, sjPlot, effectsize, ggplot2, ggpubr, parameters, tidyverse
# Notes:
#   - Standardization is performed using `standardize_parameters(method = "refit")`
#   - Plots are saved to Dropbox for inclusion in manuscript
# ============================================================================

# Load Required Packages
library(lme4)
library(parameters)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(effectsize)

# Define variable name mapping for clarity in plots
variable_names <- c(
  "HRV.cm" = "HRV (BP)",
  "HRV.cwc" = "HRV (WP)",
  "HRV_CoV" = "HRV CoV (BP)",
  "HRV_7d_CoV.cwc" = "7 Day HRV CoV (WP)",
  "HR.cm" = "HR (BP)",
  "HR.cwc" = "HR (WP)",
  "HR_CoV" = "HR CoV (BP)",
  "HR_7d_CoV.cwc" = "7 Day HR CoV (WP)",
  "BreathRate.cm" = "BreathRate (BP)",
  "BreathRate.cwc" = "BreathRate (WP)",
  "BR_CoV" = "BreathRate CoV (BP)",
  "BR_7d_CoV.cwc" = "7 Day BreathRate CoV (WP)",
  "observation_date_pk" = "Observation Date",
  "age" = "Age",
  "gender3" = "Gender (NB)",
  "gender2" = "Gender (F)",
  "Crash_1d_lag" = "Crash Prior Day",
  "Fatigue_1d_lag" = "Fatigue Prior Day",
  "BrainFog_1d_lag" = "Brain Fog Prior Day",
  "source_device" = "Sensor Modality",
  "max_obs" = "Number of Obs"
)

# Set order for plotting
within_person_order <- rev(c("HR (WP)", "HRV (WP)", "BreathRate (WP)", "7 Day HR CoV (WP)", "7 Day HRV CoV (WP)", "7 Day BreathRate CoV (WP)"))
between_person_order <- rev(c("HR (BP)", "HRV (BP)", "BreathRate (BP)", "HR CoV (BP)", "HRV CoV (BP)", "BreathRate CoV (BP)"))
covariates_order <- rev(c("Observation Date", "Age", "Gender (F)", "Gender (NB)", "Crash Prior Day", "Fatigue Prior Day", "Brain Fog Prior Day", "Sensor Modality", "Number of Obs"))
desired_order <- c(within_person_order, between_person_order, covariates_order)

#---------------------------#
# Crash Model (Binary)
#---------------------------#

# Null model
log_reg_un <- glmer(Crash ~ (1 | user_id_pk),
                    data = crash_df_stats,
                    family = binomial(link = "logit"),
                    nAGQ = 0,
                    control = glmerControl(optimizer = "nloptwrap"))
summary(log_reg_un)
sjPlot::tab_model(log_reg_un, show.std = TRUE)

# Filter to users with >1 datapoint
crash_df_stats_2 <- crash_df_stats_2 %>%
  group_by(user_id_pk) %>%
  filter(n() > 1) %>%
  ungroup()

# Final model
log_reg <- glmer(Crash ~ HR.cwc + HRV.cwc + BreathRate.cwc +
                   HR_7d_CoV.cwc + HRV_7d_CoV.cwc + BR_7d_CoV.cwc +
                   HR.cm + HRV.cm + BreathRate.cm +
                   HR_CoV + HRV_CoV + BR_CoV +
                   observation_date_pk + max_obs + age + gender +
                   Crash_1d_lag + source_device +
                   (1 | user_id_pk),
                 data = crash_df_stats_2,
                 family = binomial(link = "logit"),
                 nAGQ = 0,
                 control = glmerControl(optimizer = "nloptwrap"))

summary(log_reg)
sjPlot::tab_model(log_reg, show.std = TRUE, pred.labels = variable_names)

# Standardized coefficients
std_coefs1 <- standardize_parameters(log_reg, method = "refit") %>% as.data.frame()[-1,]

plot_data1 <- std_coefs1 %>%
  mutate(Predictors = recode(Parameter, !!!variable_names),
         color = ifelse(CI_low < 0 & CI_high > 0, "black", "#FF00FF"),
         Predictors = factor(Predictors, levels = desired_order),
         Category = case_when(str_detect(Predictors, "BP") ~ "Between-Person (BP)",
                              str_detect(Predictors, "WP") ~ "Within-Person (WP)",
                              TRUE ~ "Covariates"),
         Category = factor(Category, levels = c("Within-Person (WP)", "Between-Person (BP)", "Covariates"))) %>%
  ungroup()

crash_forest <- ggplot(plot_data1, aes(x = Predictors, y = Std_Coefficient)) +
  geom_point(aes(color = color), size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  coord_flip() +
  scale_color_manual(values = c("#FF00FF" = "#FF00FF", "black" = "black"), labels = c("Significant", "NS")) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(title = "Crash", x = "Predictors", y = "Standardized Beta Coefficients", color = "CI passes through 0") +
  theme(text = element_text(family = "Times New Roman", size = 10), plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(face = "bold"), strip.background = element_rect(fill = "lightgrey", color = "black", size = 0.5))

#---------------------------#
# Fatigue Model (Continuous)
#---------------------------#

fatigue_mlm <- lmer(Fatigue ~ HR.cwc + HRV.cwc + BreathRate.cwc +
                      HR_7d_CoV.cwc + HRV_7d_CoV.cwc + BR_7d_CoV.cwc +
                      HR.cm + HRV.cm + BreathRate.cm +
                      HR_CoV + HRV_CoV + BR_CoV +
                      observation_date_pk + max_obs + age + gender +
                      Fatigue_1d_lag + source_device +
                      (1 | user_id_pk),
                    data = fatigue_df_stats_2)

summary(fatigue_mlm)
sjPlot::tab_model(fatigue_mlm, show.std = TRUE, pred.labels = variable_names)

std_coefs2 <- standardize_parameters(fatigue_mlm, method = "refit") %>% as.data.frame()[-1,]

plot_data2 <- std_coefs2 %>%
  mutate(Predictors = recode(Parameter, !!!variable_names),
         color = ifelse(CI_low < 0 & CI_high > 0, "black", "#FF00FF"),
         Predictors = factor(Predictors, levels = desired_order),
         Category = case_when(str_detect(Predictors, "BP") ~ "Between-Person (BP)",
                              str_detect(Predictors, "WP") ~ "Within-Person (WP)",
                              TRUE ~ "Covariates"),
         Category = factor(Category, levels = c("Within-Person (WP)", "Between-Person (BP)", "Covariates"))) %>%
  ungroup()

fatigue_forest <- ggplot(plot_data2, aes(x = Predictors, y = Std_Coefficient)) +
  geom_point(aes(color = color), size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  coord_flip() +
  scale_color_manual(values = c("#FF00FF" = "#FF00FF", "black" = "black"), labels = c("Significant", "NS")) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(title = "Fatigue", x = "Predictors", y = "Standardized Beta Coefficients", color = "CI passes through 0") +
  theme(text = element_text(family = "Times New Roman", size = 10), plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(face = "bold"), strip.background = element_rect(fill = "lightgrey", color = "black", size = 0.5))

#---------------------------#
# Brain Fog Model (Continuous)
#---------------------------#

BF_mlm <- lmer(Brain.Fog ~ HR.cwc + HRV.cwc + BreathRate.cwc +
                 HR_7d_CoV.cwc + HRV_7d_CoV.cwc + BR_7d_CoV.cwc +
                 HR.cm + HRV.cm + BreathRate.cm +
                 HR_CoV + HRV_CoV + BR_CoV +
                 observation_date_pk + max_obs + age + gender +
                 BrainFog_1d_lag + source_device +
                 (1 | user_id_pk),
               data = bf_df_stats_2)

summary(BF_mlm)
sjPlot::tab_model(BF_mlm, show.std = TRUE, pred.labels = variable_names)

std_coefs3 <- standardize_parameters(BF_mlm, method = "refit") %>% as.data.frame()[-1,]

plot_data3 <- std_coefs3 %>%
  mutate(Predictors = recode(Parameter, !!!variable_names),
         color = ifelse(CI_low < 0 & CI_high > 0, "black", "#FF00FF"),
         Predictors = factor(Predictors, levels = desired_order),
         Category = case_when(str_detect(Predictors, "BP") ~ "Between-Person (BP)",
                              str_detect(Predictors, "WP") ~ "Within-Person (WP)",
                              TRUE ~ "Covariates"),
         Category = factor(Category, levels = c("Within-Person (WP)", "Between-Person (BP)", "Covariates"))) %>%
  ungroup()

bf_forest <- ggplot(plot_data3, aes(x = Predictors, y = Std_Coefficient)) +
  geom_point(aes(color = color), size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  coord_flip() +
  scale_color_manual(values = c("#FF00FF" = "#FF00FF", "black" = "black"), labels = c("Significant", "NS")) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(title = "Brain Fog", x = "Predictors", y = "Standardized Beta Coefficients", color = "CI passes through 0") +
  theme(text = element_text(family = "Times New Roman", size = 10), plot.title = element_text(hjust = 0.5),
        strip.text.y = element_text(face = "bold"), strip.background = element_rect(fill = "lightgrey", color = "black", size = 0.5))

#---------------------------#
# Combine Forest Plots
#---------------------------#

arrange_fp <- ggarrange(crash_forest, fatigue_forest, bf_forest,
                        ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom",
                        labels = c("A", "B", "C"),
                        label.x = 0.02, label.y = 0.98,
                        label.font = list(size = 14, face = "bold"))

# Save final figure
fig_path <- "/Users/annieaitken/Dropbox/Consulting/Visible/Manuscript_Figures/mlm_forest_plots.png"
ggsave(fig_path, arrange_fp, width = 11, height = 9)
