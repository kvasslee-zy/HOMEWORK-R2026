# ================================================================
# Analysis Script: Study 1 — Steinmetz, Tausen & Risen (2018)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
# Personality and Social Psychology Bulletin, 44(3), 406-417
# ================================================================
# Study 1 is divided into:
#   - Study 1a: Single-factor between-subjects (Cold vs Warm simulation)
#   - Study 1b: 2x2 between-subjects (Temperature: Cold/Warm × Method: Simulating vs Priming)
# ================================================================

# ---- 0. Load required packages ----
library(haven)      # Read SPSS .sav files
library(tidyverse)  # Data manipulation and plotting
library(psych)      # Describe, alpha reliability
library(effsize)    # Cohen's d
library(car)        # Levene's test, ANOVA
library(emmeans)    # Post-hoc comparisons

# ---- 1. Read data ----
# Set your working directory to the folder containing this script
# or use the full path:
data_path <- file.path(
  "osfstorage-archive",
  "Study1.sav"
)

df <- read_sav(data_path)

# ---- 2. First look at the data structure ----
cat("============== VARIABLE NAMES ==============\n")
print(names(df))

cat("\n============== VARIABLE LABELS ==============\n")
for (v in names(df)) {
  lbl <- attr(df[[v]], "label")
  if (!is.null(lbl)) {
    cat(sprintf("  %-15s: %s\n", v, lbl))
  }
}

cat("\n============== VALUE LABELS ==============\n")
for (v in names(df)) {
  if (is.labelled(df[[v]])) {
    cat(sprintf("  %-15s:\n", v))
    print(attr(df[[v]], "labels"))
  }
}

cat("\n============== DATA SAMPLE (first 6 rows) ==============\n")
print(head(df))

cat("\n============== DATA SUMMARY ==============\n")
print(summary(df))

# ---- 3. Identify study design variables ----
# Based on the paper:
# Study 1a: CONDITION (Cold vs Warm) — single factor between-subjects
# Study 1b: Temperature (Cold/Warm) × Method (Simulating/Priming) — 2x2

# You need to MAP the actual variable names from the output above
# Common SPSS naming patterns:
# - CONDITION or similar → experimental condition
# - Items like pref1, pref2, ... or similar → preference ratings
# - vividness or VIVID → manipulation check
# - age, gender → demographics

# 🔍 After you run section 2, use this section to define your variables:
# (Edit these names to match your actual data)

# EXAMPLE - adjust after checking names(df):
# condition_var <- "CONDITION"   # replace with actual name
# pref_items <- c("pref1", "pref2", "pref3", "pref4", "pref5")  # replace
# vividness_var <- "VIVIDNESS"
# age_var <- "AGE"
# gender_var <- "GENDER"

# If you need to find the preference items, look for 5 items about
# temperature-related preferences (cold vs warm activities)
# that form a scale with Cronbach's alpha = .64

# ---- 4. Split Study 1a and 1b ----
# NOTE: Both sub-studies are stored in the same .sav file.
# You may need a variable that indicates which sub-study each row belongs to.
# Check if there's a variable like "STUDY", "SUBSAMPLE", or similar.

# If there's a variable distinguishing 1a vs 1b:
# df_1a <- df %>% filter(STUDY == 1)
# df_1b <- df %>% filter(STUDY == 2)

# Otherwise, the first ~119 rows (in-person students) are Study 1a,
# and the next ~218 rows (MTurk) are Study 1b.
# df_1a <- df[1:119, ]
# df_1b <- df[120:nrow(df), ]

# ---- 5. Study 1a Analysis ----
# Design: Cold vs Warm simulation → preference for warming activities

cat("\n============== STUDY 1a ANALYSIS ==============\n")

# 5a. Create composite score (average of preference items)
# df_1a <- df_1a %>%
#   mutate(
#     warmth_pref = rowMeans(select(., all_of(pref_items)), na.rm = TRUE)
#   )

# 5b. Reliability check
# cat("\nCronbach's alpha for preference scale:\n")
# alpha(df_1a %>% select(all_of(pref_items)))

# 5c. Descriptive statistics by condition
# desc <- df_1a %>%
#   group_by(CONDITION) %>%
#   summarise(
#     n = n(),
#     mean_pref = mean(warmth_pref, na.rm = TRUE),
#     sd_pref = sd(warmth_pref, na.rm = TRUE),
#     se_pref = sd_pref / sqrt(n)
#   )
# print(desc)

# 5d. Visualization
# ggplot(df_1a, aes(x = CONDITION, y = warmth_pref, fill = CONDITION)) +
#   geom_boxplot(alpha = 0.6) +
#   geom_jitter(width = 0.15, alpha = 0.4) +
#   stat_summary(fun = "mean", geom = "point", shape = 18, 
#                size = 4, color = "red") +
#   labs(title = "Study 1a: Preference by Simulation Condition",
#        y = "Preference for Warming Activities", x = "") +
#   theme_minimal()

# 5e. Independent samples t-test
# t_result <- t.test(warmth_pref ~ CONDITION, 
#                    data = df_1a, 
#                    var.equal = TRUE,
#                    alternative = "two.sided")
# print(t_result)

# 5f. Effect size (Cohen's d)
# d_result <- cohen.d(df_1a$warmth_pref, df_1a$CONDITION)
# print(d_result)

# ---- 6. Study 1b Analysis ----
# Design: 2x2 between-subjects
#   Factor 1: Temperature (Cold / Warm)
#   Factor 2: Method (Simulating / Priming)
# Dependent variable: preference for warming activities

cat("\n============== STUDY 1b ANALYSIS ==============\n")

# 6a. Create composite score
# df_1b <- df_1b %>%
#   mutate(
#     warmth_pref = rowMeans(select(., all_of(pref_items)), na.rm = TRUE)
#   )

# 6b. Descriptive
# desc_1b <- df_1b %>%
#   group_by(TEMPERATURE, METHOD) %>%
#   summarise(
#     n = n(),
#     mean_pref = mean(warmth_pref, na.rm = TRUE),
#     sd_pref = sd(warmth_pref, na.rm = TRUE)
#   )
# print(desc_1b)

# 6c. 2x2 ANOVA
# aov_1b <- aov(warmth_pref ~ TEMPERATURE * METHOD, data = df_1b)
# cat("\nANOVA results:\n")
# print(summary(aov_1b))

# 6d. Check assumptions
# Levene's test for homogeneity of variance
# leveneTest(warmth_pref ~ TEMPERATURE * METHOD, data = df_1b)

# 6e. Post-hoc comparisons (if interaction is significant)
# emm <- emmeans(aov_1b, ~ TEMPERATURE | METHOD)
# pairs(emm)

# ---- 7. Save results ----
# sink("Study1_results.txt")
# ... your analysis output ...
# sink()

cat("\n====================================\n")
cat("Script complete! Edit the variable names above after running Section 2.\n")
cat("====================================\n")
