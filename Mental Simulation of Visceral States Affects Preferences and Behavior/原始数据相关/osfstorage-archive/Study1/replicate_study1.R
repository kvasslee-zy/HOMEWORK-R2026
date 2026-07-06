# ============================================================================
# Replication of Study 1a
# Steinmetz, J., Tausen, B. M., & Risen, J. L. (2018)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
# Personality and Social Psychology Bulletin, 44(3), 406-417.
# ============================================================================
# Study 1a: Does mentally simulating warmth (vs. cold) shift people's
# preferences for warming (vs. cooling) activities?
# ============================================================================

library(haven)
library(dplyr)

# --------------------------------------------------------------------------
# 1. Load data
# --------------------------------------------------------------------------
df <- read_spss("S1.sav")

cat("========================================\n")
cat("STUDY 1a REPLICATION\n")
cat("========================================\n\n")
cat("Sample size:", nrow(df), "participants\n")
cat("Condition 1 (simulate HOT / warmth):", sum(df[["condition"]] == 1, na.rm = TRUE), "\n")
cat("Condition 2 (simulate COLD):",         sum(df[["condition"]] == 2, na.rm = TRUE), "\n\n")

# Condition labels: 1 = "hot" (simulate warmth), 2 = "cold" (simulate cold)
# DV: warm_pref = mean of 5 temperature-related items (higher = prefer warming)
#   A3, A8     — warming-preference items (higher = prefer warm, used as-is)
#   A4, A6, A10 — cooling-preference items (higher = prefer cold, reverse-coded)

# --------------------------------------------------------------------------
# 2. Reliability (Cronbach's alpha)
# --------------------------------------------------------------------------
cat("--- 2. Reliability Analysis ---\n")
items <- data.frame(
  A4_rev  = df[["A4_rev"]],
  A6_rev  = df[["A6_rev"]],
  A10_rev = df[["A10_rev"]],
  A3      = df[["A3"]],
  A8      = df[["A8"]]
)

cor_mat <- cor(items, use = "pairwise.complete.obs")
cat("Correlation matrix of the 5 items:\n")
print(round(cor_mat, 3))

k        <- ncol(items)
item_var <- apply(items, 2, var, na.rm = TRUE)
total_var <- var(rowSums(items, na.rm = TRUE), na.rm = TRUE)
alpha    <- (k / (k - 1)) * (1 - sum(item_var) / total_var)
cat(sprintf("Cronbach's alpha = %.3f  (Paper reports .64)\n\n", alpha))

# --------------------------------------------------------------------------
# 3. Descriptive statistics
# --------------------------------------------------------------------------
cat("--- 3. Descriptive Statistics: warm_pref by Condition ---\n")
cond1 <- df[["warm_pref"]][df[["condition"]] == 1]
cond2 <- df[["warm_pref"]][df[["condition"]] == 2]

cat(sprintf("Hot  (simulate warmth): M = %.2f, SD = %.2f, n = %d\n",
            mean(cond1, na.rm = TRUE), sd(cond1, na.rm = TRUE), sum(!is.na(cond1))))
cat(sprintf("Cold (simulate cold):   M = %.2f, SD = %.2f, n = %d\n\n",
            mean(cond2, na.rm = TRUE), sd(cond2, na.rm = TRUE), sum(!is.na(cond2))))

# --------------------------------------------------------------------------
# 4. Independent-samples t-test
# --------------------------------------------------------------------------
cat("--- 4. Independent t-test: warm_pref by Condition ---\n")
ttest <- t.test(df[["warm_pref"]] ~ df[["condition"]], var.equal = TRUE)

# Format p-value
p_val <- ttest$p.value
p_str <- ifelse(p_val < 0.001, "p < .001", sprintf("p = %.3f", p_val))

cat(sprintf("t(%d) = %.2f, %s\n", ttest$parameter, ttest$statistic, p_str))
cat(sprintf("95%% CI of difference: [%.3f, %.3f]\n",
            ttest$conf.int[1], ttest$conf.int[2]))

# Cohen's d
n1 <- sum(!is.na(cond1))
n2 <- sum(!is.na(cond2))
pooled_sd <- sqrt(((n1 - 1) * var(cond1, na.rm = TRUE) + (n2 - 1) * var(cond2, na.rm = TRUE)) /
                    (n1 + n2 - 2))
d <- (mean(cond2, na.rm = TRUE) - mean(cond1, na.rm = TRUE)) / pooled_sd
cat(sprintf("Cohen's d = %.3f\n\n", d))

# --------------------------------------------------------------------------
# 5. Manipulation check: Q42.0 (How warm/cold do you feel right now?)
#    1 = very cold, 9 = very warm
# --------------------------------------------------------------------------
cat("--- 5. Manipulation Check: Q42.0 (feeling warm/cold) ---\n")
q42_1 <- df[["Q42.0"]][df[["condition"]] == 1]
q42_2 <- df[["Q42.0"]][df[["condition"]] == 2]
cat(sprintf("Hot condition:  M = %.2f, SD = %.2f\n", mean(q42_1, na.rm = TRUE), sd(q42_1, na.rm = TRUE)))
cat(sprintf("Cold condition: M = %.2f, SD = %.2f\n", mean(q42_2, na.rm = TRUE), sd(q42_2, na.rm = TRUE)))

ttest_mc <- t.test(df[["Q42.0"]] ~ df[["condition"]], var.equal = TRUE)
p_val_mc <- ttest_mc$p.value
p_str_mc <- ifelse(p_val_mc < 0.001, "p < .001", sprintf("p = %.3f", p_val_mc))
cat(sprintf("t(%d) = %.2f, %s\n", ttest_mc$parameter, ttest_mc$statistic, p_str_mc))

d_mc <- (mean(q42_2, na.rm = TRUE) - mean(q42_1, na.rm = TRUE)) /
        sqrt((var(q42_1, na.rm = TRUE) + var(q42_2, na.rm = TRUE)) / 2)
cat(sprintf("Cohen's d = %.3f\n\n", d_mc))

# --------------------------------------------------------------------------
# 6. Regression: warm_pref ~ feeling (Q42.0)
# --------------------------------------------------------------------------
cat("--- 6. Regression: warm_pref ~ Q42.0 (feeling warm/cold) ---\n")
reg1 <- lm(warm_pref ~ Q42.0, data = df)
print(summary(reg1))

# --------------------------------------------------------------------------
# 7. Regression: warm_pref ~ condition + Q42.0 (feeling)
# --------------------------------------------------------------------------
cat("\n--- 7. Regression: warm_pref ~ condition + Q42.0 ---\n")
reg2 <- lm(warm_pref ~ condition + Q42.0, data = df)
print(summary(reg2))

# --------------------------------------------------------------------------
# 8. Summary of results
# --------------------------------------------------------------------------
cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat(sprintf("Cronbach's alpha:         %.3f (paper: .64)\n", alpha))
cat(sprintf("t-test (warm_pref):       t(%.0f) = %.2f, %s, d = %.3f\n",
            ttest$parameter, ttest$statistic, p_str, d))
cat(sprintf("Manipulation check:       t(%.0f) = %.2f, %s, d = %.3f\n",
            ttest_mc$parameter, ttest_mc$statistic, p_str_mc, d_mc))
cat(sprintf("Regression warm_pref ~ feel:  beta = %.3f, %s\n",
            coef(reg1)["Q42.0"],
            ifelse(summary(reg1)$coef["Q42.0", "Pr(>|t|)"] < 0.001,
                   "p < .001",
                   sprintf("p = %.3f", summary(reg1)$coef["Q42.0", "Pr(>|t|)"]))))
cat("========================================\n")
