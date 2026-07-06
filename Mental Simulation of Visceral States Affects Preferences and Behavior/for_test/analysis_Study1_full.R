# ====================================================================
# Full Analysis: Steinmetz, Tausen & Risen (2018) — Study 1 (a & b)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
# PSPB, 44(3), 406-417
#
# Usage: 
#   1. Open this file in RStudio
#   2. Set your working directory to the folder containing this script
#      Session -> Set Working Directory -> To Source File Location
#   3. Run the ENTIRE script
# ====================================================================

# ---- 0. Install & load packages ----
required <- c("haven", "tidyverse", "psych", "effsize", "car", "emmeans", "ggplot2", "dplyr", "tidyr")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- 1. Read data ----
df <- read_sav("osfstorage-archive/Study1.sav")

# ---- 2. ============================================================
#     DATA EXPLORATION — Run this first to identify variable names
#     ============================================================ ---

cat(rep("=", 70), sep = "")
cat("\n\n                    DATA EXPLORATION  >>>  CHECK OUTPUT BELOW\n\n")
cat(rep("=", 70), sep = "")

cat("\n\n========== ALL VARIABLE NAMES ==========\n")
print(data.frame(
  Index = seq_along(names(df)),
  Name = names(df)
), row.names = FALSE)

cat("\n\n========== VARIABLE LABELS (from SPSS) ==========\n")
for (v in names(df)) {
  lbl <- attr(df[[v]], "label")
  if (!is.null(lbl)) cat(sprintf("  %-20s : %s\n", v, lbl))
}

cat("\n\n========== VALUE LABELS (from SPSS) ==========\n")
for (v in names(df)) {
  if (is.labelled(df[[v]])) {
    cat(sprintf("\n  >>> %s:\n", v))
    print(attr(df[[v]], "labels"))
  }
}

cat("\n\n========== FIRST 10 ROWS ==========\n")
print(df[1:10, ])

cat("\n\n========== DATA TYPES ==========\n")
print(sapply(df, class))

cat("\n\n========== SUMMARY STATISTICS ==========\n")
print(summary(df))

# ---- 3. ============================================================
#     VARIABLE MAPPING — EDIT THESE AFTER SEEING THE OUTPUT ABOVE
#     ============================================================ ---

# 🔍 IMPORTANT: Based on the paper's design, typical SPSS variable names are:
#   - Condition variable: something like "Condition", "COND", or "group"
#   - Preference items: 5 items for warmth preference (alpha = .64)
#   - Filler items: 5 items to mask purpose
#   - Manipulation check: vividness rating
#   - Demographics: age, gender

# ✏️ =============================================================
# ✏️ PLEASE EDIT THE FOLLOWING VARIABLE NAMES to match your data:
# ================================================================

# --- EDIT THESE ---
condition_var <- "CONDITION"        # The condition variable name
pref_items_warm <- c("Pref1", "Pref2", "Pref3", "Pref4", "Pref5")  # 5 warmth items
vividness_var <- "Vividness"         # Manipulation check
age_var <- "Age"
gender_var <- "Gender"

# After exploring the data:
# 1. Look for 5 items about temperature-related preferences (cold vs warm)
# 2. Check the condition variable's value labels
# 3. Update the above variable names
# 4. Re-run from here

# ---- 4. Identify sub-studies ----
# Study 1a: N ≈ 119 (in-person students), single factor Cold vs Warm
# Study 1b: N ≈ 218 (MTurk), 2×2 Temperature × Method

# If there's a STUDY variable:
# df_1a <- df %>% filter(STUDY == "1a")
# df_1b <- df %>% filter(STUDY == "1b")

# Otherwise, split by row position (1a = first ~119, 1b = rest)
# df_1a <- df[1:119, ]
# df_1b <- df[120:nrow(df), ]

# ⚠️ PLEASE CHECK THE ACTUAL SPLIT after looking at the data

# ---- 5. Create composite scores ----
# df <- df %>%
#   mutate(
#     warmth_pref = rowMeans(select(., all_of(pref_items_warm)), na.rm = TRUE)
#   )

# ================================================================
# ================================================================
# 

# ---- 2b. 如果上面的自动识别有问题，再试试手动探索 ----
cat("\n\n========== 手动探索指引 ==========\n")
cat("请在 RStudio 中运行以下代码来检查每个变量的值分布：\n\n")
cat("# 查看每个变量的唯一值\n")
cat("for (v in names(df)) {\n")
cat("  cat(v, ':', paste(unique(df[[v]]), collapse=', '), '\\n')\n")
cat("}\n\n")
cat("# 查看条件变量\n")
cat("table(df$", names(df)[1], ")\n", sep = "")
cat("table(df$", names(df)[2], ")\n", sep = "")
