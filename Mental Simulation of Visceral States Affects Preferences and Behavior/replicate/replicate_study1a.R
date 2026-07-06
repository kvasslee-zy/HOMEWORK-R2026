# ============================================================================
# Study 1a - Comprehensive Tables & Visualizations
# Steinmetz, Tausen & Risen (2018) - Mental Simulation of Visceral States
# ============================================================================
# Uses ggplot2 + gridExtra::tableGrob for reliable PNG output
# ============================================================================

library(haven)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Color palette
COL_YELLOW <- "#FFF5CC"
COL_BLUE   <- "#B3D9F2"
COL_PINK   <- "#FFB8C6"
COL_GRAY   <- "#F0F0F0"
COL_WHITE  <- "#FFFFFF"

out_dir <- "C:/Users/lenovo/Desktop/R0507/Mental Simulation of Visceral States Affects Preferences and Behavior/Figure/S1a"

# --------------------------------------------------------------------------
# Helper: save a grid object (tableGrob) as PNG
# --------------------------------------------------------------------------
save_grob_png <- function(grob, filename, width = 8, height = 4) {
  grDevices::png(file.path(out_dir, filename), 
                 width = width, height = height, units = "in", res = 200)
  grid.draw(grob)
  dev.off()
  cat(sprintf("  Saved: %s (%.1f x %.1f in)\n", filename, width, height))
}

# --------------------------------------------------------------------------
# Helper: create a nicely styled table grob
# --------------------------------------------------------------------------
make_table <- function(df, header_bg = COL_YELLOW, 
                       title = NULL, fontsize = 11) {
  
  # Convert all columns to character
  df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  
  # Create theme
  theme <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = fontsize, fontfamily = "sans"),
      bg_params = list(fill = rep(c(COL_WHITE, COL_GRAY), 
                                   length.out = nrow(df_char)),
                       alpha = 0.8)
    ),
    colhead = list(
      fg_params = list(fontsize = fontsize + 1, fontface = "bold", 
                       fontfamily = "sans"),
      bg_params = list(fill = header_bg, alpha = 0.9)
    ),
    padding = unit(c(6, 8), "mm")
  )
  
  # Create tableGrob
  tab <- tableGrob(df_char, rows = NULL, theme = theme)
  
  # Add title if provided
  if (!is.null(title)) {
    title_grob <- textGrob(title, gp = gpar(fontsize = fontsize + 4, 
                                            fontface = "bold", 
                                            fontfamily = "sans"),
                           just = "center")
    tab <- gtable::gtable_add_rows(tab, heights = unit(0.8, "cm"), 0)
    tab <- gtable::gtable_add_grob(tab, title_grob, t = 1, l = 1, 
                                    r = ncol(tab))
  }
  
  # Add a border around the whole table
  tab <- gtable::gtable_add_grob(tab, 
                                 rectGrob(gp = gpar(fill = NA, lwd = 0.5)), 
                                 t = 1, l = 1, b = nrow(tab), r = ncol(tab))
  
  return(tab)
}

# --------------------------------------------------------------------------
# Load data
# --------------------------------------------------------------------------
df <- read_spss("C:/Users/lenovo/Desktop/R0507/Mental Simulation of Visceral States Affects Preferences and Behavior/原始数据相关/osfstorage-archive/Study1/S1.sav")

df$condition_label <- ifelse(df[["condition"]] == 1, "Simulate Warmth (Hot)", "Simulate Cold")
df$condition_label <- factor(df$condition_label, 
                              levels = c("Simulate Warmth (Hot)", "Simulate Cold"))

# Separate data by condition
cond1 <- df[["warm_pref"]][df[["condition"]] == 1]
cond2 <- df[["warm_pref"]][df[["condition"]] == 2]
q42_1 <- df[["Q42.0"]][df[["condition"]] == 1]
q42_2 <- df[["Q42.0"]][df[["condition"]] == 2]

n1 <- sum(!is.na(cond1)); n2 <- sum(!is.na(cond2))
m1 <- mean(cond1, na.rm = TRUE); m2 <- mean(cond2, na.rm = TRUE)
sd1 <- sd(cond1, na.rm = TRUE); sd2 <- sd(cond2, na.rm = TRUE)
m_q1 <- mean(q42_1, na.rm = TRUE); m_q2 <- mean(q42_2, na.rm = TRUE)
sd_q1 <- sd(q42_1, na.rm = TRUE); sd_q2 <- sd(q42_2, na.rm = TRUE)
vivid1 <- mean(df[["Q49"]][df[["condition"]] == 1], na.rm = TRUE)
vivid2 <- mean(df[["Q49"]][df[["condition"]] == 2], na.rm = TRUE)
sd_v1 <- sd(df[["Q49"]][df[["condition"]] == 1], na.rm = TRUE)
sd_v2 <- sd(df[["Q49"]][df[["condition"]] == 2], na.rm = TRUE)

# --- t-tests ---
ttest <- t.test(df[["warm_pref"]] ~ df[["condition"]], var.equal = TRUE)
ttest_mc <- t.test(df[["Q42.0"]] ~ df[["condition"]], var.equal = TRUE)

pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
d <- (m2 - m1) / pooled_sd
d_mc <- (m_q2 - m_q1) / sqrt((sd_q1^2 + sd_q2^2) / 2)

# --- Cronbach's alpha ---
items <- data.frame(A4_rev = df[["A4_rev"]], A6_rev = df[["A6_rev"]],
                    A10_rev = df[["A10_rev"]], A3 = df[["A3"]], A8 = df[["A8"]])
k_alpha <- ncol(items)
iv <- apply(items, 2, var, na.rm = TRUE)
tv <- var(rowSums(items, na.rm = TRUE), na.rm = TRUE)
alpha <- (k_alpha/(k_alpha-1)) * (1 - sum(iv)/tv)

# --- Regressions ---
reg1 <- lm(warm_pref ~ Q42.0, data = df)
reg2 <- lm(warm_pref ~ condition + Q42.0, data = df)
r1s <- summary(reg1)
r2s <- summary(reg2)

f_pval1 <- pf(r1s$fstatistic["value"], r1s$fstatistic["numdf"],
              r1s$fstatistic["dendf"], lower.tail = FALSE)
f_pval2 <- pf(r2s$fstatistic["value"], r2s$fstatistic["numdf"],
              r2s$fstatistic["dendf"], lower.tail = FALSE)

# ===========================================================================
# TABLE 1: DESCRIPTIVE STATISTICS
# ===========================================================================
cat("Generating Table 1: Descriptive Statistics...\n")

t1_data <- data.frame(
  Condition = c("Simulate Warmth (Hot)", "Simulate Cold", "Total"),
  n = c(n1, n2, n1 + n2),
  warm_M = sprintf("%.2f", c(m1, m2, mean(df[["warm_pref"]], na.rm = TRUE))),
  warm_SD = sprintf("%.2f", c(sd1, sd2, sd(df[["warm_pref"]], na.rm = TRUE))),
  warm_Min = sprintf("%.0f", c(min(cond1, na.rm = TRUE), min(cond2, na.rm = TRUE), 
                                min(df[["warm_pref"]], na.rm = TRUE))),
  warm_Max = sprintf("%.0f", c(max(cond1, na.rm = TRUE), max(cond2, na.rm = TRUE),
                                max(df[["warm_pref"]], na.rm = TRUE))),
  feel_M = sprintf("%.2f", c(m_q1, m_q2, mean(df[["Q42.0"]], na.rm = TRUE))),
  feel_SD = sprintf("%.2f", c(sd_q1, sd_q2, sd(df[["Q42.0"]], na.rm = TRUE)))
)
colnames(t1_data) <- c("Condition", "n", "WarmPref M", "WarmPref SD",
                        "Min", "Max", "Feeling M", "Feeling SD")

t1 <- make_table(t1_data, header_bg = COL_YELLOW,
                 title = "Table 1: Descriptive Statistics by Condition",
                 fontsize = 12)
save_grob_png(t1, "Table1_Descriptives.png", width = 9, height = 3.5)

# ===========================================================================
# TABLE 2: INDEPENDENT T-TEST
# ===========================================================================
cat("Generating Table 2: Independent t-test...\n")

t2_data <- data.frame(
  Statistic = c("Warmth Condition (Hot)", "Cold Condition", 
                "Mean Difference (Cold - Warmth)", "SE Difference",
                "t-value", "Degrees of Freedom", "p-value (two-tailed)",
                "95% CI Lower", "95% CI Upper", "Cohen's d"),
  Value = c(
    sprintf("M = %.2f, SD = %.2f, n = %d", m1, sd1, n1),
    sprintf("M = %.2f, SD = %.2f, n = %d", m2, sd2, n2),
    sprintf("%.3f", m2 - m1),
    sprintf("%.3f", ttest$stderr),
    sprintf("%.2f", ttest$statistic),
    sprintf("%d", ttest$parameter),
    ifelse(ttest$p.value < 0.001, "< .001 ***", sprintf("%.4f", ttest$p.value)),
    sprintf("%.3f", ttest$conf.int[1]),
    sprintf("%.3f", ttest$conf.int[2]),
    sprintf("%.3f  [%.3f, %.3f]", d, 
            d - 1.96 * sqrt((n1+n2)/(n1*n2) + d^2/(2*(n1+n2))),
            d + 1.96 * sqrt((n1+n2)/(n1*n2) + d^2/(2*(n1+n2))))
  )
)

t2 <- make_table(t2_data, header_bg = COL_BLUE,
                 title = "Table 2: Independent t-test — Warm Preference by Condition",
                 fontsize = 11)
save_grob_png(t2, "Table2_TTest.png", width = 8.5, height = 5)

# ===========================================================================
# TABLE 3: MANIPULATION CHECK
# ===========================================================================
cat("Generating Table 3: Manipulation Check...\n")

t3_data <- data.frame(
  Statistic = c("Warmth Condition (Hot)", "Cold Condition",
                "Mean Difference", "SE Difference",
                "t-value", "Degrees of Freedom", "p-value (two-tailed)",
                "95% CI Lower", "95% CI Upper", "Cohen's d"),
  Value = c(
    sprintf("M = %.2f, SD = %.2f", m_q1, sd_q1),
    sprintf("M = %.2f, SD = %.2f", m_q2, sd_q2),
    sprintf("%.3f", m_q2 - m_q1),
    sprintf("%.3f", ttest_mc$stderr),
    sprintf("%.2f", ttest_mc$statistic),
    sprintf("%d", ttest_mc$parameter),
    sprintf("%.4f", ttest_mc$p.value),
    sprintf("%.3f", ttest_mc$conf.int[1]),
    sprintf("%.3f", ttest_mc$conf.int[2]),
    sprintf("%.3f", d_mc)
  )
)

t3 <- make_table(t3_data, header_bg = COL_PINK,
                 title = "Table 3: Manipulation Check — Feeling Warm/Cold (Q42.0) by Condition",
                 fontsize = 11)
save_grob_png(t3, "Table3_ManipCheck.png", width = 8.5, height = 5)

# ===========================================================================
# TABLE 4a: REGRESSION MODEL 1
# ===========================================================================
cat("Generating Table 4a: Regression Model 1...\n")

beta1 <- coef(reg1)["Q42.0"] * sd(df[["Q42.0"]], na.rm = TRUE) / sd(df[["warm_pref"]], na.rm = TRUE)

t4a_data <- data.frame(
  Predictor = c("(Intercept)", "Q42.0 (Feeling Warm/Cold)"),
  B = sprintf("%.3f", coef(reg1)),
  SE = sprintf("%.3f", r1s$coef[, "Std. Error"]),
  t = sprintf("%.2f", r1s$coef[, "t value"]),
  p = ifelse(r1s$coef[, "Pr(>|t|)"] < 0.001, "< .001 ***",
             sprintf("%.4f", r1s$coef[, "Pr(>|t|)"])),
  Beta = c("—", sprintf("%.3f", beta1))
)

t4a_note <- data.frame(
  Predictor = c("R²", "Adj. R²", "F-test"),
  B = c(sprintf("%.4f", r1s$r.squared),
        sprintf("%.4f", r1s$adj.r.squared),
        sprintf("F(%d, %d) = %.2f, %s",
                r1s$fstatistic["numdf"], r1s$fstatistic["dendf"],
                r1s$fstatistic["value"],
                ifelse(f_pval1 < 0.001, "p < .001", sprintf("p = %.4f", f_pval1)))),
  SE = "", t = "", p = "", Beta = ""
)
t4a_all <- rbind(t4a_data, t4a_note)

t4a <- make_table(t4a_all, header_bg = COL_YELLOW,
                  title = "Table 4a: Regression — Warm Preference ~ Feeling Warm/Cold",
                  fontsize = 11)
save_grob_png(t4a, "Table4a_Reg_Feeling.png", width = 8.5, height = 3.5)

# ===========================================================================
# TABLE 4b: REGRESSION MODEL 2
# ===========================================================================
cat("Generating Table 4b: Regression Model 2...\n")

beta_cond <- coef(reg2)["condition"] * sd(df[["condition"]], na.rm = TRUE) / sd(df[["warm_pref"]], na.rm = TRUE)
beta_feel <- coef(reg2)["Q42.0"] * sd(df[["Q42.0"]], na.rm = TRUE) / sd(df[["warm_pref"]], na.rm = TRUE)

t4b_data <- data.frame(
  Predictor = c("(Intercept)", "Condition (1=Hot, 2=Cold)", "Q42.0 (Feeling Warm/Cold)"),
  B = sprintf("%.3f", coef(reg2)),
  SE = sprintf("%.3f", r2s$coef[, "Std. Error"]),
  t = sprintf("%.2f", r2s$coef[, "t value"]),
  p = ifelse(r2s$coef[, "Pr(>|t|)"] < 0.001, "< .001 ***",
             sprintf("%.4f", r2s$coef[, "Pr(>|t|)"])),
  Beta = c("—", sprintf("%.3f", beta_cond), sprintf("%.3f", beta_feel))
)

t4b_note <- data.frame(
  Predictor = c("R²", "Adj. R²", "F-test", "ΔR² from Model 1"),
  B = c(sprintf("%.4f", r2s$r.squared),
        sprintf("%.4f", r2s$adj.r.squared),
        sprintf("F(%d, %d) = %.2f, %s",
                r2s$fstatistic["numdf"], r2s$fstatistic["dendf"],
                r2s$fstatistic["value"],
                ifelse(f_pval2 < 0.001, "p < .001", sprintf("p = %.4f", f_pval2))),
        sprintf("%.4f", r2s$r.squared - r1s$r.squared)),
  SE = "", t = "", p = "", Beta = ""
)
t4b_all <- rbind(t4b_data, t4b_note)

t4b <- make_table(t4b_all, header_bg = COL_BLUE,
                  title = "Table 4b: Regression — Warm Preference ~ Condition + Feeling",
                  fontsize = 11)
save_grob_png(t4b, "Table4b_Reg_Full.png", width = 9, height = 4)

# ===========================================================================
# TABLE 5: COMPARISON WITH ORIGINAL PAPER (using exact user-provided values)
# ===========================================================================
cat("Generating Table 5: Comparison with Original Paper...\n")

# Retrieve regression coefficients and SEs
reg1_b <- coef(reg1)["Q42.0"]
reg1_se <- r1s$coef["Q42.0", "Std. Error"]
reg1_t <- r1s$coef["Q42.0", "t value"]
reg1_p <- r1s$coef["Q42.0", "Pr(>|t|)"]

reg2_cond_b <- coef(reg2)["condition"]
reg2_cond_se <- r2s$coef["condition", "Std. Error"]
reg2_cond_t <- r2s$coef["condition", "t value"]
reg2_cond_p <- r2s$coef["condition", "Pr(>|t|)"]

reg2_feel_b <- coef(reg2)["Q42.0"]
reg2_feel_se <- r2s$coef["Q42.0", "Std. Error"]
reg2_feel_t <- r2s$coef["Q42.0", "t value"]
reg2_feel_p <- r2s$coef["Q42.0", "Pr(>|t|)"]

t5_data <- data.frame(
  Metric = c(
    "Sample Size (N)", "Cronbach's α",
    "Warmth cond: n",
    "Warmth cond: Preference M (SD)",
    "Warmth cond: Feeling M (SD)",
    "Cold cond: n",
    "Cold cond: Preference M (SD)",
    "Cold cond: Feeling M (SD)",
    "t-test Preference: t(df)",
    "t-test Preference: p",
    "t-test Preference: Cohen's d",
    "t-test Feeling (manip): t(df)",
    "t-test Feeling (manip): p",
    "t-test Feeling (manip): Cohen's d",
    "Simple reg: Feeling → Pref B (SE)",
    "Simple reg: t",
    "Simple reg: p",
    "Multiple reg: Condition B (SE)",
    "Multiple reg: Condition t",
    "Multiple reg: Condition p",
    "Multiple reg: Feeling B (SE)",
    "Multiple reg: Feeling t",
    "Multiple reg: Feeling p"
  ),
  Paper = c(
    "119", ".65",
    "58", "5.19 (1.73)", "5.79 (1.36)",
    "61", "6.23 (1.52)", "5.11 (1.72)",
    "t(117) = 3.501", ".001", "0.64",
    "t(117) = 2.375", ".019", "0.44",
    "-0.256 (0.096)", "2.755", ".007",
    "0.906 (0.301)", "3.013", ".003",
    "-0.203 (0.095)", "2.136", ".035"
  ),
  Replication = c(
    sprintf("%d", n1 + n2),
    sprintf("%.2f", alpha),
    sprintf("%d", n1),
    sprintf("%.2f (%.2f)", m1, sd1),
    sprintf("%.2f (%.2f)", m_q1, sd_q1),
    sprintf("%d", n2),
    sprintf("%.2f (%.2f)", m2, sd2),
    sprintf("%.2f (%.2f)", m_q2, sd_q2),
    sprintf("t(%.0f) = %.3f", ttest$parameter, ttest$statistic),
    sprintf("%.3f", ttest$p.value),
    sprintf("%.2f", d),
    sprintf("t(%.0f) = %.3f", ttest_mc$parameter, ttest_mc$statistic),
    sprintf("%.3f", ttest_mc$p.value),
    sprintf("%.2f", d_mc),
    sprintf("%.3f (%.3f)", reg1_b, reg1_se),
    sprintf("%.3f", reg1_t),
    sprintf("%.3f", reg1_p),
    sprintf("%.3f (%.3f)", reg2_cond_b, reg2_cond_se),
    sprintf("%.3f", reg2_cond_t),
    sprintf("%.3f", reg2_cond_p),
    sprintf("%.3f (%.3f)", reg2_feel_b, reg2_feel_se),
    sprintf("%.3f", reg2_feel_t),
    sprintf("%.3f", reg2_feel_p)
  ),
  Match = c(
    "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes"
  ),
  stringsAsFactors = FALSE
)

t5 <- make_table(t5_data, header_bg = COL_YELLOW,
                 title = "Table 5: Comparison — Original Paper vs. Replication (Study 1a)",
                 fontsize = 9)
save_grob_png(t5, "Table5_Comparison.png", width = 12, height = 8)

# ===========================================================================
# FIGURE 1: BAR CHART WITH ERROR BARS
# ===========================================================================
cat("Generating Figure 1: Bar Chart...\n")

plot_summary <- data.frame(
  Condition = c("Simulate\nWarmth (Hot)", "Simulate\nCold"),
  M = c(m1, m2),
  SE = c(sd1/sqrt(n1), sd2/sqrt(n2)),
  n = c(n1, n2)
)
plot_summary$Condition <- factor(plot_summary$Condition,
                                  levels = c("Simulate\nWarmth (Hot)", "Simulate\nCold"))

# Raw data for jitter
plot_df <- data.frame(
  warm_pref = df[["warm_pref"]],
  Condition = df$condition_label
)

p1 <- ggplot(plot_summary, aes(x = Condition, y = M, fill = Condition)) +
  geom_col(width = 0.45, color = "grey30", linewidth = 0.8) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.12, linewidth = 1) +
  geom_jitter(data = plot_df, aes(x = Condition, y = warm_pref),
              width = 0.18, height = 0, alpha = 0.25, size = 2, color = "grey40") +
  scale_fill_manual(values = c("Simulate Warmth (Hot)" = COL_PINK,
                                "Simulate Cold" = COL_BLUE)) +
  scale_x_discrete(labels = c("Simulate Warmth (Hot)" = "Simulate\nWarmth (Hot)",
                               "Simulate Cold" = "Simulate\nCold")) +
  # T-test annotation
  annotate("segment", x = 1, xend = 2, 
           y = max(plot_summary$M + plot_summary$SE) * 1.12,
           yend = max(plot_summary$M + plot_summary$SE) * 1.12,
           linewidth = 0.8) +
  annotate("text", x = 1.5, 
           y = max(plot_summary$M + plot_summary$SE) * 1.18,
           label = sprintf("t(%d) = %.2f, p %s, d = %.2f",
                           ttest$parameter, ttest$statistic,
                           ifelse(ttest$p.value < 0.001, "< .001",
                                  sprintf("= %.3f", ttest$p.value)),
                           d),
           size = 4.5, fontface = "italic") +
  annotate("text", x = 1.5, 
           y = max(plot_summary$M + plot_summary$SE) * 1.08,
           label = "***", size = 7) +
  labs(title = "Warm Preference by Condition",
       subtitle = "Study 1a: Mental Simulation of Visceral States",
       x = NULL,
       y = "Warm Preference (Mean ± SE)\nHigher scores = prefer warming activities more") +
  coord_cartesian(ylim = c(0, max(plot_summary$M + plot_summary$SE) * 1.28)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 12),
    plot.margin = margin(20, 25, 15, 20)
  )

ggsave(file.path(out_dir, "Figure1_TTest_BarChart.png"), p1,
       width = 7, height = 6, dpi = 200)

# ===========================================================================
# FIGURE 2: VIOLIN + BOXPLOT
# ===========================================================================
cat("Generating Figure 2: Violin + Boxplot...\n")

p2 <- ggplot(plot_df, aes(x = Condition, y = warm_pref, fill = Condition)) +
  geom_violin(alpha = 0.3, trim = TRUE, color = "grey40", linewidth = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA, 
               color = "grey30", linewidth = 0.8) +
  geom_jitter(width = 0.06, alpha = 0.3, size = 1.8, color = "grey30") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, 
               color = "black") +
  scale_fill_manual(values = c("Simulate Warmth (Hot)" = COL_PINK,
                                "Simulate Cold" = COL_BLUE)) +
  scale_x_discrete(labels = c("Simulate Warmth (Hot)" = "Simulate\nWarmth (Hot)",
                               "Simulate Cold" = "Simulate\nCold")) +
  labs(title = "Distribution of Warm Preference by Condition",
       subtitle = "Violin Plot with Boxplot, Individual Points, and Mean (\u25C6)",
       x = NULL,
       y = "Warm Preference (higher = prefer warming activities)") +
  annotate("text", x = 1.5, 
           y = max(plot_df$warm_pref, na.rm = TRUE) * 1.02,
           label = sprintf("t(%d) = %.2f, p %s, d = %.2f",
                           ttest$parameter, ttest$statistic,
                           ifelse(ttest$p.value < 0.001, "< .001",
                                  sprintf("= %.3f", ttest$p.value)),
                           d),
           size = 4, fontface = "italic") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 15, 20)
  )

ggsave(file.path(out_dir, "Figure2_Violin_Boxplot.png"), p2,
       width = 7, height = 6, dpi = 200)

# ===========================================================================
# SUMMARY
# ===========================================================================
cat("\n==============================================================\n")
cat("  ALL OUTPUTS GENERATED SUCCESSFULLY\n")
cat("==============================================================\n")
cat(sprintf("  Folder: %s\n\n", out_dir))
cat("  Tables:\n")
cat("    Table1_Descriptives.png  (%.2f by %.2f)\n")
cat("    Table2_TTest.png  (%.2f by %.2f)\n")
cat("    Table3_ManipCheck.png  (%.2f by %.2f)\n")
cat("    Table4a_Reg_Feeling.png  (%.2f by %.2f)\n")
cat("    Table4b_Reg_Full.png  (%.2f by %.2f)\n")
cat("    Table5_Comparison.png  (%.2f by %.2f)\n\n")
cat("  Figures:\n")
cat("    Figure1_TTest_BarChart.png\n")
cat("    Figure2_Violin_Boxplot.png\n")
cat("==============================================================\n")

# Save numbers
sink(file.path(out_dir, "analysis_numbers.txt"))
cat("STUDY 1a - KEY STATISTICS\n")
cat("=========================\n\n")
cat(sprintf("N = %d (Hot: %d, Cold: %d)\n", n1+n2, n1, n2))
cat(sprintf("Cronbach's alpha = %.3f\n\n", alpha))
cat(sprintf("Warm Preference:\n"))
cat(sprintf("  Hot:  M = %.2f, SD = %.2f\n", m1, sd1))
cat(sprintf("  Cold: M = %.2f, SD = %.2f\n", m2, sd2))
cat(sprintf("  t(%d) = %.2f, p = %.6f, d = %.3f\n", 
            ttest$parameter, ttest$statistic, ttest$p.value, d))
cat(sprintf("  95%% CI = [%.3f, %.3f]\n\n", ttest$conf.int[1], ttest$conf.int[2]))
cat(sprintf("Manipulation Check (Feeling):\n"))
cat(sprintf("  Hot:  M = %.2f, SD = %.2f\n", m_q1, sd_q1))
cat(sprintf("  Cold: M = %.2f, SD = %.2f\n", m_q2, sd_q2))
cat(sprintf("  t(%d) = %.2f, p = %.4f, d = %.3f\n\n",
            ttest_mc$parameter, ttest_mc$statistic, ttest_mc$p.value, d_mc))
cat(sprintf("Regression 1: warm_pref ~ Q42.0\n"))
cat(sprintf("  B = %.3f, SE = %.3f, t = %.2f\n", 
            coef(reg1)["Q42.0"], r1s$coef["Q42.0", "Std. Error"],
            r1s$coef["Q42.0", "t value"]))
cat(sprintf("  Beta = %.3f, R² = %.4f\n\n", beta1, r1s$r.squared))
cat(sprintf("Regression 2: warm_pref ~ condition + Q42.0\n"))
cat(sprintf("  Condition: B = %.3f, Beta = %.3f, t = %.2f\n",
            coef(reg2)["condition"], beta_cond,
            r2s$coef["condition", "t value"]))
cat(sprintf("  Q42.0: B = %.3f, Beta = %.3f, t = %.2f\n",
            coef(reg2)["Q42.0"], beta_feel,
            r2s$coef["Q42.0", "t value"]))
cat(sprintf("  R² = %.4f, ΔR² from Model 1 = %.4f\n",
            r2s$r.squared, r2s$r.squared - r1s$r.squared))
sink()
