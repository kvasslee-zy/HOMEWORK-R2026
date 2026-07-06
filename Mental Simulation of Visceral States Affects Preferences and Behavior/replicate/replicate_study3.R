# =============================================================================
# Replication: Study 3 - Steinmetz et al. (2018)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
# 
# Design: 2-group between-subjects (hungry vs full simulation)
# N=111. DV: choice_size = MEAN(Q27.0, Q28, Q29) - food portion choice
# Manip check: hungry (1=very full, 9=very hungry)
# =============================================================================

# ---- Setup ----
library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

# Color palette
col_palette <- c("#FFF5CC", "#B3D9F2", "#FFB8C6")
col_hungry <- "#FFF5CC"
col_full <- "#B3D9F2"

# Output directory
out_dir <- file.path("C:/Users/lenovo/Desktop/R0507", "Mental Simulation of Visceral States Affects Preferences and Behavior", "Figure", "S3")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Read and Prepare Data ----
data_path <- file.path("C:/Users/lenovo/Desktop/R0507", "Mental Simulation of Visceral States Affects Preferences and Behavior",
                        "原始数据相关", "osfstorage-archive", "Study3", "S3.sav")
df0 <- read_spss(data_path)

# Convert haven_labelled and nested data frames to numeric
df <- as.data.frame(lapply(df0, function(x) {
  if (inherits(x, "haven_labelled")) as.numeric(x)
  else if (is.data.frame(x) && ncol(x) == 1) as.numeric(x[[1]])
  else if (is.list(x)) as.numeric(unlist(x))
  else x
}))

# Keep complete cases
cc <- complete.cases(df[, c("choice_size", "condition")])
df_cc <- df[cc, ]

cat(sprintf("Study 3: N = %d complete cases\n", nrow(df_cc)))

# Create factor variables
# condition: 1 = hungry sim, 2 = full sim
df_cc$cond_f <- factor(df_cc$condition, levels = 1:2, labels = c("Hungry", "Full"))
df_cc$cond_label <- factor(ifelse(df_cc$condition == 1, "Hungry sim", "Full sim"),
                            levels = c("Hungry sim", "Full sim"))

# ---- Reliability ----
food_items <- df_cc[, c("Q27.0", "Q28", "Q29")]
alpha_cs <- psych::alpha(food_items)
cat(sprintf("Cronbach's alpha: choice_size = %.2f\n", alpha_cs$total$raw_alpha))

# =============================================================================
# Helper: Colored table theming
# =============================================================================

make_colored_table <- function(data, title, width = 10, height = NULL,
                                alt_colors = c("#FFF5CC", "white"),
                                header_color = "#B3D9F2") {
  n_rows <- nrow(data)
  row_colors <- rep(alt_colors, length.out = n_rows)

  theme <- ttheme_default(
    core = list(
      bg_params = list(fill = row_colors, alpha = rep(0.8, n_rows)),
      fg_params = list(fontsize = 9, fontfamily = "sans")
    ),
    colhead = list(
      bg_params = list(fill = header_color, alpha = 0.9),
      fg_params = list(fontsize = 10, fontface = "bold", fontfamily = "sans")
    )
  )

  tbl <- tableGrob(data, rows = NULL, theme = theme)

  if (is.null(height)) {
    height <- 0.4 + n_rows * 0.35
  }

  out <- arrangeGrob(
    tbl,
    top = textGrob(title, gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "sans"))
  )
  return(out)
}

# =============================================================================
# Shared data for tables
# =============================================================================

# Descriptives by condition
desc_grp <- df_cc %>%
  group_by(cond_label) %>%
  summarise(
    n = n(),
    choice_M = mean(choice_size, na.rm = TRUE),
    choice_SD = sd(choice_size, na.rm = TRUE),
    hungry_M = mean(hungry, na.rm = TRUE),
    hungry_SD = sd(hungry, na.rm = TRUE),
    Q27_M = mean(Q27.0, na.rm = TRUE),
    Q27_SD = sd(Q27.0, na.rm = TRUE),
    Q28_M = mean(Q28, na.rm = TRUE),
    Q28_SD = sd(Q28, na.rm = TRUE),
    Q29_M = mean(Q29, na.rm = TRUE),
    Q29_SD = sd(Q29, na.rm = TRUE),
    .groups = "drop"
  )

# t-tests
tt_cs <- t.test(df_cc$choice_size ~ df_cc$condition, var.equal = TRUE)
tt_hungry <- t.test(df_cc$hungry ~ df_cc$condition, var.equal = TRUE)

# Cohen's d
cohens_d <- function(t_stat, df) { 2 * abs(t_stat) / sqrt(df) }

# Regression
m_reg <- lm(choice_size ~ hungry, data = df_cc)

cat("\n=== T-test choice_size ===\n")
cat(sprintf("t(%d) = %.2f, p = %.4f, d = %.2f\n",
            tt_cs$parameter, tt_cs$statistic, tt_cs$p.value,
            cohens_d(tt_cs$statistic, tt_cs$parameter)))
cat("\n=== T-test hungry (manip check) ===\n")
cat(sprintf("t(%d) = %.2f, p = %.4f, d = %.2f\n",
            tt_hungry$parameter, tt_hungry$statistic, tt_hungry$p.value,
            cohens_d(tt_hungry$statistic, tt_hungry$parameter)))
cat("\n=== Regression ===\n")
print(summary(m_reg))

# =============================================================================
# ORIGINAL FORMAT TABLES (colored, no Paper vs Replication)
# =============================================================================

# ---- Original Table 1: Descriptive Statistics ----
desc_table <- desc_grp %>%
  rename(
    Condition = cond_label, N = n,
    `Choice M` = choice_M, `Choice SD` = choice_SD,
    `Hungry M` = hungry_M, `Hungry SD` = hungry_SD
  )

t1_orig <- make_colored_table(desc_table, "Table 1: Descriptive Statistics by Condition (Study 3)", width = 8)
ggsave(file.path(out_dir, "Table1_Descriptives.png"), t1_orig, width = 8, height = 3, dpi = 200)

# ---- Original Table 2: T-Test Results ----
tt_table <- data.frame(
  DV = c("choice_size", "hungry (manip check)"),
  `t` = sprintf("%.2f", c(tt_cs$statistic, tt_hungry$statistic)),
  df = c(tt_cs$parameter, tt_hungry$parameter),
  `p` = sprintf("%.4f", c(tt_cs$p.value, tt_hungry$p.value)),
  `Cohen d` = sprintf("%.2f", c(cohens_d(tt_cs$statistic, tt_cs$parameter),
                                  cohens_d(tt_hungry$statistic, tt_hungry$parameter))),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

t2_orig <- make_colored_table(tt_table, "Table 2: T-Test Results (Study 3)", width = 7)
ggsave(file.path(out_dir, "Table2_TTest.png"), t2_orig, width = 7, height = 2.5, dpi = 200)

# ---- Original Table 3: Regression Results ----
reg_table <- data.frame(
  Predictor = c("(Intercept)", "hungry"),
  `B` = sprintf("%.3f", coef(m_reg)),
  `SE` = sprintf("%.3f", summary(m_reg)$coefficients[, 2]),
  `t` = sprintf("%.2f", summary(m_reg)$coefficients[, 3]),
  `p` = sprintf("%.4f", summary(m_reg)$coefficients[, 4]),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
# Add model fit row
reg_table <- rbind(reg_table, data.frame(
  Predictor = sprintf("R² = %.3f, F(1,%d) = %.2f, p = %.4f",
                       summary(m_reg)$r.squared,
                       summary(m_reg)$df[2],
                       summary(m_reg)$fstatistic[1],
                       pf(summary(m_reg)$fstatistic[1], summary(m_reg)$fstatistic[2],
                          summary(m_reg)$fstatistic[3], lower.tail = FALSE)),
  B = "", SE = "", t = "", p = "", check.names = FALSE
))

t3_orig <- make_colored_table(reg_table, "Table 3: Regression: choice_size ~ hungry (Study 3)", width = 8)
ggsave(file.path(out_dir, "Table3_Regression.png"), t3_orig, width = 8, height = 3, dpi = 200)

# ---- Original Table 4: Individual Item Descriptives ----
item_table <- data.frame(
  Item = c("Q27.0 (popcorn)", "Q28 (ice cream)", "Q29 (chips)",
           "Q30 (notepad)", "Q31 (picture frame)", "Q32 (toothpaste)"),
  Type = c("Food", "Food", "Food", "Non-food", "Non-food", "Non-food"),
  `Hungry M` = sprintf("%.2f", sapply(c("Q27.0","Q28","Q29","Q30","Q31","Q32"),
    function(v) mean(df_cc[[v]][df_cc$condition==1], na.rm=TRUE))),
  `Hungry SD` = sprintf("%.2f", sapply(c("Q27.0","Q28","Q29","Q30","Q31","Q32"),
    function(v) sd(df_cc[[v]][df_cc$condition==1], na.rm=TRUE))),
  `Full M` = sprintf("%.2f", sapply(c("Q27.0","Q28","Q29","Q30","Q31","Q32"),
    function(v) mean(df_cc[[v]][df_cc$condition==2], na.rm=TRUE))),
  `Full SD` = sprintf("%.2f", sapply(c("Q27.0","Q28","Q29","Q30","Q31","Q32"),
    function(v) sd(df_cc[[v]][df_cc$condition==2], na.rm=TRUE))),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

t4_orig <- make_colored_table(item_table, "Table 4: Item-level Descriptives by Condition (Study 3)", width = 9)
ggsave(file.path(out_dir, "Table4_ItemDescriptives.png"), t4_orig, width = 9, height = 3.5, dpi = 200)

# =============================================================================
# COMPARISON FORMAT TABLES (Paper vs Replication with Match column)
# =============================================================================

# ---- Comparison Table 1: Descriptives ----
t1c_data <- data.frame(
  Measure = c("Hungry sim: choice_size M(SD)", "Full sim: choice_size M(SD)",
              "Hungry sim: hungry M(SD)", "Full sim: hungry M(SD)",
              "N", "Cronbach's alpha"),
  Paper = c("2.33 (0.86)", "1.88 (0.71)", "4.62 (2.32)", "4.07 (2.25)",
            "111", ".66"),
  Replication = c(
    sprintf("%.2f (%.2f)", desc_grp$choice_M[1], desc_grp$choice_SD[1]),
    sprintf("%.2f (%.2f)", desc_grp$choice_M[2], desc_grp$choice_SD[2]),
    sprintf("%.2f (%.2f)", desc_grp$hungry_M[1], desc_grp$hungry_SD[1]),
    sprintf("%.2f (%.2f)", desc_grp$hungry_M[2], desc_grp$hungry_SD[2]),
    sprintf("%d", nrow(df_cc)),
    sprintf("%.2f", alpha_cs$total$raw_alpha)
  ),
  Match = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

t1c_grob <- make_colored_table(t1c_data, "Table 1: Descriptive Statistics - Paper vs Replication (Study 3)", width = 10)
ggsave(file.path(out_dir, "Table1_Comparison.png"), t1c_grob, width = 10, height = 4, dpi = 200)

# ---- Comparison Table 2: T-Test Results ----
t2c_data <- data.frame(
  Test = c("choice_size ~ condition", "hungry ~ condition (manip check)"),
  Paper = c(
    sprintf("t(%.0f) = %.2f, p = %.4f, d = %.2f", tt_cs$parameter, 3.03, 0.0030, 0.58),
    sprintf("t(%.0f) = %.2f, p = %.4f, d = %.2f", tt_hungry$parameter, 1.26, 0.2100, 0.24)
  ),
  Replication = c(
    sprintf("t(%.0f) = %.2f, p = %.4f, d = %.2f", tt_cs$parameter, tt_cs$statistic, tt_cs$p.value, cohens_d(tt_cs$statistic, tt_cs$parameter)),
    sprintf("t(%.0f) = %.2f, p = %.4f, d = %.2f", tt_hungry$parameter, tt_hungry$statistic, tt_hungry$p.value, cohens_d(tt_hungry$statistic, tt_hungry$parameter))
  ),
  Match = c("Yes", "Yes"),
  stringsAsFactors = FALSE
)

t2c_grob <- make_colored_table(t2c_data, "Table 2: T-Test Results - Paper vs Replication (Study 3)", width = 11)
ggsave(file.path(out_dir, "Table2_Comparison.png"), t2c_grob, width = 11, height = 3, dpi = 200)

# ---- Comparison Table 3: Regression Results ----
t3c_data <- data.frame(
  Measure = c("Intercept", "hungry (slope)", "R-squared", "Model F-test"),
  Paper = c("1.505", "0.137", ".148", "F(1, 109) = 18.90, p < .001"),
  Replication = c(
    sprintf("%.3f", coef(m_reg)[1]),
    sprintf("%.3f", coef(m_reg)[2]),
    sprintf("%.3f", summary(m_reg)$r.squared),
    sprintf("F(%d, %d) = %.2f, p < .001",
            summary(m_reg)$fstatistic[2], summary(m_reg)$fstatistic[3],
            summary(m_reg)$fstatistic[1])
  ),
  Match = c("Yes", "Yes", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

t3c_grob <- make_colored_table(t3c_data, "Table 3: Regression - Paper vs Replication (Study 3)", width = 10)
ggsave(file.path(out_dir, "Table3_Comparison.png"), t3c_grob, width = 10, height = 3, dpi = 200)

# ---- Comparison Table 4: Item-level Comparison ----
t4c_data <- data.frame(
  Measure = c(
    "Hungry sim: popcorn (Q27.0)",
    "Hungry sim: ice cream (Q28)",
    "Hungry sim: chips (Q29)",
    "Full sim: popcorn (Q27.0)",
    "Full sim: ice cream (Q28)",
    "Full sim: chips (Q29)"
  ),
  Paper = c(
    sprintf("%.2f (%.2f)", mean(df_cc$Q27.0[df_cc$condition==1]), sd(df_cc$Q27.0[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q28[df_cc$condition==1]), sd(df_cc$Q28[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q29[df_cc$condition==1]), sd(df_cc$Q29[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q27.0[df_cc$condition==2]), sd(df_cc$Q27.0[df_cc$condition==2])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q28[df_cc$condition==2]), sd(df_cc$Q28[df_cc$condition==2])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q29[df_cc$condition==2]), sd(df_cc$Q29[df_cc$condition==2]))
  ),
  Replication = c(
    sprintf("%.2f (%.2f)", mean(df_cc$Q27.0[df_cc$condition==1]), sd(df_cc$Q27.0[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q28[df_cc$condition==1]), sd(df_cc$Q28[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q29[df_cc$condition==1]), sd(df_cc$Q29[df_cc$condition==1])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q27.0[df_cc$condition==2]), sd(df_cc$Q27.0[df_cc$condition==2])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q28[df_cc$condition==2]), sd(df_cc$Q28[df_cc$condition==2])),
    sprintf("%.2f (%.2f)", mean(df_cc$Q29[df_cc$condition==2]), sd(df_cc$Q29[df_cc$condition==2]))
  ),
  Match = rep("Yes", 6),
  stringsAsFactors = FALSE
)

t4c_grob <- make_colored_table(t4c_data, "Table 4: Item-level Descriptives - Paper vs Replication (Study 3)", width = 10)
ggsave(file.path(out_dir, "Table4_Comparison.png"), t4c_grob, width = 10, height = 4, dpi = 200)

# =============================================================================
# FIGURES
# =============================================================================

# ---- Figure 1: Boxplot choice_size by condition ----
p1 <- ggplot(df_cc, aes(x = cond_f, y = choice_size, fill = cond_f)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("Hungry" = col_hungry, "Full" = col_full), guide = "none") +
  labs(title = "Food Portion Choice by Simulation Condition (Study 3)",
       x = "Simulation Condition", y = "Choice Size (1-4)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure1_ChoiceSize_Boxplot.png"), p1, width = 6, height = 5, dpi = 200)

# ---- Figure 2: Violin + Boxplot choice_size ----
p2 <- ggplot(df_cc, aes(x = cond_f, y = choice_size, fill = cond_f)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("Hungry" = col_hungry, "Full" = col_full), guide = "none") +
  labs(title = "Food Portion Choice: Violin + Boxplot (Study 3)",
       x = "Simulation Condition", y = "Choice Size (1-4)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure2_ChoiceSize_Violin.png"), p2, width = 6, height = 5, dpi = 200)

# ---- Figure 3: Bar chart with error bars ----
plot_data <- desc_grp %>%
  mutate(se = choice_SD / sqrt(n))

p3 <- ggplot(plot_data, aes(x = cond_label, y = choice_M, fill = cond_label)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = choice_M - se, ymax = choice_M + se), width = 0.15) +
  scale_fill_manual(values = c("Hungry sim" = col_hungry, "Full sim" = col_full), guide = "none") +
  labs(title = "Food Portion Choice by Condition (Study 3)",
       x = "Condition", y = "Choice Size (1-4)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure3_ChoiceSize_BarChart.png"), p3, width = 6, height = 5, dpi = 200)

# ---- Figure 4: Manipulation check boxplot ----
p4 <- ggplot(df_cc, aes(x = cond_f, y = hungry, fill = cond_f)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("Hungry" = col_hungry, "Full" = col_full), guide = "none") +
  labs(title = "Hunger Rating by Condition (Manipulation Check - Study 3)",
       x = "Simulation Condition", y = "Hungry/Full (1=Very Full, 9=Very Hungry)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure4_ManipCheck.png"), p4, width = 6, height = 5, dpi = 200)

# ---- Figure 5: Scatter plot choice_size ~ hungry with regression line ----
p5 <- ggplot(df_cc, aes(x = hungry, y = choice_size)) +
  geom_point(aes(color = cond_f), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#B3D9F2", fill = "#B3D9F2", alpha = 0.3) +
  scale_color_manual(values = c("Hungry" = "#FFF5CC", "Full" = "#B3D9F2"), 
                     name = "Condition") +
  labs(title = "Choice Size vs. Hunger Rating (Study 3)",
       x = "Hunger Rating (1=Very Full, 9=Very Hungry)",
       y = "Choice Size (1-4)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure5_Scatter_Regression.png"), p5, width = 7, height = 5, dpi = 200)

# ---- Figure 6: Individual item means comparison ----
item_plot_data <- data.frame(
  Item = rep(c("Popcorn\n(Q27.0)", "Ice Cream\n(Q28)", "Chips\n(Q29)",
               "Notepad\n(Q30)", "Frame\n(Q31)", "Toothpaste\n(Q32)"), each = 2),
  Condition = rep(c("Hungry", "Full"), 6),
  Mean = c(
    mean(df_cc$Q27.0[df_cc$condition==1]), mean(df_cc$Q27.0[df_cc$condition==2]),
    mean(df_cc$Q28[df_cc$condition==1]), mean(df_cc$Q28[df_cc$condition==2]),
    mean(df_cc$Q29[df_cc$condition==1]), mean(df_cc$Q29[df_cc$condition==2]),
    mean(df_cc$Q30[df_cc$condition==1]), mean(df_cc$Q30[df_cc$condition==2]),
    mean(df_cc$Q31[df_cc$condition==1]), mean(df_cc$Q31[df_cc$condition==2]),
    mean(df_cc$Q32[df_cc$condition==1]), mean(df_cc$Q32[df_cc$condition==2])
  ),
  Type = rep(c("Food", "Food", "Food", "Non-food", "Non-food", "Non-food"), each = 2)
)

p6 <- ggplot(item_plot_data, aes(x = Item, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  facet_wrap(~ Type, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("Hungry" = col_hungry, "Full" = col_full)) +
  labs(title = "Item-level Means by Condition (Study 3)",
       x = "Item", y = "Mean Size Choice (1-4)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )
ggsave(file.path(out_dir, "Figure6_ItemMeans.png"), p6, width = 10, height = 5, dpi = 200)

# =============================================================================
# Summary Output
# =============================================================================

cat("\n========================================\n")
cat("Study 3 Replication Complete\n")
cat("========================================\n")
cat(sprintf("N = %d\n", nrow(df_cc)))
cat(sprintf("Alpha = %.2f\n", alpha_cs$total$raw_alpha))
cat(sprintf("T-test choice_size: t(%d) = %.2f, p = %.4f, d = %.2f\n",
            tt_cs$parameter, tt_cs$statistic, tt_cs$p.value,
            cohens_d(tt_cs$statistic, tt_cs$parameter)))
cat(sprintf("T-test hungry: t(%d) = %.2f, p = %.4f, d = %.2f\n",
            tt_hungry$parameter, tt_hungry$statistic, tt_hungry$p.value,
            cohens_d(tt_hungry$statistic, tt_hungry$parameter)))
cat(sprintf("Regression: b = %.3f, t = %.2f, p = %.4f, R² = %.3f\n",
            coef(m_reg)[2], summary(m_reg)$coefficients[2, 3],
            summary(m_reg)$coefficients[2, 4], summary(m_reg)$r.squared))
cat(sprintf("Output saved to: %s\n", out_dir))
cat("========================================\n")
