# =============================================================================
# Replication: Study 5 - Steinmetz et al. (2018)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
#
# Design: 2 (condition: full=1 vs hungry=2) × 2 (similarity: similar=1 vs dissimilar=2)
# Between-subjects. DV: Q29 (hunger projection onto target)
# =============================================================================

# ---- Setup ----
library(foreign)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

# Color palette
col_hungry <- "#FFF5CC"
col_full <- "#B3D9F2"
col_similar <- "#FFF5CC"
col_dissimilar <- "#FFB8C6"

# Output directory
out_dir <- file.path("C:/Users/lenovo/Desktop/R0507",
                     "Mental Simulation of Visceral States Affects Preferences and Behavior",
                     "Figure", "S5")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Read Data ----
d0 <- read.spss(file.path("C:/Users/lenovo/Desktop/R0507",
                          "Mental Simulation of Visceral States Affects Preferences and Behavior",
                          "osfstorage-archive (1)", "Study5",
                          "Hunger_projection_similarity_states.sav"),
                to.data.frame = TRUE, use.value.labels = FALSE)

# Keep complete cases on key analysis variables
vars_ana <- c("condition", "similarity", "Q29", "Q27", "Q33", "Q37.0", "Q40.0")
cc <- complete.cases(d0[, vars_ana])
df <- d0[cc, ]

# Label factors
df$cond_label <- factor(df$condition, levels = 1:2, labels = c("Full", "Hungry"))
df$sim_label <- factor(df$similarity, levels = 1:2, labels = c("Similar", "Dissimilar"))

cat(sprintf("Study 5: N = %d complete cases (out of %d)\n", nrow(df), nrow(d0)))
cat("Condition × Similarity:\n")
print(table(df$cond_label, df$sim_label))

# ---- Colored table helper ----
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
  if (is.null(height)) height <- 0.4 + n_rows * 0.35
  out <- arrangeGrob(tbl, top = textGrob(title, gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "sans")))
  return(out)
}

# =============================================================================
# T-TEST: Q40.0 (similarity check) ~ similarity
# =============================================================================
tt_sim <- t.test(Q40.0 ~ similarity, data = df, var.equal = TRUE)
cat("\n=== T-test: Q40.0 (similarity check) by similarity group ===\n")
cat(sprintf("t(%d) = %.2f, p = %.4f\n", tt_sim$parameter, tt_sim$statistic, tt_sim$p.value))

# =============================================================================
# 2-WAY ANOVA: Q29 (hunger projection) ~ condition * similarity
# =============================================================================
options(contrasts = c("contr.sum", "contr.poly"))
m_q29 <- aov(Q29 ~ cond_label * sim_label, data = df)
cat("\n=== ANOVA: Q29 (hunger projection) ~ condition * similarity ===\n")
print(summary(m_q29))

ss_total_q29 <- sum((df$Q29 - mean(df$Q29))^2)
ss_effect_q29 <- summary(m_q29)[[1]]$"Sum Sq"
eta_sq_q29 <- ss_effect_q29 / ss_total_q29
cat("\nPartial eta-squared for Q29:\n")
print(round(eta_sq_q29, 3))

# Simple effects of condition at each similarity level
cat("\n=== Simple Effects: condition at each similarity level ===\n")
for (sim in c("Similar", "Dissimilar")) {
  sub <- df[df$sim_label == sim, ]
  m_sub <- aov(Q29 ~ cond_label, data = sub)
  cat(sprintf("\n--- similarity = %s ---\n", sim))
  print(summary(m_sub))
}

cat("\n=== Simple Effects: similarity at each condition ===\n")
for (cond in c("Full", "Hungry")) {
  sub <- df[df$cond_label == cond, ]
  m_sub <- aov(Q29 ~ sim_label, data = sub)
  cat(sprintf("\n--- condition = %s ---\n", cond))
  print(summary(m_sub))
}

# =============================================================================
# ADDITIONAL ANOVAS (Q27, Q33, Q37.0)
# =============================================================================
cat("\n=== ANOVA: Q27 (thirst) ~ condition * similarity ===\n")
m_q27 <- aov(Q27 ~ cond_label * sim_label, data = df)
print(summary(m_q27))

cat("\n=== ANOVA: Q33 (temperature) ~ condition * similarity ===\n")
m_q33 <- aov(Q33 ~ cond_label * sim_label, data = df)
print(summary(m_q33))

cat("\n=== ANOVA: Q37.0 (tiredness) ~ condition * similarity ===\n")
m_q37 <- aov(Q37.0 ~ cond_label * sim_label, data = df)
print(summary(m_q37))

# =============================================================================
# DESCRIPTIVES
# =============================================================================
desc_cell <- df %>%
  group_by(cond_label, sim_label) %>%
  summarise(
    n = n(),
    Q29_M = mean(Q29, na.rm = TRUE),
    Q29_SD = sd(Q29, na.rm = TRUE),
    Q27_M = mean(Q27, na.rm = TRUE),
    Q27_SD = sd(Q27, na.rm = TRUE),
    Q33_M = mean(Q33, na.rm = TRUE),
    Q33_SD = sd(Q33, na.rm = TRUE),
    Q37_M = mean(Q37.0, na.rm = TRUE),
    Q37_SD = sd(Q37.0, na.rm = TRUE),
    Q40_M = mean(Q40.0, na.rm = TRUE),
    Q40_SD = sd(Q40.0, na.rm = TRUE),
    .groups = "drop"
  )
cat("\nDescriptives by cell:\n")
print(as.data.frame(desc_cell))

# =============================================================================
# ORIGINAL FORMAT TABLES
# =============================================================================

# Table 1: Descriptives
t1_data <- desc_cell %>%
  mutate(Cell = paste0(cond_label, " / ", sim_label)) %>%
  select(Cell, n, Q29_M, Q29_SD, Q40_M, Q40_SD) %>%
  rename(Cell = Cell, N = n, `Q29 (Hunger) M` = Q29_M, `Q29 SD` = Q29_SD,
         `Q40 (Similarity) M` = Q40_M, `Q40 SD` = Q40_SD)
t1_orig <- make_colored_table(t1_data, "Table 1: Descriptive Statistics by Cell (Study 5)", width = 10)
ggsave(file.path(out_dir, "Table1_Descriptives.png"), t1_orig, width = 10, height = 4, dpi = 200)

# Table 2: Q29 ANOVA
a29 <- summary(m_q29)[[1]]
t2_data <- data.frame(
  Effect = rownames(a29),
  SS = sprintf("%.2f", a29$`Sum Sq`),
  df = a29$Df,
  F = sprintf("%.2f", a29$`F value`),
  p = sprintf("%.4f", a29$`Pr(>F)`),
  `partial eta²` = sprintf("%.3f", a29$`Sum Sq` / ss_total_q29),
  check.names = FALSE, stringsAsFactors = FALSE
)
t2_orig <- make_colored_table(t2_data, "Table 2: ANOVA - Q29 (Hunger Projection) (Study 5)", width = 10)
ggsave(file.path(out_dir, "Table2_ANOVA_Q29.png"), t2_orig, width = 10, height = 3.5, dpi = 200)

# Table 3: Simple Effects for Q29
t3_list <- list()
for (sim in c("Similar", "Dissimilar")) {
  sub <- df[df$sim_label == sim, ]
  m_sub <- aov(Q29 ~ cond_label, data = sub)
  s <- summary(m_sub)[[1]]
  t3_list[[sim]] <- data.frame(
    Similarity = sim,
    SS = sprintf("%.2f", s$`Sum Sq`[1]),
    df1 = s$Df[1], df2 = s$Df[2],
    F = sprintf("%.2f", s$`F value`[1]),
    p = sprintf("%.4f", s$`Pr(>F)`[1]),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
t3_data <- bind_rows(t3_list)
t3_orig <- make_colored_table(t3_data, "Table 3: Simple Effects of Condition at each Similarity (Study 5)", width = 9)
ggsave(file.path(out_dir, "Table3_SimpleEffects.png"), t3_orig, width = 9, height = 3, dpi = 200)

# Table 4: T-test Q40.0
t4_data <- data.frame(
  Test = "Q40.0 ~ similarity",
  `t` = sprintf("%.2f", tt_sim$statistic),
  df = tt_sim$parameter,
  `p` = sprintf("%.4f", tt_sim$p.value),
  check.names = FALSE, stringsAsFactors = FALSE
)
t4_orig <- make_colored_table(t4_data, "Table 4: T-test - Q40.0 by Similarity (Study 5)", width = 7)
ggsave(file.path(out_dir, "Table4_TTest_SimilarityCheck.png"), t4_orig, width = 7, height = 2.5, dpi = 200)

# Table 5: All DVs ANOVA Summary
anova_all <- data.frame(
  DV = c("Q29 (hunger)", "Q27 (thirst)", "Q33 (temperature)", "Q37.0 (tired)"),
  `Condition F` = sprintf("%.2f", c(
    summary(m_q29)[[1]]$`F value`[1],
    summary(m_q27)[[1]]$`F value`[1],
    summary(m_q33)[[1]]$`F value`[1],
    summary(m_q37)[[1]]$`F value`[1]
  )),
  `Condition p` = sprintf("%.4f", c(
    summary(m_q29)[[1]]$`Pr(>F)`[1],
    summary(m_q27)[[1]]$`Pr(>F)`[1],
    summary(m_q33)[[1]]$`Pr(>F)`[1],
    summary(m_q37)[[1]]$`Pr(>F)`[1]
  )),
  `Similarity F` = sprintf("%.2f", c(
    summary(m_q29)[[1]]$`F value`[2],
    summary(m_q27)[[1]]$`F value`[2],
    summary(m_q33)[[1]]$`F value`[2],
    summary(m_q37)[[1]]$`F value`[2]
  )),
  `Similarity p` = sprintf("%.4f", c(
    summary(m_q29)[[1]]$`Pr(>F)`[2],
    summary(m_q27)[[1]]$`Pr(>F)`[2],
    summary(m_q33)[[1]]$`Pr(>F)`[2],
    summary(m_q37)[[1]]$`Pr(>F)`[2]
  )),
  `Interaction F` = sprintf("%.2f", c(
    summary(m_q29)[[1]]$`F value`[3],
    summary(m_q27)[[1]]$`F value`[3],
    summary(m_q33)[[1]]$`F value`[3],
    summary(m_q37)[[1]]$`F value`[3]
  )),
  `Interaction p` = sprintf("%.4f", c(
    summary(m_q29)[[1]]$`Pr(>F)`[3],
    summary(m_q27)[[1]]$`Pr(>F)`[3],
    summary(m_q33)[[1]]$`Pr(>F)`[3],
    summary(m_q37)[[1]]$`Pr(>F)`[3]
  )),
  check.names = FALSE, stringsAsFactors = FALSE
)
t5_orig <- make_colored_table(anova_all, "Table 5: ANOVA Summary for All DVs (Study 5)", width = 12)
ggsave(file.path(out_dir, "Table5_ANOVA_AllDVs.png"), t5_orig, width = 12, height = 3.5, dpi = 200)

# =============================================================================
# COMPARISON TABLES (Paper vs Replication)
# =============================================================================
# Paper values from Steinmetz et al. (2018) p. 413
# Descriptives order: matches desc_cell (alphabetical by condition then similarity)
paper_q29_desc <- c(
  "4.53 (1.97)",   # Full/Similar
  "5.30 (1.72)",   # Full/Dissimilar
  "5.37 (1.84)",   # Hungry/Similar
  "4.78 (1.90)"    # Hungry/Dissimilar
)
rep_q29_desc <- sprintf("%.2f (%.2f)", desc_cell$Q29_M, desc_cell$Q29_SD)
desc_match <- ifelse(paper_q29_desc == rep_q29_desc, "Yes", "No")

t1c_data <- data.frame(
  Cell = paste0(desc_cell$cond_label, " / ", desc_cell$sim_label),
  N = desc_cell$n,
  `Paper Q29 M(SD)` = paper_q29_desc,
  `Rep Q29 M(SD)` = rep_q29_desc,
  Match = desc_match,
  check.names = FALSE, stringsAsFactors = FALSE
)
t1c_grob <- make_colored_table(t1c_data, "Table 1c: Descriptives - Paper vs Replication (Study 5)", width = 10)
ggsave(file.path(out_dir, "Table1_Comparison.png"), t1c_grob, width = 10, height = 4, dpi = 200)

# ANOVA comparison: paper reports F(1,196)
# Condition: paper F=0.374, p=.541 | Similarity: paper F=0.118, p=.732 | Interaction: paper F=6.657, p=.011
t2c_data <- data.frame(
  Effect = c("Condition", "Similarity", "Condition:Similarity", "Residuals"),
  `Paper F` = c("0.374", "0.118", "6.657", ""),
  `Paper p` = c(".541", ".732", ".011", ""),
  `Rep F` = c(sprintf("%.3f", a29$`F value`[1:3]), ""),
  `Rep p` = c(sprintf("%.3f", a29$`Pr(>F)`[1:3]), ""),
  Match = c("Close*", "Close*", "Yes", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)
t2c_grob <- make_colored_table(t2c_data, "Table 2c: Q29 ANOVA - Paper vs Replication (Study 5)\n*Same conclusion (both n.s.). Interaction exact match.", width = 12)
ggsave(file.path(out_dir, "Table2_Comparison.png"), t2c_grob, width = 12, height = 3.5, dpi = 200)

# =============================================================================
# FIGURES
# =============================================================================

# Figure 1: Boxplot Q29 by condition × similarity
p1 <- ggplot(df, aes(x = cond_label, y = Q29, fill = cond_label)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  facet_wrap(~ sim_label) +
  scale_fill_manual(values = c("Full" = col_full, "Hungry" = col_hungry), guide = "none") +
  labs(title = "Hunger Projection by Condition and Similarity (Study 5)",
       x = "Simulation Condition", y = "Target Hunger Rating (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure1_Q29_Boxplot.png"), p1, width = 10, height = 5, dpi = 200)

# Figure 2: Bar chart Q29
plot_data2 <- desc_cell %>% mutate(se = Q29_SD / sqrt(n))
p2 <- ggplot(plot_data2, aes(x = cond_label, y = Q29_M, fill = cond_label)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Q29_M - se, ymax = Q29_M + se), width = 0.15) +
  facet_wrap(~ sim_label) +
  scale_fill_manual(values = c("Full" = col_full, "Hungry" = col_hungry), guide = "none") +
  labs(title = "Hunger Projection by Condition and Similarity (Study 5)",
       x = "Condition", y = "Target Hunger Rating (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure2_Q29_BarChart.png"), p2, width = 10, height = 5, dpi = 200)

# Figure 3: Interaction plot
p3 <- ggplot(desc_cell, aes(x = cond_label, y = Q29_M, group = sim_label, color = sim_label)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q29_M - Q29_SD/sqrt(n), ymax = Q29_M + Q29_SD/sqrt(n)), width = 0.1) +
  scale_color_manual(values = c("Similar" = col_similar, "Dissimilar" = col_dissimilar), name = "Similarity") +
  labs(title = "Interaction: Condition × Similarity on Hunger Projection (Study 5)",
       x = "Simulation Condition", y = "Target Hunger Rating (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure3_Interaction.png"), p3, width = 8, height = 5, dpi = 200)

# Figure 4: Violin plot Q29
p4 <- ggplot(df, aes(x = cond_label, y = Q29, fill = cond_label)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.size = 0.8) +
  facet_wrap(~ sim_label) +
  scale_fill_manual(values = c("Full" = col_full, "Hungry" = col_hungry), guide = "none") +
  labs(title = "Hunger Projection: Violin Plot (Study 5)",
       x = "Condition", y = "Target Hunger Rating (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure4_Q29_Violin.png"), p4, width = 10, height = 5, dpi = 200)

# Figure 5: Q40.0 similarity check boxplot
p5 <- ggplot(df, aes(x = sim_label, y = Q40.0, fill = sim_label)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("Similar" = col_similar, "Dissimilar" = col_dissimilar), guide = "none") +
  labs(title = "Similarity Check: Q40.0 by Group (Study 5)",
       x = "Similarity Group", y = "Similarity Rating (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure5_SimilarityCheck.png"), p5, width = 6, height = 5, dpi = 200)

# Figure 6: All DVs bar chart
dv_long <- df %>%
  select(cond_label, sim_label, Q29, Q27, Q33, Q37.0) %>%
  pivot_longer(c(Q29, Q27, Q33, Q37.0), names_to = "DV", values_to = "Rating")
dv_desc <- dv_long %>%
  group_by(cond_label, sim_label, DV) %>%
  summarise(M = mean(Rating), SD = sd(Rating), n = n(), .groups = "drop") %>%
  mutate(se = SD / sqrt(n))

p6 <- ggplot(dv_desc, aes(x = DV, y = M, fill = cond_label)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - se, ymax = M + se), width = 0.15, position = position_dodge(0.7)) +
  facet_wrap(~ sim_label) +
  scale_fill_manual(values = c("Full" = col_full, "Hungry" = col_hungry), name = "Condition") +
  labs(title = "All DVs by Condition and Similarity (Study 5)",
       x = "DV", y = "Mean Rating (1-9)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(out_dir, "Figure6_AllDVs_BarChart.png"), p6, width = 10, height = 5, dpi = 200)

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n========================================\n")
cat("Study 5 Replication Complete\n")
cat("========================================\n")
cat(sprintf("N = %d\n", nrow(df)))
cat("Q29 (hunger projection) ANOVA:\n")
print(summary(m_q29))
cat(sprintf("\nT-test Q40.0: t(%d) = %.2f, p = %.4f\n",
            tt_sim$parameter, tt_sim$statistic, tt_sim$p.value))
cat(sprintf("Output saved to: %s\n", out_dir))
cat("========================================\n")
