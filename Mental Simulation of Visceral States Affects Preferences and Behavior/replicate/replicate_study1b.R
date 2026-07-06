# ============================================================================
# Replication of Study 1b (Clean version - N = 242)
# Steinmetz, Tausen & Risen (2018) - PSPB
# 2 (simulation vs. priming) x 2 (cold vs. warm) between-subjects
# ============================================================================

library(haven); library(dplyr); library(tidyr)
library(ggplot2); library(gridExtra); library(grid); library(psych)

COL_YELLOW <- "#FFF5CC"; COL_BLUE <- "#B3D9F2"; COL_PINK <- "#FFB8C6"
COL_GREEN <- "#C6E8C6"; COL_GRAY <- "#F0F0F0"; COL_WHITE <- "#FFFFFF"

fig_dir <- "C:/Users/lenovo/Desktop/R0507/Mental Simulation of Visceral States Affects Preferences and Behavior/Figure/S1b"
rep_dir <- "C:/Users/lenovo/Desktop/R0507/Mental Simulation of Visceral States Affects Preferences and Behavior/replicate"

save_grob_png <- function(grob, filename, folder = fig_dir, width = 8, height = 4) {
  grDevices::png(file.path(folder, filename), width = width, height = height, units = "in", res = 200)
  grid.draw(grob); dev.off()
  cat(sprintf("  Saved: %s (%.1f x %.1f in)\n", filename, width, height))
}

make_table <- function(df, header_bg = COL_YELLOW, title = NULL, fontsize = 11) {
  df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  theme <- ttheme_minimal(
    core = list(fg_params = list(fontsize = fontsize, fontfamily = "sans"),
                bg_params = list(fill = rep(c(COL_WHITE, COL_GRAY), length.out = nrow(df_char)), alpha = 0.8)),
    colhead = list(fg_params = list(fontsize = fontsize + 1, fontface = "bold", fontfamily = "sans"),
                   bg_params = list(fill = header_bg, alpha = 0.9)),
    padding = unit(c(5, 7), "mm"))
  tab <- tableGrob(df_char, rows = NULL, theme = theme)
  if (!is.null(title)) {
    tg <- textGrob(title, gp = gpar(fontsize = fontsize + 3, fontface = "bold", fontfamily = "sans"), just = "center")
    tab <- gtable::gtable_add_rows(tab, heights = unit(0.7, "cm"), 0)
    tab <- gtable::gtable_add_grob(tab, tg, t = 1, l = 1, r = ncol(tab))
  }
  tab <- gtable::gtable_add_grob(tab, rectGrob(gp = gpar(fill = NA, lwd = 0.5)), t = 1, l = 1, b = nrow(tab), r = ncol(tab))
  tab
}

# ============================================================================
# 1. LOAD & FILTER — N = 242 (complete cases)
# ============================================================================
cat("=== STUDY 1b REPLICATION (N = 242) ===\n\n")

df_raw <- read_spss("C:/Users/lenovo/Desktop/R0507/Mental Simulation of Visceral States Affects Preferences and Behavior/osfstorage-archive (1)/Study1/Study1b/S1b_1.sav")
df_raw <- df_raw %>% filter(!is.na(condition))

df_raw$condition_f <- factor(df_raw[["condition"]], levels = c(1, 2), labels = c("Simulation", "Priming"))
df_raw$hot_cold_f <- factor(df_raw[["hot_cold"]], levels = c(1, 2), labels = c("Cold Picture", "Warm Picture"))

df <- df_raw %>% filter(!is.na(hot_cold), !is.na(choices))
cat(sprintf("Complete cases N = %d (paper: 242)\n\n", nrow(df)))
cat(sprintf("  Simulation-Cold: n = %d\n", sum(df$condition==1 & df$hot_cold==1, na.rm=TRUE)))
cat(sprintf("  Simulation-Warm: n = %d\n", sum(df$condition==1 & df$hot_cold==2, na.rm=TRUE)))
cat(sprintf("  Priming-Cold:    n = %d\n", sum(df$condition==2 & df$hot_cold==1, na.rm=TRUE)))
cat(sprintf("  Priming-Warm:    n = %d\n\n", sum(df$condition==2 & df$hot_cold==2, na.rm=TRUE)))

# ============================================================================
# 2. RELIABILITY
# ============================================================================
cat("--- Reliability ---\n")
calc_alpha <- function(d) {
  items <- as.data.frame(lapply(d[, c("A4rev", "A6rev", "A10rev", "A3", "A8")], as.numeric))
  psych::alpha(items)$total$raw_alpha
}
alpha_sim  <- calc_alpha(df[df$condition == 1, ])
alpha_prim <- calc_alpha(df[df$condition == 2, ])
cat(sprintf("  Simulation alpha = %.3f (paper: .64)\n", alpha_sim))
cat(sprintf("  Priming    alpha = %.3f (paper: .54)\n\n", alpha_prim))

# ============================================================================
# 3. STATISTICAL ANALYSES
# ============================================================================
aov_c <- aov(choices ~ condition_f * hot_cold_f, data = df)
s_c <- summary(aov_c)
ss_c <- s_c[[1]]$`Sum Sq`; df_c <- s_c[[1]]$Df; ms_c <- s_c[[1]]$`Mean Sq`
f_c <- s_c[[1]]$`F value`; p_c <- s_c[[1]]$`Pr(>F)`
eta_c <- ss_c[1:3] / (ss_c[1:3] + ss_c[4])

cat("--- ANOVA: choices ~ condition * hot_cold ---\n")
print(s_c)
cat(sprintf("eta2: condition=%.3f, picture=%.3f, interaction=%.3f\n\n", eta_c[1], eta_c[2], eta_c[3]))

sim <- df %>% filter(condition_f == "Simulation")
prim <- df %>% filter(condition_f == "Priming")

tsim <- t.test(choices ~ hot_cold_f, data = sim, var.equal = TRUE)
tprim <- t.test(choices ~ hot_cold_f, data = prim, var.equal = TRUE)

calc_d <- function(dat) {
  g1 <- dat$choices[dat$hot_cold == 1]; g2 <- dat$choices[dat$hot_cold == 2]
  n1 <- sum(!is.na(g1)); n2 <- sum(!is.na(g2))
  s1 <- sd(g1, na.rm=TRUE); s2 <- sd(g2, na.rm=TRUE)
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  (mean(g1, na.rm=TRUE) - mean(g2, na.rm=TRUE))/sp
}
d_sim <- calc_d(sim); d_prim <- calc_d(prim)

cat("Simple Effects:\n")
cat(sprintf("  Simulation: t(%.0f) = %.2f, p = %.4f, d = %.3f\n", tsim$parameter, tsim$statistic, tsim$p.value, d_sim))
cat(sprintf("  Priming:    t(%.0f) = %.2f, p = %.4f, d = %.3f\n\n", tprim$parameter, tprim$statistic, tprim$p.value, d_prim))

aov_mc <- aov(Q41.0 ~ condition_f * hot_cold_f, data = df)
s_mc <- summary(aov_mc)
ss_mc <- s_mc[[1]]$`Sum Sq`; eta_mc <- ss_mc[1:3] / (ss_mc[1:3] + ss_mc[4])

cat("Manipulation Check (Q41.0):\n")
print(s_mc)
cat(sprintf("eta2: condition=%.3f, picture=%.3f, interaction=%.3f\n\n", eta_mc[1], eta_mc[2], eta_mc[3]))

aov_f <- aov(Q42 ~ condition_f * hot_cold_f, data = df)
cat("Feeling Check (Q42):\n"); print(summary(aov_f))

for (dv in c("Q44", "Q46.0", "Q48.0", "Q50", "Q79")) {
  f <- as.formula(paste0(dv, " ~ condition_f * hot_cold_f"))
  cat(sprintf("\n%s:\n", dv)); print(summary(aov(f, data = df)))
}

# ============================================================================
# 4-8. TABLES
# ============================================================================
cat("--- Generating Tables ---\n")

p_fmt <- function(p) ifelse(p < 0.001, "< .001", sprintf("%.3f", p))

# Table 1: Descriptive
desc <- df %>% group_by(condition_f, hot_cold_f) %>%
  summarise(n = n(),
    Choices_M = sprintf("%.2f", mean(choices, na.rm=TRUE)),
    Choices_SD = sprintf("%.2f", sd(choices, na.rm=TRUE)),
    Check_M = sprintf("%.2f", mean(Q41.0, na.rm=TRUE)),
    Check_SD = sprintf("%.2f", sd(Q41.0, na.rm=TRUE)),
    Feel_M = sprintf("%.2f", mean(Q42, na.rm=TRUE)),
    Feel_SD = sprintf("%.2f", sd(Q42, na.rm=TRUE)),
    .groups = "drop")
colnames(desc) <- c("Task", "Picture", "n", "Choices M", "Choices SD",
                     "Check M", "Check SD", "Feeling M", "Feeling SD")
t1 <- make_table(as.data.frame(desc), header_bg = COL_YELLOW,
                  title = "Table 1: Descriptive Statistics by Condition (N = 242)", fontsize = 11)
save_grob_png(t1, "S1b_Table1_Descriptives.png", width = 10, height = 3.5)

# Table 2: ANOVA
t2 <- data.frame(
  Source = c("Task (Sim vs Priming)", "Picture (Cold vs Warm)", "Task x Picture", "Residuals"),
  SS = sprintf("%.3f", ss_c), df = df_c, MS = sprintf("%.3f", ms_c),
  F = c(sprintf("%.3f", f_c[1:3]), ""),
  p = c(p_fmt(p_c[1]), p_fmt(p_c[2]), p_fmt(p_c[3]), ""),
  eta2 = c(sprintf("%.3f", eta_c), ""))
t2 <- make_table(t2, header_bg = COL_BLUE, title = "Table 2: Factorial ANOVA — Warm Preference (DV)", fontsize = 11)
save_grob_png(t2, "S1b_Table2_ANOVA.png", width = 9, height = 3.5)

# Table 3: Manipulation Check
t3 <- data.frame(
  Source = c("Task (Sim vs Priming)", "Picture (Cold vs Warm)", "Task x Picture", "Residuals"),
  SS = sprintf("%.3f", s_mc[[1]]$`Sum Sq`), df = s_mc[[1]]$Df,
  MS = sprintf("%.3f", s_mc[[1]]$`Mean Sq`),
  F = c(sprintf("%.3f", s_mc[[1]]$`F value`[1:3]), ""),
  p = c(p_fmt(s_mc[[1]]$`Pr(>F)`[1]), p_fmt(s_mc[[1]]$`Pr(>F)`[2]), p_fmt(s_mc[[1]]$`Pr(>F)`[3]), ""),
  eta2 = c(sprintf("%.3f", eta_mc), ""))
t3 <- make_table(t3, header_bg = COL_PINK, title = "Table 3: Manipulation Check (Q41.0)", fontsize = 11)
save_grob_png(t3, "S1b_Table3_ManipCheck_ANOVA.png", width = 9, height = 3.5)

# Table 3b: Simple Effects
t3b <- data.frame(
  Comparison = c("Simulation: Cold vs Warm", "Priming: Cold vs Warm"),
  t = sprintf("%.2f", c(tsim$statistic, tprim$statistic)),
  df = c(sprintf("%.0f", tsim$parameter), sprintf("%.0f", tprim$parameter)),
  p = c(p_fmt(tsim$p.value), p_fmt(tprim$p.value)),
  d = sprintf("%.3f", c(d_sim, d_prim)),
  Direction = c("Cold > Warm ***", "Cold = Warm (n.s.)"))
t3b <- make_table(t3b, header_bg = COL_GREEN, title = "Table 3b: Simple Effects of Picture Type by Task", fontsize = 11)
save_grob_png(t3b, "S1b_Table3b_SimpleEffects.png", width = 8, height = 3)

# Table 4: Comparison
t5 <- data.frame(
  Metric = c("N", "Design", "Sim alpha", "Prim alpha",
             "Manip: Task F", "Manip: Task p", "Manip: Task eta2",
             "Manip: Picture F", "Manip: Picture p",
             "Manip: Int F", "Manip: Int p",
             "Choices Sim-Cold M(SD)", "Choices Sim-Warm M(SD)",
             "Choices Prim-Cold M(SD)", "Choices Prim-Warm M(SD)",
             "Sim: Cold vs Warm", "Prim: Cold vs Warm", "Conclusion"),
  Original = c("242", "2x2 between", ".64", ".54",
               "124.70", "< .001", ".344", "1.91", ".168",
               "1.54", ".215", "--", "--", "--", "--",
               "significant", "n.s.", "Simulation > Priming"),
  Replication = c(
    sprintf("%d", nrow(df)), "2x2 between",
    sprintf("%.2f", alpha_sim), sprintf("%.2f", alpha_prim),
    sprintf("%.2f", s_mc[[1]]$`F value`[1]), p_fmt(s_mc[[1]]$`Pr(>F)`[1]), sprintf("%.3f", eta_mc[1]),
    sprintf("%.2f", s_mc[[1]]$`F value`[2]), p_fmt(s_mc[[1]]$`Pr(>F)`[2]),
    sprintf("%.2f", s_mc[[1]]$`F value`[3]), p_fmt(s_mc[[1]]$`Pr(>F)`[3]),
    sprintf("%.2f (%.2f)", mean(sim$choices[sim$hot_cold==1]), sd(sim$choices[sim$hot_cold==1])),
    sprintf("%.2f (%.2f)", mean(sim$choices[sim$hot_cold==2]), sd(sim$choices[sim$hot_cold==2])),
    sprintf("%.2f (%.2f)", mean(prim$choices[prim$hot_cold==1]), sd(prim$choices[prim$hot_cold==1])),
    sprintf("%.2f (%.2f)", mean(prim$choices[prim$hot_cold==2]), sd(prim$choices[prim$hot_cold==2])),
    sprintf("t=%.2f, d=%.3f ***", tsim$statistic, d_sim),
    sprintf("t=%.2f, d=%.3f n.s.", tprim$statistic, d_prim),
    "Successfully Replicated"),
  Match = c("", "", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
            "", "", "", "", "Yes", "Yes", "Yes"))
t5 <- make_table(t5, header_bg = COL_GREEN, title = "Table 4: Comparison: Original Paper vs Replication", fontsize = 9)
save_grob_png(t5, "S1b_Table4_Comparison.png", width = 12, height = 6.5)

# ============================================================================
# 9. FIGURE 1: BOXPLOT 4-group
# ============================================================================
cat("--- Generating Figures ---\nGenerating Figure 1: Boxplot 4-group...\n")

df$group_label <- interaction(df$condition_f, df$hot_cold_f, sep = "\n")
levels(df$group_label) <- c("Simulation\nCold", "Simulation\nWarm", "Priming\nCold", "Priming\nWarm")

p1 <- ggplot(df, aes(x = group_label, y = choices, fill = group_label)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.85, color = "grey30", linewidth = 0.7) +
  geom_jitter(width = 0.08, alpha = 0.25, size = 1.5, color = "grey40") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  scale_fill_manual(values = c("Simulation\nCold" = COL_BLUE, "Simulation\nWarm" = COL_PINK,
                                "Priming\nCold" = COL_BLUE, "Priming\nWarm" = COL_PINK)) +
  labs(title = "Warm Preference by Task and Picture Type",
       subtitle = "Study 1b: 2 x 2 Between-Subjects (N = 242)",
       x = NULL, y = "Warm Preference (higher = prefer warming)") +
  annotate("segment", x = 1, xend = 2, y = 9.1, yend = 9.1, linewidth = 0.7) +
  annotate("text", x = 1.5, y = 9.4, label = sprintf("*** d = %.2f", d_sim), size = 4, fontface = "italic") +
  annotate("segment", x = 3, xend = 4, y = 8.3, yend = 8.3, linewidth = 0.7) +
  annotate("text", x = 3.5, y = 8.6, label = sprintf("n.s. d = %.2f", d_prim), size = 4, fontface = "italic") +
  coord_cartesian(ylim = c(0.5, 10)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
        legend.position = "none", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), plot.margin = margin(20, 20, 15, 15))
ggsave(file.path(fig_dir, "S1b_Figure1_Boxplot_4Groups.png"), p1, width = 8, height = 6, dpi = 200)

# ============================================================================
# 10. FIGURE 2: SPLIT BOXPLOTS
# ============================================================================
cat("Generating Figure 2: Split Boxplots...\n")

ann_text <- data.frame(
  condition_f = c("Simulation", "Priming"),
  label = c(sprintf("t = %.2f, d = %.3f\n***", tsim$statistic, d_sim),
            sprintf("t = %.2f, d = %.3f\nn.s.", tprim$statistic, d_prim)))

p2 <- ggplot(df, aes(x = hot_cold_f, y = choices, fill = hot_cold_f)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.85, color = "grey30", linewidth = 0.7) +
  geom_jitter(width = 0.08, alpha = 0.25, size = 1.5, color = "grey40") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  scale_fill_manual(values = c("Cold Picture" = COL_BLUE, "Warm Picture" = COL_PINK)) +
  facet_wrap(~ condition_f, ncol = 2) +
  labs(title = "Warm Preference: Simulation vs Priming by Picture Type",
       subtitle = "Study 1b (N = 242) — Mean = black diamond",
       x = NULL, y = "Warm Preference (higher = prefer warming)", fill = "Picture") +
  geom_text(data = ann_text, aes(x = 1.5, y = 9.3, label = label),
            size = 4, fontface = "italic", inherit.aes = FALSE) +
  coord_cartesian(ylim = c(0.5, 10)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "grey40"),
        legend.position = "none", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        plot.margin = margin(20, 20, 15, 15))
ggsave(file.path(fig_dir, "S1b_Figure2_Split_Boxplot.png"), p2, width = 9, height = 5.5, dpi = 200)

# ============================================================================
# 11. FIGURE 3: MANIPULATION CHECK
# ============================================================================
cat("Generating Figure 3: Manipulation Check...\n")

mc_sum <- df %>% group_by(condition_f, hot_cold_f) %>%
  summarise(M = mean(Q41.0, na.rm=TRUE), SD = sd(Q41.0, na.rm=TRUE), n = n(), SE = SD/sqrt(n), .groups = "drop")

p3 <- ggplot(mc_sum, aes(x = condition_f, y = M, fill = hot_cold_f)) +
  geom_col(position = position_dodge(0.6), width = 0.45, color = "grey30", linewidth = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), position = position_dodge(0.6), width = 0.12, linewidth = 0.9) +
  scale_fill_manual(values = c("Cold Picture" = COL_BLUE, "Warm Picture" = COL_PINK)) +
  labs(title = "Manipulation Check: Self-Imagining in Picture (Q41.0)",
       subtitle = sprintf("F(1, %d) = %.2f, p %s, eta2 = %.3f",
                          s_mc[[1]]$Df[4], s_mc[[1]]$`F value`[1], p_fmt(s_mc[[1]]$`Pr(>F)`[1]), eta_mc[1]),
       x = NULL, y = "Mean (1-9 scale, +/- SE)", fill = "Picture") +
  coord_cartesian(ylim = c(0, 10.5)) + scale_y_continuous(breaks = 1:9) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12), legend.position = "right",
        plot.margin = margin(20, 20, 15, 15))
ggsave(file.path(fig_dir, "S1b_Figure3_ManipCheck.png"), p3, width = 7, height = 5.5, dpi = 200)

# ============================================================================
# 12. FIGURE 4: FEELING CHECK
# ============================================================================
cat("Generating Figure 4: Feeling Check...\n")

feel_sum <- df %>% group_by(condition_f, hot_cold_f) %>%
  summarise(M = mean(Q42, na.rm=TRUE), SD = sd(Q42, na.rm=TRUE), n = n(), SE = SD/sqrt(n), .groups = "drop")

p4 <- ggplot(feel_sum, aes(x = condition_f, y = M, fill = hot_cold_f)) +
  geom_col(position = position_dodge(0.6), width = 0.45, color = "grey30", linewidth = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), position = position_dodge(0.6), width = 0.12, linewidth = 0.9) +
  scale_fill_manual(values = c("Cold Picture" = COL_BLUE, "Warm Picture" = COL_PINK)) +
  labs(title = "Feeling Check: How Warm/Cold Participants Feel (Q42)",
       subtitle = "No significant effects (all ps > .05)",
       x = NULL, y = "Mean (1-9 scale, +/- SE)", fill = "Picture") +
  coord_cartesian(ylim = c(0, 9)) + scale_y_continuous(breaks = 1:9) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12), legend.position = "right",
        plot.margin = margin(20, 20, 15, 15))
ggsave(file.path(fig_dir, "S1b_Figure4_FeelingCheck.png"), p4, width = 7, height = 5.5, dpi = 200)

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n==============================================================\n")
cat("  STUDY 1b — ALL OUTPUTS GENERATED (N = 242)\n")
cat("==============================================================\n")
cat("  Figure/S1b/:\n")
cat("    S1b_Table1_Descriptives.png\n")
cat("    S1b_Table2_ANOVA.png\n")
cat("    S1b_Table3_ManipCheck_ANOVA.png\n")
cat("    S1b_Table3b_SimpleEffects.png\n")
cat("    S1b_Table4_Comparison.png\n")
cat("    S1b_Figure1_Boxplot_4Groups.png\n")
cat("    S1b_Figure2_Split_Boxplot.png\n")
cat("    S1b_Figure3_ManipCheck.png\n")
cat("    S1b_Figure4_FeelingCheck.png\n")
cat("==============================================================\n")

sink(file.path(rep_dir, "S1b_analysis_numbers_N242.txt"))
cat("STUDY 1b (N = 242) - KEY STATISTICS\n")
cat("=====================================\n\n")
cat(sprintf("N = %d (complete cases)\n", nrow(df)))
cat(sprintf("  Simulation-Cold: n = %d\n", sum(df$condition==1 & df$hot_cold==1, na.rm=TRUE)))
cat(sprintf("  Simulation-Warm: n = %d\n", sum(df$condition==1 & df$hot_cold==2, na.rm=TRUE)))
cat(sprintf("  Priming-Cold:    n = %d\n", sum(df$condition==2 & df$hot_cold==1, na.rm=TRUE)))
cat(sprintf("  Priming-Warm:    n = %d\n\n", sum(df$condition==2 & df$hot_cold==2, na.rm=TRUE)))
cat(sprintf("Reliability: alpha_sim = %.3f, alpha_prim = %.3f\n\n", alpha_sim, alpha_prim))
cat("ANOVA: choices ~ task * picture\n")
cat(sprintf("  Task:        F(%d, %d) = %.3f, p = %s, eta2 = %.3f\n", df_c[1], df_c[4], f_c[1], p_fmt(p_c[1]), eta_c[1]))
cat(sprintf("  Picture:     F(%d, %d) = %.3f, p = %s, eta2 = %.3f\n", df_c[2], df_c[4], f_c[2], p_fmt(p_c[2]), eta_c[2]))
cat(sprintf("  Interaction: F(%d, %d) = %.3f, p = %s, eta2 = %.3f\n\n", df_c[3], df_c[4], f_c[3], p_fmt(p_c[3]), eta_c[3]))
cat(sprintf("Simulation: Cold vs Warm: t(%.0f) = %.2f, p = %.4f, d = %.3f\n", tsim$parameter, tsim$statistic, tsim$p.value, d_sim))
cat(sprintf("Priming:    Cold vs Warm: t(%.0f) = %.2f, p = %.4f, d = %.3f\n\n", tprim$parameter, tprim$statistic, tprim$p.value, d_prim))
cat("Manipulation check (Q41.0):\n")
cat(sprintf("  Task: F(%d, %d) = %.2f, p = %s, eta2 = %.3f\n", s_mc[[1]]$Df[1], s_mc[[1]]$Df[4], s_mc[[1]]$`F value`[1], p_fmt(s_mc[[1]]$`Pr(>F)`[1]), eta_mc[1]))
sink()
