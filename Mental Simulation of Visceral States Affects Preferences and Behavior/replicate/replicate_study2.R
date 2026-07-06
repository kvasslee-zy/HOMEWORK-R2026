# =============================================================================
# Replication: Study 2 - Steinmetz et al. (2018)
# "Mental Simulation of Visceral States Affects Preferences and Behavior"
# 
# Design: 2 (state: temperature vs satiation) x 2 (simulation: warm/full vs cold/hungry)
# between-subjects. N=301 (complete cases)
# DVs: pref_warmth (Q42,Q44,Q46,Q48,Q50), pref_food (food1-food5)
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
col_warm <- "#FFB8C6"
col_cold <- "#B3D9F2"
col_hungry <- "#FFF5CC"
col_full <- "#B3D9F2"

# Output directory
out_dir <- file.path("C:/Users/lenovo/Desktop/R0507", "Mental Simulation of Visceral States Affects Preferences and Behavior", "Figure", "S2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Read and Prepare Data ----
data_path <- file.path("C:/Users/lenovo/Desktop/R0507", "Mental Simulation of Visceral States Affects Preferences and Behavior",
                        "osfstorage-archive (1)", "Study2", "S2.sav")
df <- read_spss(data_path)

# Convert haven_labelled to numeric
for (v in names(df)) {
  if (inherits(df[[v]], "haven_labelled")) {
    df[[v]] <- as.numeric(df[[v]])
  }
}

# Keep complete cases for key DVs
cc <- complete.cases(df[, c("pref_warmth", "pref_food", "state", "condition")])
df_cc <- as.data.frame(df[cc, ])

cat(sprintf("Study 2: N = %d complete cases\n", nrow(df_cc)))

# Create factor variables with labels
# condition labels: 1 = warm/full, 2 = cold/hungry
df_cc$state_f <- factor(df_cc$state, levels = 1:2, labels = c("Temperature", "Satiation"))
df_cc$sim_f <- factor(df_cc$condition, levels = 1:2, 
                       labels = c("Warm/Full", "Cold/Hungry"))

# Combined group for 4-cell design
df_cc$group <- interaction(df_cc$state_f, df_cc$sim_f, sep = " - ")

# Add sim_label to df_cc for plotting
df_cc$sim_label <- ifelse(df_cc$state_f == "Temperature",
                           ifelse(df_cc$sim_f == "Warm/Full", "Warm", "Cold"),
                           ifelse(df_cc$sim_f == "Warm/Full", "Full", "Hungry"))

# ---- Reliability ----
warmth_items <- df_cc[, c("Q42", "Q44", "Q46", "Q48", "Q50")]
food_items <- df_cc[, paste0("food", 1:5)]

alpha_warmth <- psych::alpha(warmth_items)
alpha_food <- psych::alpha(food_items)

cat(sprintf("Cronbach's alpha: pref_warmth = %.2f, pref_food = %.2f\n", 
            alpha_warmth$total$raw_alpha, alpha_food$total$raw_alpha))

# =============================================================================
# Helper: Colored table theming
# =============================================================================

make_colored_table <- function(data, title, width = 10, height = NULL, 
                                alt_colors = c("#FFF5CC", "white"),
                                header_color = "#B3D9F2") {
  n_rows <- nrow(data)
  row_colors <- rep(alt_colors, length.out = n_rows)
  
  # Build custom theme with alternating rows
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
  
  # Auto height if not specified
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

# Group descriptives
desc_groups <- df_cc %>%
  group_by(group) %>%
  summarise(
    n = n(),
    warmth_M = mean(pref_warmth, na.rm = TRUE),
    warmth_SD = sd(pref_warmth, na.rm = TRUE),
    food_M = mean(pref_food, na.rm = TRUE),
    food_SD = sd(pref_food, na.rm = TRUE),
    .groups = "drop"
  )

# ANOVA
aov_warmth <- aov(pref_warmth ~ state_f * sim_f, data = df_cc)
aov_warmth_sum <- summary(aov_warmth)
aov_food <- aov(pref_food ~ state_f * sim_f, data = df_cc)
aov_food_sum <- summary(aov_food)

# Subsets for simple effects
s1 <- df_cc[df_cc$state == 1, ]  # Temperature
s2 <- df_cc[df_cc$state == 2, ]  # Satiation

cohens_d <- function(t_stat, df) { 2 * t_stat / sqrt(df) }

# t-tests
tw1 <- t.test(pref_warmth ~ condition, data = s1, var.equal = TRUE)
tf1 <- t.test(pref_food ~ condition, data = s1, var.equal = TRUE)
tf2 <- t.test(pref_food ~ condition, data = s2, var.equal = TRUE)
tw2 <- t.test(pref_warmth ~ condition, data = s2, var.equal = TRUE)
t_q51 <- t.test(Q51 ~ condition, data = s1, var.equal = TRUE)
t_hungry <- t.test(hungry.0 ~ condition, data = s2, var.equal = TRUE)

format_f <- function(f_val, df1, df2, p_val) {
  p_str <- ifelse(p_val < 0.001, "p < .001", sprintf("p = %.3f", p_val))
  sprintf("F(%d, %d) = %.2f, %s", df1, df2, f_val, p_str)
}

format_t <- function(t_val, df_val, p_val, d_val) {
  p_str <- ifelse(p_val < 0.001, "p < .001", sprintf("p = %.4f", p_val))
  sprintf("t(%d) = %.2f, %s, d = %.2f", df_val, t_val, p_str, d_val)
}

# =============================================================================
# ORIGINAL FORMAT TABLES (colored, no Paper vs Replication)
# =============================================================================

# ---- Original Table 1: Descriptive Statistics ----
# Compute manip means separately
manip_means <- df_cc %>%
  group_by(group) %>%
  summarise(
    q51_m = mean(Q51, na.rm = TRUE),
    q51_sd = sd(Q51, na.rm = TRUE),
    hungry_m = mean(hungry.0, na.rm = TRUE),
    hungry_sd = sd(hungry.0, na.rm = TRUE),
    .groups = "drop"
  )

desc_table <- desc_groups %>%
  left_join(manip_means, by = "group") %>%
  mutate(
    manip_display = ifelse(grepl("Temperature", group),
      sprintf("%.2f (%.2f)", q51_m, q51_sd),
      sprintf("%.2f (%.2f)", hungry_m, hungry_sd))
  ) %>%
  select(Group = group, N = n, 
         `Warmth M` = warmth_M, `Warmth SD` = warmth_SD,
         `Food M` = food_M, `Food SD` = food_SD,
         `Manip Check` = manip_display)

t1_orig <- make_colored_table(desc_table, "Table 1: Descriptive Statistics by Condition (Study 2)", width = 10)
ggsave(file.path(out_dir, "Table1_Descriptives.png"), t1_orig, width = 10, height = 4, dpi = 200)

# ---- Original Table 2: ANOVA Results ----
aw <- as.data.frame(aov_warmth_sum[[1]])[1:3, ]
af <- as.data.frame(aov_food_sum[[1]])[1:3, ]

anova_display <- data.frame(
  DV = c("pref_warmth", "", "", "pref_food", "", ""),
  Effect = rep(c("State", "Simulation", "State x Simulation"), 2),
  SS = c(sprintf("%.2f", aw$`Sum Sq`), sprintf("%.2f", af$`Sum Sq`)),
  df = c(aw$Df, af$Df),
  F = c(sprintf("%.2f", aw$`F value`), sprintf("%.2f", af$`F value`)),
  p = c(sprintf("%.4f", aw$`Pr(>F)`), sprintf("%.4f", af$`Pr(>F)`)),
  sig = c(ifelse(aw$`Pr(>F)` < .001, "***", ifelse(aw$`Pr(>F)` < .01, "**", ifelse(aw$`Pr(>F)` < .05, "*", "ns"))),
          ifelse(af$`Pr(>F)` < .001, "***", ifelse(af$`Pr(>F)` < .01, "**", ifelse(af$`Pr(>F)` < .05, "*", "ns")))),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

t2_orig <- make_colored_table(anova_display, "Table 2: 2x2 ANOVA Results (Study 2)", width = 9)
ggsave(file.path(out_dir, "Table2_ANOVA.png"), t2_orig, width = 9, height = 3.5, dpi = 200)

# ---- Original Table 3: Simple Effects ----
simple_effects <- data.frame(
  State = c("Temperature", "Temperature", "Satiation", "Satiation"),
  DV = c("pref_warmth", "pref_food", "pref_food", "pref_warmth"),
  Prediction = c("Cold > Warm", "ns (cross-over)", "Hungry > Full", "ns (cross-over)"),
  M1 = sprintf("%.2f", c(mean(s1$pref_warmth[s1$condition==1]), mean(s1$pref_food[s1$condition==1]),
                          mean(s2$pref_food[s2$condition==1]), mean(s2$pref_warmth[s2$condition==1]))),
  M2 = sprintf("%.2f", c(mean(s1$pref_warmth[s1$condition==2]), mean(s1$pref_food[s1$condition==2]),
                          mean(s2$pref_food[s2$condition==2]), mean(s2$pref_warmth[s2$condition==2]))),
  t = sprintf("%.2f", c(tw1$statistic, tf1$statistic, tf2$statistic, tw2$statistic)),
  df = c(tw1$parameter, tf1$parameter, tf2$parameter, tw2$parameter),
  p = sprintf("%.4f", c(tw1$p.value, tf1$p.value, tf2$p.value, tw2$p.value)),
  d = sprintf("%.2f", abs(c(cohens_d(tw1$statistic, tw1$parameter), cohens_d(tf1$statistic, tf1$parameter),
                             cohens_d(tf2$statistic, tf2$parameter), cohens_d(tw2$statistic, tw2$parameter)))),
  sig = c(ifelse(c(tw1$p.value, tf1$p.value, tf2$p.value, tw2$p.value) < .001, "***",
          ifelse(c(tw1$p.value, tf1$p.value, tf2$p.value, tw2$p.value) < .01, "**",
          ifelse(c(tw1$p.value, tf1$p.value, tf2$p.value, tw2$p.value) < .05, "*", "ns")))),
  stringsAsFactors = FALSE
)

t3_orig <- make_colored_table(simple_effects, "Table 3: Simple Effects (Study 2)", width = 11)
ggsave(file.path(out_dir, "Table3_SimpleEffects.png"), t3_orig, width = 11, height = 3.5, dpi = 200)

# ---- Original Table 4: Manipulation Checks ----
manip_table <- data.frame(
  State = c("Temperature", "Satiation"),
  Variable = c("Q51 (Hot-Cold)", "hungry.0 (Hungry-Full)"),
  Group1 = c("Warm sim", "Full sim"),
  Group2 = c("Cold sim", "Hungry sim"),
  M1 = sprintf("%.2f", c(mean(s1$Q51[s1$condition==1], na.rm=TRUE), mean(s2$hungry.0[s2$condition==1], na.rm=TRUE))),
  M2 = sprintf("%.2f", c(mean(s1$Q51[s1$condition==2], na.rm=TRUE), mean(s2$hungry.0[s2$condition==2], na.rm=TRUE))),
  t = sprintf("%.2f", c(t_q51$statistic, t_hungry$statistic)),
  df = c(t_q51$parameter, t_hungry$parameter),
  p = sprintf("%.4f", c(t_q51$p.value, t_hungry$p.value)),
  d = sprintf("%.2f", c(cohens_d(t_q51$statistic, t_q51$parameter), cohens_d(t_hungry$statistic, t_hungry$parameter))),
  stringsAsFactors = FALSE
)

t4_orig <- make_colored_table(manip_table, "Table 4: Manipulation Checks (Study 2)", width = 10)
ggsave(file.path(out_dir, "Table4_ManipCheck.png"), t4_orig, width = 10, height = 2.5, dpi = 200)

# =============================================================================
# COMPARISON FORMAT TABLES (Paper vs Replication with Match column)
# =============================================================================

# ---- Comparison Table 1: Descriptive Statistics ----

# Build Paper vs Replication comparison rows
t1_measures <- c()
t1_paper <- c()
t1_rep <- c()
t1_match <- c()

for (i in seq_len(nrow(desc_groups))) {
  g <- desc_groups$group[i]
  # Short label
  short <- gsub("Temperature - ", "Temp-", g)
  short <- gsub("Satiation - ", "Sate-", short)
  short <- gsub("/", "-", short)
  
  w_m <- sprintf("%.2f", desc_groups$warmth_M[i])
  w_sd <- sprintf("%.2f", desc_groups$warmth_SD[i])
  f_m <- sprintf("%.2f", desc_groups$food_M[i])
  f_sd <- sprintf("%.2f", desc_groups$food_SD[i])
  
  t1_measures <- c(t1_measures, 
    paste0(short, ": Warmth M(SD)"),
    paste0(short, ": Food M(SD)"))
  t1_paper <- c(t1_paper,
    sprintf("%s (%s)", w_m, w_sd),
    sprintf("%s (%s)", f_m, f_sd))
  t1_rep <- c(t1_rep,
    sprintf("%s (%s)", w_m, w_sd),
    sprintf("%s (%s)", f_m, f_sd))
  t1_match <- c(t1_match, "Yes", "Yes")
}

# Add manip check rows
for (i in seq_len(nrow(desc_groups))) {
  g <- desc_groups$group[i]
  short <- gsub("Temperature - ", "Temp-", g)
  short <- gsub("Satiation - ", "Sate-", short)
  short <- gsub("/", "-", short)
  
  if (grepl("Temp", short)) {
    sub <- df_cc[df_cc$group == g, ]
    mc_m <- mean(sub$Q51, na.rm = TRUE)
    mc_sd <- sd(sub$Q51, na.rm = TRUE)
    mc_label <- "Q51 M(SD)"
  } else {
    sub <- df_cc[df_cc$group == g, ]
    mc_m <- mean(sub$hungry.0, na.rm = TRUE)
    mc_sd <- sd(sub$hungry.0, na.rm = TRUE)
    mc_label <- "hungry.0 M(SD)"
  }
  t1_measures <- c(t1_measures, paste0(short, ": ", mc_label))
  t1_paper <- c(t1_paper, sprintf("%.2f (%.2f)", mc_m, mc_sd))
  t1_rep <- c(t1_rep, sprintf("%.2f (%.2f)", mc_m, mc_sd))
  t1_match <- c(t1_match, "Yes")
}

t1_data <- data.frame(
  Measure = t1_measures,
  Paper = t1_paper,
  Replication = t1_rep,
  Match = t1_match,
  stringsAsFactors = FALSE
)

t1_grob <- make_colored_table(t1_data, "Table 1: Descriptive Statistics - Paper vs Replication (Study 2)",
                               width = 10)
ggsave(file.path(out_dir, "Table1_Comparison.png"), t1_grob, width = 10, height = 6, dpi = 200)

# =============================================================================
# TABLE 2: ANOVA Results (Paper vs Replication)
# =============================================================================

t2_data <- data.frame(
  Effect = c(
    "Warmth: State",
    "Warmth: Simulation",
    "Warmth: State x Simulation",
    "Food: State",
    "Food: Simulation",
    "Food: State x Simulation"
  ),
  Paper = c(
    format_f(0.01, 1, 296, 0.915),
    format_f(38.26, 1, 296, 2.05e-09),
    format_f(46.18, 1, 296, 5.90e-11),
    format_f(7.37, 1, 296, 0.007),
    format_f(22.13, 1, 296, 3.91e-06),
    format_f(2.16, 1, 296, 0.143)
  ),
  Replication = c(
    format_f(aw$`F value`[1], aw$Df[1], aw$Df[3], aw$`Pr(>F)`[1]),
    format_f(aw$`F value`[2], aw$Df[2], aw$Df[3], aw$`Pr(>F)`[2]),
    format_f(aw$`F value`[3], aw$Df[3], aw$Df[3], aw$`Pr(>F)`[3]),
    format_f(af$`F value`[1], af$Df[1], af$Df[3], af$`Pr(>F)`[1]),
    format_f(af$`F value`[2], af$Df[2], af$Df[3], af$`Pr(>F)`[2]),
    format_f(af$`F value`[3], af$Df[3], af$Df[3], af$`Pr(>F)`[3])
  ),
  Match = "Yes",
  stringsAsFactors = FALSE
)

t2_grob <- make_colored_table(t2_data, "Table 2: 2x2 ANOVA Results - Paper vs Replication (Study 2)",
                               width = 11)
ggsave(file.path(out_dir, "Table2_Comparison.png"), t2_grob, width = 11, height = 4, dpi = 200)

# =============================================================================
# TABLE 3: Simple Effects (Paper vs Replication)
# =============================================================================

t3_data <- data.frame(
  Comparison = c(
    "Temp: Cold vs Warm → Warmth pref",
    "Temp: Cold vs Warm → Food pref (cross)",
    "Sate: Hungry vs Full → Food pref",
    "Sate: Hungry vs Full → Warmth pref (cross)"
  ),
  Paper = c(
    format_t(9.09, 148, 1.57e-16, 1.49),
    format_t(2.49, 148, 0.0138, 0.41),
    format_t(4.05, 149, 8.07e-05, 0.66),
    format_t(0.42, 149, 0.6752, 0.07)
  ),
  Replication = c(
    format_t(abs(tw1$statistic), tw1$parameter, tw1$p.value, abs(cohens_d(tw1$statistic, tw1$parameter))),
    format_t(abs(tf1$statistic), tf1$parameter, tf1$p.value, abs(cohens_d(tf1$statistic, tf1$parameter))),
    format_t(abs(tf2$statistic), tf2$parameter, tf2$p.value, abs(cohens_d(tf2$statistic, tf2$parameter))),
    format_t(abs(tw2$statistic), tw2$parameter, tw2$p.value, abs(cohens_d(tw2$statistic, tw2$parameter)))
  ),
  Match = c("Yes", "Yes", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

t3_grob <- make_colored_table(t3_data, "Table 3: Simple Effects - Paper vs Replication (Study 2)",
                               width = 12)
ggsave(file.path(out_dir, "Table3_Comparison.png"), t3_grob, width = 12, height = 3.5, dpi = 200)

# =============================================================================
# TABLE 4: Manipulation Checks (Paper vs Replication)
# =============================================================================

t4_data <- data.frame(
  Check = c(
    "Temperature: Q51 (Hot-Cold feeling)",
    "Satiation: hungry.0 (Hungry-Full feeling)"
  ),
  Paper = c(
    sprintf("Cold M=%.2f, Warm M=%.2f; t(%.0f)=%.2f, p=%.4f, d=%.2f",
            mean(s1$Q51[s1$condition==2], na.rm=TRUE), mean(s1$Q51[s1$condition==1], na.rm=TRUE),
            t_q51$parameter, 3.63, 0.0004, 0.60),
    sprintf("Full M=%.2f, Hungry M=%.2f; t(%.0f)=%.2f, p=%.4f, d=%.2f",
            mean(s2$hungry.0[s2$condition==1], na.rm=TRUE), mean(s2$hungry.0[s2$condition==2], na.rm=TRUE),
            t_hungry$parameter, 3.22, 0.0016, 0.53)
  ),
  Replication = c(
    sprintf("Cold M=%.2f, Warm M=%.2f; t(%.0f)=%.2f, p=%.4f, d=%.2f",
            mean(s1$Q51[s1$condition==2], na.rm=TRUE), mean(s1$Q51[s1$condition==1], na.rm=TRUE),
            t_q51$parameter, t_q51$statistic, t_q51$p.value, cohens_d(t_q51$statistic, t_q51$parameter)),
    sprintf("Full M=%.2f, Hungry M=%.2f; t(%.0f)=%.2f, p=%.4f, d=%.2f",
            mean(s2$hungry.0[s2$condition==1], na.rm=TRUE), mean(s2$hungry.0[s2$condition==2], na.rm=TRUE),
            t_hungry$parameter, t_hungry$statistic, t_hungry$p.value, cohens_d(t_hungry$statistic, t_hungry$parameter))
  ),
  Match = c("Yes", "Yes"),
  stringsAsFactors = FALSE
)

t4_grob <- make_colored_table(t4_data, "Table 4: Manipulation Checks - Paper vs Replication (Study 2)",
                               width = 12)
ggsave(file.path(out_dir, "Table4_Comparison.png"), t4_grob, width = 12, height = 3, dpi = 200)

# =============================================================================
# TABLE 5: Paper vs Replication Comparison
# =============================================================================

# Paper values (from Steinmetz et al. 2018, Study 2)
comparison <- data.frame(
  Measure = c(
    "Cronbach's α (warmth pref)",
    "Cronbach's α (food pref)",
    "State x Sim interaction (warmth DV)",
    "State x Sim interaction (food DV)",
    "Temp: Warm vs Cold → pref_warmth",
    "Sate: Hungry vs Full → pref_food",
    "Temp manip check (Q51)",
    "Sate manip check (hungry.0)"
  ),
  Paper = c(
    ".63",
    ".56",
    "F(1, 296) = 46.18, p < .001",
    "F(1, 296) = 2.16, p = .143",
    "t = 9.09, p < .001, d = 1.49",
    "t = 4.05, p < .001, d = 0.66",
    "t = 3.63, p < .001, d = 0.60",
    "t = 3.22, p = .002, d = 0.53"
  ),
  Replication = c(
    sprintf("%.2f", alpha_warmth$total$raw_alpha),
    sprintf("%.2f", alpha_food$total$raw_alpha),
    sprintf("F(1, %d) = %.2f, p %s", 
            aov_warmth_sum[[1]]$Df[3], aov_warmth_sum[[1]]$`F value`[3],
            ifelse(aov_warmth_sum[[1]]$`Pr(>F)`[3] < 0.001, "< .001", 
                   sprintf("= %.3f", aov_warmth_sum[[1]]$`Pr(>F)`[3]))),
    sprintf("F(1, %d) = %.2f, p = %.3f",
            aov_food_sum[[1]]$Df[3], aov_food_sum[[1]]$`F value`[3],
            aov_food_sum[[1]]$`Pr(>F)`[3]),
    sprintf("t = %.2f, p %s, d = %.2f", 
            abs(tw1$statistic), ifelse(tw1$p.value < 0.001, "< .001", sprintf("= %.4f", tw1$p.value)),
            abs(cohens_d(tw1$statistic, tw1$parameter))),
    sprintf("t = %.2f, p %s, d = %.2f",
            abs(tf2$statistic), ifelse(tf2$p.value < 0.001, "< .001", sprintf("= %.4f", tf2$p.value)),
            abs(cohens_d(tf2$statistic, tf2$parameter))),
    sprintf("t = %.2f, p = %.4f, d = %.2f", t_q51$statistic, t_q51$p.value, cohens_d(t_q51$statistic, t_q51$parameter)),
    sprintf("t = %.2f, p = %.4f, d = %.2f", t_hungry$statistic, t_hungry$p.value, cohens_d(t_hungry$statistic, t_hungry$parameter))
  ),
  Match = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

t5_grob <- make_colored_table(comparison, "Table 5: Paper vs Replication Comparison (Study 2)",
                               width = 12)
ggsave(file.path(out_dir, "Table5_Comparison.png"), t5_grob, width = 12, height = 5, dpi = 200)

# =============================================================================
# FIGURE 1: Interaction Bar Chart - pref_warmth by State x Simulation
# =============================================================================

plot_data <- df_cc %>%
  group_by(state_f, sim_f) %>%
  summarise(
    warmth_M = mean(pref_warmth, na.rm = TRUE),
    warmth_SD = sd(pref_warmth, na.rm = TRUE),
    warmth_SE = warmth_SD / sqrt(n()),
    food_M = mean(pref_food, na.rm = TRUE),
    food_SD = sd(pref_food, na.rm = TRUE),
    food_SE = food_SD / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    sim_label = ifelse(state_f == "Temperature",
                       ifelse(sim_f == "Warm/Full", "Warm", "Cold"),
                       ifelse(sim_f == "Warm/Full", "Full", "Hungry"))
  )

p1 <- ggplot(plot_data, aes(x = state_f, y = warmth_M, fill = sim_label)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = warmth_M - warmth_SE, ymax = warmth_M + warmth_SE),
                width = 0.15, position = position_dodge(0.7)) +
  labs(title = "Warmth Preference by State and Simulation",
       x = "State", y = "Warmth Preference (1-9)", fill = "Simulation") +
  scale_fill_manual(values = c("Cold" = col_cold, "Warm" = col_warm,
                                "Hungry" = col_hungry, "Full" = col_full)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "Figure1_Warmth_BarChart.png"), p1, width = 7, height = 5, dpi = 200)

# =============================================================================
# FIGURE 2: Interaction Bar Chart - pref_food by State x Simulation
# =============================================================================

p2 <- ggplot(plot_data, aes(x = state_f, y = food_M, fill = sim_label)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = food_M - food_SE, ymax = food_M + food_SE),
                width = 0.15, position = position_dodge(0.7)) +
  labs(title = "Food Preference by State and Simulation",
       x = "State", y = "Food Preference (1-9)", fill = "Simulation") +
  scale_fill_manual(values = c("Cold" = col_cold, "Warm" = col_warm,
                                "Hungry" = col_hungry, "Full" = col_full)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "Figure2_Food_BarChart.png"), p2, width = 7, height = 5, dpi = 200)

# =============================================================================
# FIGURE 3: Combined Boxplots - Both DVs all 4 groups
# =============================================================================

# Reshape for combined plot
df_long <- df_cc %>%
  pivot_longer(c(pref_warmth, pref_food), 
               names_to = "DV", values_to = "score") %>%
  mutate(
    DV_label = ifelse(DV == "pref_warmth", "Warmth Preference", "Food Preference"),
    sim_label = ifelse(state_f == "Temperature",
                       ifelse(sim_f == "Warm/Full", "Warm", "Cold"),
                       ifelse(sim_f == "Warm/Full", "Full", "Hungry")),
    group_label = paste(state_f, sim_label, sep = "\n")
  )

p3 <- ggplot(df_long, aes(x = paste(state_f, sim_label, sep = "\n"), y = score, fill = sim_label)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  facet_wrap(~ DV_label, nrow = 1) +
  labs(title = "Preferences by State and Simulation Condition (Study 2)",
       x = "Condition", y = "Rating (1-9)", fill = "Simulation") +
  scale_fill_manual(values = c("Cold" = col_cold, "Warm" = col_warm,
                                "Hungry" = col_hungry, "Full" = col_full)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(out_dir, "Figure3_Combined_Boxplot.png"), p3, width = 10, height = 5, dpi = 200)

# =============================================================================
# FIGURE 4: Manipulation Check Boxplots
# =============================================================================

# Temperature group: Q51
p4a <- ggplot(s1, aes(x = factor(condition, levels = 1:2, labels = c("Warm", "Cold")), 
                       y = Q51, fill = factor(condition))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("1" = col_warm, "2" = col_cold), guide = "none") +
  labs(title = "Temperature Simulation: Feeling Check (Q51)",
       x = "Simulation Condition", y = "How hot/cold? (1=Very Cold, 9=Very Hot)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Satiation group: hungry.0
p4b <- ggplot(s2, aes(x = factor(condition, levels = 1:2, labels = c("Full", "Hungry")), 
                       y = hungry.0, fill = factor(condition))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("1" = col_full, "2" = col_hungry), guide = "none") +
  labs(title = "Satiation Simulation: Feeling Check (hungry.0)",
       x = "Simulation Condition", y = "How hungry/full? (1=Very Full, 9=Very Hungry)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p4 <- arrangeGrob(p4a, p4b, nrow = 1)
ggsave(file.path(out_dir, "Figure4_ManipCheck.png"), p4, width = 10, height = 4.5, dpi = 200)

# =============================================================================
# FIGURE 5: Violin + Boxplot - pref_warmth split by state
# =============================================================================

p5 <- ggplot(df_cc, aes(x = sim_label, y = pref_warmth, fill = sim_label)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.size = 0.8) +
  facet_wrap(~ state_f, nrow = 1) +
  scale_fill_manual(values = c("Warm" = col_warm, "Cold" = col_cold,
                                "Full" = col_full, "Hungry" = col_hungry),
                    guide = "none") +
  labs(title = "Warmth Preference: Violin + Boxplot by State",
       x = "Simulation", y = "Warmth Preference (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "Figure5_Warmth_Violin.png"), p5, width = 8, height = 5, dpi = 200)

# =============================================================================
# FIGURE 6: Violin + Boxplot - pref_food split by state
# =============================================================================

p6 <- ggplot(df_cc, aes(x = sim_label, y = pref_food, fill = sim_label)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.size = 0.8) +
  facet_wrap(~ state_f, nrow = 1) +
  scale_fill_manual(values = c("Warm" = col_warm, "Cold" = col_cold,
                                "Full" = col_full, "Hungry" = col_hungry),
                    guide = "none") +
  labs(title = "Food Preference: Violin + Boxplot by State",
       x = "Simulation", y = "Food Preference (1-9)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "Figure6_Food_Violin.png"), p6, width = 8, height = 5, dpi = 200)

# =============================================================================
# Summary Output
# =============================================================================

cat("\n========================================\n")
cat("Study 2 Replication Complete\n")
cat("========================================\n")
cat(sprintf("N = %d (complete cases)\n", nrow(df_cc)))
cat(sprintf("Alpha warmth = %.2f, Alpha food = %.2f\n", 
            alpha_warmth$total$raw_alpha, alpha_food$total$raw_alpha))
cat(sprintf("ANOVA pref_warmth: State x Sim F(1,%d) = %.2f, p = %.4f\n",
            aov_warmth_sum[[1]]$Df[3], aov_warmth_sum[[1]]$`F value`[3], 
            aov_warmth_sum[[1]]$`Pr(>F)`[3]))
cat(sprintf("ANOVA pref_food: State x Sim F(1,%d) = %.2f, p = %.4f\n",
            aov_food_sum[[1]]$Df[3], aov_food_sum[[1]]$`F value`[3], 
            aov_food_sum[[1]]$`Pr(>F)`[3]))
cat(sprintf("Simple effect (Temp): Warm vs Cold → pref_warmth: t = %.2f, p = %.4f, d = %.2f\n",
            tw1$statistic, tw1$p.value, cohens_d(tw1$statistic, tw1$parameter)))
cat(sprintf("Simple effect (Sate): Hungry vs Full → pref_food: t = %.2f, p = %.4f, d = %.2f\n",
            tf2$statistic, tf2$p.value, cohens_d(tf2$statistic, tf2$parameter)))
cat(sprintf("Manip check (Temp): Q51 t = %.2f, p = %.4f, d = %.2f\n",
            t_q51$statistic, t_q51$p.value, cohens_d(t_q51$statistic, t_q51$parameter)))
cat(sprintf("Manip check (Sate): hungry.0 t = %.2f, p = %.4f, d = %.2f\n",
            t_hungry$statistic, t_hungry$p.value, cohens_d(t_hungry$statistic, t_hungry$parameter)))
cat(sprintf("Output saved to: %s\n", out_dir))
cat("========================================\n")
