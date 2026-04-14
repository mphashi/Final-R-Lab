# =============================================================================
# VTPEH 6270 - Check Point 06: Statistical Analyses
# Author: Ashita Singhal
# Date: 2026-04-13
#
# Research Question: Is physical inactivity associated with obesity prevalence
# across U.S. states?
#
# Data: CDC Nutrition, Physical Activity, and Obesity - BRFSS (2011-2024)
#
# GitHub Repository: https://github.com/mphashi/Final-R-Lab
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

setwd("/Users/ashitasinghal/Desktop/CORNELL /6270:R:Biostat/Checkpoints")

# Install any missing packages
required_packages <- c("tidyverse", "ggplot2", "scales", "broom", "ggrepel")
to_install <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(ggplot2)
library(scales)
library(broom)
library(ggrepel)


# =============================================================================
# 1. LOAD DATA
# =============================================================================

df_raw <- read.csv(
  "Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv",
  encoding         = "latin1",
  stringsAsFactors = FALSE
)

cat("Raw dataset dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")


# =============================================================================
# 2. DATA CLEANING & TRANSFORMATION
# =============================================================================

# Define questions of interest
pa_question <- "Percent of adults who engage in no leisure-time physical activity"
ob_question <- "Percent of adults aged 18 years and older who have obesity"

# Subset physical inactivity data (Total stratum only)
pa_df <- df_raw %>%
  filter(Question == pa_question,
         StratificationCategory1 == "Total") %>%
  select(YearStart, LocationDesc, pct_inactive = Data_Value)

# Subset obesity data (Total stratum only)
ob_df <- df_raw %>%
  filter(Question == ob_question,
         StratificationCategory1 == "Total") %>%
  select(YearStart, LocationDesc, pct_obese = Data_Value)

# Merge on state and year, drop missing values
df <- pa_df %>%
  inner_join(ob_df, by = c("YearStart", "LocationDesc")) %>%
  drop_na(pct_inactive, pct_obese)

cat("Analytic dataset: ", nrow(df), "state-year observations\n")
cat("States/territories:", n_distinct(df$LocationDesc), "\n")
cat("Years covered:     ", min(df$YearStart), "-", max(df$YearStart), "\n")


# =============================================================================
# 3. DESCRIPTIVE STATISTICS
# =============================================================================

summary_tbl <- df %>%
  summarise(
    N_state_years      = n(),
    Mean_inactive_pct  = round(mean(pct_inactive), 1),
    SD_inactive        = round(sd(pct_inactive), 1),
    Mean_obese_pct     = round(mean(pct_obese), 1),
    SD_obese           = round(sd(pct_obese), 1)
  )

print(summary_tbl)


# =============================================================================
# 4. FIGURES
# =============================================================================

# ── Figure theme (clean, minimal) ────────────────────────────────────────────
theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 1),
      plot.subtitle     = element_text(size = base_size - 1, colour = "grey40"),
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size - 1),
      legend.title      = element_text(size = base_size - 1),
      legend.text       = element_text(size = base_size - 2),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey90"),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA)
    )
}

# ── Figure 1: National trends ─────────────────────────────────────────────────
trend_df <- df %>%
  group_by(YearStart) %>%
  summarise(
    `No Leisure-Time Physical Activity` = mean(pct_inactive, na.rm = TRUE),
    `Obesity`                           = mean(pct_obese,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-YearStart, names_to = "Indicator", values_to = "Percent")

fig1 <- ggplot(trend_df, aes(x = YearStart, y = Percent, colour = Indicator)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = c(
    "No Leisure-Time Physical Activity" = "#e07b39",
    "Obesity"                           = "#4477aa"
  )) +
  scale_x_continuous(breaks = 2011:2024) +
  labs(
    title    = "Trends in Physical Inactivity and Obesity Prevalence",
    subtitle = "Mean across U.S. states, 2011-2024 (CDC BRFSS)",
    x        = "Year",
    y        = "Prevalence (%)",
    colour   = NULL
  ) +
  theme_report() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Save Figure 1
pdf("figure1_trends.pdf", width = 7.2, height = 4.5)
print(fig1)
dev.off()
cat("Saved: figure1_trends.pdf\n")


# ── Figure 2: Scatter plot — inactivity vs obesity ────────────────────────────
label_df <- df %>%
  filter(YearStart == 2023) %>%
  arrange(desc(pct_obese)) %>%
  slice(c(1:3, (n() - 2):n()))

fig2 <- ggplot(df, aes(x = pct_inactive, y = pct_obese)) +
  geom_point(aes(colour = factor(YearStart)), alpha = 0.45, size = 1.6) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#2166ac", fill = "#bdd7e7", linewidth = 1.1) +
  geom_text_repel(
    data          = label_df,
    aes(label     = LocationDesc),
    size          = 2.8,
    colour        = "grey30",
    max.overlaps  = 8
  ) +
  scale_colour_viridis_d(name = "Year", option = "C") +
  labs(
    title    = "Physical Inactivity vs. Obesity Prevalence",
    subtitle = "State-level estimates, CDC BRFSS 2011-2024",
    x        = "Adults with No Leisure-Time Physical Activity (%)",
    y        = "Adults with Obesity (%)"
  ) +
  theme_report() +
  theme(legend.key.size = unit(0.4, "cm"))

# Save Figure 2
pdf("figure2_scatter.pdf", width = 7.2, height = 5.5)
print(fig2)
dev.off()
cat("Saved: figure2_scatter.pdf\n")


# ── Figure 3: Regression diagnostics ─────────────────────────────────────────
m_simple <- lm(pct_obese ~ pct_inactive, data = df)

pdf("figure3_diagnostics.pdf", width = 7.2, height = 4)
par(
  mfrow = c(1, 2),
  mar   = c(4, 4, 3, 1),
  bg    = "white",
  family = "sans"
)
plot(m_simple, which = 1, main = "Residuals vs Fitted")
plot(m_simple, which = 2, main = "Normal Q-Q")
par(mfrow = c(1, 1))
dev.off()
cat("Saved: figure3_diagnostics.pdf\n")


# =============================================================================
# 5. STATISTICAL ANALYSES
# =============================================================================

# ── Assumption check: Shapiro-Wilk ───────────────────────────────────────────
sw <- shapiro.test(residuals(m_simple))
cat("\nShapiro-Wilk test on residuals:\n")
cat("  W =", round(sw$statistic, 4), "\n")
cat("  p =", round(sw$p.value, 4), "\n")

# ── Pearson correlation ───────────────────────────────────────────────────────
cor_res <- cor.test(df$pct_inactive, df$pct_obese, method = "pearson")
cat("\nPearson Correlation:\n")
cat("  r        =", round(cor_res$estimate, 3), "\n")
cat("  95% CI   = [", round(cor_res$conf.int[1], 3), ",",
    round(cor_res$conf.int[2], 3), "]\n")
cat("  p-value  =", format.pval(cor_res$p.value, digits = 3), "\n")

# ── Simple linear regression ──────────────────────────────────────────────────
cat("\nSimple Linear Regression (obesity ~ inactivity):\n")
tidy_s <- tidy(m_simple, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))
print(tidy_s)

g_s <- glance(m_simple)
cat("  R-squared     =", round(g_s$r.squared, 3), "\n")
cat("  Adj R-squared =", round(g_s$adj.r.squared, 3), "\n")
cat("  RMSE          =", round(g_s$sigma, 2), "\n")

# ── Multiple regression (adjusted for year) ───────────────────────────────────
m_adj  <- lm(pct_obese ~ pct_inactive + YearStart, data = df)
tidy_a <- tidy(m_adj, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

cat("\nMultiple Regression (obesity ~ inactivity + year):\n")
print(tidy_a)
cat("  Adjusted R-squared =", round(glance(m_adj)$adj.r.squared, 3), "\n")

cat("\nDone. Three figure PDFs saved to your Checkpoints folder.\n")

