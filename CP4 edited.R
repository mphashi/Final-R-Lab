# ============================================================
# VTPEH 6270 - Checkpoint 4
# Author: Ashita Singhal
# ============================================================

library(ggplot2)

# Nature palette (colorblind-friendly, commonly used):
nature_colors <- c(
  "Less than $15,000"  = "#E64B35",  # red
  "$15,000 - $24,999"  = "#F39B7F",  # salmon
  "$25,000 - $34,999"  = "#FFDC91",  # yellow
  "$35,000 - $49,999"  = "#91D1C2",  # teal
  "$50,000 - $74,999"  = "#4DBBD5",  # blue
  "$75,000 or greater" = "#3C5488"   # navy
)

# Nature figure dimensions (mm → inches): 
# Single column = 89mm = 3.50 in | Double column = 174mm = 6.85 in
nature_width_in  <- 6.85   # double-column (recommended for multi-panel)
nature_height_in <- 4.72   # ~120 mm — adjust per figure

# Helper: save a ggplot as a Nature-formatted PDF
save_nature_pdf <- function(plot_obj, filename,
                            width  = nature_width_in,
                            height = nature_height_in) {
  ggsave(
    filename = filename,
    plot     = plot_obj,
    device   = "pdf",
    width    = width,
    height   = height,
    units    = "in"
  )
  message("Saved: ", filename)
}

# Nature base theme (replaces obesity_theme)
nature_theme <- theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(face = "bold", size = 9),
    plot.subtitle    = element_text(color = "grey40", size = 7),
    axis.text.x      = element_text(angle = 35, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    axis.title       = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 7),
    strip.text       = element_text(face = "bold", size = 8)
  )


# ============================================================
# LOADING DATA
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(knitr)
library(tidyr)

# Set working directory — update this path to your local project folder
setwd("/Users/ashitasinghal/Desktop/CORNELL /6270:R:Biostat/Checkpoints")

# Load file
data_obesity <- read.csv(
  "Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv"
)

head(data_obesity)


# ============================================================
# VARIABLE OVERVIEW
# ============================================================

data_subset <- data_obesity %>%
  select(
    YearStart, LocationDesc, Topic, Question,
    Data_Value, Sample_Size,
    StratificationCategory1, Stratification1,
    Class, Datasource
  ) %>%
  mutate(
    YearStart   = as.numeric(YearStart),
    Data_Value  = as.numeric(Data_Value),
    Sample_Size = as.numeric(Sample_Size)
  )

variable_overview <- data.frame(
  Variable = names(data_subset),
  Class    = sapply(data_subset, class),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Type = case_when(
      Variable %in% c("Data_Value", "Sample_Size")  ~ "Continuous",
      Variable %in% c("YearStart")                  ~ "Discrete (Numeric Time Variable)",
      Class    %in% c("character", "factor")         ~ "Categorical",
      TRUE                                           ~ "Other"
    )
  )

kable(
  variable_overview,
  caption = "Overview of selected variables from the BRFSS Nutrition, Physical Activity, and Obesity dataset"
)


# ============================================================
# DATA CLEANING — INCOME SUBSET
# ============================================================

income_levels <- c(
  "Less than $15,000",
  "$15,000 - $24,999",
  "$25,000 - $34,999",
  "$35,000 - $49,999",
  "$50,000 - $74,999",
  "$75,000 or greater"
)

data_income <- data_obesity %>%
  filter(
    StratificationCategory1 == "Income",
    !is.na(Data_Value),
    is.na(Data_Value_Footnote) | Data_Value_Footnote == ""
  ) %>%
  mutate(
    Data_Value  = as.numeric(Data_Value),
    Sample_Size = as.numeric(Sample_Size),
    Income = factor(Stratification1, levels = income_levels)
  ) %>%
  filter(!is.na(Income))


# ============================================================
# PLOT 1: Boxplot — Distribution of Obesity % by Income
# ============================================================

p1 <- ggplot(data_income, aes(x = Income, y = Data_Value, fill = Income)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white", outlier.color = "grey50",
               linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = "white", color = "black") +
  scale_fill_manual(values = nature_colors) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Plot 1: Distribution of Obesity Prevalence by Income Category",
    subtitle = "Diamond = group mean; boxes show median \u00b1 IQR",
    x        = "Income Category",
    y        = "Obesity Prevalence (%)"
  ) +
  nature_theme

print(p1)
save_nature_pdf(p1, "Plot1_Boxplot_Obesity_Income.pdf")


# ============================================================
# PLOT 2: Bar Chart — Mean Obesity % per Income Group
# ============================================================

income_summary <- data_income %>%
  group_by(Income) %>%
  summarise(
    Mean_Obesity = mean(Data_Value, na.rm = TRUE),
    SE           = sd(Data_Value,   na.rm = TRUE) / sqrt(n()),
    n            = n(),
    .groups = "drop"
  )

p2 <- ggplot(income_summary, aes(x = Income, y = Mean_Obesity, fill = Income)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_errorbar(aes(ymin = Mean_Obesity - 1.96 * SE,
                    ymax = Mean_Obesity + 1.96 * SE),
                width = 0.25, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Mean_Obesity, 1), "%")),
            vjust = -0.8, size = 2.5, fontface = "bold") +
  scale_fill_manual(values = nature_colors) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Plot 2: Mean Obesity Prevalence by Income Category",
    subtitle = "Error bars represent 95% confidence intervals",
    x        = "Income Category",
    y        = "Mean Obesity Prevalence (%)"
  ) +
  nature_theme

print(p2)
save_nature_pdf(p2, "Plot2_BarChart_Mean_Obesity.pdf")


# NOTE: Plot 3 (Obesity Prevalence Trends Over Time by Income) has been removed.


# ============================================================
# PLOT 4: Violin + Jitter — Full Data Distribution
# ============================================================

p4 <- ggplot(data_income, aes(x = Income, y = Data_Value, fill = Income)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_jitter(aes(color = Income), width = 0.15, alpha = 0.25, size = 0.6) +
  stat_summary(fun = median, geom = "crossbar",
               width = 0.5, linewidth = 0.5, color = "black") +
  scale_fill_manual(values  = nature_colors) +
  scale_color_manual(values = nature_colors) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Plot 4: Full Distribution of Obesity Prevalence by Income",
    subtitle = "Violin = density; dots = individual observations; bar = median",
    x        = "Income Category",
    y        = "Obesity Prevalence (%)"
  ) +
  nature_theme

print(p4)
save_nature_pdf(p4, "Plot4_Violin_Obesity_Income.pdf")


# ============================================================
# 2.4 DESCRIPTION OF PLAUSIBLE RELATIONSHIPS
# ============================================================

income_means <- data_obesity %>%
  filter(
    StratificationCategory1 == "Income",
    !is.na(Data_Value)
  ) %>%
  mutate(
    Data_Value = as.numeric(Data_Value),
    Income = factor(Stratification1, levels = income_levels)
  ) %>%
  filter(!is.na(Income)) %>%
  group_by(Income) %>%
  summarise(
    Mean_Obesity = round(mean(Data_Value, na.rm = TRUE), 1),
    .groups = "drop"
  )

kable(
  income_means,
  col.names = c("Income Category", "Mean Obesity Prevalence (%)"),
  caption   = "Mean obesity prevalence by income category (BRFSS)"
)


# ============================================================
# 2.5 PARAMETERS OF INTEREST
# ============================================================

parameters_table <- data.frame(
  Parameter   = c("Intercept", "Slope", "Noise / Residual SD"),
  Symbol      = c("alpha", "beta", "epsilon"),
  Description = c(
    "Expected obesity prevalence (%) at the lowest income category (< $15,000)",
    "Change in mean obesity prevalence (pp) per one-step increase in income; expected to be negative",
    "Residual variation in obesity prevalence not explained by income (percentage points)"
  ),
  stringsAsFactors = FALSE
)

kable(
  parameters_table,
  col.names = c("Parameter", "Symbol", "Description"),
  caption   = "Parameters of interest for the income–obesity association"
)


# ============================================================
# 3.1 SIMULATION BASIS + 3.3 SIMULATION FUNCTION
# ============================================================

simulate_obesity <- function(n, beta, epsilon, alpha = 35) {
  
  income_steps <- 1:6
  
  sim_data <- data.frame(
    income_step  = rep(income_steps, each = n),
    income_label = rep(income_levels, each = n)
  ) %>%
    mutate(
      obesity_prev = alpha + beta * income_step + rnorm(n(), mean = 0, sd = epsilon),
      obesity_prev = pmax(10, pmin(70, obesity_prev)),
      income_label = factor(income_label, levels = income_levels)
    )
  
  # 3.2 STRATIFIED SUMMARY STATISTICS
  summary_stats <- sim_data %>%
    group_by(income_step, income_label) %>%
    summarise(
      n      = n(),
      mean   = round(mean(obesity_prev), 2),
      sd     = round(sd(obesity_prev),   2),
      q25    = round(quantile(obesity_prev, 0.25), 2),
      median = round(median(obesity_prev), 2),
      q75    = round(quantile(obesity_prev, 0.75), 2),
      .groups = "drop"
    )
  
  return(list(data = sim_data, summary = summary_stats))
}

# Quick test run
test_run <- simulate_obesity(n = 500, beta = -2, epsilon = 5)
kable(
  test_run$summary,
  col.names = c("Income Step", "Income Label", "N", "Mean", "SD", "Q25", "Median", "Q75"),
  caption   = "Stratified summary statistics from a single simulation run"
)


# ============================================================
# 3.4 SIMULATION AUTOMATION
# ============================================================

# Real dataset context:
#   The BRFSS income-stratified subset contains roughly 50,000–90,000 rows,
#   split across 6 income groups → ~8,000–15,000 observations per group.
#   Sample sizes below are chosen to span well BELOW and AROUND that range
#   so power comparisons are meaningful relative to the real data.

effect_sizes <- seq(-5, -0.5, length.out = 10)  # beta values (negative = inverse relationship)

sample_sizes <- c(
  100,    # ~1% of real group size  — very underpowered
  500,    # ~5%                     — underpowered
  1000,   # ~10%                    — low power
  2500,   # ~25%                    — moderate
  5000,   # ~50%                    — approaching real data
  8000,   # ~80%                    — near real data
  10000,  # ~100%                   — at real data order of magnitude
  15000   # slightly above          — well-powered reference
)
# Removed: 50, 200, 12000 replaced with values that better bracket
# the real dataset (~8,000–15,000 per group).  Max = 15,000 keeps
# the grid within the same order of magnitude as the real data.

noise_levels <- c(3, 7, 12)  # low / medium / high noise (SD in %)

param_grid <- expand.grid(
  beta    = effect_sizes,
  n       = sample_sizes,
  epsilon = noise_levels,
  stringsAsFactors = FALSE
)

cat("Total simulation runs:", nrow(param_grid), "\n")

set.seed(42)
sim_results <- vector("list", nrow(param_grid))

for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  result <- simulate_obesity(
    n       = params$n,
    beta    = params$beta,
    epsilon = params$epsilon
  )
  sim_results[[i]] <- list(
    params  = params,
    data    = result$data,
    summary = result$summary
  )
}

cat("Simulations complete:", length(sim_results), "runs stored.\n")


# ============================================================
# HEATMAP OF MEAN OBESITY GRADIENT
# Fix: use ggsave with pdf device + explicit large dimensions
#      to eliminate blurriness (vector output, no rasterization)
# ============================================================

# Extract: slope of mean obesity across income steps per simulation
# (i.e., how strong is the income gradient in each run?)
gradient_df <- lapply(sim_results, function(run) {
  s     <- run$summary
  fit   <- lm(mean ~ income_step, data = s)
  slope <- round(coef(fit)[2], 3)
  data.frame(
    beta      = run$params$beta,
    n         = run$params$n,
    epsilon   = run$params$epsilon,
    obs_slope = slope
  )
}) %>% bind_rows()

gradient_df <- gradient_df %>%
  mutate(
    noise_label = factor(
      paste0("Noise SD = ", epsilon),
      levels = paste0("Noise SD = ", sort(unique(epsilon)))
    ),
    n_label = factor(
      paste0("n = ", formatC(n, format = "d", big.mark = ",")),
      levels = paste0("n = ", formatC(sort(unique(n)), format = "d", big.mark = ","))
    )
  )

p_heat <- ggplot(gradient_df,
                 aes(x    = factor(round(beta, 2)),
                     y    = n_label,
                     fill = obs_slope)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(obs_slope, 1)),
            size = 2.2, color = "white", fontface = "bold") +
  facet_wrap(~ noise_label, ncol = 3) +
  scale_fill_gradient2(
    low      = "#3C5488",
    mid      = "#FFFFBF",
    high     = "#E64B35",
    midpoint = mean(gradient_df$obs_slope),
    name     = "Observed\nSlope"
  ) +
  labs(
    title    = "Observed Income-Obesity Slope Across Simulation Parameters",
    subtitle = "Each cell = estimated slope (% per income step); facets = noise level",
    x        = "True Effect Size (\u03b2)",
    y        = "Sample Size per Group"
  ) +
  theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y   = element_text(size = 7),
    axis.title    = element_text(size = 8),
    strip.text    = element_text(face = "bold", size = 8),
    panel.grid    = element_blank(),
    plot.title    = element_text(face = "bold", size = 9),
    plot.subtitle = element_text(color = "grey40", size = 7),
    legend.text   = element_text(size = 7),
    legend.title  = element_text(size = 7)
  )

print(p_heat)

# Save as PDF — vector format = no blurriness regardless of zoom
# Wider/taller than standard so all tiles and labels fit clearly
ggsave(
  filename = "Heatmap_Obesity_Gradient.pdf",
  plot     = p_heat,
  device   = "pdf",
  width    = 10,    # inches — wide enough for 3 facets with 10 beta columns
  height   = 6,     # inches — tall enough for 8 sample-size rows + labels
  units    = "in"
)
message("Saved: Heatmap_Obesity_Gradient.pdf")
