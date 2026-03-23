# ============================================================
# VTPEH 6270 - Checkpoint 2
# Author: Ashita Singhal
# ============================================================


# ---- Loading Data ----

library(readr)
library(dplyr)
library(ggplot2)

# Set working directory
setwd("/Users/ashitasinghal/Desktop/CORNELL /6270:R:Biostat/Checkpoints")

# Load file
data_obesity <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# Preview the data
head(data_obesity)


# ---- Variable Overview ----

library(dplyr)
library(knitr)
library(kableExtra)

data_subset <- data_obesity %>%
  select(
    YearStart,
    LocationDesc,
    Topic,
    Question,
    Data_Value,
    Sample_Size,
    StratificationCategory1,
    Stratification1,
    Class,
    Datasource
  )

# STEP 1: Ensure correct variable classes
data_subset <- data_subset %>%
  mutate(
    YearStart   = as.numeric(YearStart),
    Data_Value  = as.numeric(Data_Value),
    Sample_Size = as.numeric(Sample_Size)
  )

# STEP 2: Build Overview Table
variable_overview <- data.frame(
  Variable = names(data_subset),
  Class    = sapply(data_subset, class),
  stringsAsFactors = FALSE
)

# STEP 3: Capitalize class, add Type and Description, remove redundant row names
variable_overview <- variable_overview %>%
  mutate(
    Class = tools::toTitleCase(Class),
    Type = case_when(
      Variable %in% c("Data_Value", "Sample_Size")  ~ "Continuous",
      Variable %in% c("YearStart")                  ~ "Discrete (Numeric Time Variable)",
      Class %in% c("Character", "Factor")            ~ "Categorical",
      TRUE                                           ~ "Other"
    ),
    Description = case_when(
      Variable == "YearStart"               ~ "Year the data collection began",
      Variable == "LocationDesc"            ~ "State or territory name",
      Variable == "Topic"                   ~ "Broad topic area (e.g., Obesity, Physical Activity)",
      Variable == "Question"                ~ "Survey question asked of respondents",
      Variable == "Data_Value"              ~ "Estimated prevalence (%) for the given question and group",
      Variable == "Sample_Size"             ~ "Number of survey respondents for that estimate",
      Variable == "StratificationCategory1" ~ "Category used to stratify results (e.g., Age, Gender)",
      Variable == "Stratification1"         ~ "Specific stratum within the stratification category",
      Variable == "Class"                   ~ "Broader classification grouping related topics",
      Variable == "Datasource"              ~ "Source of the data (e.g., BRFSS)",
      TRUE                                  ~ NA_character_
    )
  )

# STEP 4: Render table with fixed column widths to prevent overlap
kable(
  variable_overview,
  caption  = "Overview of selected variables from the BRFSS Nutrition, Physical Activity, and Obesity dataset",
  row.names = FALSE,
  format   = "latex",
  booktabs = TRUE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  column_spec(1, width = "3cm") %>%   # Variable
  column_spec(2, width = "2cm") %>%   # Class
  column_spec(3, width = "3.5cm") %>% # Type
  column_spec(4, width = "6cm")       # Description


# ---- Visualizing and Summarizing Data ----

library(ggplot2)

data_subset <- na.omit(data_subset)

# Calculate statistics
mean_val   <- mean(data_subset$Data_Value, na.rm = TRUE)
median_val <- median(data_subset$Data_Value, na.rm = TRUE)

# Figure 1: Histogram
ggplot(data_subset, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = mean_val,   color = "Mean",   linetype = "Mean"),   size = 0.7) +
  geom_vline(aes(xintercept = median_val, color = "Median", linetype = "Median"), size = 2) +
  scale_color_manual(values    = c("Mean" = "red",   "Median" = "navy")) +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted")) +
  labs(
    title    = "Distribution of Reported Nutrition and Obesity Health Percentages\nAmong U.S. Adults Surveyed via BRFSS (2011-2023)",
    x        = "Reported Health Percentage (%)",
    y        = "Frequency",
    color    = "Statistic",
    linetype = "Statistic",
    caption  = stringr::str_wrap(
      "Figure 1. Histogram of Data_Value showing the distribution of reported nutrition and obesity health percentages among U.S. adults from the Behavioral Risk Factor Surveillance System (BRFSS), with mean and median reference lines.",
      width = 100
    )
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(hjust = 0, size = 8),
    plot.title   = element_text(size = 12, face = "bold")
  )


# Figure 2: Scatterplot
ggplot(data_subset, aes(x = YearStart, y = Data_Value)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_x_continuous(breaks = seq(min(data_subset$YearStart),
                                  max(data_subset$YearStart), by = 2)) +
  labs(
    title   = "Reported Nutrition and Obesity Health Percentages\nAmong U.S. Adults by Survey Year (BRFSS, 2011-2023)",
    x       = "Survey Year",
    y       = "Reported Health Percentage (%)",
    caption = stringr::str_wrap(
      "Figure 2. Scatterplot illustrating the relationship between survey year and reported nutrition and obesity health percentages among U.S. adults in the BRFSS dataset. The red trend line (LOESS) reflects the overall trajectory across time.",
      width = 100
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 8),
    plot.title   = element_text(size = 12, face = "bold")
  )
geom_smooth(method = "loess", color = "red", se = FALSE)
