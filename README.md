# Final-R-Lab

## Project Title
Income-Based Disparities in Obesity Prevalence Among U.S. Adults (BRFSS, 2011–2023)

## Description
This repository contains R scripts and outputs for VTPEH 6270 checkpoints analyzing the association between income category and obesity prevalence among U.S. adults, using data from the Behavioral Risk Factor Surveillance System (BRFSS).

## Author
**Ashita Singhal**  
Cornell University — VTPEH 6270

## Contact
Please reach out at as4493@cornell.edu for any questions regarding this repository.

## Research Question
Is obesity prevalence associated with income category among adults, such that higher income categories have lower obesity prevalence?

## Objectives
- Visualize and summarize obesity prevalence across six income categories
- Identify the direction and magnitude of the income–obesity gradient
- Simulate the relationship under varying effect sizes, sample sizes, and noise levels to assess statistical power

## Data Source
**Dataset:** Nutrition, Physical Activity, and Obesity — Behavioral Risk Factor Surveillance System (BRFSS)  
**Source:** U.S. Centers for Disease Control and Prevention (CDC) / Data.gov  
**Link:** https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system  
**Direct download URL:** https://data.cdc.gov/api/views/hn4x-zwk7/rows.csv?accessType=DOWNLOAD  
**Years covered:** 2011–2023  
**Unit of analysis:** State-level survey responses from U.S. adults  
**Key variables used:** `Data_Value` (obesity prevalence %), `Stratification1` (income category), `YearStart`, `Sample_Size`  
**Note:** The full dataset is loaded directly from the CDC URL in CP4.R. A filtered income-only subset (`data_income_subset.csv`) is included in the `data/` folder.

## Repository Contents
| File | Description |
|------|-------------|
| `CP4.R` | Main analysis script: data cleaning, visualization (Plots 1, 2, 4), summary statistics, simulation (Sections 3.1–3.4), heatmap |
| `CP4 edited.R` | Draft/working version of CP4 script |
| `CP2_Script_COMPLETE.R` | Checkpoint 2 analysis script |
| `data/data_income_subset.csv` | Filtered subset of BRFSS data (income stratification only, 1.6MB) |
| `Plot1_Boxplot_Obesity_Income.pdf` | Output figure: boxplot of obesity by income |
| `Plot2_BarChart_Mean_Obesity.pdf` | Output figure: bar chart of mean obesity by income |
| `Plot4_Violin_Obesity_Income.pdf` | Output figure: violin plot of obesity by income |
| `Heatmap_Obesity_Gradient.pdf` | Output figure: simulation heatmap |
| `README.md` | This file |
| `.gitignore` | Excludes large CSV and R-specific artifacts |

## Key Findings
Visual analysis indicates a negative monotonic relationship between income and obesity prevalence. Mean obesity prevalence is highest in the lowest income group (< $15,000) and decreases progressively with each higher income bracket. Simulation results confirm this gradient is detectable across a wide range of sample sizes and effect sizes.

## AI Tool Disclosure
Claude (Anthropic) was used to assist with code generation and debugging. All code was reviewed, tested, and modified by the author prior to submission.

## References
- CDC Behavioral Risk Factor Surveillance System (BRFSS): https://www.cdc.gov/brfss
- Data.gov BRFSS dataset: https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system
