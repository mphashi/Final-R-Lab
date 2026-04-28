# Final-R-Lab

## Project Title
Income-Based Disparities in Obesity Prevalence Among U.S. Adults (BRFSS, 2011–2023)
Shiny App: Physical Inactivity to Statewise Obesity Prevalence Among U.S. Adults (BRFSS, 2011-2023)

## Description
This repository contains R scripts and outputs for VTPEH 6270 checkpoints analyzing 
the association between income category and obesity prevalence among U.S. adults, and physical inactivity and obesity prevalence (state wise) in the USA using data from the Behavioral Risk Factor Surveillance System (BRFSS).

## Author
**Ashita Singhal**  
Cornell University — VTPEH 6270

## Contact
Please reach out at [as4493@cornell.edu](mailto:as4493@cornell.edu) for any questions 
regarding this repository.

## 🌐 Shiny App
An interactive Shiny app for exploring the physical inactivity – obesity gradient is available here:  
(https://mphashita.shinyapps.io/BRFSSProject/)
The app allows users to filter by year and income category, and visualize obesity 
prevalence trends dynamically.

## Research Questions
Is obesity prevalence associated with income category among adults, such that higher 
income categories have lower obesity prevalence?
How does physical inactivity differ statewise on obesity prevalence among adults?

## Objectives
- Visualize and summarize obesity prevalence across six income categories
- Identify the direction and magnitude of the income gradient
- Simulate the relationship under varying effect sizes, sample sizes, and noise levels 
  to assess statistical power
- Provide an interactive tool (Shiny app) for dynamic data exploration when related to physical inactivity and obesity

## Data Source
**Dataset:** Nutrition, Physical Activity, and Obesity — Behavioral Risk Factor 
Surveillance System (BRFSS)  
**Source:** U.S. Centers for Disease Control and Prevention (CDC) / [Data.gov](http://Data.gov)  
**Link:** https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system  
**Direct download URL:** https://data.cdc.gov/api/views/hn4x-zwk7/rows.csv?accessType=DOWNLOAD  
**Years covered:** 2011–2023  
**Unit of analysis:** State-level survey responses from U.S. adults  
**Key variables used:** `Data_Value` (obesity prevalence %), `Stratification1` 
(income category), `YearStart`, `Sample_Size`  
**Note:** The full dataset is loaded directly from the CDC URL in CP4.R. A filtered 
income-only subset (`data_income_subset.csv`) is included in the `data/` folder.
