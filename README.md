# Exposure to nitrogen dioxide and fine particulate matter when cooking with electricity compared to gas, a randomized crossover study in Quito, Ecuador
Repo supporting Gould et al. "Exposure to nitrogen dioxide and fine particulate matter when cooking with electricity compared to gas, a randomized crossover study in Quito, Ecuador" (2023) published in Environmental health perspectives.

The materials in this repository enable replication of the main results. 

If you find meaningful errors or have questions or suggestions, please contact Carlos Gould at gould.cf@gmail.com

# Organization of the repository

Scripts are data are primarily contained in this respository.

- **data**: inputs for analysis
- **scripts**: R code for analysis

## List of scripts

- **0_Process_Data.R**: processes raw data (not publicly-available) and generates cleaned time series data (ts_data.rds) and two-day summarized data (summary_48hr.rds)
- **1_Descriptives.R**: generates descriptive statistics
- **2_Analyze_Data.R**: conducts regressions and other analyses
- **3_MakeFigsTables.R**: makes figures and tables

## List of datasets

- **ts_data.rds**: cleaned minute-resolved time series sensor data
- **summary_48hr.rds**: two-day summarized data

### R session info
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 14.1
