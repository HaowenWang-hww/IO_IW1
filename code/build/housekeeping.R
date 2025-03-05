# Run housekeeping script to set up directories
source("housekeeping.R")  

# Load necessary libraries
library(dplyr)
library(readr)

# Define the full file path using raw_dir from housekeeping
IP1_original <- file.path(raw_dir, "IP1_original.csv")

# Check if file exists before proceeding
if (!file.exists(file_path)) {
  stop("Error: The file 'IP1_original.csv' was not found in the directory.")
}

# Load the dataset
data <- read_csv(IP1_original)

# Select key financial variables
key_vars <- data %>%
  select(gvkey, datadate, fyearq, fqtr, tic, conm, 
         cogsq, epsfxq, finxoprq, oiadpq, revtq, oibdpy)

# Convert `datadate` to Date format
key_vars <- key_vars %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

# Summary statistics for key financial variables
summary_stats <- key_vars %>%
  summarise(
    count = n(),
    mean_revenue = mean(revtq, na.rm = TRUE),
    median_revenue = median(revtq, na.rm = TRUE),
    sd_revenue = sd(revtq, na.rm = TRUE),
    min_revenue = min(revtq, na.rm = TRUE),
    max_revenue = max(revtq, na.rm = TRUE),
    
    mean_operating_income = mean(oiadpq, na.rm = TRUE),
    mean_cogs = mean(cogsq, na.rm = TRUE),
    mean_eps = mean(epsfxq, na.rm = TRUE),
    
    mean_fin_expense = mean(finxoprq, na.rm = TRUE),
    mean_operating_profit = mean(oibdpy, na.rm = TRUE)
  )

# Display the summary statistics
print(summary_stats)