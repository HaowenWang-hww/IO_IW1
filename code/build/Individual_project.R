# Run housekeeping script to set up directories
  source("housekeeping.R")  # Ensure this script is in the correct location

# Load necessary libraries
library(dplyr)
library(readr)

# Load the dataset
Financial_data <- read_csv(file.path(raw_dir, "IP1_original.csv"))
stock_data <- read_csv(file.path(raw_dir, "IP1_sp.csv"))

# Filter dataset for the four companies of interest
companies <- c("SWCH", "EQIX", "DLR", "COR.4")
filtered_data <- Financial_data  %>% filter(tic %in% companies)

# Convert `datadate` to Date format
filtered_data <- filtered_data %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

# Select key financial variables
key_vars <- filtered_data %>%
  select(gvkey, datadate, fyearq, fqtr, tic, conm, 
         revtq,   # Revenue - Key variable for market share analysis
         oiadpq,   # Operating income before depreciation - Measures profitability
         oibdpq,   # Operating income before depreciation and amortization
         cogsq,    # Cost of goods sold - Evaluates cost efficiency
         epsfxq,   # Earnings per share - Measures profitability
         oepf12,   # Earnings per share (diluted) - long-term view
         oeps12,   # Earnings per share from operations - focus on core business
         aqpq,     # Acquisition-related expenses - Indicator of M&A cost impact
         aqdq,     # Acquisition/Merger diluted EPS effect - Investor impact
         exchg,    # Exchange indicator - Checks if companies trade in the same market
         costat)   # Company status - Ensures companies are active

# Grouped summary statistics by company
summary_stats <- key_vars %>%
  group_by(tic) %>%
  summarise(
    count = n(),
    mean_revenue = mean(revtq, na.rm = TRUE),
    median_revenue = median(revtq, na.rm = TRUE),
    sd_revenue = sd(revtq, na.rm = TRUE),
    min_revenue = min(revtq, na.rm = TRUE),
    max_revenue = max(revtq, na.rm = TRUE),
    
    mean_operating_income = mean(oiadpq, na.rm = TRUE),
    mean_operating_profit = mean(oibdpq, na.rm = TRUE),
    mean_cogs = mean(cogsq, na.rm = TRUE),
    mean_eps = mean(epsfxq, na.rm = TRUE),
    mean_eps_diluted = mean(oepf12, na.rm = TRUE),
    
    mean_acquisition_expense = mean(aqpq, na.rm = TRUE),
    mean_merger_eps_effect = mean(aqdq, na.rm = TRUE)
  ) %>%
  arrange(tic)  # Sort results by ticker symbol

# Display the summary statistics


# Filter dataset for the four companies of interest
companies <- c("SWCH", "EQIX", "DLR", "COR")
filtered_stock_data <- stock_data %>% filter(tic %in% companies)

# Convert `datadate` to Date format
filtered_stock_data <- filtered_stock_data %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

# Select key stock price-related financial variables
stock_vars <- filtered_stock_data %>%
  select(gvkey, datadate, fyearq, fqtr, tic, conm, 
         epsfxq,   # Earnings per Share (EPS) - Key for stock performance
         oiadpq,   # Operating Income Before Depreciation - Core profitability
         revtq,    # Revenue - Tracks firm growth
         oibdpy,   # Operating Income Before Depreciation and Amortization
         finxoprq, # Financial Operating Expenses - Impacts firm profitability
         aqdq,     # Acquisition/Merger diluted EPS effect - Market reaction to M&A
         exchg,    # Exchange indicator - Ensures companies trade in same markets
         costat)   # Company status - Ensures active firms

# Grouped summary statistics by company
stock_summary_stats <- stock_vars %>%
  group_by(tic) %>%
  summarise(
    count = n(),
    mean_revenue = mean(revtq, na.rm = TRUE),
    median_revenue = median(revtq, na.rm = TRUE),
    sd_revenue = sd(revtq, na.rm = TRUE),
    min_revenue = min(revtq, na.rm = TRUE),
    max_revenue = max(revtq, na.rm = TRUE),
    
    mean_operating_income = mean(oiadpq, na.rm = TRUE),
    mean_operating_profit = mean(oibdpy, na.rm = TRUE),
    mean_eps = mean(epsfxq, na.rm = TRUE),
    
    mean_financial_expense = mean(finxoprq, na.rm = TRUE),
    mean_merger_eps_effect = mean(aqdq, na.rm = TRUE)
  ) %>%
  arrange(tic)  # Sort results by ticker symbol

# Display the summary statistics
print(summary_stats)
print(stock_summary_stats)

