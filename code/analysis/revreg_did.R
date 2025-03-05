# Run housekeeping script to set up directories
source("housekeeping.R")  # Ensure this script is in the correct location

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lmtest)
library(fixest)

# Load the dataset
Financial_data <- read_csv(file.path(raw_dir, "IP1_original.csv"))

# Filter dataset for the four companies of interest
companies <- c("SWCH", "EQIX", "DLR", "COR.4")
filtered_data <- Financial_data %>%
  filter(tic %in% companies) %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

# Normalize revenue by setting each company's first recorded revenue as baseline
filtered_data <- filtered_data %>%
  group_by(tic) %>%
  arrange(datadate) %>%
  mutate(revenue_baseline = first(revtq),
         normalized_revenue = revtq / revenue_baseline) %>%
  ungroup()

# Create a single regression plot for all companies
revenue_plot <- ggplot(filtered_data, aes(x = fyearq, y = normalized_revenue, color = tic)) +
  geom_point(alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +  # Linear regression lines
  labs(title = "Regression of Normalized Revenue on Year (All Companies)",
       x = "Year",
       y = "Normalized Revenue (Baseline = 1)",
       color = "Company") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend for better readability

# Define output file path
output_file <- file.path(output_dir, "revenue_trend_combined.png")

# Save the plot as a PNG file
ggsave(output_file, plot = revenue_plot, width = 10, height = 6, dpi = 300)

# Print message confirming export
print(paste("Plot saved to:", output_file))

