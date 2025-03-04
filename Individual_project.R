# Load required libraries
library(httr)   # For downloading the file
library(readr)  # For reading CSV files

# Define the WRDS file link
wrds_url <- "https://wrds-www.wharton.upenn.edu/query-manager/query-document/20010412/"

# Define the local file name
output_file <- "qts_data.csv"

# Download the file from WRDS
GET(wrds_url, write_disk(output_file, overwrite = TRUE))

# Read the CSV file into R
data <- read_csv(output_file)

# Print first few rows to check the data
head(data)