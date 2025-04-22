# ---------------------------------------------------------------------------------------------
# File: housekeeping.R
# By: YOUR NAME HERE
# Date: Today's Date
# Description: This file installs and loads packages. It also defines the file paths. Run it 
# before running any other files. In fact, include it all files you run.
# ---------------------------------------------------------------------------------------------

# Include any other folders you may want

# Install packages
#install.packages("tidycensus")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("Himsc")
#install.packages("knitr")
#install.packages("viridis")
#install.packages("tidygeocoder")
#install.packages("rvest")

library(tidycensus)
library(tidyverse)
library(sf)
library(Hmisc)  # For weighted variance function
library(knitr)  # For table output
library(viridis) # Changes colors of graphs
library(tidygeocoder)
library(rvest)
library(lubridate)
library(broom)
library(tidyquant)
library(forecast)
library(ggrepel)

# Directory objects

# Create directories
data_dir <- 'data'
raw_dir <- file.path(data_dir,'raw')
work_dir <- file.path(data_dir,'work')
output_dir <- 'output'
code_dir <- 'code'
build_dir <- file.path(code_dir,'build')
analysis_dir <- file.path(code_dir,'analysis')
documentation_dir <- 'documentation'
literature_dir <-'literature'

# Create directories
suppressWarnings({
  dir.create(data_dir)
  dir.create(raw_dir)
  dir.create(work_dir)
  dir.create(documentation_dir)
  dir.create(code_dir)
  dir.create(build_dir)
  dir.create(analysis_dir)
  dir.create(literature_dir)
  dir.create(output_dir)
})


# Create .placeholder's in each folder, these ensure that the folders are included in the git repository
# Git does not track empty folders, so we need to create a file in each folder to ensure that the folder
# is pushed

suppressWarnings({
  for (dir in c(raw_dir,work_dir,build_dir,analysis_dir,documentation_dir,literature_dir)){
    # If file created, it prints "TRUE"
    file.create(file.path(dir,'.placeholder'),
      showWarnings=FALSE)}
})