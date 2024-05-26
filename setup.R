# setup.R

# Ensure the required packages are installed and loaded for a quantitative trading analysis environment

# Function to install and load packages
install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  sapply(packages, require, character.only = TRUE)
}

# Packages to be installed from CRAN
cran_packages <- c("quantmod", "TTR", "PerformanceAnalytics", "xts", "zoo", "dplyr", "ggplot2", 
                   "tidyquant", "lubridate", "caret", "data.table", "readr", "httr", "jsonlite", 
                   "ROI", "forecast", "nloptr", "rmarkdown", "bookdown", "knitr", "ggcorrplot", 
                   "plotly", "factoextra", "dygraphs" , "tidyr" , "reshape2" , "shiny" , "RColorBrewer")

# Install and load CRAN packages
install_and_load(cran_packages)

# Ensure 'devtools' is installed for installing GitHub packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# Install blotter and quantstrat from GitHub if not already installed
if (!requireNamespace("blotter", quietly = TRUE)) {
  devtools::install_github("braverock/blotter")
}
if (!requireNamespace("quantstrat", quietly = TRUE)) {
  devtools::install_github("braverock/quantstrat")
}

# Load blotter and quantstrat libraries
library(blotter)
library(quantstrat)

# Load all other libraries
sapply(cran_packages, library, character.only = TRUE)

# Print confirmation message
cat("All packages have been installed and loaded successfully.\n")

# End of script