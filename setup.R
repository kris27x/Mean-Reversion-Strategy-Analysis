# setup.R

# Resolve package conflicts and suppress specific warnings
conflictRules("dplyr", exclude = c("lag", "filter", "intersect", "setdiff", "setequal", "union"))
options(xts.warn_dplyr_breaks_lag = FALSE)

# Function to install and load packages
install_and_load <- function(packages) {
  # Identify packages that are not yet installed
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  # Install any new packages
  if (length(new_packages)) install.packages(new_packages)
  # Load all packages, and return the result
  sapply(packages, require, character.only = TRUE)
}

# CRAN packages required for quantitative trading analysis environment
cran_packages <- c("quantmod", "TTR", "PerformanceAnalytics", "xts", "zoo", "dplyr", "ggplot2", 
                   "tidyquant", "lubridate", "caret", "data.table", "readr", "httr", "jsonlite", 
                   "ROI", "forecast", "nloptr", "rmarkdown", "bookdown", "knitr", "ggcorrplot", 
                   "plotly", "factoextra", "dygraphs")

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

# Function to resolve package conflicts by explicitly calling the correct function
resolve_conflicts <- function() {
  # Base package conflicts
  base_conflicts <- c("date", "intersect", "setdiff", "setequal", "union")
  stats_conflicts <- c("filter", "lag")
  xts_conflicts <- c("first", "last")
  dplyr_conflicts <- c("between", "first", "last")
  data.table_conflicts <- c("hour", "isoweek", "mday", "minute", "month", "quarter", "second", "wday", "week", "yday", "year")
  zoo_conflicts <- c("yearmon", "yearqtr")
  
  # Resolve conflicts by reassigning functions from namespaces to global environment
  for (conflict in base_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("base")), envir = .GlobalEnv)
  }
  for (conflict in stats_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("stats")), envir = .GlobalEnv)
  }
  for (conflict in xts_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("xts")), envir = .GlobalEnv)
  }
  for (conflict in dplyr_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("dplyr")), envir = .GlobalEnv)
  }
  for (conflict in data.table_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("data.table")), envir = .GlobalEnv)
  }
  for (conflict in zoo_conflicts) {
    assign(conflict, get(conflict, envir = asNamespace("zoo")), envir = .GlobalEnv)
  }
}

# Resolve conflicts after loading all packages
resolve_conflicts()

# Print confirmation message
cat("All packages have been installed and loaded successfully.\n")

# End of script
