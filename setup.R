# setup.R

# Install devtools package if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install packages from CRAN
install.packages(c("quantmod", "TTR", "PerformanceAnalytics", "xts", "zoo", "dplyr", "ggplot2", "tidyquant", "lubridate", "caret",
                   "data.table", "readr", "httr", "jsonlite", "ROI", "forecast", "nloptr", 
                   "rmarkdown", "bookdown", "knitr"))

# Install blotter and quantstrat from GitHub
devtools::install_github("braverock/blotter")
devtools::install_github("braverock/quantstrat")

# Load all libraries
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(dplyr)
library(ggplot2)
library(tidyquant)
library(lubridate)
library(caret)
library(quantstrat)
library(data.table)
library(readr)
library(httr)
library(jsonlite)
library(blotter)
library(ROI)
library(forecast)
library(nloptr)
library(rmarkdown)
library(bookdown)
library(knitr)

# Confirmation message
cat("All packages have been installed and loaded successfully.")
