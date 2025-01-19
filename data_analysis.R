###############################################################################
# data_analysis_script.R
# Comprehensive script for analyzing Dow Jones (DJIA) and Nasdaq (IXIC)
# with data pulled from Yahoo Finance using quantmod, plus extensive EDA.
###############################################################################

# 1) Load necessary libraries
required_packages <- c(
  "quantmod", "dplyr", "ggplot2", "lubridate", "tidyr", 
  "TTR", "PerformanceAnalytics", "ggcorrplot", "zoo"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

###############################################################################
# 2) Function to handle missing values
###############################################################################
handle_missing_values <- function(data) {
  # 1) Linear interpolation for any internal NA
  data <- na.approx(data, maxgap = Inf, na.rm = FALSE)
  # 2) Forward-fill
  data <- na.locf(data, fromLast = FALSE)
  # 3) Backward-fill
  data <- na.locf(data, fromLast = TRUE)
  # 4) Finally, remove any rows still containing NA
  na.omit(data)
}

###############################################################################
# 3) Function to calculate returns (Overnight, Intraday, etc.)
###############################################################################
calculate_returns <- function(data, close_col, open_col) {
  data %>%
    mutate(
      Closing          = lag(.data[[close_col]], 1),
      Opening          = .data[[open_col]],
      Overnight_Return = (Opening / Closing) - 1,
      Intraday_Return  = (.data[[close_col]] / Opening) - 1
    ) %>%
    mutate(
      # Cumulative returns set NA returns to 0, so they won't break cumprod
      Cumulative_Overnight_Return = cumprod(1 + ifelse(is.na(Overnight_Return), 0, Overnight_Return)),
      Cumulative_Intraday_Return  = cumprod(1 + ifelse(is.na(Intraday_Return), 0, Intraday_Return))
    )
}

###############################################################################
# 4) Function to download and handle data
###############################################################################
get_data <- function(symbol) {
  tryCatch({
    data <- quantmod::getSymbols(symbol, src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    data <- handle_missing_values(data)
    data
  }, error = function(e) {
    stop(paste("Error downloading data for symbol:", symbol))
  })
}

###############################################################################
# 5) Function to calculate returns for different periods
###############################################################################
calculate_period_returns <- function(data, periods) {
  returns <- list()
  for (period in periods) {
    period_return <- periodReturn(data, period = period, type = 'log')
    returns[[period]] <- period_return
  }
  returns_df <- do.call(merge, returns)
  colnames(returns_df) <- periods
  return(returns_df)
}

###############################################################################
# Download historical data for Dow Jones and Nasdaq
###############################################################################
djia_data   <- get_data("^DJI")
nasdaq_data <- get_data("^IXIC")

###############################################################################
# Convert xts to data.frame for certain tasks
###############################################################################
djia_df   <- data.frame(date = index(djia_data),   coredata(djia_data))
nasdaq_df <- data.frame(date = index(nasdaq_data), coredata(nasdaq_data))

###############################################################################
# Calculate returns (overnight, intraday, cumulative)
###############################################################################
djia_returns   <- calculate_returns(djia_df,   "DJI.Close",  "DJI.Open")
nasdaq_returns <- calculate_returns(nasdaq_df, "IXIC.Close", "IXIC.Open")

###############################################################################
# Exploratory Data Analysis (EDA)
###############################################################################
print(head(djia_df))
print(head(nasdaq_df))

# Descriptive statistics
djia_stats           <- summary(djia_df$DJI.Close)
nasdaq_stats         <- summary(nasdaq_df$IXIC.Close)
djia_returns_stats   <- summary(dailyReturn(djia_data))
nasdaq_returns_stats <- summary(dailyReturn(nasdaq_data))

cat("Descriptive Statistics for DJI Closing Prices:\n", djia_stats, "\n")
cat("Descriptive Statistics for IXIC Closing Prices:\n", nasdaq_stats, "\n")
cat("Descriptive Statistics for DJI Daily Returns:\n", djia_returns_stats, "\n")
cat("Descriptive Statistics for IXIC Daily Returns:\n", nasdaq_returns_stats, "\n")

###############################################################################
# 6) Utility function for creating & saving plots
###############################################################################
create_and_save_plot <- function(plot, filename) {
  print(plot)
  ggsave(filename, plot = plot, width = 10, height = 6)
}

###############################################################################
# Plot closing prices
###############################################################################
p1 <- ggplot() +
  geom_line(data = djia_df, aes(x = date, y = DJI.Close),   color = "blue", na.rm = TRUE) +
  geom_line(data = nasdaq_df, aes(x = date, y = IXIC.Close), color = "red",  na.rm = TRUE) +
  labs(title = "Dow Jones and Nasdaq Closing Prices", x = "Date", y = "Closing Price") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))

create_and_save_plot(p1, "closing_prices_plot.png")

###############################################################################
# Plot histograms of daily returns
###############################################################################
djia_returns_daily   <- dailyReturn(djia_data)
nasdaq_returns_daily <- dailyReturn(nasdaq_data)

p2 <- ggplot() +
  geom_histogram(data = data.frame(djia_returns_daily),
                 aes(x = daily.returns),
                 bins = 100, fill = "blue", alpha = 0.5, na.rm = TRUE) +
  geom_histogram(data = data.frame(nasdaq_returns_daily),
                 aes(x = daily.returns),
                 bins = 100, fill = "red", alpha = 0.5, na.rm = TRUE) +
  labs(title = "Histogram of Daily Returns", x = "Daily Returns", y = "Frequency") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p2, "daily_returns_histogram.png")

###############################################################################
# 7) Function to calculate annual returns (simple measure, not log)
###############################################################################
calc_annual_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date)) %>%
    summarize(
      AnnualReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) /
        first(.data[[open_col]]),
      .groups = 'drop'
    )
}

djia_annual_returns   <- calc_annual_return(djia_df, "DJI.Open",  "DJI.Close")
nasdaq_annual_returns <- calc_annual_return(nasdaq_df, "IXIC.Open","IXIC.Close")

p3 <- ggplot() +
  geom_point(data = djia_annual_returns,
             aes(x = year, y = AnnualReturn, color = "Dow Jones")) +
  geom_point(data = nasdaq_annual_returns,
             aes(x = year, y = AnnualReturn, color = "Nasdaq")) +
  scale_color_manual(values = c("Dow Jones" = "blue", "Nasdaq" = "red")) +
  labs(title = "Annual Returns of Dow Jones and Nasdaq",
       x = "Year", y = "Annual Return", color = "Index") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))

create_and_save_plot(p3, "annual_returns_scatter_plot.png")

###############################################################################
# 8) Calculate and plot monthly returns
###############################################################################
calc_monthly_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date), month = month(date)) %>%
    summarize(
      MonthlyReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) /
        first(.data[[open_col]]),
      .groups = 'drop'
    ) %>%
    mutate(date = make_date(year, month, 1))
}

djia_monthly_returns   <- calc_monthly_return(djia_df,   "DJI.Open",  "DJI.Close")
nasdaq_monthly_returns <- calc_monthly_return(nasdaq_df, "IXIC.Open", "IXIC.Close")

p4 <- ggplot() +
  geom_point(data = djia_monthly_returns,
             aes(x = date, y = MonthlyReturn, color = "Dow Jones")) +
  geom_point(data = nasdaq_monthly_returns,
             aes(x = date, y = MonthlyReturn, color = "Nasdaq")) +
  scale_color_manual(values = c("Dow Jones" = "blue", "Nasdaq" = "red")) +
  labs(title = "Monthly Returns of Dow Jones and Nasdaq",
       x = "Date", y = "Monthly Return", color = "Index") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p4, "monthly_returns_scatter_plot.png")

###############################################################################
# 9) Volatility Analysis (30-day rolling, annualized)
###############################################################################
djia_volatility   <- runSD(dailyReturn(djia_data),   n = 30) * sqrt(252)
nasdaq_volatility <- runSD(dailyReturn(nasdaq_data), n = 30) * sqrt(252)

volatility_data <- data.frame(
  date            = index(djia_volatility),
  DJI_Volatility  = coredata(djia_volatility),
  IXIC_Volatility = coredata(nasdaq_volatility)
)
volatility_data <- na.omit(volatility_data)

cat("First few rows of volatility data:\n")
print(head(volatility_data))

# Plot volatility
p5 <- ggplot(volatility_data, aes(x = date)) +
  geom_line(aes(y = DJI_Volatility),  color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Volatility), color = "red",  na.rm = TRUE) +
  labs(title = "Rolling 30-Day Volatility", x = "Date", y = "Volatility (Annualized)") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p5, "volatility_plot.png")

###############################################################################
# 10) Correlation Analysis of daily returns
###############################################################################
returns_xts <- merge(dailyReturn(djia_data), dailyReturn(nasdaq_data), all = FALSE)
colnames(returns_xts) <- c("DJI_Returns", "IXIC_Returns")
returns_data <- data.frame(date = index(returns_xts), coredata(returns_xts)) %>%
  na.omit()

correlation <- cor(returns_data$DJI_Returns, returns_data$IXIC_Returns, use = "complete.obs")
cat("Correlation between Dow Jones and Nasdaq returns: ", correlation, "\n")

# Plot daily returns
p6 <- ggplot(returns_data, aes(x = date)) +
  geom_line(aes(y = DJI_Returns),  color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Returns), color = "red",  na.rm = TRUE) +
  labs(title = "Daily Returns of Dow Jones and Nasdaq",
       x = "Date", y = "Daily Returns") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p6, "daily_returns_plot.png")

# Scatter plot of returns
p7 <- ggplot(returns_data, aes(x = DJI_Returns, y = IXIC_Returns)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Daily Returns",
       x = "DJI Returns", y = "IXIC Returns") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p7, "scatter_plot_returns.png")

###############################################################################
# 11) Rolling Correlation
###############################################################################
rolling_correlation <- rollapply(
  returns_xts, width = 30, 
  FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"), 
  by.column = FALSE, align = "right", fill = NA
)
rolling_correlation_data <- data.frame(
  date               = index(rolling_correlation),
  Rolling_Correlation= coredata(rolling_correlation)
) %>%
  na.omit()

# Plot rolling correlation
p8 <- ggplot(rolling_correlation_data, aes(x = date, y = Rolling_Correlation)) +
  geom_line(color = "purple") +
  labs(title = "Rolling 30-Day Correlation between Dow Jones and Nasdaq Returns",
       x = "Date", y = "Correlation") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p8, "rolling_correlation_plot.png")

###############################################################################
# 12) Combine data for plotting cumulative overnight/intraday returns
###############################################################################
combined_returns <- dplyr::bind_rows(
  nasdaq_returns %>%
    dplyr::select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>%
    mutate(Index = "Nasdaq"),
  djia_returns %>%
    dplyr::select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>%
    mutate(Index = "Dow Jones")
)

# Reshape the data for ggplot
combined_returns_long <- combined_returns %>%
  tidyr::pivot_longer(
    cols      = c(Cumulative_Overnight_Return, Cumulative_Intraday_Return),
    names_to  = "Return_Type",
    values_to = "Cumulative_Return"
  )

# Plot all cumulative returns on one chart
p9 <- ggplot(combined_returns_long,
             aes(x = date, y = Cumulative_Return,
                 color = interaction(Index, Return_Type))) +
  geom_line() +
  scale_color_manual(values = c(
    "Nasdaq.Cumulative_Overnight_Return"     = "blue", 
    "Nasdaq.Cumulative_Intraday_Return"      = "red",
    "Dow Jones.Cumulative_Overnight_Return"  = "green",
    "Dow Jones.Cumulative_Intraday_Return"   = "purple"
  )) +
  labs(title = "Cumulative Overnight and Intraday Returns for Nasdaq and Dow Jones",
       x = "Date", y = "Cumulative Return", color = "Index & Return Type") +
  theme_minimal(base_size = 15) +
  theme(plot.background   = element_rect(fill = "white"),
        panel.background  = element_rect(fill = "white"))
create_and_save_plot(p9, "cumulative_returns_combined_plot.png")

###############################################################################
# Print final cumulative returns
###############################################################################
cat("Final Cumulative Returns for Dow Jones:\n")
print(
  djia_returns %>%
    select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>%
    tail(1)
)

cat("Final Cumulative Returns for Nasdaq:\n")
print(
  nasdaq_returns %>%
    select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>%
    tail(1)
)

###############################################################################
# 13) Moving Averages (50-day and 200-day)
###############################################################################
plot_moving_averages <- function(ma_data, title) {
  ggplot(ma_data, aes(x = date)) +
    geom_line(aes(y = Close),  color = "black", na.rm = TRUE) +
    geom_line(aes(y = MA_50),  color = "blue",  na.rm = TRUE) +
    geom_line(aes(y = MA_200), color = "red",   na.rm = TRUE) +
    labs(title = title, x = "Date", y = "Price") +
    theme_minimal(base_size = 15) +
    theme(plot.background   = element_rect(fill = "white"),
          panel.background  = element_rect(fill = "white"))
}

# Calculate and plot moving averages
djia_ma <- data.frame(
  date  = djia_df$date,
  Close = djia_df$DJI.Close,
  MA_50 = SMA(djia_df$DJI.Close,  n = 50),
  MA_200= SMA(djia_df$DJI.Close,  n = 200)
)
nasdaq_ma <- data.frame(
  date  = nasdaq_df$date,
  Close = nasdaq_df$IXIC.Close,
  MA_50 = SMA(nasdaq_df$IXIC.Close,  n = 50),
  MA_200= SMA(nasdaq_df$IXIC.Close,  n = 200)
)

p10 <- plot_moving_averages(djia_ma,   "Dow Jones with 50 & 200-Day Moving Averages")
p11 <- plot_moving_averages(nasdaq_ma, "Nasdaq with 50 & 200-Day Moving Averages")

print(p10)
print(p11)
ggsave("djia_moving_averages_plot.png",   plot = p10, width = 10, height = 6)
ggsave("nasdaq_moving_averages_plot.png", plot = p11, width = 10, height = 6)

###############################################################################
# 14) Heatmap of Returns Correlations (Daily, Weekly, Monthly, etc.)
###############################################################################
# Calculate returns for different periods
periods <- c("daily", "weekly", "monthly")
djia_returns_periods   <- calculate_period_returns(djia_data,   periods)
nasdaq_returns_periods <- calculate_period_returns(nasdaq_data, periods)

# Merge the returns by date
djia_returns_df   <- data.frame(Date = index(djia_returns_periods),   coredata(djia_returns_periods))
nasdaq_returns_df <- data.frame(Date = index(nasdaq_returns_periods), coredata(nasdaq_returns_periods))

# Combine returns into a single data frame
all_returns <- merge(djia_returns_df, nasdaq_returns_df, by = "Date", suffixes = c("_DJI", "_IXIC"))
all_returns <- na.omit(all_returns)

# Calculate correlations
correlations <- cor(all_returns[,-1], use = "complete.obs")

# Plot enhanced heatmap using ggcorrplot
heatmap_plot <- ggcorrplot(
  correlations,
  method = "circle",
  type   = "lower",
  lab    = TRUE,
  lab_size = 3,
  colors   = c("blue", "white", "red"),
  title    = "Heatmap of Returns Correlations",
  ggtheme  = theme_minimal(base_size = 15) +
    theme(
      plot.background   = element_rect(fill = "lightgrey"),
      panel.background  = element_rect(fill = "white")
    )
)

create_and_save_plot(heatmap_plot, "enhanced_heatmap_returns_correlations.png")

###############################################################################
# 15) Function to calculate monthly & quarterly returns for seasonal analysis
###############################################################################
calculate_seasonal_returns <- function(data) {
  monthly_returns    <- periodReturn(data, period = "monthly",   type = "log")
  quarterly_returns  <- periodReturn(data, period = "quarterly", type = "log")
  
  monthly_df <- data.frame(date = index(monthly_returns), monthly.returns = coredata(monthly_returns))
  quarterly_df <- data.frame(date = index(quarterly_returns), quarterly.returns = coredata(quarterly_returns))
  
  monthly_df$month   <- factor(month(monthly_df$date), levels = 1:12, labels = month.name)
  quarterly_df$quarter <- factor(quarter(quarterly_df$date), levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
  
  list(monthly = monthly_df, quarterly = quarterly_df)
}

###############################################################################
# 16) Function to create and save seasonal plots
###############################################################################
create_seasonal_plots <- function(seasonal_data, filename_prefix, index_name) {
  # High-quality theme for plots
  professional_theme <- theme_minimal(base_size = 15) +
    theme(
      plot.background   = element_rect(fill = "white"),
      panel.background  = element_rect(fill = "white"),
      panel.grid.major  = element_line(color = "gray80"),
      panel.grid.minor  = element_line(color = "gray90"),
      legend.position   = "none",
      axis.text         = element_text(color = "black"),
      axis.title        = element_text(face = "bold"),
      plot.title        = element_text(hjust = 0.5, face = "bold")
    )
  
  # Monthly histogram plot
  p_monthly_hist <- ggplot(seasonal_data$monthly, aes(x = monthly.returns, fill = month)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_brewer(palette = "Paired") +
    labs(
      title   = paste("Histogram of Monthly Returns for", index_name),
      x       = "Log Returns",
      y       = "Frequency",
      caption = paste("Data Source: Yahoo Finance\nPeriod: 2000-2024\nIndex:", index_name)
    ) +
    facet_wrap(~month, scales = "free_y") +
    professional_theme
  
  cat("Monthly Histogram Plot:\n")
  print(p_monthly_hist)
  ggsave(paste0(filename_prefix, "_monthly_hist.png"), 
         plot = p_monthly_hist, width = 10, height = 6, dpi = 300)
  
  # Quarterly density plot
  p_quarterly_density <- ggplot(seasonal_data$quarterly, aes(x = quarterly.returns, fill = quarter)) +
    geom_density(alpha = 0.7) +
    scale_fill_brewer(palette = "Paired") +
    labs(
      title   = paste("Density Plot of Quarterly Returns for", index_name),
      x       = "Log Returns",
      y       = "Density",
      caption = paste("Data Source: Yahoo Finance\nPeriod: 2000-2024\nIndex:", index_name)
    ) +
    facet_wrap(~quarter, scales = "free_y") +
    professional_theme
  
  cat("Quarterly Density Plot:\n")
  print(p_quarterly_density)
  ggsave(paste0(filename_prefix, "_quarterly_density.png"), 
         plot = p_quarterly_density, width = 10, height = 6, dpi = 300)
  
  # Average monthly return plot
  avg_monthly_returns <- seasonal_data$monthly %>%
    group_by(month) %>%
    summarise(avg_return = mean(monthly.returns))
  
  p_avg_monthly_return <- ggplot(avg_monthly_returns, aes(x = month, y = avg_return, fill = month)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    scale_fill_brewer(palette = "Paired") +
    labs(
      title   = paste("Average Monthly Returns for", index_name),
      x       = "Month",
      y       = "Average Log Return",
      caption = paste("Data Source: Yahoo Finance\nPeriod: 2000-2024\nIndex:", index_name)
    ) +
    professional_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  cat("Average Monthly Return Plot:\n")
  print(p_avg_monthly_return)
  ggsave(paste0(filename_prefix, "_avg_monthly_return.png"), 
         plot = p_avg_monthly_return, width = 10, height = 6, dpi = 300)
  
  # Median monthly return plot
  median_monthly_returns <- seasonal_data$monthly %>%
    group_by(month) %>%
    summarise(median_return = median(monthly.returns))
  
  p_median_monthly_return <- ggplot(median_monthly_returns, aes(x = month, y = median_return, fill = month)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    scale_fill_brewer(palette = "Paired") +
    labs(
      title   = paste("Median Monthly Returns for", index_name),
      x       = "Month",
      y       = "Median Log Return",
      caption = paste("Data Source: Yahoo Finance\nPeriod: 2000-2024\nIndex:", index_name)
    ) +
    professional_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  cat("Median Monthly Return Plot:\n")
  print(p_median_monthly_return)
  ggsave(paste0(filename_prefix, "_median_monthly_return.png"), 
         plot = p_median_monthly_return, width = 10, height = 6, dpi = 300)
  
  # Return these plots if you want to reuse them
  list(
    monthly_hist         = p_monthly_hist, 
    quarterly_density    = p_quarterly_density, 
    avg_monthly_return   = p_avg_monthly_return, 
    median_monthly_return= p_median_monthly_return
  )
}

###############################################################################
# 17) Calculate seasonal returns for Nasdaq and DJIA, then create seasonal plots
###############################################################################
nasdaq_seasonal_returns <- calculate_seasonal_returns(nasdaq_data)
djia_seasonal_returns   <- calculate_seasonal_returns(djia_data)

nasdaq_plots <- create_seasonal_plots(nasdaq_seasonal_returns, "nasdaq", "NASDAQ Composite")
djia_plots   <- create_seasonal_plots(djia_seasonal_returns,   "djia",   "Dow Jones Industrial Average")

# Print average and median monthly returns
cat("Average Monthly Returns for Dow Jones:\n")
print(
  djia_seasonal_returns$monthly %>%
    group_by(month) %>%
    summarise(avg_return = mean(monthly.returns))
)

cat("Average Monthly Returns for Nasdaq:\n")
print(
  nasdaq_seasonal_returns$monthly %>%
    group_by(month) %>%
    summarise(avg_return = mean(monthly.returns))
)

cat("Median Monthly Returns for Dow Jones:\n")
print(
  djia_seasonal_returns$monthly %>%
    group_by(month) %>%
    summarise(median_return = median(monthly.returns))
)

cat("Median Monthly Returns for Nasdaq:\n")
print(
  nasdaq_seasonal_returns$monthly %>%
    group_by(month) %>%
    summarise(median_return = median(monthly.returns))
)

###############################################################################
# 18) Save the environment for future usage
###############################################################################
save.image(file = "data_analysis_environment.RData")
