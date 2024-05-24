# data_analysis.R

# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(TTR)
library(PerformanceAnalytics)
library(reshape2)
library(ggcorrplot)

# Function to handle missing values
handle_missing_values <- function(data) {
  data <- na.approx(data, maxgap = Inf, na.rm = FALSE)  # Linear interpolation
  data <- na.locf(data, fromLast = FALSE)  # Forward fill remaining NAs
  data <- na.locf(data, fromLast = TRUE)  # Backward fill remaining NAs
  na.omit(data)  # Remove any remaining NAs
}

# Function to calculate returns
calculate_returns <- function(data, close_col, open_col) {
  data %>%
    mutate(
      Closing = lag(.data[[close_col]], 1),
      Opening = .data[[open_col]],
      Overnight_Return = (Opening / Closing) - 1,
      Intraday_Return = (.data[[close_col]] / Opening) - 1
    ) %>%
    mutate(
      Cumulative_Overnight_Return = cumprod(1 + ifelse(is.na(Overnight_Return), 0, Overnight_Return)),
      Cumulative_Intraday_Return = cumprod(1 + ifelse(is.na(Intraday_Return), 0, Intraday_Return))
    )
}

# Function to download and handle data
get_data <- function(symbol) {
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    handle_missing_values(data)
  }, error = function(e) {
    stop(paste("Error downloading data for symbol:", symbol))
  })
}

# Function to calculate returns for different periods
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

# Download historical data
djia_data <- get_data("^DJI")
nasdaq_data <- get_data("^IXIC")

# Convert to data frames
djia_df <- data.frame(date = index(djia_data), coredata(djia_data))
nasdaq_df <- data.frame(date = index(nasdaq_data), coredata(nasdaq_data))

# Calculate returns
djia_returns <- calculate_returns(djia_df, "DJI.Close", "DJI.Open")
nasdaq_returns <- calculate_returns(nasdaq_df, "IXIC.Close", "IXIC.Open")

# Exploratory Data Analysis (EDA)
print(head(djia_df))
print(head(nasdaq_df))

# Descriptive Statistics
djia_stats <- summary(djia_df[["DJI.Close"]])
nasdaq_stats <- summary(nasdaq_df[["IXIC.Close"]])
djia_returns_stats <- summary(dailyReturn(djia_data))
nasdaq_returns_stats <- summary(dailyReturn(nasdaq_data))

cat("Descriptive Statistics for DJI Closing Prices:\n", djia_stats)
cat("Descriptive Statistics for IXIC Closing Prices:\n", nasdaq_stats)
cat("Descriptive Statistics for DJI Daily Returns:\n", djia_returns_stats)
cat("Descriptive Statistics for IXIC Daily Returns:\n", nasdaq_returns_stats)

# Function to create and save plots
create_and_save_plot <- function(plot, filename) {
  print(plot)
  ggsave(filename, plot = plot, width = 10, height = 6)
}

# Plot closing prices
p1 <- ggplot() +
  geom_line(data = djia_df, aes(x = date, y = DJI.Close), color = "blue", na.rm = TRUE) +
  geom_line(data = nasdaq_df, aes(x = date, y = IXIC.Close), color = "red", na.rm = TRUE) +
  labs(title = "Dow Jones and Nasdaq Closing Prices", x = "Date", y = "Closing Price") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p1, "closing_prices_plot.png")

# Plot histograms of returns
djia_returns_daily <- dailyReturn(djia_data)
nasdaq_returns_daily <- dailyReturn(nasdaq_data)
p2 <- ggplot() +
  geom_histogram(data = data.frame(djia_returns_daily), aes(x = daily.returns), bins = 100, fill = "blue", alpha = 0.5, na.rm = TRUE) +
  geom_histogram(data = data.frame(nasdaq_returns_daily), aes(x = daily.returns), bins = 100, fill = "red", alpha = 0.5, na.rm = TRUE) +
  labs(title = "Histogram of Daily Returns", x = "Daily Returns", y = "Frequency") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p2, "daily_returns_histogram.png")

# Function to calculate annual returns
calc_annual_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date)) %>%
    summarize(AnnualReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) / first(.data[[open_col]]), .groups = 'drop')
}

# Calculate and plot annual returns as scatter plot
djia_annual_returns <- calc_annual_return(djia_df, "DJI.Open", "DJI.Close")
nasdaq_annual_returns <- calc_annual_return(nasdaq_df, "IXIC.Open", "IXIC.Close")

p3 <- ggplot() +
  geom_point(data = djia_annual_returns, aes(x = year, y = AnnualReturn, color = "Dow Jones")) +
  geom_point(data = nasdaq_annual_returns, aes(x = year, y = AnnualReturn, color = "Nasdaq")) +
  scale_color_manual(values = c("Dow Jones" = "blue", "Nasdaq" = "red")) +
  labs(title = "Annual Returns of Dow Jones and Nasdaq", x = "Year", y = "Annual Return", color = "Index") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p3, "annual_returns_scatter_plot.png")

# Function to calculate monthly returns
calc_monthly_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date), month = month(date)) %>%
    summarize(MonthlyReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) / first(.data[[open_col]]), .groups = 'drop') %>%
    mutate(date = make_date(year, month, 1))
}

# Calculate and plot monthly returns as scatter plot
djia_monthly_returns <- calc_monthly_return(djia_df, "DJI.Open", "DJI.Close")
nasdaq_monthly_returns <- calc_monthly_return(nasdaq_df, "IXIC.Open", "IXIC.Close")

p4 <- ggplot() +
  geom_point(data = djia_monthly_returns, aes(x = date, y = MonthlyReturn, color = "Dow Jones")) +
  geom_point(data = nasdaq_monthly_returns, aes(x = date, y = MonthlyReturn, color = "Nasdaq")) +
  scale_color_manual(values = c("Dow Jones" = "blue", "Nasdaq" = "red")) +
  labs(title = "Monthly Returns of Dow Jones and Nasdaq", x = "Date", y = "Monthly Return", color = "Index") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p4, "monthly_returns_scatter_plot.png")

# Volatility Analysis
djia_volatility <- runSD(dailyReturn(djia_data), n = 30) * sqrt(252)
nasdaq_volatility <- runSD(dailyReturn(nasdaq_data), n = 30) * sqrt(252)

volatility_data <- data.frame(
  date = index(djia_volatility),
  DJI_Volatility = coredata(djia_volatility),
  IXIC_Volatility = coredata(nasdaq_volatility)
)

volatility_data <- na.omit(volatility_data)
print("First few rows of volatility data:")
print(head(volatility_data))

# Plot volatility
p5 <- ggplot(volatility_data, aes(x = date)) +
  geom_line(aes(y = DJI_Volatility), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Volatility), color = "red", na.rm = TRUE) +
  labs(title = "Rolling 30-Day Volatility", x = "Date", y = "Volatility (Annualized)") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p5, "volatility_plot.png")

# Correlation Analysis
returns_xts <- merge(dailyReturn(djia_data), dailyReturn(nasdaq_data), all = FALSE)
colnames(returns_xts) <- c("DJI_Returns", "IXIC_Returns")
returns_data <- data.frame(date = index(returns_xts), coredata(returns_xts))
returns_data <- na.omit(returns_data)

correlation <- cor(returns_data$DJI_Returns, returns_data$IXIC_Returns, use = "complete.obs")
cat("Correlation between Dow Jones and Nasdaq returns: ", correlation)

# Plot daily returns
p6 <- ggplot(returns_data, aes(x = date)) +
  geom_line(aes(y = DJI_Returns), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Returns), color = "red", na.rm = TRUE) +
  labs(title = "Daily Returns of Dow Jones and Nasdaq", x = "Date", y = "Daily Returns") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p6, "daily_returns_plot.png")

# Scatter plot of returns
p7 <- ggplot(returns_data, aes(x = DJI_Returns, y = IXIC_Returns)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Daily Returns", x = "DJI Returns", y = "IXIC Returns") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p7, "scatter_plot_returns.png")

# Rolling Correlation
rolling_correlation <- rollapply(returns_xts, width = 30, FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
rolling_correlation_data <- data.frame(date = index(rolling_correlation), Rolling_Correlation = coredata(rolling_correlation))
rolling_correlation_data <- na.omit(rolling_correlation_data)

# Plot rolling correlation
p8 <- ggplot(rolling_correlation_data, aes(x = date, y = Rolling_Correlation)) +
  geom_line(color = "purple") +
  labs(title = "Rolling 30-Day Correlation between Dow Jones and Nasdaq Returns", x = "Date", y = "Correlation") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
create_and_save_plot(p8, "rolling_correlation_plot.png")

# Combine the data for plotting cumulative returns
combined_returns <- bind_rows(
  nasdaq_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% mutate(Index = "Nasdaq"),
  djia_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% mutate(Index = "Dow Jones")
)

# Reshape the data for ggplot
combined_returns_long <- combined_returns %>%
  pivot_longer(cols = c(Cumulative_Overnight_Return, Cumulative_Intraday_Return), 
               names_to = "Return_Type", values_to = "Cumulative_Return")

# Plot all cumulative returns on one chart
p9 <- ggplot(combined_returns_long, aes(x = date, y = Cumulative_Return, color = interaction(Index, Return_Type))) +
  geom_line() +
  scale_color_manual(values = c("Nasdaq.Cumulative_Overnight_Return" = "blue", 
                                "Nasdaq.Cumulative_Intraday_Return" = "red",
                                "Dow Jones.Cumulative_Overnight_Return" = "green",
                                "Dow Jones.Cumulative_Intraday_Return" = "purple")) +
  labs(title = "Cumulative Overnight and Intraday Returns for Nasdaq and Dow Jones",
       x = "Date", y = "Cumulative Return", color = "Index and Return Type") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))
create_and_save_plot(p9, "cumulative_returns_combined_plot.png")

# Print cumulative returns
cat("Final Cumulative Returns for Dow Jones:\n")
print(djia_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% tail(1))

cat("Final Cumulative Returns for Nasdaq:\n")
print(nasdaq_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% tail(1))

# Moving Averages
plot_moving_averages <- function(ma_data, title) {
  ggplot(ma_data, aes(x = date)) +
    geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
    geom_line(aes(y = MA_50), color = "blue", na.rm = TRUE) +
    geom_line(aes(y = MA_200), color = "red", na.rm = TRUE) +
    labs(title = title, x = "Date", y = "Price") +
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
}

# Calculate and plot moving averages
djia_ma <- data.frame(date = djia_df$date, Close = djia_df$DJI.Close,
                      MA_50 = SMA(djia_df$DJI.Close, n = 50),
                      MA_200 = SMA(djia_df$DJI.Close, n = 200))

nasdaq_ma <- data.frame(date = nasdaq_df$date, Close = nasdaq_df$IXIC.Close,
                        MA_50 = SMA(nasdaq_df$IXIC.Close, n = 50),
                        MA_200 = SMA(nasdaq_df$IXIC.Close, n = 200))

p10 <- plot_moving_averages(djia_ma, "Dow Jones with 50 and 200-Day Moving Averages")
p11 <- plot_moving_averages(nasdaq_ma, "Nasdaq with 50 and 200-Day Moving Averages")

print(p10)
print(p11)
ggsave("djia_moving_averages_plot.png", plot = p10, width = 10, height = 6)
ggsave("nasdaq_moving_averages_plot.png", plot = p11, width = 10, height = 6)

# Heatmap of Returns Correlations
# Calculate returns for different periods
periods <- c("daily", "weekly", "monthly")
djia_returns_periods <- calculate_period_returns(djia_data, periods)
nasdaq_returns_periods <- calculate_period_returns(nasdaq_data, periods)

# Merge the returns by date
djia_returns <- data.frame(Date = index(djia_returns_periods), coredata(djia_returns_periods))
nasdaq_returns <- data.frame(Date = index(nasdaq_returns_periods), coredata(nasdaq_returns_periods))

# Combine returns into a single data frame
all_returns <- merge(djia_returns, nasdaq_returns, by = "Date", suffixes = c("_DJI", "_IXIC"))
all_returns <- na.omit(all_returns)

# Calculate correlations
correlations <- cor(all_returns[,-1], use = "complete.obs")

# Plot enhanced heatmap using ggcorrplot
heatmap_plot <- ggcorrplot(correlations, 
                           method = "circle", 
                           type = "lower", 
                           lab = TRUE, 
                           lab_size = 3, 
                           colors = c("blue", "white", "red"), 
                           title = "Heatmap of Returns Correlations",
                           ggtheme = theme_minimal(base_size = 15) +
                             theme(plot.background = element_rect(fill = "lightgrey"), 
                                   panel.background = element_rect(fill = "white")))

# Save the plot
create_and_save_plot(heatmap_plot, "enhanced_heatmap_returns_correlations.png")

# Save the environment for later use
save.image(file = "data_analysis_environment.RData")
