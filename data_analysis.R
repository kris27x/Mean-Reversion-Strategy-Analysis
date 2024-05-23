# data_analysis.R
# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(TTR)
library(PerformanceAnalytics)

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

# Download historical data for Dow Jones and Nasdaq
getSymbols(c("^DJI", "^IXIC"), src = "yahoo", from = "2000-01-01")

# Extract the necessary data and calculate returns for Nasdaq
nasdaq_data <- data.frame(date = index(IXIC), coredata(IXIC))
nasdaq_returns <- calculate_returns(nasdaq_data, "IXIC.Close", "IXIC.Open")

# Extract the necessary data and calculate returns for Dow Jones
djia_data <- data.frame(date = index(DJI), coredata(DJI))
djia_returns <- calculate_returns(djia_data, "DJI.Close", "DJI.Open")

# Exploratory Data Analysis (EDA)
# Display first few rows of the data
head(djia_data)
head(nasdaq_data)

# Descriptive Statistics
djia_stats <- summary(djia_data[["DJI.Close"]])
nasdaq_stats <- summary(nasdaq_data[["IXIC.Close"]])
djia_returns_stats <- summary(dailyReturn(DJI))
nasdaq_returns_stats <- summary(dailyReturn(IXIC))

print("Descriptive Statistics for DJI Closing Prices:")
print(djia_stats)
print("Descriptive Statistics for IXIC Closing Prices:")
print(nasdaq_stats)
print("Descriptive Statistics for DJI Daily Returns:")
print(djia_returns_stats)
print("Descriptive Statistics for IXIC Daily Returns:")
print(nasdaq_returns_stats)

# Plot the closing prices
p1 <- ggplot() +
  geom_line(data = djia_data, aes(x = date, y = DJI.Close), color = "blue", na.rm = TRUE) +
  geom_line(data = nasdaq_data, aes(x = date, y = IXIC.Close), color = "red", na.rm = TRUE) +
  labs(title = "Dow Jones and Nasdaq Closing Prices", x = "Date", y = "Closing Price")
print(p1)
ggsave("closing_prices_plot.png")

# Plot histograms of returns
djia_returns_daily <- dailyReturn(DJI)
nasdaq_returns_daily <- dailyReturn(IXIC)
p2 <- ggplot() +
  geom_histogram(data = data.frame(djia_returns_daily), aes(x = daily.returns), bins = 100, fill = "blue", alpha = 0.5, na.rm = TRUE) +
  geom_histogram(data = data.frame(nasdaq_returns_daily), aes(x = daily.returns), bins = 100, fill = "red", alpha = 0.5, na.rm = TRUE) +
  labs(title = "Histogram of Daily Returns", x = "Daily Returns", y = "Frequency")
print(p2)
ggsave("daily_returns_histogram.png")

# Calculate annual returns as the difference between the first day of the year's opening price and the last day of the year's closing price
calc_annual_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date)) %>%
    summarize(AnnualReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) / first(.data[[open_col]]), .groups = 'drop')
}

djia_annual_returns <- calc_annual_return(djia_data, "DJI.Open", "DJI.Close")
nasdaq_annual_returns <- calc_annual_return(nasdaq_data, "IXIC.Open", "IXIC.Close")
p3 <- ggplot() +
  geom_line(data = djia_annual_returns, aes(x = year, y = AnnualReturn), color = "blue") +
  geom_line(data = nasdaq_annual_returns, aes(x = year, y = AnnualReturn), color = "red") +
  labs(title = "Annual Returns of Dow Jones and Nasdaq", x = "Year", y = "Annual Return")
print(p3)
ggsave("annual_returns_plot.png")

# Calculate monthly returns as the difference between the first day of the month's opening price and the last day of the month's closing price
calc_monthly_return <- function(data, open_col, close_col) {
  data %>%
    group_by(year = year(date), month = month(date)) %>%
    summarize(MonthlyReturn = (last(.data[[close_col]]) - first(.data[[open_col]])) / first(.data[[open_col]]), .groups = 'drop') %>%
    mutate(date = make_date(year, month, 1))
}

djia_monthly_returns <- calc_monthly_return(djia_data, "DJI.Open", "DJI.Close")
nasdaq_monthly_returns <- calc_monthly_return(nasdaq_data, "IXIC.Open", "IXIC.Close")
p4 <- ggplot() +
  geom_line(data = djia_monthly_returns, aes(x = date, y = MonthlyReturn), color = "blue") +
  geom_line(data = nasdaq_monthly_returns, aes(x = date, y = MonthlyReturn), color = "red") +
  labs(title = "Monthly Returns of Dow Jones and Nasdaq", x = "Date", y = "Monthly Return")
print(p4)
ggsave("monthly_returns_plot.png")

# Volatility Analysis
djia_volatility <- runSD(dailyReturn(DJI), n = 30) * sqrt(252)
nasdaq_volatility <- runSD(dailyReturn(IXIC), n = 30) * sqrt(252)

volatility_data <- data.frame(
  date = index(djia_volatility),
  DJI_Volatility = coredata(djia_volatility),
  IXIC_Volatility = coredata(nasdaq_volatility)
)

volatility_data <- na.omit(volatility_data)
print("First few rows of volatility data:")
print(head(volatility_data))

p5 <- ggplot(volatility_data, aes(x = date)) +
  geom_line(aes(y = DJI_Volatility), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Volatility), color = "red", na.rm = TRUE) +
  labs(title = "Rolling 30-Day Volatility", x = "Date", y = "Volatility (Annualized)")
print(p5)
ggsave("volatility_plot.png")

# Performance during Bull and Bear Markets
# Define bull and bear markets (e.g., based on a 20% move from peak/trough)
# This section needs specific business logic for identifying bull/bear phases

# Sector Composition Analysis (Placeholder, data needs to be added)
# Example: Comparing sector weights if sector data is available

# Risk On/Off Market Sentiment (Placeholder, requires additional data)
# Example: Using VIX index data for sentiment analysis

# Correlation Analysis
returns_xts <- merge(dailyReturn(DJI), dailyReturn(IXIC), all = FALSE)
colnames(returns_xts) <- c("DJI_Returns", "IXIC_Returns")
returns_data <- data.frame(date = index(returns_xts), coredata(returns_xts))
returns_data <- na.omit(returns_data)

correlation <- cor(returns_data$DJI_Returns, returns_data$IXIC_Returns, use = "complete.obs")
print(paste("Correlation between Dow Jones and Nasdaq returns: ", correlation))

p6 <- ggplot(returns_data, aes(x = date)) +
  geom_line(aes(y = DJI_Returns), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = IXIC_Returns), color = "red", na.rm = TRUE) +
  labs(title = "Daily Returns of Dow Jones and Nasdaq", x = "Date", y = "Daily Returns")
print(p6)
ggsave("daily_returns_plot.png")

p7 <- ggplot(returns_data, aes(x = DJI_Returns, y = IXIC_Returns)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Daily Returns", x = "DJI Returns", y = "IXIC Returns")
print(p7)
ggsave("scatter_plot_returns.png")

# Rolling Correlation
rolling_correlation <- rollapply(returns_xts, width = 30, FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
rolling_correlation_data <- data.frame(date = index(rolling_correlation), Rolling_Correlation = coredata(rolling_correlation))
rolling_correlation_data <- na.omit(rolling_correlation_data)

p8 <- ggplot(rolling_correlation_data, aes(x = date, y = Rolling_Correlation)) +
  geom_line(color = "purple", na.rm = TRUE) +
  labs(title = "Rolling 30-Day Correlation between Dow Jones and Nasdaq Returns", x = "Date", y = "Correlation")
print(p8)
ggsave("rolling_correlation_plot.png")

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
p12 <- ggplot(combined_returns_long, aes(x = date, y = Cumulative_Return, color = interaction(Index, Return_Type))) +
  geom_line() +
  scale_color_manual(values = c("Nasdaq.Cumulative_Overnight_Return" = "blue", 
                                "Nasdaq.Cumulative_Intraday_Return" = "red",
                                "Dow Jones.Cumulative_Overnight_Return" = "green",
                                "Dow Jones.Cumulative_Intraday_Return" = "purple")) +
  labs(title = "Cumulative Overnight and Intraday Returns for Nasdaq and Dow Jones",
       x = "Date", y = "Cumulative Return", color = "Index and Return Type") +
  theme_minimal()

# Save the plot
print(p12)
ggsave("cumulative_returns_combined_plot.png", width = 10, height = 6)

# Print cumulative returns
print("Final Cumulative Returns for Dow Jones:")
print(djia_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% tail(1))

print("Final Cumulative Returns for Nasdaq:")
print(nasdaq_returns %>% select(date, Cumulative_Overnight_Return, Cumulative_Intraday_Return) %>% tail(1))

# Moving Averages
djia_ma <- data.frame(date = djia_data$date, Close = djia_data$DJI.Close,
                      MA_50 = SMA(djia_data$DJI.Close, n = 50),
                      MA_200 = SMA(djia_data$DJI.Close, n = 200))

nasdaq_ma <- data.frame(date = nasdaq_data$date, Close = nasdaq_data$IXIC.Close,
                        MA_50 = SMA(nasdaq_data$IXIC.Close, n = 50),
                        MA_200 = SMA(nasdaq_data$IXIC.Close, n = 200))

p10 <- ggplot(djia_ma, aes(x = date)) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  geom_line(aes(y = MA_50), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = MA_200), color = "red", na.rm = TRUE) +
  labs(title = "Dow Jones with 50 and 200-Day Moving Averages", x = "Date", y = "Price")
print(p10)
ggsave("djia_moving_averages_plot.png")

p11 <- ggplot(nasdaq_ma, aes(x = date)) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  geom_line(aes(y = MA_50), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = MA_200), color = "red", na.rm = TRUE) +
  labs(title = "Nasdaq with 50 and 200-Day Moving Averages", x = "Date", y = "Price")
print(p11)
ggsave("nasdaq_moving_averages_plot.png")

# Save the environment for later use
save.image(file = "data_analysis_environment.RData")
