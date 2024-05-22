# data_analysis.R
# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(TTR)

# Download historical data for Dow Jones and Nasdaq
getSymbols(c("^DJI", "^IXIC"), src = "yahoo", from = "2000-01-01")

# Convert data to data frames for analysis
djia_data <- data.frame(date = index(DJI), coredata(DJI))
nasdaq_data <- data.frame(date = index(IXIC), coredata(IXIC))

# Exploratory Data Analysis (EDA)
# Display first few rows of the data
head(djia_data)
head(nasdaq_data)

# Descriptive Statistics
djia_stats <- summary(djia_data$DJI.Close)
nasdaq_stats <- summary(nasdaq_data$IXIC.Close)
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
  geom_line(data = djia_data, aes(x = date, y = DJI.Close), color = "blue") +
  geom_line(data = nasdaq_data, aes(x = date, y = IXIC.Close), color = "red") +
  labs(title = "Dow Jones and Nasdaq Closing Prices", x = "Date", y = "Closing Price")
print(p1)
ggsave("closing_prices_plot.png")

# Plot histograms of returns
djia_returns <- dailyReturn(DJI)
nasdaq_returns <- dailyReturn(IXIC)
p2 <- ggplot() +
  geom_histogram(data = data.frame(djia_returns), aes(x = daily.returns), bins = 100, fill = "blue", alpha = 0.5) +
  geom_histogram(data = data.frame(nasdaq_returns), aes(x = daily.returns), bins = 100, fill = "red", alpha = 0.5) +
  labs(title = "Histogram of Daily Returns", x = "Daily Returns", y = "Frequency")
print(p2)
ggsave("daily_returns_histogram.png")

# Correlation Analysis
# Calculate daily returns
djia_returns <- dailyReturn(DJI)
nasdaq_returns <- dailyReturn(IXIC)

# Merge returns into a single xts object
returns_xts <- merge(djia_returns, nasdaq_returns, all = FALSE)
colnames(returns_xts) <- c("DJI_Returns", "IXIC_Returns")

# Convert to data frame and remove NA rows
returns_data <- data.frame(date = index(returns_xts), coredata(returns_xts))
returns_data <- na.omit(returns_data)

# Calculate and print correlation
correlation <- cor(returns_data$DJI_Returns, returns_data$IXIC_Returns, use = "complete.obs")
print(paste("Correlation between Dow Jones and Nasdaq returns: ", correlation))

# Plot the returns
p3 <- ggplot(returns_data, aes(x = date)) +
  geom_line(aes(y = DJI_Returns), color = "blue") +
  geom_line(aes(y = IXIC_Returns), color = "red") +
  labs(title = "Daily Returns of Dow Jones and Nasdaq", x = "Date", y = "Daily Returns")
print(p3)
ggsave("daily_returns_plot.png")

# Scatter plot of returns
p4 <- ggplot(returns_data, aes(x = DJI_Returns, y = IXIC_Returns)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Daily Returns", x = "DJI Returns", y = "IXIC Returns")
print(p4)
ggsave("scatter_plot_returns.png")

# Volatility Analysis
djia_volatility <- runSD(djia_returns, n = 30) * sqrt(252)
nasdaq_volatility <- runSD(nasdaq_returns, n = 30) * sqrt(252)

# Convert volatility to data frames and add dates
volatility_data <- data.frame(
  date = index(djia_volatility),
  DJI_Volatility = coredata(djia_volatility),
  IXIC_Volatility = coredata(nasdaq_volatility)
)

# Remove rows with NA values in volatility data
volatility_data <- na.omit(volatility_data)

# Debugging: Print the first few rows of the volatility data frame
print("First few rows of volatility data:")
print(head(volatility_data))

# Plot the rolling volatility
p5 <- ggplot(volatility_data, aes(x = date)) +
  geom_line(aes(y = DJI_Volatility), color = "blue") +
  geom_line(aes(y = IXIC_Volatility), color = "red") +
  labs(title = "Rolling 30-Day Volatility", x = "Date", y = "Volatility (Annualized)")
print(p5)
ggsave("volatility_plot.png")

# Moving Averages
djia_ma <- data.frame(date = djia_data$date, Close = djia_data$DJI.Close,
                      MA_50 = SMA(djia_data$DJI.Close, n = 50),
                      MA_200 = SMA(djia_data$DJI.Close, n = 200))
nasdaq_ma <- data.frame(date = nasdaq_data$date, Close = nasdaq_data$IXIC.Close,
                        MA_50 = SMA(nasdaq_data$IXIC.Close, n = 50),
                        MA_200 = SMA(nasdaq_data$IXIC.Close, n = 200))

p6 <- ggplot(djia_ma, aes(x = date)) +
  geom_line(aes(y = Close), color = "black") +
  geom_line(aes(y = MA_50), color = "blue") +
  geom_line(aes(y = MA_200), color = "red") +
  labs(title = "Dow Jones with 50 and 200-Day Moving Averages", x = "Date", y = "Price")
print(p6)
ggsave("djia_moving_averages_plot.png")

p7 <- ggplot(nasdaq_ma, aes(x = date)) +
  geom_line(aes(y = Close), color = "black") +
  geom_line(aes(y = MA_50), color = "blue") +
  geom_line(aes(y = MA_200), color = "red") +
  labs(title = "Nasdaq with 50 and 200-Day Moving Averages", x = "Date", y = "Price")
print(p7)
ggsave("nasdaq_moving_averages_plot.png")

# Rolling Correlation
rolling_correlation <- rollapply(returns_xts, width = 30, FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)

rolling_correlation_data <- data.frame(date = index(rolling_correlation), Rolling_Correlation = coredata(rolling_correlation))

# Remove rows with NA values in rolling correlation data
rolling_correlation_data <- na.omit(rolling_correlation_data)

p8 <- ggplot(rolling_correlation_data, aes(x = date, y = Rolling_Correlation)) +
  geom_line(color = "purple") +
  labs(title = "Rolling 30-Day Correlation between Dow Jones and Nasdaq Returns", x = "Date", y = "Correlation")
print(p8)
ggsave("rolling_correlation_plot.png")

# Save the environment for later use
save.image(file = "data_analysis_environment.RData")
