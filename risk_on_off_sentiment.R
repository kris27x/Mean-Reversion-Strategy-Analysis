# risk_on_off_sentiment.R

# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(TTR)
library(lubridate)

# Function to handle missing values in VIX data
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
    replace_na(list(Overnight_Return = 0, Intraday_Return = 0)) %>%
    mutate(
      Cumulative_Overnight_Return = cumprod(1 + Overnight_Return) - 1,
      Cumulative_Intraday_Return = cumprod(1 + Intraday_Return) - 1,
      Total_Return = cumprod((1 + Overnight_Return) * (1 + Intraday_Return)) - 1
    )
}

# Download historical data for VIX index
getSymbols("^VIX", src = "yahoo", from = "2000-01-01")
VIX <- handle_missing_values(VIX)

# Convert VIX data to a data frame
vix_data <- data.frame(date = index(VIX), coredata(VIX))

# Calculate historical mean and standard deviation of the VIX
vix_mean <- mean(vix_data$VIX.Close, na.rm = TRUE)
vix_sd <- sd(vix_data$VIX.Close, na.rm = TRUE)

# Calculate z-scores for VIX values
vix_data <- vix_data %>%
  mutate(
    Z_Score = (VIX.Close - vix_mean) / vix_sd,
    Sentiment_Score = 50 + (Z_Score * 10),
    Sentiment_Score = ifelse(Sentiment_Score < 0, 0, ifelse(Sentiment_Score > 100, 100, Sentiment_Score)),
    Sentiment_Zone = cut(Sentiment_Score, breaks = c(-Inf, 39, 42, 58, 66, Inf), labels = c("Very High", "High", "Neutral", "Low", "Very Low")),
    lead_date = lead(date, default = last(date))
  )

# Plot the VIX index and sentiment score with adjusted ranges and colors
p_risk_sentiment <- ggplot(vix_data, aes(x = date)) +
  geom_line(aes(y = VIX.Close), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Sentiment_Score), color = "red", linetype = "dashed", na.rm = TRUE) +
  geom_rect(aes(xmin = date, xmax = lead_date, ymin = 0, ymax = Inf, fill = Sentiment_Zone), alpha = 0.2) +
  scale_fill_manual(values = c("Very High" = "darkred", "High" = "darkorange", "Neutral" = "white", "Low" = "darkgreen", "Very Low" = "blue")) +
  labs(title = "VIX Index and Market Sentiment", x = "Date", y = "VIX", fill = "Sentiment Zone") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))

# Save the plot
print(p_risk_sentiment)
ggsave("vix_market_sentiment_plot.png", plot = p_risk_sentiment, width = 10, height = 6)

# Print the first few rows of VIX data with sentiment
print("First few rows of VIX data with market sentiment:")
print(head(vix_data))

# Download historical data for Dow Jones and Nasdaq
getSymbols(c("^DJI", "^IXIC"), src = "yahoo", from = "2000-01-01")

# Calculate returns for Nasdaq and Dow Jones
nasdaq_returns <- calculate_returns(data.frame(date = index(IXIC), coredata(IXIC)), "IXIC.Close", "IXIC.Open")
djia_returns <- calculate_returns(data.frame(date = index(DJI), coredata(DJI)), "DJI.Close", "DJI.Open")

# Merge VIX sentiment data with Nasdaq and Dow Jones data
nasdaq_sentiment <- nasdaq_returns %>% left_join(vix_data %>% select(date, Sentiment_Zone), by = "date")
djia_sentiment <- djia_returns %>% left_join(vix_data %>% select(date, Sentiment_Zone), by = "date")

# Function to calculate and print performance based on sentiment
calculate_performance_by_sentiment <- function(data, index_name) {
  sentiments <- c("Very Low", "Low", "Neutral", "High", "Very High")
  
  for (sentiment in sentiments) {
    cat("\nPerformance for", index_name, "during", sentiment, "sentiment:\n")
    filtered_data <- data %>% filter(Sentiment_Zone == sentiment)
    if (nrow(filtered_data) > 0) {
      overnight_return <- prod(1 + filtered_data$Overnight_Return) - 1
      intraday_return <- prod(1 + filtered_data$Intraday_Return) - 1
      total_return <- prod((1 + filtered_data$Overnight_Return) * (1 + filtered_data$Intraday_Return)) - 1
      
      cat("Overnight Return:", overnight_return, "\n")
      cat("Intraday Return:", intraday_return, "\n")
      cat("Total Return:", total_return, "\n")
    } else {
      cat("No data available for", sentiment, "sentiment.\n")
    }
  }
}

# Calculate and print performance for each sentiment for Dow Jones and Nasdaq
calculate_performance_by_sentiment(djia_sentiment, "Dow Jones")
calculate_performance_by_sentiment(nasdaq_sentiment, "Nasdaq")

# Save the environment for later use
save.image(file = "performance_analysis_based_on_sentiment.RData")
