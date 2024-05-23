# risk_on_off_sentiment.R
# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(TTR)
library(lubridate)

# Download historical data for VIX index
getSymbols("^VIX", src = "yahoo", from = "2000-01-01")

# Handle missing values in VIX data
VIX <- na.approx(VIX, maxgap = Inf, na.rm = FALSE)  # Linear interpolation
VIX <- na.locf(VIX, fromLast = FALSE)  # Forward fill remaining NAs
VIX <- na.locf(VIX, fromLast = TRUE)  # Backward fill remaining NAs
VIX <- na.omit(VIX)  # Remove any remaining NAs

# Convert VIX data to a data frame
vix_data <- data.frame(date = index(VIX), coredata(VIX))

# Calculate historical mean and standard deviation of the VIX
vix_mean <- mean(vix_data$VIX.Close, na.rm = TRUE)
vix_sd <- sd(vix_data$VIX.Close, na.rm = TRUE)

# Calculate z-scores for VIX values
vix_data <- vix_data %>%
  mutate(
    Z_Score = (VIX.Close - vix_mean) / vix_sd,
    Sentiment_Score = 50 + (Z_Score * 10)
  )

# Ensure the sentiment score is within the range [0, 100]
vix_data <- vix_data %>%
  mutate(Sentiment_Score = ifelse(Sentiment_Score < 0, 0, ifelse(Sentiment_Score > 100, 100, Sentiment_Score)))

# Create sentiment zones for plotting with adjusted ranges
vix_data <- vix_data %>%
  mutate(Sentiment_Zone = cut(Sentiment_Score, 
                              breaks = c(-Inf, 39, 42, 58, 66, Inf), 
                              labels = c("Very High", "High", "Neutral", "Low", "Very Low")))

# Create a lead date to avoid missing values in geom_rect
vix_data <- vix_data %>%
  mutate(lead_date = lead(date, default = last(date)))

# Plot the VIX index and sentiment score with adjusted ranges and colors
p_risk_sentiment <- ggplot(vix_data, aes(x = date)) +
  geom_line(aes(y = VIX.Close), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Sentiment_Score), color = "red", linetype = "dashed", na.rm = TRUE) +
  geom_rect(aes(xmin = date, xmax = lead_date, ymin = 0, ymax = Inf, fill = Sentiment_Zone), alpha = 0.2) +
  scale_fill_manual(values = c("Very High" = "darkred", "High" = "darkorange", "Neutral" = "white", "Low" = "darkgreen", "Very Low" = "blue")) +
  labs(title = "VIX Index and Market Sentiment", x = "Date", y = "VIX",
       fill = "Sentiment Zone") +
  theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Save the plot
print(p_risk_sentiment)
ggsave("vix_market_sentiment_plot.png", plot = p_risk_sentiment, width = 10, height = 6)

# Print the first few rows of VIX data with sentiment
print("First few rows of VIX data with market sentiment:")
print(head(vix_data))
