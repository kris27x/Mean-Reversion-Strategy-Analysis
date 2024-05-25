# risk_on_off_sentiment.R

# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(TTR)
library(lubridate)
library(tidyr)
library(ggcorrplot)

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

# Function to download and handle VIX data
get_vix_data <- function() {
  getSymbols("^VIX", src = "yahoo", from = "2000-01-01", auto.assign = FALSE) %>%
    handle_missing_values() %>%
    data.frame(date = index(.), coredata(.))
}

# Function to calculate sentiment scores
calculate_sentiment_scores <- function(data) {
  vix_mean <- mean(data$VIX.Close, na.rm = TRUE)
  vix_sd <- sd(data$VIX.Close, na.rm = TRUE)
  
  data %>%
    mutate(
      Z_Score = (VIX.Close - vix_mean) / vix_sd,
      Sentiment_Score = 50 + (Z_Score * 10),
      Sentiment_Score = ifelse(Sentiment_Score < 0, 0, ifelse(Sentiment_Score > 100, 100, Sentiment_Score)),
      Sentiment_Zone = cut(Sentiment_Score, breaks = c(-Inf, 39, 42, 58, 66, Inf), labels = c("Very High", "High", "Neutral", "Low", "Very Low")),
      lead_date = lead(date, default = last(date))
    )
}

# Function to download historical data for indices
get_index_data <- function(symbol) {
  getSymbols(symbol, src = "yahoo", from = "2000-01-01", auto.assign = FALSE) %>%
    data.frame(date = index(.), coredata(.))
}

# Function to plot VIX and sentiment scores
plot_vix_sentiment <- function(data) {
  ggplot(data, aes(x = date)) +
    geom_line(aes(y = VIX.Close), color = "blue", na.rm = TRUE) +
    geom_line(aes(y = Sentiment_Score), color = "red", linetype = "dashed", na.rm = TRUE) +
    geom_rect(aes(xmin = date, xmax = lead_date, ymin = 0, ymax = Inf, fill = Sentiment_Zone), alpha = 0.2) +
    scale_fill_manual(values = c("Very High" = "darkred", "High" = "darkorange", "Neutral" = "white", "Low" = "darkgreen", "Very Low" = "blue")) +
    labs(title = "VIX Index and Market Sentiment", x = "Date", y = "VIX", fill = "Sentiment Zone") +
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
}

# Function to calculate correlation between VIX and market indices during different sentiment zones
calculate_and_plot_correlation <- function(data, index_col, vix_col, sentiment_col, index_name) {
  # Calculate correlation for each sentiment zone
  correlation_results <- data %>%
    group_by(.data[[sentiment_col]]) %>%
    summarize(
      Correlation = cor(.data[[index_col]], .data[[vix_col]], use = "complete.obs")
    ) %>%
    ungroup()
  
  print(correlation_results)
  
  # Plot the correlation results
  p <- ggplot(correlation_results, aes(x = .data[[sentiment_col]], y = Correlation, fill = .data[[sentiment_col]])) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Very High" = "darkred", "High" = "darkorange", "Neutral" = "gray", "Low" = "darkgreen", "Very Low" = "blue")) +
    labs(title = paste("Correlation between VIX and", index_name, "by Sentiment Zone"),
         subtitle = paste(index_name, "and VIX Index Correlation Analysis"),
         x = "Sentiment Zone", y = "Correlation",
         fill = "Sentiment Zone") +
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
  
  return(p)
}

# Function to calculate performance based on sentiment
calculate_performance_by_sentiment <- function(data, index_name) {
  sentiments <- c("Very Low", "Low", "Neutral", "High", "Very High")
  results <- data.frame()
  
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
      
      results <- rbind(results, data.frame(Sentiment = sentiment, Overnight_Return = overnight_return, Intraday_Return = intraday_return, Total_Return = total_return))
    } else {
      cat("No data available for", sentiment, "sentiment.\n")
    }
  }
  
  return(results)
}

# Function to calculate performance over different periods
calculate_period_performance <- function(data, index_name, period, period_name) {
  sentiments <- c("Very Low", "Low", "Neutral", "High", "Very High")
  results <- data.frame()
  
  for (sentiment in sentiments) {
    cat("\n", period_name, "Performance for", index_name, "during", sentiment, "sentiment:\n")
    filtered_data <- data %>% filter(Sentiment_Zone == sentiment)
    if (nrow(filtered_data) > 0) {
      buy_dates <- filtered_data$date
      period_returns <- sapply(buy_dates, function(buy_date) {
        sell_date <- buy_date + period
        closest_sell_date <- data$date[data$date >= sell_date][1]
        if (!is.na(closest_sell_date) && closest_sell_date > buy_date) {
          buy_price <- data %>% filter(date == buy_date) %>% pull(.data[[paste0(index_name, ".Close")]])
          sell_price <- data %>% filter(date == closest_sell_date) %>% pull(.data[[paste0(index_name, ".Close")]])
          return((sell_price / buy_price) - 1)
        } else {
          return(NA)
        }
      })
      period_returns <- na.omit(period_returns)
      
      if (length(period_returns) > 0) {
        avg_return <- mean(period_returns)
        cat("Average", period_name, "Return:", avg_return, "\n")
        results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = avg_return))
      } else {
        cat("Not enough data to calculate", period_name, "returns for", sentiment, "sentiment.\n")
        results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = NA))
      }
    } else {
      cat("No data available for", sentiment, "sentiment.\n")
      results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = NA))
    }
  }
  return(results)
}

# Combine results into a table
combine_results <- function(data, index_name) {
  periods <- list("3 Months" = months(3), "6 Months" = months(6), "1 Year" = years(1), "2 Years" = years(2))
  all_results <- data.frame()
  
  for (period_name in names(periods)) {
    period <- periods[[period_name]]
    period_results <- calculate_period_performance(data, index_name, period, period_name)
    all_results <- rbind(all_results, period_results)
  }
  
  return(all_results)
}

# Function to calculate and plot the correlation matrix
calculate_and_plot_correlation_matrix <- function(data, sentiment_col, returns_cols, index_name) {
  # Check if the necessary columns are present in the data
  required_cols <- c(sentiment_col, returns_cols)
  if (!all(required_cols %in% colnames(data))) {
    stop("Data does not contain the necessary columns.")
  }
  
  # Ensure data is sorted by date
  data <- data %>% arrange(date)
  
  # Initialize an empty list to store plots
  plots <- list()
  
  # Calculate and plot correlation matrix for each sentiment zone
  unique_sentiments <- unique(data[[sentiment_col]])
  
  for (sentiment in unique_sentiments) {
    sentiment_data <- data %>% filter(.data[[sentiment_col]] == sentiment)
    
    # Calculate correlation matrix
    correlation_matrix <- cor(sentiment_data[, returns_cols], use = "complete.obs")
    
    # Plot the correlation matrix using ggcorrplot
    p <- ggcorrplot(correlation_matrix, method = "circle", type = "lower",
                    lab = TRUE, lab_size = 8, p.mat = NULL, insig = "blank",
                    colors = c("#003366", "white", "#FF3300"), 
                    title = paste("Correlation Matrix for", index_name, "during", sentiment, "Sentiment"),
                    ggtheme = theme_minimal(base_size = 20) + 
                      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                            axis.text = element_text(size = 14, face = "bold"),
                            axis.title = element_text(size = 16, face = "bold"),
                            plot.background = element_rect(fill = "white", color = NA), 
                            panel.background = element_rect(fill = "white", color = NA), 
                            panel.grid.major = element_line(color = "gray90"),
                            panel.grid.minor = element_blank(),
                            plot.margin = margin(10, 10, 10, 10)))
    
    # Save the plot
    ggsave(filename = paste0("correlation_matrix_", sentiment, ".png"), plot = p, width = 12, height = 8)
    
    # Store the plot in the list
    plots[[sentiment]] <- p
  }
  
  return(plots)
}

# Function to calculate and print general correlation between Nasdaq and Dow Jones
calculate_and_print_general_correlation <- function(nasdaq_data, djia_data) {
  combined_data <- nasdaq_data %>%
    inner_join(djia_data, by = "date", suffix = c("_nasdaq", "_djia"))
  
  correlation <- cor(combined_data$IXIC.Close, combined_data$DJI.Close, use = "complete.obs")
  cat("General Correlation between Nasdaq and Dow Jones:", correlation, "\n")
}

# Function to calculate performance summary metrics for each sentiment zone
calculate_performance_summary <- function(data, sentiment_col, returns_col) {
  # Check if the necessary columns are present in the data
  required_cols <- c(sentiment_col, returns_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Data does not contain the necessary columns.")
  }
  
  # Calculate performance metrics
  performance_summary <- data %>%
    group_by(.data[[sentiment_col]]) %>%
    summarize(
      Mean_Return = mean(.data[[returns_col]], na.rm = TRUE),
      Median_Return = median(.data[[returns_col]], na.rm = TRUE),
      Std_Dev_Return = sd(.data[[returns_col]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(.data[[sentiment_col]])
  
  return(performance_summary)
}

# Function to print performance summary table
print_performance_summary <- function(performance_summary) {
  # Print the performance summary table
  print(performance_summary)
}

# Main script execution
main <- function() {
  # Fetch and process VIX data
  vix_data <- get_vix_data()
  vix_data <- calculate_sentiment_scores(vix_data)
  
  # Plot VIX and sentiment scores
  p_risk_sentiment <- plot_vix_sentiment(vix_data)
  print(p_risk_sentiment)
  ggsave("vix_market_sentiment_plot.png", plot = p_risk_sentiment, width = 10, height = 6)
  
  # Print the first few rows of VIX data with sentiment
  print("First few rows of VIX data with market sentiment:")
  print(head(vix_data))
  
  # Fetch and process index data
  nasdaq_data <- get_index_data("^IXIC")
  djia_data <- get_index_data("^DJI")
  
  # Calculate returns for indices
  nasdaq_returns <- calculate_returns(nasdaq_data, "IXIC.Close", "IXIC.Open")
  djia_returns <- calculate_returns(djia_data, "DJI.Close", "DJI.Open")
  
  # Merge VIX sentiment data with Nasdaq and Dow Jones data
  nasdaq_sentiment <- nasdaq_returns %>% left_join(vix_data %>% select(date, VIX.Close, Sentiment_Zone), by = "date")
  djia_sentiment <- djia_returns %>% left_join(vix_data %>% select(date, VIX.Close, Sentiment_Zone), by = "date")
  
  # Calculate and print performance for each sentiment for Dow Jones and Nasdaq
  calculate_performance_by_sentiment(djia_sentiment, "Dow Jones")
  calculate_performance_by_sentiment(nasdaq_sentiment, "Nasdaq")
  
  # Combine and present results in a table for Dow Jones
  djia_results <- combine_results(djia_sentiment, "DJI")
  print("Combined Results for Dow Jones:")
  print(djia_results)
  
  # Combine and present results in a table for Nasdaq
  nasdaq_results <- combine_results(nasdaq_sentiment, "IXIC")
  print("Combined Results for Nasdaq:")
  print(nasdaq_results)
  
  # Calculate and plot correlation for Dow Jones
  p_correlation_djia <- calculate_and_plot_correlation(djia_sentiment, index_col = "DJI.Close", vix_col = "VIX.Close", sentiment_col = "Sentiment_Zone", index_name = "Dow Jones")
  print(p_correlation_djia)
  ggsave("correlation_djia_plot.png", plot = p_correlation_djia, width = 10, height = 6)
  
  # Calculate and plot correlation for Nasdaq
  p_correlation_nasdaq <- calculate_and_plot_correlation(nasdaq_sentiment, index_col = "IXIC.Close", vix_col = "VIX.Close", sentiment_col = "Sentiment_Zone", index_name = "Nasdaq")
  print(p_correlation_nasdaq)
  ggsave("correlation_nasdaq_plot.png", plot = p_correlation_nasdaq, width = 10, height = 6)
  
  # Calculate and plot the correlation matrix for Dow Jones and Nasdaq
  returns_cols <- c("VIX.Close", "IXIC.Close", "DJI.Close")
  combined_data <- nasdaq_sentiment %>% 
    full_join(djia_sentiment, by = c("date", "Sentiment_Zone"), suffix = c("_nasdaq", "_djia")) %>% 
    select(date, Sentiment_Zone, starts_with("VIX"), starts_with("IXIC"), starts_with("DJI")) %>% 
    rename("VIX.Close" = "VIX.Close_nasdaq")
  
  p_correlation_matrices <- calculate_and_plot_correlation_matrix(combined_data, sentiment_col = "Sentiment_Zone", returns_cols = returns_cols, index_name = "Market Indices")
  
  # Print each plot
  for (sentiment in names(p_correlation_matrices)) {
    print(p_correlation_matrices[[sentiment]])
  }
  
  # Calculate and print the general correlation between Nasdaq and Dow Jones
  calculate_and_print_general_correlation(nasdaq_data, djia_data)
  
  # Calculate and print performance summary for Nasdaq and Dow Jones
  nasdaq_performance_summary <- calculate_performance_summary(nasdaq_sentiment, sentiment_col = "Sentiment_Zone", returns_col = "Total_Return")
  djia_performance_summary <- calculate_performance_summary(djia_sentiment, sentiment_col = "Sentiment_Zone", returns_col = "Total_Return")
  
  cat("Performance Summary for Nasdaq:\n")
  print_performance_summary(nasdaq_performance_summary)
  
  cat("Performance Summary for Dow Jones:\n")
  print_performance_summary(djia_performance_summary)
  
  # Save the environment for later use
  save.image(file = "performance_analysis_based_on_sentiment.RData")
}

# Run the main script
main()
