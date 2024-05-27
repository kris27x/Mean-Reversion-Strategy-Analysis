# risk_on_off_sentiment.R

# Load necessary libraries
required_packages <- c("quantmod", "dplyr", "ggplot2", "TTR", "lubridate", "tidyr", "ggcorrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Function to handle missing values in VIX data
handle_missing_values <- function(data, interpolation_method = "linear") {
  # Handle missing values using specified interpolation method
  if (interpolation_method == "linear") {
    data <- na.approx(data, maxgap = Inf, na.rm = FALSE)  # Linear interpolation
  }
  data <- na.locf(data, fromLast = FALSE)  # Forward fill remaining NAs
  data <- na.locf(data, fromLast = TRUE)  # Backward fill remaining NAs
  data <- na.omit(data)  # Remove any remaining NAs
  return(data)
}

# Function to calculate returns
calculate_returns <- function(data, close_col, open_col) {
  # Ensure specified columns exist in the data
  if (!(close_col %in% colnames(data)) | !(open_col %in% colnames(data))) {
    stop("The specified columns do not exist in the data.")
  }
  
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
  # Attempt to download VIX data and handle errors
  tryCatch({
    data <- getSymbols("^VIX", src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    data <- handle_missing_values(data)
    data <- data.frame(date = index(data), coredata(data))
    return(data)
  }, error = function(e) {
    cat("Error in downloading VIX data:", e$message, "\n")
    return(NULL)
  })
}

# Function to calculate sentiment scores
calculate_sentiment_scores <- function(data) {
  # Calculate Z-score and sentiment scores based on VIX data
  vix_mean <- mean(data$VIX.Close, na.rm = TRUE)
  vix_sd <- sd(data$VIX.Close, na.rm = TRUE)
  
  data %>%
    mutate(
      Z_Score = (VIX.Close - vix_mean) / vix_sd,
      Sentiment_Score = 50 + (Z_Score * 10),
      Sentiment_Score = ifelse(Sentiment_Score < 0, 0, ifelse(Sentiment_Score > 100, 100, Sentiment_Score)),
      Sentiment_Zone = factor(cut(Sentiment_Score, breaks = c(-Inf, 39, 42, 58, 66, Inf), 
                                  labels = c("Very High", "High", "Neutral", "Low", "Very Low")), 
                              levels = c("Very High", "High", "Neutral", "Low", "Very Low")),
      lead_date = lead(date, default = last(date))
    )
}

# Function to download historical data for indices
get_index_data <- function(symbol) {
  # Attempt to download index data and handle errors
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    data <- data.frame(date = index(data), coredata(data))
    return(data)
  }, error = function(e) {
    cat("Error in downloading data for symbol", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

# Function to plot VIX and sentiment scores
plot_vix_sentiment <- function(data) {
  # Plot VIX and sentiment scores over time
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
  # Calculate and plot correlation between VIX and index data for different sentiment zones
  correlation_results <- data %>%
    group_by(.data[[sentiment_col]]) %>%
    summarize(Correlation = cor(.data[[index_col]], .data[[vix_col]], use = "complete.obs")) %>%
    ungroup()
  
  print(correlation_results)
  
  ggplot(correlation_results, aes(x = .data[[sentiment_col]], y = Correlation, fill = .data[[sentiment_col]])) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Very High" = "darkred", "High" = "darkorange", "Neutral" = "gray", "Low" = "darkgreen", "Very Low" = "blue")) +
    labs(title = paste("Correlation between VIX and", index_name, "by Sentiment Zone"),
         subtitle = paste(index_name, "and VIX Index Correlation Analysis"),
         x = "Sentiment Zone", y = "Correlation",
         fill = "Sentiment Zone") +
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
}

# Function to calculate performance based on sentiment
calculate_performance_by_sentiment <- function(data, index_name) {
  # Calculate performance metrics for each sentiment zone
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
  # Calculate performance over specified periods for each sentiment zone
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
        med_return <- median(period_returns)
        cat("Average", period_name, "Return:", avg_return, "\n")
        cat("Median", period_name, "Return:", med_return, "\n")
        results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = period_returns, Average_Return = avg_return, Median_Return = med_return))
      } else {
        cat("Not enough data to calculate", period_name, "returns for", sentiment, "sentiment.\n")
        results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = NA, Average_Return = NA, Median_Return = NA))
      }
    } else {
      cat("No data available for", sentiment, "sentiment.\n")
      results <- rbind(results, data.frame(Sentiment = sentiment, Period = period_name, Return = NA, Average_Return = NA, Median_Return = NA))
    }
  }
  return(results)
}

# Combine results into a table
combine_results <- function(data, index_name) {
  # Combine performance results for various periods
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
  # Calculate and plot correlation matrix for each sentiment zone
  required_cols <- c(sentiment_col, returns_cols)
  if (!all(required_cols %in% colnames(data))) {
    stop("Data does not contain the necessary columns.")
  }
  
  data <- data %>% arrange(date)
  plots <- list()
  unique_sentiments <- unique(data[[sentiment_col]])
  
  for (sentiment in unique_sentiments) {
    sentiment_data <- data %>% filter(.data[[sentiment_col]] == sentiment)
    correlation_matrix <- cor(sentiment_data[, returns_cols], use = "complete.obs")
    
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
    
    ggsave(filename = paste0("correlation_matrix_", sentiment, ".png"), plot = p, width = 12, height = 8)
    plots[[sentiment]] <- p
  }
  
  return(plots)
}

# Function to calculate and print general correlation between Nasdaq and Dow Jones
calculate_and_print_general_correlation <- function(nasdaq_data, djia_data) {
  # Calculate and print general correlation between Nasdaq and Dow Jones
  combined_data <- nasdaq_data %>%
    inner_join(djia_data, by = "date", suffix = c("_nasdaq", "_djia"))
  
  correlation <- cor(combined_data$IXIC.Close, combined_data$DJI.Close, use = "complete.obs")
  cat("General Correlation between Nasdaq and Dow Jones:", correlation, "\n")
}

# Function to calculate performance summary metrics for each sentiment zone
calculate_performance_summary <- function(data, sentiment_col, returns_col) {
  # Calculate performance summary metrics for each sentiment zone
  required_cols <- c(sentiment_col, returns_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Data does not contain the necessary columns.")
  }
  
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

# Function to create bar chart for sentiment zone performance comparison
create_bar_chart <- function(data, title) {
  # Create bar chart to compare performance across sentiment zones
  data_summary <- data %>%
    group_by(Sentiment, Period) %>%
    summarise(
      Average_Return = mean(Average_Return, na.rm = TRUE),
      Median_Return = mean(Median_Return, na.rm = TRUE)
    ) %>%
    ungroup()
  
  data_summary$Sentiment <- factor(data_summary$Sentiment, levels = c("Very High", "High", "Neutral", "Low", "Very Low"))
  
  ggplot(data_summary, aes(x = Sentiment, fill = Sentiment)) +
    geom_bar(aes(y = Average_Return), stat = "identity", position = "dodge", alpha = 0.6, show.legend = TRUE) +
    geom_point(aes(y = Median_Return), color = "red", size = 3, shape = 16) +
    geom_text(aes(y = Average_Return, label = round(Average_Return, 2)), vjust = -0.5, size = 3) +
    geom_text(aes(y = Median_Return, label = round(Median_Return, 2)), vjust = 1.5, size = 3, color = "red") +
    facet_wrap(~ Period, scales = "free_y") +
    labs(title = title, x = "Sentiment Zone", y = "Return") +
    theme_minimal(base_size = 15) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white")) +
    guides(fill = guide_legend(title = "Sentiment Zone"),
           shape = guide_legend(override.aes = list(color = "red"))) +
    annotate("text", x = 1, y = -0.05, label = "Red Dots: Median Return\nBars: Average Return", 
             hjust = 0, vjust = 0, color = "black", size = 3, fontface = "italic")
}

# Function to create boxplot for sentiment zone performance comparison
create_boxplot <- function(data, title) {
  # Create boxplot to compare performance across sentiment zones
  data <- data %>%
    unnest(Return) %>%
    drop_na(Return)
  
  data$Sentiment <- factor(data$Sentiment, levels = c("Very High", "High", "Neutral", "Low", "Very Low"))
  
  ggplot(data, aes(x = Sentiment, y = Return, fill = Sentiment)) +
    geom_boxplot() +
    facet_wrap(~ Period, scales = "free_y") +
    labs(title = title, x = "Sentiment Zone", y = "Return") +
    theme_minimal(base_size = 15) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"))
}

# Function to visualize performance summary
visualize_performance_summary <- function(performance_summary, title) {
  # Reshape data for easier plotting
  performance_summary_long <- performance_summary %>%
    pivot_longer(cols = c(Mean_Return, Median_Return, Std_Dev_Return), 
                 names_to = "Metric", values_to = "Value")
  
  # Create bar plot for performance summary
  p <- ggplot(performance_summary_long, aes(x = Sentiment_Zone, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(title = title, x = "Sentiment Zone", y = "Value", fill = "Metric") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          legend.position = "top")
  
  return(p)
}

# Main script execution
main <- function() {
  # Main function to execute the entire analysis workflow
  
  # Fetch and process VIX data
  vix_data <- get_vix_data()
  if (is.null(vix_data)) return(NULL)
  vix_data <- calculate_sentiment_scores(vix_data)
  
  # Plot and save VIX and sentiment scores
  p_risk_sentiment <- plot_vix_sentiment(vix_data)
  print(p_risk_sentiment)
  ggsave("vix_market_sentiment_plot.png", plot = p_risk_sentiment, width = 10, height = 6)
  
  # Fetch and process index data
  nasdaq_data <- get_index_data("^IXIC")
  djia_data <- get_index_data("^DJI")
  if (is.null(nasdaq_data) | is.null(djia_data)) return(NULL)
  
  # Calculate returns for indices
  nasdaq_returns <- calculate_returns(nasdaq_data, "IXIC.Close", "IXIC.Open")
  djia_returns <- calculate_returns(djia_data, "DJI.Close", "DJI.Open")
  
  # Merge VIX sentiment data with index data
  nasdaq_sentiment <- nasdaq_returns %>% left_join(vix_data %>% select(date, VIX.Close, Sentiment_Zone), by = "date")
  djia_sentiment <- djia_returns %>% left_join(vix_data %>% select(date, VIX.Close, Sentiment_Zone), by = "date")
  
  # Calculate and print performance for each sentiment zone
  calculate_performance_by_sentiment(djia_sentiment, "Dow Jones")
  calculate_performance_by_sentiment(nasdaq_sentiment, "Nasdaq")
  
  # Combine and present results for Dow Jones
  djia_results <- combine_results(djia_sentiment, "DJI")
  print("Combined Results for Dow Jones:")
  print(djia_results)
  
  # Combine and present results for Nasdaq
  nasdaq_results <- combine_results(nasdaq_sentiment, "IXIC")
  print("Combined Results for Nasdaq:")
  print(nasdaq_results)
  
  # Create and save bar charts for performance comparison
  p_bar_djia <- create_bar_chart(djia_results, "Dow Jones Performance by Sentiment Zone")
  p_bar_nasdaq <- create_bar_chart(nasdaq_results, "Nasdaq Performance by Sentiment Zone")
  print(p_bar_djia)
  print(p_bar_nasdaq)
  ggsave("dow_performance_bar_chart.png", plot = p_bar_djia, width = 10, height = 8)
  ggsave("nasdaq_performance_bar_chart.png", plot = p_bar_nasdaq, width = 10, height = 8)
  
  # Create and save boxplots for performance comparison
  p_box_djia <- create_boxplot(djia_results, "Dow Jones Performance by Sentiment Zone")
  p_box_nasdaq <- create_boxplot(nasdaq_results, "Nasdaq Performance by Sentiment Zone")
  print(p_box_djia)
  print(p_box_nasdaq)
  ggsave("dow_performance_boxplot.png", plot = p_box_djia, width = 10, height = 8)
  ggsave("nasdaq_performance_boxplot.png", plot = p_box_nasdaq, width = 10, height = 8)
  
  # Calculate and plot correlation for Dow Jones
  p_correlation_djia <- calculate_and_plot_correlation(djia_sentiment, index_col = "DJI.Close", vix_col = "VIX.Close", sentiment_col = "Sentiment_Zone", index_name = "Dow Jones")
  print(p_correlation_djia)
  ggsave("correlation_djia_plot.png", plot = p_correlation_djia, width = 10, height = 6)
  
  # Calculate and plot correlation for Nasdaq
  p_correlation_nasdaq <- calculate_and_plot_correlation(nasdaq_sentiment, index_col = "IXIC.Close", vix_col = "VIX.Close", sentiment_col = "Sentiment_Zone", index_name = "Nasdaq")
  print(p_correlation_nasdaq)
  ggsave("correlation_nasdaq_plot.png", plot = p_correlation_nasdaq, width = 10, height = 6)
  
  # Combine data for correlation matrix analysis
  returns_cols <- c("VIX.Close", "IXIC.Close", "DJI.Close")
  combined_data <- nasdaq_sentiment %>% 
    full_join(djia_sentiment, by = c("date", "Sentiment_Zone"), suffix = c("_nasdaq", "_djia")) %>% 
    select(date, Sentiment_Zone, starts_with("VIX"), starts_with("IXIC"), starts_with("DJI")) %>% 
    rename("VIX.Close" = "VIX.Close_nasdaq")
  
  # Calculate and plot correlation matrices
  p_correlation_matrices <- calculate_and_plot_correlation_matrix(combined_data, sentiment_col = "Sentiment_Zone", returns_cols = returns_cols, index_name = "Market Indices")
  
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
  
  # Visualize the performance summary for Nasdaq
  p_nasdaq_summary <- visualize_performance_summary(nasdaq_performance_summary, "Performance Summary for Nasdaq")
  print(p_nasdaq_summary)
  ggsave("nasdaq_performance_summary.png", plot = p_nasdaq_summary, width = 10, height = 8)
  
  # Visualize the performance summary for Dow Jones
  p_djia_summary <- visualize_performance_summary(djia_performance_summary, "Performance Summary for Dow Jones")
  print(p_djia_summary)
  ggsave("djia_performance_summary.png", plot = p_djia_summary, width = 10, height = 8)
  
  # Save the environment for later use
  save.image(file = "performance_analysis_based_on_sentiment.RData")
}

# Run the main script
main()
