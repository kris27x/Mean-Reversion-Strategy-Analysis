# Load necessary libraries
library(shiny)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(quantmod)
library(plotly)
library(dygraphs)
library(RColorBrewer)

# Function to calculate and plot drawdowns using plotly
plot_drawdowns <- function(nasdaq_data, dow_data) {
  # Calculate daily returns
  nasdaq_returns <- dailyReturn(nasdaq_data)
  dow_returns <- dailyReturn(dow_data)
  
  # Calculate drawdowns
  nasdaq_drawdown <- PerformanceAnalytics::Drawdowns(nasdaq_returns)
  dow_drawdown <- PerformanceAnalytics::Drawdowns(dow_returns)
  
  # Convert to data frames for plotly
  nasdaq_drawdown_df <- data.frame(date = index(nasdaq_drawdown), drawdown = coredata(nasdaq_drawdown))
  dow_drawdown_df <- data.frame(date = index(dow_drawdown), drawdown = coredata(dow_drawdown))
  
  # Rename columns for clarity
  colnames(nasdaq_drawdown_df) <- c("date", "Nasdaq_Drawdown")
  colnames(dow_drawdown_df) <- c("date", "Dow_Drawdown")
  
  # Merge data frames
  drawdown_df <- merge(nasdaq_drawdown_df, dow_drawdown_df, by = "date", all = TRUE)
  
  # Pivot longer for plotly
  drawdown_long <- pivot_longer(drawdown_df, cols = c("Nasdaq_Drawdown", "Dow_Drawdown"), names_to = "Index", values_to = "Drawdown")
  
  # Create an interactive plotly plot
  p <- plot_ly(drawdown_long, x = ~date, y = ~Drawdown, color = ~Index, type = 'scatter', mode = 'lines',
               colors = c("red", "blue"),
               hoverinfo = 'text',
               text = ~paste(Index, ": ", round(Drawdown, 4))) %>%
    layout(title = "Drawdowns of Nasdaq and Dow Jones",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Drawdown"),
           legend = list(title = list(text = 'Index')),
           plot_bgcolor = 'white',
           paper_bgcolor = 'white')
  
  p
}

# Function to handle missing values
handle_missing_values <- function(data) {
  data <- na.approx(data, maxgap = Inf, na.rm = FALSE)  # Linear interpolation
  data <- na.locf(data, fromLast = FALSE)  # Forward fill remaining NAs
  data <- na.locf(data, fromLast = TRUE)  # Backward fill remaining NAs
  na.omit(data)  # Remove any remaining NAs
}

# Function to download and handle data
get_data <- function(symbol) {
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    adjusted_data <- data[, 6]  # Use adjusted close price for accurate calculations
    adjusted_data <- handle_missing_values(adjusted_data)
    return(adjusted_data)
  }, error = function(e) {
    stop(paste("Error downloading data for symbol:", symbol))
  })
}

# Function to calculate rolling Sharpe ratio
calculate_rolling_sharpe <- function(returns, window) {
  sharpe_ratio <- rollapply(returns, width = window, FUN = function(x) mean(x) / sd(x) * sqrt(252), by.column = FALSE, fill = NA)
  return(sharpe_ratio)
}

# Function to plot rolling Sharpe ratio
plot_rolling_sharpe <- function(nasdaq_sharpe, dow_sharpe) {
  # Create a data frame for plotting
  sharpe_data <- data.frame(
    date = index(nasdaq_sharpe),
    Nasdaq_Sharpe = coredata(nasdaq_sharpe),
    Dow_Sharpe = coredata(dow_sharpe)
  )
  
  # Create the plotly object
  p <- plot_ly(sharpe_data, x = ~date) %>%
    add_lines(y = ~Nasdaq_Sharpe, name = "Nasdaq Sharpe Ratio", line = list(color = 'rgb(0, 176, 246)')) %>%
    add_lines(y = ~Dow_Sharpe, name = "Dow Sharpe Ratio", line = list(color = 'rgb(255, 0, 0)')) %>%
    layout(title = "Rolling Sharpe Ratio Over Time",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Sharpe Ratio"),
           plot_bgcolor = 'white',
           paper_bgcolor = 'white')
  
  return(p)
}

# Function to plot Volatility (VIX) vs. Market Indices (Nasdaq and Dow Jones) interactively
plot_volatility_vs_indices_interactive <- function(vix_data, nasdaq_data, djia_data) {
  # Convert xts objects to data frames
  vix_df <- data.frame(date = index(vix_data), coredata(vix_data))
  nasdaq_df <- data.frame(date = index(nasdaq_data), coredata(nasdaq_data))
  djia_df <- data.frame(date = index(djia_data), coredata(djia_data))
  
  # Rename columns for consistency
  colnames(vix_df) <- c("date", "VIX.Close")
  colnames(nasdaq_df) <- c("date", "IXIC.Close")
  colnames(djia_df) <- c("date", "DJI.Close")
  
  # Merge the data frames by date
  combined_data <- vix_df %>%
    inner_join(nasdaq_df, by = "date") %>%
    inner_join(djia_df, by = "date")
  
  # Normalize the indices to the same starting point for better comparison
  combined_data <- combined_data %>%
    mutate(VIX_Scaled = scale(VIX.Close, center = FALSE, scale = max(VIX.Close)),
           IXIC_Scaled = scale(IXIC.Close, center = FALSE, scale = max(IXIC.Close)),
           DJI_Scaled = scale(DJI.Close, center = FALSE, scale = max(DJI.Close)))
  
  # Create the ggplot
  p <- ggplot(combined_data, aes(x = date)) +
    geom_line(aes(y = IXIC_Scaled, color = "IXIC.Close"), size = 1) +
    geom_line(aes(y = DJI_Scaled, color = "DJI.Close"), size = 1) +
    geom_line(aes(y = VIX_Scaled, color = "VIX.Close"), size = 0.5) +
    scale_color_manual(values = c("IXIC.Close" = "red", "DJI.Close" = "green", "VIX.Close" = "blue")) +
    scale_y_continuous(
      name = "Market Indices (Scaled)",
      sec.axis = sec_axis(~ . * max(combined_data$VIX.Close), name = "VIX Index")
    ) +
    labs(title = "Volatility (VIX) vs. Market Indices (Nasdaq and Dow Jones)",
         x = "Date",
         y = "Market Indices (Scaled)",
         color = "Index") +
    theme_minimal(base_size = 15) +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.title.y.right = element_text(color = "blue"),
          axis.text.y.right = element_text(color = "blue"))
  
  # Convert the ggplot to a plotly object
  p_interactive <- ggplotly(p)
  
  return(p_interactive)
}

# Download historical data
nasdaq_data <- get_data("^IXIC")
dow_data <- get_data("^DJI")
vix_data <- get_data("^VIX")

# Merge data frames on date and ensure alignment
merged_data <- merge(nasdaq_data, dow_data, all = TRUE)
colnames(merged_data) <- c("Nasdaq", "Dow")

# Handle missing values after merging to ensure complete cases
merged_data <- merged_data %>%
  na.locf(fromLast = FALSE) %>%
  na.locf(fromLast = TRUE) %>%
  na.omit()

# Calculate relative performance
relative_performance <- merged_data$Nasdaq / merged_data$Dow

# Apply k-means clustering to identify regimes
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(as.numeric(relative_performance), centers = 3)
clusters <- factor(kmeans_result$cluster)

# Create an xts object for relative performance and clusters
relative_performance_xts <- xts(cbind(relative_performance, clusters), order.by = index(merged_data))
colnames(relative_performance_xts) <- c("Relative_Performance", "Cluster")

# Define UI for the application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        background-color: #f8f9fa;
      }
      .content-panel {
        width: 80%;
        background-color: white;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .title {
        text-align: center;
        font-weight: bold;
        font-size: 24px;
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(class = "main-container",
      div(class = "content-panel",
          div(class = "title", "Nasdaq and Dow Jones Analysis"),
          tabsetPanel(
            tabPanel("Drawdowns", plotlyOutput("drawdownPlot")),
            tabPanel("Relative Performance", dygraphOutput("relativePerformancePlot")),
            tabPanel("Rolling Sharpe Ratio", plotlyOutput("rollingSharpePlot")),
            tabPanel("Volatility vs. Market Indices", plotlyOutput("volatilityPlot"))  # New tab for the interactive plot
          )
      )
  )
)

# Define server logic
server <- function(input, output) {
  output$drawdownPlot <- renderPlotly({
    plot_drawdowns(nasdaq_data, dow_data)
  })
  
  output$relativePerformancePlot <- renderDygraph({
    dygraph(relative_performance_xts[, "Relative_Performance"], main = "Relative Performance of Nasdaq vs. Dow Jones") %>%
      dySeries("Relative_Performance", label = "Relative Performance (IXIC / DJI)") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "both") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyShading(from = index(relative_performance_xts)[clusters == 1], to = index(relative_performance_xts)[clusters == 1], color = "#FFCCCC") %>%
      dyShading(from = index(relative_performance_xts)[clusters == 2], to = index(relative_performance_xts)[clusters == 2], color = "#CCCCFF") %>%
      dyShading(from = index(relative_performance_xts)[clusters == 3], to = index(relative_performance_xts)[clusters == 3], color = "#CCFFCC")
  })
  
  output$rollingSharpePlot <- renderPlotly({
    # Calculate daily returns
    nasdaq_returns <- dailyReturn(nasdaq_data)
    dow_returns <- dailyReturn(dow_data)
    
    # Calculate rolling Sharpe ratio
    nasdaq_sharpe <- calculate_rolling_sharpe(nasdaq_returns, window = 252)
    dow_sharpe <- calculate_rolling_sharpe(dow_returns, window = 252)
    
    # Ensure the data has no NA values before plotting
    nasdaq_sharpe <- na.omit(nasdaq_sharpe)
    dow_sharpe <- na.omit(dow_sharpe)
    
    # Plot rolling Sharpe ratio
    plot_rolling_sharpe(nasdaq_sharpe, dow_sharpe)
  })
  
  output$volatilityPlot <- renderPlotly({
    plot_volatility_vs_indices_interactive(vix_data, nasdaq_data, dow_data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
