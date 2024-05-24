# Load necessary libraries
library(shiny)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(quantmod)
library(plotly)

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

# Load historical data for testing
getSymbols(c("^IXIC", "^DJI"), src = "yahoo", from = "2000-01-01")

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
          div(class = "title", "Nasdaq and Dow Jones Drawdowns"),
          plotlyOutput("drawdownPlot")
      )
  )
)

# Define server logic
server <- function(input, output) {
  output$drawdownPlot <- renderPlotly({
    plot_drawdowns(IXIC, DJI)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
