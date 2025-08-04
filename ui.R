##UI ==========
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel(
    div(
      h2("CPI Forecasting Tool"),
      p("Monthly Inflation Forecasts using ARIMA and VAR Models", style = "font-size:14px; color:gray;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = c(
        "Singapore" = "SG",
        "Indonesia" = "IDN",
        "Vietnam" = "VN",
        "Japan" = "JN",
        "Thailand" = "TH"
      )),
      
      radioButtons("model_type", "Choose Model Type:",
                   choices = c("ARIMA" = "arima", "VAR" = "var"),
                   selected = "var"),
      
      numericInput("horizon", "Forecast Horizon (Months):", value = 12, min = 1, max = 36),
      
      #date range input
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range to Display:",
        start = NULL,
        end = NULL,
        format = "yyyy-mm",
        startview = "month"
      ),
      
      # Show oil growth input only if 'var' and 'oil_px' are selected
      conditionalPanel(
        condition = "input.model_type == 'var' && $.inArray('oil_px', input.predictors) !== -1",
        numericInput("custom_oil_level", "Set Flat Oil Price Level (USD/barrel):", value = 70)
      ),
      
      # Show predictor selection UI when 'var' model is chosen
      conditionalPanel(
        condition = "input.model_type == 'var'",
        uiOutput("predictor_ui")
      ),
      
      checkboxInput("show_yoy", "Show Historical YoY CPI", value = TRUE),
      
      actionButton("run_forecast", "Run Forecast", width = "100%"),
      
      br(), #vertical spacing
      downloadButton("download_data", "Download Forecast CSV", width = "100%", disabled = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast Plot",
                 withSpinner(plotOutput("cpi_plot"), type = 4, color = "#0072B2")),
        tabPanel("Forecast Table",
                 withSpinner(tableOutput("forecast_table"), type = 4, color = "#0072B2")),
        tabPanel("Backtest Accuracy (Coming Soon)",
                 p("Backtesting and RMSE/MAE comparison will be added later."),
                 plotOutput("backtest_plot"),
                 verbatimTextOutput("rmse_output"),
                 verbatimTextOutput("mae_output")),
        tabPanel("Oil Path Debug", 
                 plotOutput("oil_plot"))
      )
    )
    
  )
)
