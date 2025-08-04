server <- function(input, output, session) {
  
  preprocess_data <- function(df, skip_cols = c("date", "cpi", "oil_px")) {
    df <- df %>%
      arrange(date) %>%
      mutate(
        across(setdiff(names(.), skip_cols), ~ 100 * (. / lag(.) - 1), .names = "{.col}_mom"),
        cpi_mom = 100 * (cpi / lag(cpi) - 1),
        cpi_yoy = 100 * (cpi / lag(cpi, 12) - 1)
      )
    return(df)
  }
  
  default_date_range <- function() {
    list(
      start = floor_date(Sys.Date() %m-% months(36), "month"),
      end   = floor_date(Sys.Date() %m+% months(12), "month")
    )
  }
  
  selected_data <- reactive({
    req(input$country)
    df <- all_country_data[[input$country]]
    preprocess_data(df)
  })
  
  observeEvent(input$country, {
    dr <- default_date_range()
    updateDateRangeInput(session, "date_range", start = dr$start, end = dr$end)
  })
  
  get_predictors <- function(df, excluded_vars = c("date", "cpi", "cpi_yoy", "cpi_mom", "year", "month")) {
    possible <- setdiff(names(df), excluded_vars)
    possible <- possible[grepl("_mom$|^oil_px$", possible)]
    display <- gsub("_", " ", possible)
    setNames(possible, display)
  }
  
  output$predictor_ui <- renderUI({
    req(input$model_type == "var", input$country)
    df <- selected_data()
    predictor_choices <- get_predictors(df)
    default_choices <- names(predictor_choices)
    preferred <- grep("_mom$", default_choices, value = TRUE)
    selected_predictors <- head(preferred, min(3, length(preferred)))
    
    selectizeInput(
      inputId = "predictors",
      label = "Select up to 3 VAR Regressors:",
      choices = predictor_choices,
      selected = selected_predictors,
      multiple = TRUE,
      options = list(maxItems = 3)
    )
  })
  
  loading <- reactiveVal(FALSE)
  forecast_result <- reactiveVal(NULL)
  forecast_start_val <- reactiveVal(NULL)
  synthetic_oil_df <- reactiveVal(NULL)
  
  generate_oil_path <- function(df, h, oil_level) {
    future_oil <- tibble(
      date = seq.Date(from = floor_date(max(df$date) %m+% months(1), "month"), by = "month", length.out = h),
      oil_px = rep(oil_level, h)
    )
    df %>% bind_rows(future_oil) %>% arrange(date)
  }
  
  fit_baseline_var <- function(df, predictors, h) {
    df_var <- df %>% dplyr::select(date, cpi, cpi_mom, all_of(predictors))
    df_var_hist <- df_var %>% filter(!is.na(cpi_mom)) %>% drop_na()
    if (nrow(df_var_hist) < 13) return(NULL)
    
    ts_var <- ts(df_var_hist %>% dplyr::select(-date), frequency = 12,
                 start = c(year(min(df_var_hist$date)), month(min(df_var_hist$date))))
    lag_selection <- VARselect(ts_var, lag.max = 6, type = "const")
    optimal_lag <- lag_selection$selection["AIC(n)"]
    model <- vars::VAR(ts_var, p = optimal_lag, type = "const")
    fc <- predict(model, n.ahead = h)
    
    forecasted_mom <- as.numeric(fc$fcst$cpi_mom[, "fcst"]) / 100
    last_12_cpi <- df_var_hist %>% tail(12) %>% pull(cpi)
    cpi_levels <- tail(last_12_cpi, 1) * cumprod(1 + forecasted_mom)
    full_cpi_series <- c(last_12_cpi, cpi_levels)
    forecasted_yoy <- 100 * (full_cpi_series[13:(12 + h)] / full_cpi_series[1:h] - 1)
    
    return(list(forecast = forecasted_yoy, optimal_lag = optimal_lag))
  }
  
  fit_custom_lm_var <- function(df, custom_predictors, h, optimal_lag) {
    variables_to_forecast <- c("cpi_mom", custom_predictors)
    
    df_lagged <- df %>% dplyr::select(date, cpi, all_of(variables_to_forecast))
    for (lag in 1:optimal_lag) {
      for (var in variables_to_forecast) {
        df_lagged[[paste0(var, "_lag", lag)]] <- lag(df_lagged[[var]], lag)
      }
    }
    lm_data <- df_lagged %>% filter(!is.na(cpi_mom)) %>% drop_na()
    model_list <- list()
    predictors <- grep("_lag", names(lm_data), value = TRUE)
    
    for (var in variables_to_forecast) {
      formula <- as.formula(paste(var, "~", paste(predictors, collapse = " + ")))
      model_list[[var]] <- lm(formula, data = lm_data)
    }
    
    last_date <- max(lm_data$date)
    fc_dates <- seq.Date(from = last_date %m+% months(1), by = "month", length.out = h)
    forecast_df <- tibble(date = fc_dates)
    for (var in variables_to_forecast) forecast_df[[var]] <- NA_real_
    history <- bind_rows(tail(lm_data, optimal_lag), forecast_df)
    
    for (i in (optimal_lag + 1):(optimal_lag + h)) {
      for (lag in 1:optimal_lag) {
        for (var in variables_to_forecast) {
          lag_col <- paste0(var, "_lag", lag)
          history[[lag_col]][i] <- history[[var]][i - lag]
        }
      }
      newdata <- history[i, predictors, drop = FALSE]
      for (var in variables_to_forecast) {
        model <- model_list[[var]]
        history[[var]][i] <- predict(model, newdata = newdata)
      }
    }
    
    history[(optimal_lag + 1):(optimal_lag + h), c("date", "cpi_mom")]
  }
  
  observeEvent(input$run_forecast, {
    loading(TRUE)
    req(input$model_type)
    df <- selected_data()
    req("date" %in% names(df), "cpi" %in% names(df))
    df <- df %>% arrange(date) %>% filter(!is.na(cpi_yoy))
    forecast_start <- max(df$date[!is.na(df$cpi_yoy)]) %m+% months(1)
    forecast_start_val(forecast_start)
    if (nrow(df) < 13) {
      showNotification("At least 13 months of data are required.", type = "error")
      loading(FALSE)
      return(NULL)
    }
    h <- input$horizon
    
    if (input$model_type == "arima") {
      ts_data <- ts(df$cpi_yoy, frequency = 12, start = c(year(min(df$date)), month(min(df$date))))
      arima_model <- forecast::auto.arima(ts_data, allowdrift = TRUE)
      fc <- forecast::forecast(arima_model, h = h)
      fc_start <- max(df$date) %m+% months(1)
      
      forecast_tbl <- tibble(Date = seq.Date(from = fc_start, by = "month", length.out = h),
                             Forecast_YoY = as.numeric(fc$mean), Model = "ARIMA")
      actuals <- df %>% dplyr::select(Date = date, Forecast_YoY = cpi_yoy) %>% mutate(Model = "Actual")
      forecast_result(bind_rows(actuals, forecast_tbl))
      range_dates <- range(c(actuals$Date, forecast_tbl$Date), na.rm = TRUE)
      
    } else if (input$model_type == "var") {
      req(input$predictors)
      predictors <- input$predictors
      df_train <- df %>% filter(!is.na(cpi_mom))
      df_synthetic <- if ("oil_px" %in% predictors) generate_oil_path(df_train, h, input$custom_oil_level) else df_train
      
      var_predictors <- setdiff(predictors, "cpi")
      baseline_result <- list(forecast = rep(NA, h), optimal_lag = 3)
      optimal_lag <- baseline_result$optimal_lag
      
      df_forecast <- df_synthetic %>% filter(date > max(df_train$date))
      df_combined <- bind_rows(df_train, df_forecast)
      if ("oil_px" %in% predictors) synthetic_oil_df(df_combined)
      custom_fc_tbl <- fit_custom_lm_var(df_combined, setdiff(var_predictors, "oil_px"), h, optimal_lag)
      
      last_12_cpi <- df %>% filter(!is.na(cpi)) %>% arrange(date) %>% tail(12) %>% pull(cpi)
      forecasted_mom <- custom_fc_tbl$cpi_mom / 100
      cpi_forecast_levels <- tail(last_12_cpi, 1) * cumprod(1 + forecasted_mom)
      full_cpi_series <- c(last_12_cpi, cpi_forecast_levels)
      cpi_yoy_forecast <- 100 * (full_cpi_series[13:(12 + h)] / full_cpi_series[1:h] - 1)
      
      custom_tbl <- tibble(Date = seq.Date(from = forecast_start, by = "month", length.out = h),
                           Forecast_YoY = cpi_yoy_forecast, Model = "Custom VAR")
      actuals <- df %>% dplyr::select(Date = date, Forecast_YoY = cpi_yoy) %>% mutate(Model = "Actual")
      forecast_tbl <- bind_rows(actuals, custom_tbl)
      forecast_result(forecast_tbl)
      range_dates <- range(forecast_tbl$Date, na.rm = TRUE)
    }
    
    updateDateRangeInput(session, "date_range", start = range_dates[1], end = range_dates[2])
    shinyjs::enable("download_data")
    Sys.sleep(0.5)
    loading(FALSE)
  })
  
  output$cpi_plot <- renderPlot({
    req(forecast_result(), !loading())
    df_plot <- forecast_result() %>% filter(Date >= input$date_range[1], Date <= input$date_range[2])
    ggplot(df_plot, aes(x = Date, y = Forecast_YoY, color = Model)) +
      geom_rect(aes(xmin = forecast_start_val(), xmax = max(df_plot$Date), ymin = -Inf, ymax = Inf),
                fill = "gray90", alpha = 0.3, inherit.aes = FALSE) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_vline(xintercept = forecast_start_val(), linetype = "dashed", color = "black") +
      scale_color_manual(values = c("Actual" = "black", "Custom VAR" = "dodgerblue", "ARIMA" = "orange")) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      labs(title = "CPI YoY: Actual vs Forecasts", x = "Date", y = "CPI YoY (%)", color = "Model") +
      theme_minimal()
  })
  
  output$oil_plot <- renderPlot({
    df <- synthetic_oil_df() %||% selected_data()
    max_actual_oil_date <- max(df$date[!is.na(df$oil_px) & df$date < Sys.Date()])
    ggplot(df, aes(x = date, y = oil_px)) +
      geom_line(data = df %>% filter(date <= max_actual_oil_date), color = "brown") +
      geom_line(data = df %>% filter(date > max_actual_oil_date), color = "blue", linetype = "dashed", linewidth = 1.2) +
      labs(title = "Oil Prices: Actual & Synthetic", x = "Date", y = "Oil Price") +
      theme_minimal()
  })
  
  output$forecast_table <- renderTable({
    req(forecast_result(), !loading())
    forecast_result() %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      filter(!(Model == "Actual" & Date >= forecast_start_val())) %>%
      arrange(Date) %>%
      mutate(Date = format(Date, "%Y-%m"), Forecast_YoY = round(Forecast_YoY, 2)) %>%
      dplyr::select(Date, Forecast_YoY, Model)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("forecast_", input$country, "_", Sys.Date(), ".csv"),
    content = function(file) write.csv(forecast_result(), file, row.names = FALSE)
  )
}