# Create lagged variables for each variable and lag
create_lagged_variables <- function(df, vars, lags) {
  for (lag in 1:lags) {
    for (var in vars) {
      lagged_name <- paste0(var, "_lag", lag)
      df[[lagged_name]] <- dplyr::lag(df[[var]], lag)
    }
  }
  return(df)
}

# Fit a linear model for each target variable
fit_lm_models <- function(df, vars) {
  models <- list()
  predictors <- names(df)[grepl("_lag", names(df))]
  
  for (var in vars) {
    formula <- as.formula(paste(var, "~", paste(predictors, collapse = " + ")))
    models[[var]] <- lm(formula, data = df)
  }
  return(models)
}

# Perform recursive forecast for h months
recursive_forecast <- function(models, history_df, h, lags, vars) {
  for (i in (lags + 1):(lags + h)) {
    for (lag in 1:lags) {
      for (var in vars) {
        lagged_col <- paste0(var, "_lag", lag)
        history_df[[lagged_col]][i] <- history_df[[var]][i - lag]
      }
    }
    
    newdata <- history_df[i, grepl("_lag", names(history_df)), drop = FALSE]
    
    for (var in vars) {
      model <- models[[var]]
      history_df[[var]][i] <- predict(model, newdata = newdata)
    }
  }
  
  return(history_df[(lags + 1):(lags + h), c("date", "cpi")])
}

# Generate synthetic oil price path
generate_synthetic_oil <- function(last_oil, growth_rate, h) {
  cumprod(rep(1 + growth_rate / 100, h)) * last_oil
}
