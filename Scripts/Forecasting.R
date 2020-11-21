finalForecast <- function(data, final.fit) {
  ## Forecasting
  # Create out-of-sample forecasted exogeneous variables
  library(rugarch)
  library(forecast)
  exovar.out <- NULL
  for(var in colnames(data)[-c(1:2, ncol(data))]) {
    # Auto-select best ETS smoothing method for each variable
    fit <- ets(data[, var], model = "ZZZ")
    fore <- as.numeric(forecast(fit, h = 3)$mean)
    
    # Create data frame of forecasted exogeneous variables
    if(is.null(exovar.out)) exovar.out <- fore
    else exovar.out <- cbind(exovar.out, fore)
  }
  # Combine forecasted variables with linear trend
  exovar.out <- cbind(exovar.out, tail(data$trend, 3) + 3)
  # Rename columns
  colnames(exovar.out) <- colnames(data)[-c(1:2)]
  
  
  # Produce 3-step Ahead Forecasts
  out <- ugarchforecast(final.fit, n.ahead = 3,
                        external.forecasts = list(mregfore = as.matrix(exovar.out)))
  
  return(out@forecast$seriesFor)
}