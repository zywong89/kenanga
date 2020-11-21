modelling <- function(data) {
  ## Model Fitting: ARIMAX(p, 1, q)-GARCH(r, s) Model
  library(rugarch)
  final.bic <- Inf
  final.order <- c(0, 0, 0, 0)
  for(p in 0:3) for (q in 0:3) for (r in 0:2) for(s in 0:2) {
    if((p == 0 && q == 0) | (r == 0 && s == 0)) next
    print(paste("Fitting models p =", p, "q =", q, "r =", r, "s =", s))
    
    # Model specification
    spec <- ugarchspec(variance.model = list(garchOrder = c(r, s)),
                       mean.model = list(armaOrder = c(p, q),
                                         external.regressors = as.matrix(data[, -c(1, 2)]),
                                         arfima = T),
                       fixed.pars = list(arfima = 1), # Ensure ARIMA(p, 1, q) model
                       distribution.model = "sged") # Allow capturing skewness & fat tail
    
    # Obtain fitting status
    fit <- tryCatch(ugarchfit(spec, data$pal2maly,
                              solver = 'hybrid', fit.control = list(stationarity = 1)),
                    error = function(err) F,
                    warning = function(err) F)
    
    # Take model with lowest BIC, ignore model if no convergence
    if(!is.logical(fit)) {
      current.bic <- infocriteria(fit)[2]
      if(current.bic < final.bic) {
        final.bic <- current.bic
        final.order <- c(p, q, r, s)
        final.fit <- fit
      }
    }
    else {
      next
    }
  }
  
  # Summary of Final Model
  print(final.fit)
  save("./Model/fittedModel.RData")
  
  return(final.fit)
}