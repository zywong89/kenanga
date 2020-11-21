## Load Functions
source("./Scripts/Data Preparation.R")
source("./Scripts/Model Fitting.R")
source("./Scripts/Forecasting.R")

#* @get /predict
#* @serializer unboxedJSON
obtainForecasts <- function(refit = 0) {
  ## Data Preparation
  data <- dataPrep(daily, monthly)
  
  ## Model Fitting
  if(refit == 1) {
    final.fit <- modelling(data)
  }
  else {
    load("./Model/fittedModel.RData")
  }
  
  ## Forecasting
  out <- finalForecast(data, final.fit)
  
  ## Output
  list(out)
}
