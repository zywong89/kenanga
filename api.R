## Load Functions
source("./Scripts/Data Preparation.R")
source("./Scripts/Model Fitting.R")
source("./Scripts/Forecasting.R")
source("./Scripts/Data Insertion.R")

#* @get /predict
#* @serializer unboxedJSON
obtainForecasts <- function(refit = 0) {
  ## Store console output as text
  sink("./ROutput.txt")
  
  ## Data Preparation
  data <- dataPrep(daily, monthly)
  
  ## Model Fitting
  if(refit == 1) {
    final.fit <- modelling(data$abt)
  }
  else {
    load("./Model/fittedModel.RData")
  }
  
  ## Forecasting
  out <- finalForecast(data$abt, final.fit)
  
  ## Store Data into Database
  dataStore("kenanga", "localhost", 5432, "postgres", "postgres",
            data$daily, data$monthly, data$abt, out)
  
  ## Close Output Sink
  cat("Script execution completed!!")
  sink()
  
  ## Output
  list(out@forecast$seriesFor)
}
