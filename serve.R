# Source Directory
setwd("./path/to/this/script")
setwd("~/../Desktop/Job Applications/Kenanga/Data Scientist Assignment")

library(plumber)
r <- plumb("api.R")
r$run(port = 8080)


# Example to call: http://127.0.0.1:8080/predict?refit=F