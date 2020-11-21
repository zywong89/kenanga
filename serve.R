# Source Directory
setwd("./path/to/this/script")

library(plumber)
r <- plumb("api.R")
r$run(port = 8080)