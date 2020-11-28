# Source Directory
args <- commandArgs(T)
setwd(args[1])

library(plumber)
r <- plumb("api.R")
r$run(port = 8080)
