## Read Data
daily <- read.csv("./Data/Assignment-dailyexcerpt.csv", na.strings = c("", "NA"))
monthly <- read.csv("./Data/Assignment-monthlyexcerpt.csv", na.strings = c("", "NA"))


dataPrep <- function(daily, monthly) {
  ## Data Processing & Tidying
  # Remove first header row
  daily <- daily[2:nrow(daily), ]
  monthly <- monthly[2:nrow(monthly), ]
  
  # Daily Data
  # Remove empty rows (based on date of USDMYR currency)
  daily <- daily[which(!is.na(daily$USDMYR.Curncy)), ]
  # Remove empty columns
  daily <- daily[, colSums(!is.na(daily)) > 0]
  # Rename columns
  colnames(daily) <- c("dt1", "klpln", "dt2", "fbmklci", "dt3", "pal2maly",
                       "dt4", "bo1", "dt5", "usdmyr", "dt6", "co1", "dt7", "qs1")
  # Reconstruct dataset
  library(dplyr)
  # Convert all columns to character
  daily <- daily %>% mutate_all(as.character)
  temp <- with(daily, data.frame(dt = dt5, usdmyr = as.numeric(usdmyr)))
  temp <- left_join(temp, daily[, c("dt1", "klpln")], by = c("dt" = "dt1"))
  temp <- left_join(temp, daily[, c("dt2", "fbmklci")], by = c("dt" = "dt2"))
  temp <- left_join(temp, daily[, c("dt3", "pal2maly")], by = c("dt" = "dt3"))
  temp <- left_join(temp, daily[, c("dt4", "bo1")], by = c("dt" = "dt4"))
  temp <- left_join(temp, daily[, c("dt6", "co1")], by = c("dt" = "dt6"))
  temp <- left_join(temp, daily[, c("dt7", "qs1")], by = c("dt" = "dt7"))
  # Replace missing values with last (if not next) observed value
  library(zoo)
  temp$dt <- as.Date(temp$dt, format = "%d-%m-%y")
  for(var in setdiff(colnames(temp), "dt"))
    temp[, var] <- as.numeric(na.locf(zoo(temp[, var]), fromLast = T))
  daily <- temp
  
  # Monthly Data
  # Remove empty rows (based on first column)
  monthly <- monthly[which(!is.na(monthly[, 1])), ]
  # Remove empty columns
  monthly <- monthly[, colSums(!is.na(monthly)) > 0]
  # Rename columns
  colnames(monthly) <- c("dt1", "bo1", "dt2", "co1", "dt3", "qs1",
                         "dt4", "pal2maly", "dt5", "usdmyr", "dt6", "noaa",
                         "dt7", "topi", "dt8", "dxy")
  monthly <- monthly %>% mutate_all(as.character) %>%
    select(dt = dt1, bo1, co1, qs1, pal2maly, usdmyr, noaa, topi, dxy)
  monthly$dt <- as.Date(monthly$dt, format = "%d-%m-%y")
  for(var in setdiff(colnames(monthly), "dt"))
    monthly[, var] <- as.numeric(monthly[, var])
  
  
  ## Feature Engineering
  # Create exogeneous variables
  data <- monthly %>% select(dt, pal2maly, topi, noaa, usdmyr, co1, qs1)
  # Linear trend variable
  data$trend <- 1:nrow(data)
  # Lagged and differencing variables
  # data$topi.t1 <- lag(data$topi, 1)
  # data$noaa.t1 <- lag(data$noaa, 1)
  # data$usdmyr.t1 <- lag(data$usdmyr, 1)
  # data$usdmyr.d1t1 <- c(NA, lag(diff(data$usdmyr, 1), 1))
  # data$co1.t1 <- lag(data$co1, 1)
  # data$co1.d1t1 <- c(NA, lag(diff(data$co1, 1), 1))
  # data$qs1.t1 <- lag(data$qs1, 1)
  # data$qs1.d1t1 <- c(NA, lag(diff(data$qs1, 1), 1))
  # data <- data %>% select(dt, pal2maly, trend, topi.t1, noaa.t1, usdmyr.t1,
  #                         usdmyr.t1, usdmyr.d1t1, co1.t1, co1.d1t1,
  #                         qs1.t1, qs1.d1t1)
  # Replace initial NA values with mean
  # for(var in colnames(data[, -1]))
  #   data[is.na(data[, var]), var] <- mean(data[, var], na.rm = T)
  # Exogeneous variables using daily data can be added here if needed
  
  return(data)
}
