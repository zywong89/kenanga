dataStore <- function(dbname, host, port, user, password, daily, monthly, abt, out) {
  # Set Up Connection
  library(RPostgreSQL)
  library(sqldf)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname,
                   host = host, port = port,
                   user = user, password = password)
  options(sqldf.RPostgreSQL.dbname = dbname,
          sqldf.RPostgreSQL.host = host,
          sqldf.RPostgreSQL.port = port,
          sqldf.RPostgreSQL.user = user,
          sqldf.RPostgreSQL.password = password)

  # Insert Data into Database (Drop Table if Exists)
  cat(rep("", 1), sep = "\n")
  if(dbExistsTable(con, c("data", "daily_data"))) {
    cat(paste("Check & drop data.daily_data:",
              dbRemoveTable(con, name = c("data", "daily_data"))),sep = "\n")
  }
  cat(rep("", 1), sep = "\n")
  cat(paste("Insert data into data.daily_data",
            dbWriteTable(con, name = c("data", "daily_data"), value = daily, row.names = F)), sep = "\n")
  cat(rep("", 1), sep = "\n")
  cat(paste("data.daily_data:", sqldf("SELECT count(*) FROM data.daily_data"), "rows inserted"), sep = "\n")
  
  cat(rep("", 1), sep = "\n")
  if(dbExistsTable(con, c("data", "monthly_data"))) {
    cat(paste("Check & drop data.monthly_data:",
              dbRemoveTable(con, name = c("data", "monthly_data"))), sep = "\n")
  }
  cat(rep("", 1), sep = "\n")
  cat(paste("Insert data into data.monthly_data",
            dbWriteTable(con, name = c("data", "monthly_data"), value = monthly, row.names = F)), sep = "\n")
  cat(rep("", 1), sep = "\n")
  cat(paste("data.monthy_data:", sqldf("SELECT count(*) FROM data.monthly_data"), "rows inserted"), sep = "\n")
  
  cat(rep("", 1), sep = "\n")
  if(dbExistsTable(con, c("data", "abt"))) {
    cat(paste("Check & drop data.abt:",
              dbRemoveTable(con, name = c("data", "abt"))), sep = "\n")
  }
  cat(rep("", 1), sep = "\n")
  cat(paste("Insert data into data.abt",
            dbWriteTable(con, name = c("data", "abt"), value = abt, row.names = F)), sep = "\n")
  cat(rep("", 1), sep = "\n")
  cat(paste("data.abt:", sqldf("SELECT count(*) FROM data.abt"), "rows inserted"), sep = "\n")
  
  # Create data frame to store forecast results
  fore <- data.frame(dt = max(abt$dt))
  fore <- cbind(fore, t(out@forecast$seriesFor))
  colnames(fore)[-1] <- c("t1_pal2maly", "t2_pal2maly", "t3_pal2maly")
  cat(rep("", 1), sep = "\n")
  if(dbExistsTable(con, c("result", "forecasts"))) {
    cat(paste("Check & drop result.forecasts:",
              dbRemoveTable(con, name = c("result", "forecasts"))), sep = "\n")
  }
  cat(rep("", 1), sep = "\n")
  cat(paste("Insert data into result.forecasts",
            dbWriteTable(con, name = c("result", "forecasts"), value = fore, row.names = F)), sep = "\n")
  cat(rep("", 1), sep = "\n")
  cat(paste("result.forecasts:", sqldf("SELECT count(*) FROM result.forecasts"), "rows inserted"), sep = "\n")
  
  # Drop Connection
  dbDisconnect(con)
}
