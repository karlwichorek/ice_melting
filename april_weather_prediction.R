library(lubridate)
library(dplyr)
library(stringr)
library(rnoaa)
library(timeDate)
library(here)

###############################################################################
# Average temp in April is highly predictive of break-up day. Year is still
# significant, but dominated by temperature in April. Below script will grab
# historical temperature data for UAF and read in historical El Nino and 
# Pacific Decadal Oscillation values. Data will be used to train a random
# forest model for predicting April temperatures.

# Nenana GHCN Code - USW00026435
# Nenana data is incomplete - use Fairbanks University instead
# Fairbanks University data: GHCND:USC00509641, WBAN: 26441
# Fairbanks GHCN code - USW00026411
# Anchorage (Merrill Field) GHCN code - USW00026409
# Merrill field data starts in 1916, but has some weird gaps
# Data on stations ID here: http://berkeleyearth.lbl.gov/stations/43660

# Grab historical Fairbanks weather data
# Data not showing up past 2016 for some reason
year.range <- seq(1915, 2000, by = 1)
fai.weather <- data.frame(Date = as.POSIXlt(character()), Temp = numeric())
for(i in 1:length(year.range)){
  start.date <- paste0(year.range[i], '-01-01')
  end.date <- paste0(year.range[i], '-12-01')
  x <- ncdc(datasetid='GHCNDMS', 
            stationid='GHCND:USC00509641', 
            datatypeid='MNTM', 
            startdate = start.date, 
            enddate = end.date, 
            limit=500, 
            token = "")
  y <- data.frame(Date = x[2]$data$date, Temp = x[2]$data$value)
  fai.weather <- rbind(fai.weather, y)
  Sys.sleep(1)
}
# Missing: Jan-Feb 1974, Feb 1932, Jul 1969

# Data from Fairbanks to fill in for missing University data
dates <- c("1932-02-01", "1969-07-01", "1974-01-01", "1974-02-01")
for(i in 1:length(dates)){
  start.date <- dates[i]
  end.date <- dates[i]
  x <- ncdc(datasetid='GHCNDMS', 
            stationid='GHCND:USW00026411', 
            datatypeid='MNTM', 
            startdate = start.date, 
            enddate = end.date, 
            limit=500, 
            token = "")
  y <- data.frame(Date = x[2]$data$date, Temp = x[2]$data$value)
  fai.weather <- rbind(fai.weather, y)
  Sys.sleep(1)
}

fai.weather$Date <- as.Date(fai.weather$Date, "%Y-%m-%d")
fai.weather <- fai.weather[order(fai.weather$Date), ]
fai.weather$Temp <- fai.weather$Temp/10

# Create separate variables for training data
var.names <- paste0("m", 1:12)
for(i in 1:12){
  assign(var.names[[i]], subset(fai.weather, 
                                format.Date(Date, "%m")==str_pad(i, 2, pad=0)))
}

###############################################################################
# Data on El Nino here: https://www.esrl.noaa.gov/psd/enso/mei/
# Read in El Nino index data
mei <- as.data.frame(read.csv(here("data", "enso_index.csv"), header = TRUE))
mei.arima <- as.data.frame(read.csv(here("data", "enso_index_arima.csv"), 
                                    header = TRUE))

###############################################################################
# Data on Pacific Decadal Oscillation here: 
# https://www.ncdc.noaa.gov/teleconnections/pdo/
# Read in PDO data
pdo <- as.data.frame(read.csv(here("data", "pdo_data.csv"), header = TRUE))
pdo.arima <- as.data.frame(read.csv(here("data", "pdo_data_arima.csv"), 
                                    header = TRUE))

###############################################################################
# Create other variables of interest: current March vs average temperature in
# March, average April temperature

# Longer March and April data from Fairbanks University for calculating means
yrange2 <- seq(1906, 1914, by = 1)
extra.data <- data.frame(Date = as.POSIXlt(character()), Temp = numeric())
for(i in 1:length(yrange2)){
  start.date <- paste0(yrange2[i], '-03-01')
  end.date <- paste0(yrange2[i], '-04-01')
  x <- ncdc(datasetid='GHCNDMS', 
            stationid='GHCND:USC00509641', 
            datatypeid='MNTM', 
            startdate = start.date, 
            enddate = end.date, 
            limit=500, 
            token = "")
  y <- data.frame(Date = x[2]$data$date, Temp = x[2]$data$value)
  extra.data <- rbind(extra.data, y)
  Sys.sleep(1)
}
extra.data$Date <- as.Date(extra.data$Date, "%Y-%m-%d")
extra.data$Temp <- extra.data$Temp/10
extra.mar <- rbind(subset(extra.data, format.Date(Date, "%m")=="03"), m3)
extra.apr <- rbind(subset(extra.data, format.Date(Date, "%m")=="04"), m4)

april.mean <- numeric(length = nrow(extra.apr))
for(i in 1:nrow(extra.apr)){april.mean[i] <- mean(extra.apr[1:i, 2])}
april.mean <- april.mean[10:95]

april.sd <- numeric(length = nrow(extra.apr))
for(i in 1:nrow(extra.apr)){april.sd[i] <- sd(extra.apr[1:i, 2])}
april.sd <- april.sd[10:95]

mar.mean <- numeric(length = nrow(extra.mar))
for(i in 1:nrow(extra.mar)){mar.mean[i] <- mean(extra.mar[1:i, 2])}
mar.mean <- mar.mean[10:95]

mar.sd <- numeric(length = nrow(extra.mar))
for(i in 1:nrow(extra.mar)){mar.sd[i] <- mean(extra.mar[1:i, 2])}
mar.sd <- mar.sd[10:95]

mar.anom <- m3[, 2] - mar.mean
apr.anom <- m4[, 2] - april.mean
summary(lm(apr.anom~mar.anom+0))

mar.zscore <- (m3[, 2] - mar.mean) / mar.sd
apr.zscore <- (m4[, 2] - april.mean) / april.sd
summary(lm(apr.zscore~mar.zscore+0))

# Compile all data into a training data frame
train.data <- data.frame(april.temp = m4[2:85, 2],
                         o1 = m3[, 2], 
                         mei.fm = mei$FM, 
                         pdo.m = pdo$M)

# Compile data into training data frame for an ARIMA model
mei.arima$Date <- as.Date(mei.arima$Date, "%m/%d/%Y")
pdo.arima$Date <- as.Date(pdo.arima$Date, "%m/%d/%Y")
data.arima <- merge(mei.arima, pdo.arima, by = "Date")
data.arima <- merge(fai.weather, data.arima, by = "Date")

###############################################################################
# Script to look at correlation between April temp and melting day
# Load melting data
data.ak <- readRDS(melting_data.RDS, here("output"))

cor(subset(fai.weather, format.Date(Date, "%m")=="04", select = c("Temp")), 
    as.numeric(data.ak$MD[1:99]))

fai.weather.ss <- subset(fai.weather, Date > as.Date("1917-01-01"))
fai.weather.ss <- subset(fai.weather.ss, Date < as.Date("2001-01-01"))
test <- data.frame(MD = as.numeric(data.ak$MD[1:84]),
                   Temp = subset(fai.weather.ss, 
                                 format.Date(Date, "%m")=="04", 
                                 select = c("Temp")),
                   Year = as.numeric(data.ak$Year[1:84]))

mod1 <- lm(MD~Year+Temp, data = test)
rmod1 <- rlm(MD~Year+Temp, data = test)

oos.test <- data.frame(MD = as.numeric(data.ak$MD[85:101]),
                       Temp = rep(best.guess, 17),
                       Year = as.numeric(data.ak$Year[85:101]))

prediction.fan <- predict(mod1, oos.test)
RMSE.fan <- sqrt(mean((prediction.fan - test.ak.data[, 6])^2))

plot(x = unlist(subset(fai.weather, format.Date(Date, "%m")=="04", 
                       select = c("Temp"))), 
     y = as.numeric(data.ak$MD[1:99]), type = "p")

###############################################################################
# Grab the data to test the different models
# Grab historical Fairbanks weather data
# Data not showing up past Mar-2016 for some reason
oos.yrange <- seq(2001, 2015, by = 1)
oos.weather <- data.frame(Date = as.POSIXlt(character()), Temp = numeric())
for(i in 1:length(oos.yrange)){
  start.date <- paste0(oos.yrange[i], '-01-01')
  end.date <- paste0(oos.yrange[i], '-12-01')
  x <- ncdc(datasetid='GHCNDMS', 
            stationid='GHCND:USC00509641', 
            datatypeid='MNTM', 
            startdate = start.date, 
            enddate = end.date, 
            limit=500, 
            token = "")
  y <- data.frame(Date = x[2]$data$date, Temp = x[2]$data$value)
  oos.weather <- rbind(oos.weather, y)
  Sys.sleep(1)
}

oos.weather$Date <- as.Date(oos.weather$Date, "%Y-%m-%d")
oos.weather$Temp <- oos.weather$Temp/10

# Create separate variables
oos.var.names <- paste0("om", 3:4)
for(i in 1:2){
  assign(oos.var.names[[i]], subset(oos.weather, 
                                    format.Date(Date, 
                                                "%m")==str_pad(i+2, 2, pad=0)))
}

# Compile all OOS data into a test data frame
test.data <- data.frame(april.temp = om4[, 2],
                        o1 = om3[, 2], 
                        mei.fm = mei.oos$FM, 
                        pdo.m = pdo.oos$M)

# Compile data into test data frame for an ARIMA model
test.arima <- merge(oos.weather, pdo.arima, by = "Date")
test.arima <- merge(test.arima, mei.arima, by = "Date")
