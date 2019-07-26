library(lubridate)
library(dplyr)
library(rnoaa)
library(timeDate)

###############################################################################
# Grab weather data to see if weather is a more significant predictor than
# year in forecasting melting month/day

# Warm days in April correlate highly with break-up day. 
# Figure out prediction for temp in April.

# Get package rsoi to access Oceanic Nino Index data and check on relationship
# to April temperatures

# Fairbanks airport goes back to 1946
# Weather underground URL not working

# Station list here = ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt

# Fairbanks GHCN code - USW00026411
# Nenana Code - USW00026435

# Anchorage GHCN code - USW00026409

# Using rnoaa function
# Fairbanks station data
res1 <- isd(usaf="702610", wban="26411", year=2016)

## remove 999, which is for data point not available
res1$temperature <- as.numeric(res1$temperature)
res1 <- res1 %>% filter(temperature < 900)

res1$Date <- as.Date(res1$date, format = "%Y%m%d")

# Convert to actual temperature (because they don't like decimals?)
# Believe this temperature is in celsius
res1$temperature <- res1$temperature/10

d.mean <- aggregate(res1$temperature, list(Date = format(res1$Date, "%Y-%m-%d")), mean, na.rm = T)
m.mean <- aggregate(res1$temperature, list(Date = format(res1$Date, "%Y-%m")), mean, na.rm = T)

# Using ncdc function
# Token: 
out1 <- ncdc(datasetid='GHCND', 
             stationid='GHCND:USW00026411', 
             datatypeid='PRCP', 
             startdate = '2010-03-01', 
             enddate = '2010-05-31', 
             limit=500, 
             token = "")
ncdc_plot(out1)

year.range <- seq(1946, 2015, by = 1)
all.weather <- data.frame(Date = as.POSIXlt(character()), 
                          Max_TemperatureF = integer(), 
                          Mean_TemperatureF = integer(), 
                          Min_TemperatureF = integer())
for(i in 1:length(year.range)){
  x <- getWeatherForYear("PAFA", year.range[i])
  all.weather <- rbind(all.weather, x)
}

wintertemp <- subset(all.weather, 
                      as.numeric(format(all.weather$Date, "%m")) %in% 
                       c(9, 10))
data$Temp <- numeric(length = nrow(data))

for(i in 1:(length(year.range)-1)){
  yr <- year.range[i]
  x <- subset(wintertemp$Mean_TemperatureF, 
              as.numeric(format(wintertemp$Date, "%Y")) %in% 
                yr)
  y <- sum(x >= 32, na.rm = TRUE) / length(x) 
  data[(28+i+1), 6] <- y
}

# Evaluate linear model with predicted April temp as additional feature
model.temp <- lm(MD~Year+Temp, data=data[29:98, ])
summary(model.temp)
