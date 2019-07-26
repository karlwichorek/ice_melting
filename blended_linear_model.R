# Use data from Yukon and Kuskokwim river to create more robust linear
# regression model

library(psych)
library(timeDate)
library(lubridate)
library(bcp)
library(weatherData)
library(rnoaa)
library(ggplot2)
library(scales)
library(plyr)
library(maptools)
library(car)
library(here)

###############################################################################
# Data from the Nenana river 
# Assume GMT timezone for all dates to simplify calculations
data.ak <- as.data.frame(read.csv(here("data", "melting_data_2018.csv"), 
                                  header = TRUE))

# Remove punctuation from the AM/PM indicator
data.ak$Date <- gsub("(.)\\.?[Mm]\\.?","\\1m", data.ak$Date)

# Create other variables from initial time data
data.ak$Date <- as.POSIXct(data.ak$Date, format="%m/%d/%Y %I:%M %p", tz="GMT")
data.ak$Year <- year(data.ak$Date)
data.ak$Month <- month(data.ak$Date)
data.ak$Day <- day(data.ak$Date)
data.ak$Time <- (hour(data.ak$Date)*60 + minute(data.ak$Date))/1440
data.ak$MD <- numeric(length = nrow(data.ak))

###############################################################################
# Data from the Yukon river 
# Taken from (http://www.yukonriverbreakup.com/statistics)
# Assume GMT timezone for all dates to simplify calculations
data.yk.o <- as.data.frame(read.csv(here("data", "yukon_data_2018.csv"), 
                                    header = TRUE))
data.yk <- data.yk.o

# Remove punctuation from the AM/PM indicator
data.yk$Date <- gsub("(.)\\.?[Mm]\\.?","\\1m", data.yk$Date)
data.yk$Date <- as.POSIXct(data.yk$Date, format="%m/%d/%Y %I:%M %p", tz="GMT")

# Create other variables from initial time data
data.yk$Year <- year(data.yk$Date)
data.yk$Month <- month(data.yk$Date)
data.yk$Day <- day(data.yk$Date)
data.yk$Time <- (hour(data.yk$Date)*60 + minute(data.yk$Date))/1440
data.yk$MD <- numeric(length = nrow(data.yk))

# Calculate median time of day for Yukon data
med.time <- median(na.omit(data.yk$Time))
medtf <- paste(floor(med.time*1440/60), ":", 
               (med.time*1440/60 - floor(med.time*1440/60))*60, 
               ":00", sep = "")

# Fill missing dates
missing <- is.na(data.yk$Date)
data.yk$Date[missing] <- as.POSIXct(paste(data.yk.o[missing, 1],
                                          medtf, sep = " "), 
                                    format = "%m/%d/%Y %H:%M:%S", 
                                    tz = "GMT")

# Perform data separation again
data.yk$Year <- year(data.yk$Date)
data.yk$Month <- month(data.yk$Date)
data.yk$Day <- day(data.yk$Date)
data.yk$Time <- (hour(data.yk$Date)*60 + minute(data.yk$Date))/1440
data.yk$MD <- numeric(length = nrow(data.yk))
data.yk$Sun <- numeric(length = nrow(data.yk))

###############################################################################
# Data from the Kuskokwim river in Bethel 
# Assume GMT timezone for all dates to simplify calculations
data.bt <- as.data.frame(read.csv(here("data", "bethel_data_2018.csv"), 
                                  header = FALSE))
colnames(data.bt) <- "Date"

# Create other variables from initial time data
data.bt$Date <- as.POSIXct(data.bt$Date, format="%m/%d/%Y", tz="GMT")
data.bt$Year <- year(data.bt$Date)
data.bt$Month <- month(data.bt$Date)
data.bt$Day <- day(data.bt$Date)
data.bt$MD <- numeric(length = nrow(data.bt))

###############################################################################
# Pulled data on the vernal equinox from 
# https://www.timeanddate.com/calendar/seasons.html
ak.ve <- as.data.frame(read.csv(here("data", "ak_vernal_equinox.csv"), 
                                header = TRUE))
yk.ve <- as.data.frame(read.csv(here("data", "yk_vernal_equinox.csv"), 
                                header = TRUE))

ak.ve$Date <- as.POSIXct(ak.ve$Date, format="%m/%d/%Y %I:%M %p", tz="GMT")
yk.ve$Date <- as.POSIXct(yk.ve$Date, format="%m/%d/%Y %I:%M %p", tz="GMT")

###############################################################################
# Calculate days since vernal equinox that breakup occurs
# Yukon
data.yk$MD <- difftime(round(data.yk$Date, "days"), round(yk.ve[1:122, 1], "days"), 
                       units = c("days"))
plot(data.yk$Year, data.yk$MD)

# Nenana
data.ak$MD <- difftime(round(data.ak$Date, "days"), round(ak.ve[22:122, 1], "days"), 
                       units = c("days"))
plot(data.ak$Year, data.ak$MD)

# Bethel
bt.ve.dates <- subset(ak.ve, Date >= as.Date("1924-01-01") & 
                        Date < as.Date("2017-12-31"))
bt.ve.dates <- subset(bt.ve.dates, year(Date) != 1965 & year(Date) != 1966 & 
                        year(Date) != 1933 & year(Date) != 1939)
bt.ve.dates$Date <- as.POSIXct(bt.ve.dates$Date, 
                               format="%m/%d/%Y %I:%M %p", tz="GMT")
data.bt$MD <- difftime(round(data.bt$Date, "days"), round(bt.ve.dates$Date, "days"), 
                       units = c("days"))
plot(data.bt$Year, data.bt$MD)

###############################################################################
# Initial model - impact of year on melting month/day
# Use Yukon and Nenana river data together
comb.data <- data.frame("MD" = cbind(data.yk$MD[1:82], data.ak$MD[1:61]), 
                        "Year" = cbind(data.yk$Year[1:82], data.ak$Year[1:61]))

model.comb <- lm(MD~Year, data=comb.data)
print(model.comb)
summary(model.comb)
plot(model3, which = 2)

# Kuskokwim river data not additive
