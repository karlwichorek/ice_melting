library(psych)
library(lubridate)
library(bcp)
library(weatherData)
library(ggplot2)
library(scales)
library(plyr)
library(maptools)
library(car)
library(here)

# Initial data
data <- as.data.frame(read.csv(here("data", "melting_data.csv"), header = TRUE))

# Remove punctuation from the AM/PM indicator
data$Date <- gsub("(.)\\.?[Mm]\\.?","\\1m", data$Date)

# Create other variables from initial time data
# Assume GMT timezone for all dates to avoid timezone issues
data$Date <- as.POSIXct(data$Date, format = "%m/%d/%Y %I:%M %p", tz="GMT")
data$Year <- year(data$Date)
data$Month <- month(data$Date)
data$Day <- day(data$Date)
data$Time <- (hour(data$Date)*60 + minute(data$Date))/1440

# Pulled data on the vernal equinox from: 
# https://www.timeanddate.com/calendar/seasons.html
ak.ve <- as.data.frame(read.csv(here("data", "ak_vernal_equinox.csv"), 
                                header = TRUE))
ak.ve$Date <- as.POSIXct(ak.ve$Date, format="%m/%d/%Y %I:%M %p", tz="GMT")

# Calculate days since vernal equinox that breakup occurs
data$MD <- difftime(round(data$Date, "days"), round(ak.ve[22:122, 1], "days"), 
                    units = c("days"))
plot(data$Year, data$MD)

# Initial model - impact of year on melting month/day
model1 <- lm(MD~Year, data=data)
print(model1)
summary(model1)
plot(data$Year, data$MD)
plot(model1, which = 2)

# Check if there's a changepoint in the mean of the month/day melting time
plot(bcp(data$MD)) # No evidence of change in mean

# Create model for time of day
med.time <- median(na.omit(data$Time))

# Prediction for date in current year
new <- data.frame(Year = 2019)
predict(model1, new, se.fit = TRUE, interval = "prediction", level = 0.50)

# Add result to median time of day for melting
