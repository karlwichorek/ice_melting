library(forecast)

# Separate training and test data
train.data.arima <- data.arima[data.arima$Date < as.Date("1991-01-01"), ]
test.data.arima <- data.arima[data.arima$Date > as.Date("1990-12-01"), ]

# Automatically select an ARIMA model for the univariate time series
auto.fit <- auto.arima(train.data.arima[, 2])

# Add the x variables based on PDO and ENSO and train an ARIMA model
fit <- Arima(train.data.arima[, 2], order = c(3,0,1), 
             xreg = train.data.arima[, 3:4])

# Evaluate the auto-ARIMA model on out of sample data
eval1 <- Arima(test.data.arima[, 2], model = auto.fit)
onestep1 <- fitted(eval1)

plot(onestep1)
lines(test.data.arima[, 2], col = "blue")

# Evaluate the ARIMA model with exogenous variables on out of sample data
eval2 <- Arima(test.data.arima[, 2], model = fit, 
               xreg = test.data.arima[, 3:4])
onestep2 <- fitted(eval2)

plot(onestep2)
lines(test.data.arima[, 2], col = "blue")

# Compare the two models' accuracy
accuracy(eval1)
accuracy(eval2)

rows <- seq(0, 14)*12 + 4
RMSE.arima <- sqrt(mean((onestep1[rows] - test.data.arima[rows, 2])^2))

plot(onestep1[rows])
lines(test.data.arima[rows, 2], col = "blue")
lines(best.guess)

rows.train <- seq(0, 85)*12 + 4
best.guess <- mean(train.data.arima[rows.train, 2])
RMSE.bg <- sqrt(mean((best.guess - test.data.arima[rows, 2])^2))

# Still can't improve on just guessing the average temp in April... 
