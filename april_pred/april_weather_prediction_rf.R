set.seed(123) 
library(here)
library(randomForest)
library(forecast)

###############################################################################
# Baseline model - predict the mean of the training data
best.guess <- mean(train.data$april.temp)

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-train.data$april.temp)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-train.data$april.temp))
MAE.baseline

###############################################################################
# Model 2: Create a multiple linear regression model using the training data
mlm <- lm(april.temp ~ ., data = train.data)

# Inspect the model
summary(mlm)
RSS <- c(crossprod(mlm$residuals))
MSE <- RSS / length(mlm$residuals)
RMSE <- sqrt(MSE)

###############################################################################
# Model 3: Create a random forest using the training data

# RF model
rf <- randomForest(april.temp ~ ., data = train.data, 
                   importance = TRUE, ntree = 200)
  
# Using the importance() function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[, 1], decreasing = TRUE), 
                     optional = T)
names(imp) <- "% Inc MSE"
imp

# Predict and evaluate on the training set
train.pred.forest <- predict(rf, train.data)
RMSE.forest <- sqrt(mean((train.pred.forest - train.data$april.temp)^2))
RMSE.forest

MAE.forest <- mean(abs(train.pred.forest - train.data$april.temp))
MAE.forest

###############################################################################
# Compare the different models on the testing data

# Simple mean:
RMSE.bl <- sqrt(mean((best.guess-test.data$april.temp)^2))
RMSE.bl

MAE.bl <- mean(abs(best.guess-test.data$april.temp))
MAE.bl

# Linear regression:
test.pred.mlm <- predict(mlm1, test.data)

RMSE.mlm <- sqrt(mean((test.pred.mlm - test.data$april.temp)^2))
RMSE.mlm

MAE.mlm <- mean(abs(test.pred.mlm - test.data$april.temp))
MAE.mlm

# Random forest:
test.pred.forest <- predict(rf, test.data)
RMSE.rf <- sqrt(mean((test.pred.forest - test.data$april.temp)^2))
RMSE.rf

MAE.rf <- mean(abs(test.pred.forest - test.data$april.temp))
MAE.rf
