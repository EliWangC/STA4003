#### Read the data, which contain weekly log return of SP500 index and 10 stocks
load("StockData.Rdata")

# Load needed packages
if(!require(tseries)) install.packages("tseries")
if(!require(forecast)) install.packages("forecast")
library(tseries)    # adf.test
library(forecast)   # auto.arima



# Dataset "StockData.test" will be used for grading. 
# Don't delete the next two comments
#load("StockData.test.Rdata")
#StockData <-StockData.test

n1 <- 208                             # The length of training set
n2 <- 52                              # The length of test set
p <- dim(StockData)[2]                # The first column is the log return for SP500

# Testing periods for recording your forecasts
index.test.Step1 <- (n1+1):(n1+n2)    # From week 209 to week 260
index.test.Step2 <- (n1+2):(n1+n2)    # From week 210 to week 260
index.test.Step3 <- (n1+3):(n1+n2)    # From week 211 to week 260

# Variables for mean squared errors
MSE <- rep(0,3)                       # The mean squared errors for 1-Step, 2-Step, 3-Step forecasts
MSE.Stock <- matrix(rep(0,(p-1)*3),3,(p-1)) # MSE for each stock and h-step forecast

# ------------------------------------------------- #
##### My code for manipulating the data around and see
##### what does the data look like

# Assign a to S&P500 data for simplicity
SP500 = StockData[, 1]

# See what our time series data (the log returns) looks like
plot(SP500, type = "l", xlab = "Week", ylab = "Log Returns (S&P500)")
plot(StockData[, 7], type = "l", xlab = "Week", ylab = "Log Returns (Stock 6)")

# Conduct ADF test for dataset
print(adf.test(SP500))

# Plot ACF and PACF
acf(SP500, main = "ACF Plot (S&P500)")
pacf(SP500, main = "PACF Plot (S&P500)")

# We apply auto-arima model to the dataset
sp500_train = SP500[1:n1]
sp500_test = SP500[(n1+1):(n1+n2)]
sp500fit <- auto.arima(sp500_train, lambda = "auto")
plot(resid(sp500fit), type = "l", xlab = "Week", ylab = "Model Residuals (S&P500)")

sp500_forecast <- forecast(sp500fit, h = 4)
plot(sp500_forecast)
# ------------------------------------------------- #

########## Your code to build the time series model  
start.time <- Sys.time()  # Start measuring the execution time

for (j in 2:p)      # For stock 1 to 10
{
  Stock <- StockData[, j]
  step1.forecasts <- rep(0, n2);
  step2.forecasts <- rep(0, n2);
  step3.forecasts <- rep(0, n2);
  
  for (i in 1:n2)   # For week i in the test set
  {
    # index.train <- i:(i+n1-1)  # Moving training windows
    index.train <- 1:n1
    #### Your code for building model ####
    # Fit the AUTO-ARIMA model that automatically selects the best model parameters 
    arima_model <- auto.arima(Stock[index.train], lambda = "auto")
    arima_forecast <- forecast(arima_model, h = n2)
    
    # Your h-step forecast for Stock j on Week i, h = 1, 2, or 3.
    step1.forecasts[i] <- arima_forecast$mean[i]
    step2.forecasts[i] <- arima_forecast$mean[i+1]
    step3.forecasts[i] <- arima_forecast$mean[i+2]
  }
  
  MSE.Stock[1, (j-1)] <- sum((Stock[index.test.Step1] - step1.forecasts)^2)
  MSE.Stock[2, (j-1)] <- sum((Stock[index.test.Step2] - step2.forecasts[1:(n2-1)])^2)
  MSE.Stock[3, (j-1)] <- sum((Stock[index.test.Step3] - step3.forecasts[1:(n2-2)])^2)
}


######## Your code for Trading stratedy
### Example: My trading strategy is buy and sell each stock with same weight
return.stock <- rowMeans(StockData[(n1+1):(n1+n2),2:p])


# End time measurement and show it
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("Runtime:", time.taken, "\n")

######### Output. Don't change any codes below.
MSE <- rowMeans(MSE.Stock)
Ex.Return <- return.stock - StockData[(n1+1):(n1+n2),1] # excessive returns over SP500
Total.Return <- sum(Ex.Return)
Var.Return <- var(Ex.Return)
Ratio.Return <- mean(Ex.Return)/sqrt(Var.Return)

print(MSE)
print(Total.Return)
print(Var.Return)
print(Ratio.Return)
