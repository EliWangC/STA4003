#### Read the data, which contain weekly log return of SP500 index and 10 stocks
load("StockData.Rdata")

# Load needed packages
if(!require(tseries)) install.packages("tseries")
if(!require(forecast)) install.packages("forecast")
if(!require(nloptr)) install.packages("nloptr")
if(!require(CVXR)) install.packages("CVXR")
library(tseries)    # adf.test
library(forecast)   # auto.arima
library(nloptr)     # general-purpose nonlinear solver
library(CVXR)       # define the inner solver based on an SOCP solver 


# Dataset "StockData.test" will be used for grading. 
# Don't delete the next two comments
#load("StockData.test.Rdata")
#StockData <-StockData.test

n1 <- 208                             # The length of training set
n2 <- 52                              # The length of test set
p <- dim(StockData)[2]                # The first column is the log return for SP500
N <- p - 1                            # The amout of individual stocks

# Testing periods for recording your forecasts
index.test.Step1 <- (n1+1):(n1+n2)    # From week 209 to week 260
index.test.Step2 <- (n1+2):(n1+n2)    # From week 210 to week 260
index.test.Step3 <- (n1+3):(n1+n2)    # From week 211 to week 260

# Variables for mean squared errors
MSE <- rep(0,3)                       # The mean squared errors for 1-Step, 2-Step, 3-Step forecasts
MSE.Stock <- matrix(rep(0,(p-1)*3),3,(p-1)) # MSE for each stock and h-step forecast


########## Your code to build the time series model  
# Start measuring the execution time
forecast_start <- Sys.time()  

for (j in 2:p)      # For stock 1 to 10
{
  Stock <- StockData[, j]
  step1.forecasts <- rep(0, n2);
  step2.forecasts <- rep(0, n2);
  step3.forecasts <- rep(0, n2);
  
  for (i in 1:n2)   # For week i in the test set
  {
    # Moving training windows
    index.train <- i:(i+n1-1) 
    train_set <- Stock[index.train]
    predictions <- c()
    
    ##### Your code for building model #####
    ### Recursive Multi-step Forecast ###
    for (h in 1:3) 
    { 
      # Fit the AUTO-ARIMA model that automatically selects the best model parameters 
      arima_model <- auto.arima(train_set, lambda = "auto")
      arima_forecast <- forecast(arima_model, h = 1)
      onestep_forecast <- arima_forecast$mean[1]             # Get the predicted value
      predictions <- c(predictions, onestep_forecast)        # Store the prediction value
      train_set <- c(train_set, onestep_forecast)            # Add the predicted value into the training set for recursive forecast
    }
    
    # Your h-step forecast for Stock j on Week i, h = 1, 2, or 3.
    step1.forecasts[i] <- predictions[1]
    step2.forecasts[i] <- predictions[2]
    step3.forecasts[i] <- predictions[3]
  }
  
  MSE.Stock[1, (j-1)] <- mean((Stock[index.test.Step1] - step1.forecasts)^2)
  MSE.Stock[2, (j-1)] <- mean((Stock[index.test.Step2] - step2.forecasts[1:(n2-1)])^2)
  MSE.Stock[3, (j-1)] <- mean((Stock[index.test.Step3] - step3.forecasts[1:(n2-2)])^2)
}

# End time measurement and show it
forecast_end <- Sys.time()
forecast_runtime <- forecast_end - forecast_start



##### Your code for Trading stratedy #####
portfolio_start <- Sys.time()  

#### Equally weighted portfolio #### 
# return.stock <- rowMeans(StockData[(n1+1):(n1+n2), 2:p])

#### Maximum Sharpe ratio portfolio (MSRP) ####

return.stock <- c()

#### MSRP via general-purpose nonlinear solver #### 

# Define the nonconvex objective function (returns the value of sharpe ratio)
fn_SR <- function(w) {
  return (as.numeric(t(w) %*% mu / sqrt(t(w) %*% sigma %*% w))) 
}

# Initial point
w0 <- rep(1/N, N)

# Iteratively get weight vector for week t + 1 using observations up to week t
for (j in (n1+1):(n1+n2)) 
{
  return.stock_j <- 0         # Initialize the portfolio return of week j
  
  index_train_po <- 1:(j-1)   # Index of training set for portfolio construction
  train_log_rtn = StockData[index_train_po, 2:p]
  mu = colMeans(train_log_rtn)
  sigma = cov(train_log_rtn)
  
  for (i in 1:N) 
  {
    # Start nonlinear programming
    res <- nloptr::slsqp(w0, fn_SR, lower = rep(0, N),            # w >= 0
                         heq = function(w) return(sum(w) - 1))    # sum(w) = 1
    w_nonlinear_solver <- res$par            # Optimized weight by nonlinear solver
    # res
    
    # (For week j) Log return of portfolio += weight of stock i * corresponding return
    return.stock_j <- return.stock_j + w_nonlinear_solver[i] * StockData[j, i+1]
  }
  
  return.stock <- c(return.stock, return.stock_j)
}


# End time measurement and show it
portfolio_end <- Sys.time()
portfolio_runtime <- portfolio_end - portfolio_start


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

cat("Forecasting Runtime:", forecast_runtime, "\n")
cat("Portfolio Optimization Runtime:", portfolio_runtime, "\n")
