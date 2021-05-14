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
# cat("Forecasting Runtime:", forecast_runtime, "\n")

##### Your code for Trading stratedy #####
#### Equally weighted portfolio #### 
# Example: My trading strategy is buy and sell each stock with same weight
# return.stock <- rowMeans(StockData[(n1+1):(n1+n2), 2:p])

#### Maximum Sharpe ratio portfolio (MSRP) ####

train_log_rtn = StockData[1:n1, 2:p]
mu = colMeans(train_log_rtn)
sigma = cov(train_log_rtn)

# Define the nonconvex objective function (returns the value of sharpe ratio)
fn_SR <- function(w) {
  return (as.numeric(t(w) %*% mu / sqrt(t(w) %*% sigma %*% w))) 
}

#### MSRP via general-purpose nonlinear solver #### 
# Initial point
w0 <- rep(1/N, N)
# Start nonlinear programming
res <- nloptr::slsqp(w0, fn_SR,
                     lower = rep(0, N),  # w >= 0
                     heq = function(w) return(sum(w) - 1))    # sum(w) = 1
w_nonlinear_solver <- res$par            # Optimized weight by nonlinear solver
res

#### MSRP via bisection #### 
sigma_12 <- chol(sigma)                     # Square-root of matrix sigma
max(abs(t(sigma_12) %*% sigma_12 - sigma))  # Sanity check

# Create function for MVP
SOCP_bisection <- function(t) {
  w <- Variable(nrow(sigma))
  prob <- Problem(Maximize(0),
                  constraints = list(t*cvxr_norm(sigma_12 %*% w, 2) <= t(mu) %*% w,
                                     sum(w) == 1,
                                     w >= 0))
  result <- solve(prob)
  return(list("status" = result$status, "w" = as.vector(result$getValue(w))))
}

# Run the bisection algorithm
t_lb <- 0   # for sure the problem is feasible in this case
t_ub <- 10  # a tighter upper bound coud be chose, but a Sharpe ratio of 10 surely cannot be achieved
while (t_ub - t_lb > 1e-6) 
{
  t <- (t_ub + t_lb)/2  # midpoint
  if (SOCP_bisection(t)$status == "infeasible")
    t_ub <- t
  else
    t_lb <- t
}
w_bisection <- SOCP_bisection(t_lb)$w

# comparison between two solutions
round(cbind(w_nonlinear_solver, w_bisection), digits = 3)

c("nonlinear_solver" = fn_SR(w_nonlinear_solver), 
  "bisection"        = fn_SR(w_bisection))

# Calculate the log return of the 10 stocks over week 209 to 260
return.stock <- c()
for (j in (n1+1):(n1+n2)) 
{
  return.stock_j <- 0      # Initialize the portfolio return of week j
  for (i in 1:N) 
  {
    # (For week j) Log return of portfolio += weight of stock i * corresponding return
    # return.stock_j <- return.stock_j + w_nonlinear_solver[i] * StockData[j, i+1]  
    return.stock_j <- return.stock_j + w_bisection[i] * StockData[j, i+1]  
  }
  return.stock <- c(return.stock, return.stock_j)
}


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
