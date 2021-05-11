#### Read the data, which contain weekly log return of SP500 index and 10 stocks
load("StockData.Rdata")

# Dataset "StockData.test" will be used for grading. 
# Don't delete the next two comments
#load("StockData.test.Rdata")
#StockData <-StockData.test

n1 <- 208 #the length of train set
n2 <- 52  # the length of test set
p <- dim(StockData)[2]  # The first column is the log return for SP500

# Testing periods for recording your forecasts
index.test.Step1 <- (n1+1):(n1+n2)
index.test.Step2 <- (n1+2):(n1+n2)
index.test.Step3 <- (n1+3):(n1+n2)

# Variables for mean squared errors
MSE <- rep(0,3) # The mean squared errors for 1-Step, 2-Step, 3-Step forecasts
MSE.Stock <- matrix(rep(0,(p-1)*3),3,(p-1)) # MSE for each stock

########## Your code to build the time series model  


for (j in 2:p)
{
  Stock  <- StockData[,j]
  step1.forecasts <- rep(0,n2);
  step2.forecasts <- rep(0,n2);
  step3.forecasts <- rep(0,n2);
  
  for (i in 1:n2)
  {
    index.train <- i:(i+n1-1)  # Moving training windows
    model <- # Your code for building model
      
    step1.forecasts[i] <- # Your 1-step forecast for Stock j on Week i 
    step2.forecasts[i] <- # Your 2-step forecast for Stock j on Week i
    step3.forecasts[i] <- # Your 3-step forecast for Stock j on Week i
  }
  
  MSE.Stock[1,(j-1)] <- sum(( Stock[index.test.Step1]- step1.forecasts)^2)
  MSE.Stock[2,(j-1)] <- sum(( Stock[index.test.Step2]- step2.forecasts[1:(n2-1)])^2)
  MSE.Stock[3,(j-1)] <- sum(( Stock[index.test.Step3]- step3.forecasts[1:(n2-2)])^2)
}


######## Your code for Trading stratedy
### Example: My trading strategy is buy and sell each stock with same weight
return.stock <- rowMeans(StockData[(n1+1):(n1+n2),2:p])



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
