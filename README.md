- 👋 Hi, I’m @MostafaNazzari
- 👀 I’m interested in learning Data science
- 📫 How to reach me mostafanazzari@ut.ac.ir

<!---
For my first project at GitHub, I recently replicated the Forecasting UK consumer price index using Box-Jenkins ARIMA models, with R-project.
--->

# load urca package 
library(urca)
# load forecast package 
library(forecast)

library(tidyverse)

library(readxl)
# Set directory
# Check you have the right file path!!
filepath <- 'C:/Users/Admin/OneDrive/Desktop/dr mehrara/HW'
setwd(filepath)

# Export figures? 0 for NO, 1 for YES
exp.plt <- 1

# Set seed
set.seed(123)




# load my unit root tests
source("urtests.r")

# unit root testing
ur.test

# UK consumer price index
mydata <- read_excel("mydata.xlsx", col_types = c("date", "numeric"))

# log CPI
mydata$lk <- ts(log(mydata$k),start=1960,frequency=1)

# plot log k 
plot(mydata$lk,type="l",main="Log of UK CPI")

# plot ACF and PACF log k
par(mfrow=c(1,2))
Acf(mydata$lk,lag.max=24,ann=FALSE)
Pacf(mydata$lk, lag.max = 12)

# first diff CPI
diff1lk <- diff(mydata$lk,differences=1)
mydata$diff1lk <- c( NA, head=diff1lk-1)
mydata$diff1lk <- ts(mydata$diff1lk, start = 1961, frequency = 1)

# plot first diffrence log k
layout(1)
plot(mydata$diff1lk,type="l",main="First difference of log of UKCPI")

# plot ACF and PACF first diff log k
par(mfrow=c(1,2))
Acf(diff1lk,lag.max=12,ann=FALSE)
Pacf(diff1lk, lag.max = 12)

# second diffrence log CPI
diff2lk <- diff(mydata$lk,differences=2)
mydata$diff2lk <- c(NA, NA, head=diff2lk-2)
mydata$diff2lk <- ts(mydata$diff2lk, start = 1962 , frequency= 1)

# plot second diff log k
layout(1)
plot(mydata$diff2lk,type="l",main="Second difference of log of UKCPI")
abline(h=-2)

# plot ACF and PACF second diff log k
par(mfrow=c(1,2))
Acf(mydata$diff2lk,lag.max=12,ann=FALSE)
Pacf(mydata$diff2lk, lag.max = 12)


#######################################################################

# TABLE 1:9



# ADF test for unit root in log CPI and with trend & constant
# ADF test for unit root in first log CPI and with trend & constant
# ADF test for unit root in second log CPI and with trend & constant



# test for unit root in log k Levels-intercept
test2 <- ur.test(mydata$lk,trend="c",method="adf.ols",kmax=12)

print.ur.test(test2)  #NON STATIONARY

# test for unit root in log k Levels-trend & intercept
test3 <- ur.test(mydata$lk,trend="ct",method="adf.ols",kmax=12)

print.ur.test(test3)   #NON STATIONARY

# test for unit root in log k with out Levels-trend & intercept
test1 <- ur.df(mydata$lk,type="none",lags=12)
test1

qnorm(c(.01,.05,.1)/2)

# test for unit root in first diff log k Levels-intercept
test4 <- ur.test(diff1lk,trend="c",method="adf.ols",kmax=12)

print.ur.test(test4)   #NON STATIONARY

# test for unit root in first diff log k  Levels-trend & intercept
test5 <- ur.test(diff1lk,trend="ct",method="adf.ols",kmax=12)

print.ur.test(test5)   #NON STATIONARY

 
# test for unit root in first diff log k with out Levels-trend & intercept
test6 <- ur.df(diff1lk,type="none",lags=0)
test6
qnorm(c(.01,.05,.1)/2)


# test for unit root in second diffrence log k Levels-intercept
test7 <- ur.test(diff2lk,trend="c",method="adf.ols",kmax=12)
print.ur.test(test7)

# test for unit root in second diffrence log k Levels-trend & intercept
test8 <- ur.test(diff2lk,trend="ct",method="adf.gls",kmax=12)


print.ur.test(test8) # STATIONARY

# test for unit root in first diff log k with out Levels-trend & intercept
test9 <- ur.df(diff2lk,type="none",lags=0)
test9
qnorm(c(.01,.05,.1)/2)


# WE USE SECOND DIFFRENCE LOG CPI LEVELS-TREND & INTERCEPT




##############################################################################

# Evaluation of ARIMA models

#estimate ARIMA(1,2,1)
# (use functions in the forecast package)
model121 <- Arima(mydata$lk,order=c(1,2,1),method="ML")
model121

#estimate ARIMA(1,2,0)
# (use functions in the forecast package)
model120 <- Arima(mydata$lk,order=c(1,2,0),method="ML")
model120

#estimate ARIMA(0,2,1)
# (use functions in the forecast package)
model021 <- Arima(mydata$lk,order=c(0,2,1),method="ML")
model021

#estimate ARIMA(2,2,1)
# (use functions in the forecast package)
model221 <- Arima(mydata$lk,order=c(2,2,1),method="ML")
model221

#estimate ARIMA(1,2,2)
# (use functions in the forecast package)
model122 <- Arima(mydata$lk,order=c(1,2,2),method="ML")
model122

# set end of sample for initial estimations
n.obs <- length(mydata$lk)
n.end <- 36

# 1-step ahead recursive predictions

# set matrix for storage
pred <- matrix(rep(0,180),36,5)

# start loop
for(i in 1:36){
  x <- mydata$lk[1:n.end+i-1]
  
  model121.tmp <- Arima(x,order=c(1,2,1),method="ML")
  pred[i,1] <- forecast(model121.tmp,h=1)$mean[1]
  
  model120.tmp <- Arima(x,order=c(1,2,0),method="ML")
  pred[i,2] <- forecast(model120.tmp,h=1)$mean[1]
  
  model021.tmp <- Arima(x,order=c(0,2,1),method="ML")
  pred[i,3] <- forecast(model021.tmp,h=1)$mean[1]
  
  model221.tmp <- Arima(x,order=c(2,2,1),method="ML")
  pred[i,4] <- forecast(model221.tmp,h=1)$mean[1]
  
  model122.tmp <- Arima(x,order=c(1,2,2),method="ML")
  pred[i,5] <- forecast(model122.tmp,h=1)$mean[1]
}


# get summary statistics
# (use functions in the forecast package) 
accuracy(pred[,1],mydata$lk[(n.end+1):n.obs])
accuracy(pred[,2],mydata$lk[(n.end+1):n.obs])
accuracy(pred[,3],mydata$diff2lk[(n.end+1):n.obs])
accuracy(pred[,4],mydata$diff2lk[(n.end+1):n.obs])
accuracy(pred[,5],mydata$diff2lk[(n.end+1):n.obs])


# TABLE 10
#AIC, ME, MAE, RMSE, MAPE FOR ARIMA MODELS

summary(model121)
summary(model120)
summary(model021)
summary(model221)
summary(model122)

##############################################################

# TABLE 11:13


#ADF Tests of the Residuals of the ARIMA (1, 2, 1) Model

# adf tests of residuals
resimodel121 <- ts(model121$residuals)
res1imodel121 <-  ts(resimodel121[-1],start=1)
res2imodel121 <-  ts(res1imodel121[-1],start=1)

#  Levels-intercept
test7 <- ur.test(res2imodel121,trend="c",method="adf.gls",kmax=12)

print.ur.test(test7)  # STATIONARY

#  Levels-trend & intercept
test8 <- ur.test(res2imodel121,trend="ct",method="adf.gls",kmax=12)

print.ur.test(test8)  # STATIONARY

#  with out Levels-trend & intercep
test9 <- ur.df(res2imodel121,type="none",lags=0)
test9
qnorm(c(.01,.05,.1)/2)

################################################################################

# TABLE 15

# ARIMA (1,2,1) MODEL
# COEF & sd for model

summary(model121) 


#############################################################################
# figure 2

# forecasting ARIMA(1,2,1) graph

# set predictions as time series object
pred.ts <- ts(pred,start=c(1997,1), end = c(2025,1),frequency=1)

# plot actual unemployment rate and 1-step ahead forecasts
layout(1)
plot(mydata$lk,ann=FALSE, xlim=c(1980,2025))

lines(pred.ts[,1],col="blue")   # ARIMA(1,2,1)

exp(pred.ts[,1])
