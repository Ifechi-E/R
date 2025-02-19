### Load Libraries ##################
library('forecast')
library('tseries')
library('readr')
library('vars')
library(readxl)
library(dplyr)



############################################################
### Setup #############################
setwd('/Users/ifechiekekwe/Documents/Uni work/4th Year/Econ/Econometrics/Topic 3 Lab/Assignment 3 (Forecasting Topic)-20241120')

data <- read_excel("EC413Data.xlsx")  
usgdpg <- data[,2]
usinfl <- data[,3]
cagdpg <- data[,4]
cainfl <- data[,5]

vardata <- data[,2:5]
candata <- data[,4:5]
usdata <- data[,2:4]

usgdpdata <- cbind(cagdpg,usgdpg)
usinfdata <- cbind(cagdpg,usinfl)
combo <- cbind(candata,usinfl)


CANGDPG <- ts(data$CANGDPGrowth, frequency = 4, start = c(1973, 2))
CANINFL <- ts(data$CANInflation, frequency = 4, start = c(1973, 2))
USGDPG <- ts(data$USAGDPGrowth, frequency = 4, start = c(1973,2))
USINFL <- ts(data$USAInflation, frequency = 4, start = c(1973,2))

### Preliminary #######################
timedata <- seq(from=1973.5,to=2020,by=1/4) 
#T0 <- which(timedata == 1973.5 + (round(length(timedata)*0.7) / 4)) #Adjust the train test split here      
T0 <- which(timedata == 2010) 
Tend <- which(timedata == 2020)               
bigT <- length(seq(T0,Tend,by=1))
hor <- 4  # 4 quarters into the future (do a shorter 1 step ahead? and compare)                            
store_forecast = matrix(0,bigT,hor)       
store_actual = matrix(0,bigT,hor)    

############################################################
### Intuition for lag max lag lengths ####
### Canada Data  ######
ts.plot(CANGDPG ,CANINFL, col= 1:2, ylab= "Idk Yet",xlab = "Rates", main = 'Canadian GDP Growth & Inflation Plot')

CANGDPGACF <- acf(CANGDPG, plot= FALSE)
CANGDPGACF$lag <- CANGDPGACF$lag *4
plot(CANGDPGACF, xlab = "Lags", ylab= "correlations", main ="CANGDPG Auto Correlations Plot", xlim= c(1,22))   

CANINFLACF <- acf(CANINFL, plot= FALSE) 
CANINFLACF$lag <- CANINFLACF$lag *4
plot(CANINFLACF, xlab = "Lags", ylab= "correlations", main ="CANINFL Auto Correlations Plot", xlim= c(1,22))   

### US Data #########
ts.plot(USGDPG ,USINFL, col= 1:2, ylab= "Idk Yet",xlab = "Year", main = 'United States GDP Growth & Inflation Plot')

USGDPGACF <- acf(USGDPG, plot= FALSE)
USGDPGACF$lag <- USGDPGACF$lag *4
plot(USGDPGACF, xlab = "Lags", ylab= "correlations", main ="USGDPG Auto Correlations Plot", xlim= c(1,22))   

USINFLACF <- acf(USINFL, plot= FALSE)
USINFLACF$lag <- USINFLACF$lag *4
plot(USINFLACF, xlab = "Lags", ylab= "correlations", main ="USINFL Auto Correlations Plot", xlim= c(1,22))   




############################################################
### The Benchmark: Iterated AR(4) i.e. 1yr Model, rolling window #######
for (i in 1:bigT) {                                           
  data2 <- data[i:(T0-2+i),4]                                  
  fmod <- ar.ols(data2,order.max = 4,demean = F,intercept = T) 
  fcast <- forecast(fmod,h=hor)                                
  store_forecast[i,] <- as.numeric(fcast$mean)                 
  iii <- T0-2+i+hor                                            
  ii <- T0-1+i
  store_actual[i,] <- t(data[ii:iii,4])
}
store_actual <- na.omit(store_actual) 

BenchmarkSFE <-(store_actual - store_forecast[1:nrow(store_actual)])^2

############################################################

#######  Auto Regressive Models ########################
### 1) The ACF Informed: Iterated AR Model, rolling window, AR(2) #######

for (i in 1:bigT) {                                           
  data2 <- data[i:(T0-2+i),4]                                  
  fmod <- ar.ols(data2,order.max = 2,demean = F,intercept = T) 
  fcast <- forecast(fmod,h=hor)                                
  store_forecast[i,] <- as.numeric(fcast$mean)                 
  iii <- T0-2+i+hor                                            
  ii <- T0-1+i
  store_actual[i,] <- t(data[ii:iii,4])    
}
store_actual <- na.omit(store_actual) 

### 2) The Over fitted: Iterated AR Model, rolling window, AR(12) #######
for (i in 1:bigT) {                                           
  data2 <- data[i:(T0-2+i),4]                                  
  fmod <- ar.ols(data2,order.max = 12,demean = F,intercept = T) 
  fcast <- forecast(fmod,h=hor)                                
  store_forecast[i,] <- as.numeric(fcast$mean)                 
  iii <- T0-2+i+hor                                            
  ii <- T0-1+i
  store_actual[i,] <- t(data[ii:iii,4])    
}
store_actual <- na.omit(store_actual) 


### 3) The ACF Informed Expanding: Iterated AR Model, Expanding window, AR(2) #######
for (i in 1:bigT) {                                           
  data2 <- data[1:(T0-2+i),4]                                  
  fmod <- ar.ols(data2,order.max = 2,demean = F,intercept = T) 
  fcast <- forecast(fmod,h=hor)                                
  store_forecast[i,] <- as.numeric(fcast$mean)                 
  iii <- T0-2+i+hor                                            
  ii <- T0-1+i
  store_actual[i,] <- t(data[ii:iii,4])    
}
store_actual <- na.omit(store_actual) 

### 4) The Over fitted Expanding: Iterated AR Model, Expanding window, AR(12) #######
for (i in 1:bigT) {                                           
  data2 <- data[1:(T0-2+i),4]                                  
  fmod <- ar.ols(data2,order.max = 12,demean = F,intercept = T) 
  fcast <- forecast(fmod,h=hor)                                
  store_forecast[i,] <- as.numeric(fcast$mean)                 
  iii <- T0-2+i+hor                                            
  ii <- T0-1+i
  store_actual[i,] <- t(data[ii:iii,4])    
}
store_actual <- na.omit(store_actual) 




#############################################################################
####### Vectorised Auto Regressive Models ###################################
### 5) Caninfl: Iterated VAR(4) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.candata <- as.ts(candata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.candata, p=4, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                      # based on the beta estimates, forecast for the next 6 periods
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   # store these in the forecast matrix
  iii <- T0-2+i+hor                                               # a counter
  ii <- T0+i-1                                                    # a counter
  store_actual[i,] <- t(data[ii:iii,4])                            #getting and storing the actual values so they can be compared to forecast
}
store_actual <- na.omit(store_actual)

### 6) Caninfl ACF informed: Iterated VAR(2) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.candata <- as.ts(candata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.candata, p=2, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                          
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)
### 7) USGDPG: Iterated VAR(4) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.usgdpgdata <- as.ts(usgdpdata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.usgdpgdata, p=4, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                      # based on the beta estimates, forecast for the next 6 periods
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   # store these in the forecast matrix
  iii <- T0-2+i+hor                                               # a counter
  ii <- T0+i-1                                                    # a counter
  store_actual[i,] <- t(data[ii:iii,4])                            #getting and storing the actual values so they can be compared to forecast
}
store_actual <- na.omit(store_actual)

### 8) USGDPG: ACF informed: Iterated VAR(2) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.usgdpgdata <- as.ts(usgdpdata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.usgdpgdata, p=2, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                        
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)
### 9) USinfl: Iterated VAR(4) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.usinfldata <- as.ts(usinfdata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.usinfldata, p=4, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                      # based on the beta estimates, forecast for the next 6 periods
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   # store these in the forecast matrix
  iii <- T0-2+i+hor                                               # a counter
  ii <- T0+i-1                                                    # a counter
  store_actual[i,] <- t(data[ii:iii,4])                            #getting and storing the actual values so they can be compared to forecast
}
store_actual <- na.omit(store_actual)

### 10) USinfl ACF informed: Iterated VAR(2) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.usinfldata <- as.ts(usinfdata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.usinfldata, p=2, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                         
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)



### 11) Combined ACF informed: Iterated VAR(2) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.combodata <- as.ts(combo[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.combodata, p=2, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                         
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)

### 12) ALL ACF informed: Iterated VAR(2) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.vardata <- as.ts(vardata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.vardata, p=2, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                         
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)

### 13) Combined : Iterated VAR(4) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.combodata <- as.ts(combo[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.combodata, p=4, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                         
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)


### 14) ALL ACF informed: Iterated VAR(4) Model, rolling window #######
for (i in 1:bigT) {                                                #for period in the testing (Forecast evaluation sample)         
  ts.vardata <- as.ts(vardata[i:(T0-2+i),])                       # expand the window by 1 (expanding window as start stays fixed (at 1))   
  fmod <- VAR(ts.vardata, p=4, type="const")                      # run a AR(6) model on this data to estimate the betas on the lags
  fcast<-forecast(fmod,h=hor)                                         
  store_forecast[i,] <- data.matrix(fcast$forecast$CANGDPGrowth$mean)   
  iii <- T0-2+i+hor                                               
  ii <- T0+i-1                                                   
  store_actual[i,] <- t(data[ii:iii,4])                           
}
store_actual <- na.omit(store_actual)

#############################################################################
############# FORECAST COMPARISON ################
MSFE <- apply((store_actual - store_forecast[1:nrow(store_actual)])^2,2,mean)      
RMSFE <- MSFE**0.5
RMSFE
# Mean Squared Forecast Error
#MSFE <- apply((store_actual - store_forecast[1:nrow(store_actual)])^2,2,mean)      
#MSFE
# Mean Absolute Error
#MAFE <- apply(abs(store_actual - store_forecast[1:nrow(store_actual)]),2,mean)   
#MAFE

#############################################################################
###### CSSED ###############
SFE <- (store_actual - store_forecast[1:nrow(store_actual)])^2
SED <- BenchmarkSFE - SFE
CSSED <- apply(SED,2,cumsum)
View(CSSED)
