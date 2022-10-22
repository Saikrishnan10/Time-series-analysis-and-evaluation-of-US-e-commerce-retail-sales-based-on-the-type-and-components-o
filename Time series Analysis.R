#Forecasting

library(fpp2)

time_data<-read.csv("C:/Users/mural/Downloads/filename.csv")
sum(is.na(time_data))

#Preliminary Assessment
#Import the sales data
dummy3<-ts(time_data$DATE,frequency=80) 
tsales <- ts(time_data$ECOMNSA , start=c(1999, 4), frequency=4) 
tsales
tsales.subset1 <- window(dummy3, start=c(1999, 4), end=c(2003, 4))
tsales.subset2 <- window(dummy3, start=c(2003, 4), end=c(2007, 4))
tsales.subset3 <- window(dummy3, start=c(2011, 4), end=c(2015, 4))
tsales.subset4 <- window(dummy3, start=c(2015, 4), end=c(2019, 4))
tsales.subset5 <- window(dummy3, start=c(2019, 4), end=c(2021, 2))
tsales.subset
#Plot the sales
plot(tsales)
autoplot(tsales,col=dummy3)
autoplot(dummy3)
start(tsales) 
end(tsales)
frequency(tsales)


ma <- function(tsales, n = 87){filter(x, rep(1 / n, n), sides = 2)}
ma
#Overall info library 
library(TSstudio)
ts_info(tsales)

ACF 

#A preliminary assessment of the nature

rollmean(tsales,87)

#Subset the data or split the data
tsales.subset <- window(tsales, start=c(1999, 4), end=c(2008, 3))
tsales.subset
autoplot(tsales.subset)

#Smooting a time series using Moving-average smoothing - ma()
autoplot(tsales, main="Raw Time Series")
autoplot(ma(tsales,1))

autoplot(tsales)+
  autoplot(ma(tsales,4))+
  autolayer(ma(tsales,8))

tsdisplay(tsales,col='red') #getting p - lag - 1 and getting q - moving average -1


# components of the raw time series - Trend  , Seasonal effects , Irregular fluctuations

tsales
plot(tsales)
monthplot(tsales) # Q4 is max
seasonplot(tsales)
subseriesplot(tsales)

par(mfrow=c(1,2))
ggseasonplot(tsales,main = "Seasonplot for E-commerce retail sales in the USA")
ggsubseriesplot(tsales,main = "Subseriesplot for E-commerce retail sales in the USA",col='blue')
#Seasonal - additive method
par(mfrow=c(2,1))

#Seasonal decomposition using decompose() - additive [Not appropriate here]
fit.decadd<-decompose(tsales, type = "additive")
fit.decadd
plot(fit.decadd)

#Seasonal decomposition using decompose() - multiplicative (Applicable plot)
fit.decmult<-decompose(tsales,type = "multiplicative")
fit.decmult
plot(fit.decmult,col='blue')


#Seasonal decomposition using stl() - to deal with missing data
ltsales <- log(tsales)
plot(ltsales, ylab="log(tsales)")
fit.stl <- stl(ltsales, s.window="period")           
plot(fit.stl)
fit.stl$time.series                                 
exp(fit.stl$time.series)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Basic Model

library(fpp2)
tsales
#Comparing with 'Basic' models
par(mfrow=c(2,2))
#mean model
fcast.mean<-meanf(tsales,h=4)
fcast.mean
summary(fcast.mean)
autoplot(fcast.mean,col='red')
#naive model
fcast.naive<-naive(tsales,h=4)
fcast.naive
summary(fcast.naive)
autoplot(fcast.naive,col='blue')

#seasonal naive model
fcast.seasonalnaive<-snaive(tsales,h=8)
fcast.seasonalnaive
summary(fcast.seasonalnaive)
autoplot(fcast.seasonalnaive,col='green')
plot(tsales,main='Normal Time plot')

accuracy(fcast.mean)
accuracy(fcast.naive)
accuracy(fcast.seasonalnaive)


autoplot(ntsales)+autolayer(fitted(ntsales),series = "Fitted")



Accuracy_simple<-rbind(accuracy(fcast.mean),accuracy(fcast.naive),accuracy(fcast.seasonalnaive))
Accuracy_simple_models<-as.data.frame(Accuracy_simple,row.names=c('Mean model Accurary','Naive model Accuracy','Seasonal Naive model Accuracy'))
Accuracy_simple_models
plot(Accuracy_simple_models)
forecast_simple<-rbind(data.frame(fcast.mean),data.frame(fcast.naive),data.frame(fcast.seasonalnaive))
forecast_simple
#---------------------------------------------------------------------------------------------------------------------------------------------------

#Arima

library(fpp2)
library(tseries)

time_data<-read.csv("C:/Users/mural/Downloads/eComm_US (1).csv")
time_data


#Import the sales data
time_data
tsales <- ts(time_data$ECOMNSA , start=c(1999, 4), frequency=4) 
tsales


# converting to Stationary series from Non stationary series which is tsales


#Assess stationarity of the differenced series [should not directly do this]
adf.test(tsales,k=4) # gives you p value which is not significant

#Performing differencing checking differencing 
#Check the order of differencing required
ndiffs(tsales)  # which is one and the default is one


#Plot the differenced Time Series
dtsales <- diff(tsales)
dtsales_2 <- diff(tsales,differences = 2)


#ADF test
adf.test(tsales,k=4) # gives you p value which is reduced non significant value after performing differencing - diff()
adf.test(dtsales,k=4) # gives you p value which is reduced non significant value after performing differencing - diff()
adf.test(dtsales_2,k=4) # gives you p value which is reduced non significant value after performing differencing twice - diff()

#Plotting
autoplot(tsales) # Before differencing
autoplot(dtsales) # After differencing with 1
autoplot(dtsales_2) # After differencing with 2

plot(tsales)
plot(dtsales_2,col='red')

plot(Acf(tsales))
Acf(tsales,main = 'ACF for NON stationary object',col='red')
Pacf(tsales,main ='PACF for NON stationary object',col='red')
Acf(dtsales_2,main ='ACF for Stationary object ',col='Green')
Pacf(dtsales_2,main ='PACF for Stationary object',col='Green')
plot(Pacf(tsales))
plot(,col='blue',)
plot(Pacf(dtsales_2))
#Arima p,d,q -> d = 2
autoplot(dtsales_2)

#ACF/PACF plots. Choosing p and q
Acf(dtsales_2) # getting p - lag - 1
Pacf(dtsales_2) # getting q - moving average -1

#Fitting an ARIMA model (p,d,q)
fit_sales <- arima(tsales, order=c(1,2,1))
fit_sales #Arima model
plot(fit_sales)
par(mfrow=c(1,1))
#Evaluating Model Fit
qqnorm(fit_sales$residuals)
qqline(fit_sales$residuals)
Box.test(fit_sales$residuals, type="Ljung-Box")
checkresiduals(fit_sales)
accuracy(fit_sales)

#Forecasting with the fitted model
forecast(fit_sales, 4)
autoplot(forecast(fit_sales), xlab="Year", ylab="Annual Flow")+
  autolayer(residuals(fit_sales),series = "Residual")+autolayer(fitted(fit_sales),series = "Fitted")
AIC(fit_sales)


fit_sales_auto <- fit_sales

#auto ARIMA function
plot(tsales)
fit_sales_auto <- auto.arima(tsales)
fit_sales_auto
fit_sales

AIC(fit_sales)


#lag plot
library(TSstudio)
ts_lags(tsales)

#-------------------------------------------------------------------------------------------------------------------------------------------------

#SARIMA model


library(fpp2)
tsales
plot(tsales)
plot(decompose(tsales))

#Use auto.arima function
#auto.arima(euretail)
auto.arima(tsales)

s_tsalesfit<-Arima(tsales,order = c(1,1,0),seasonal = c(1,1,0)) #non seasonal + seasonal

#checkresiduals
checkresiduals(s_tsalesfit)

s_tsalesfit %>%forecast(h=12) %>% autoplot()
tsales
forecast(s_tsalesfit,h=3)
forecast(tsales)
s_tsalesfit
Box.test(s_tsalesfit$residuals,type = "Ljung-Box")
accuracy(s_tsalesfit)
par(mfrow=c(2,1))
qqnorm(s_tsalesfit$residuals)
qqline(s_tsalesfit$residuals)

autoplot(forecast(s_tsalesfit), xlab="Year", ylab="Annual Flow")+
  autolayer(residuals(s_tsalesfit),series = "Residual")+autolayer(fitted(s_tsalesfit),series = "Fitted")

#ARIMA FORECAST
forecast(fit_sales, 3)
#SARIMA FORECAST
forecast(s_tsalesfit, 3)
#---------------------------------------------------------------------------------------------------------------------------------------------------------
#Exponential smoothing

library(fpp2)

# nhtemp_Mean annual temperature in New Haven 1912-1971
tsales
plot(tsales)

# 1) fit simple exponential smoothing model (ses function)

ntsales<-ses(tsales, h=2) # Forecast for next 2 yrs
ntsales
fcast.naive
ntsales$model # alpha - optimal alpha which is 0.182
#ALPHA is the smoothing parameter that defines the weighting and should be greater than 0 and less than 1 (automated)

#AIC, corrected AIC and BIC

# Alpha is based on AIC and not RMSE

round(accuracy(ntsales),2) # Accuracy measures

autoplot(ntsales)+
  autolayer(residuals(ntsales),series = "Residual")+autolayer(fitted(ntsales),series = "Fitted")
ntsales
autoplot(fcast.naive,series = "Naive")

#  Using ETS A- additive , no trend/additive/mult , no trend/additive/mult
#ETS(A,N,N)
#ets model=ZZZ
nhfit2<-ets(tsales, model = "ANN") #do not do
nhfit2
nhfit2_auto<-ets(tsales, model = "ZZZ") #auto mated use this
nhfit2_auto$states
plot(nhfit2_auto)
autoplot(nhfit2_auto)+
  autolayer(residuals(nhfit2_auto),series = "Residual")+autolayer(fitted(nhfit2_auto),series = "Fitted")

#3) Fit Holt's linear trend model -holt()
fhfit <- window(tsales)
fhfit
plot(fhfit)
fhfit_fit<-holt(fhfit, h=5)
fhfit_fit$method
fhfit_fit$model
autoplot(fhfit_fit)+
  autolayer(residuals(fhfit_fit),series = "Residual")+autolayer(fitted(fhfit_fit),series = "Fitted")
#Alpha exponential decay for level
#Beta exp decay for slope
?ets()
#4) Fit Holt-Winters model- hw()
fhfit_holt <-hw(tsales, 4)
fhfit_holt$model
accuracy(fhfit_holt)
autoplot(fhfit_holt)+
  autolayer(residuals(fhfit_holt),series = "Residual")+autolayer(fitted(fhfit_holt),series = "Fitted")

Accuracy_expo<-rbind(accuracy(ntsales),accuracy(fhfit_fit),accuracy(fhfit_holt),accuracy(nhfit2_auto))
AIC(nhfit2_auto)
Accuracy_expo_models<-as.data.frame(Accuracy_expo,row.names=c('SES','HLT','HW','ETS'))
Accuracy_expo_models

#---------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,1))
seasonplot(tsales,
           main = "Seasonplot", 
           ylab="Sales-value in Billions of dollars",
           xlab='Time [Quarters]',
           year.labels = TRUE,
           col = 1:20,
           pch = 10)
monthplot(tsales,
           main = "Subseriesplot", 
           ylab="Sales-value in Billions of dollars",
           xlab='Time [Quarters]',
           year.labels = TRUE,
           col = 'red',
          col.base='blue',
           pch = 10)



autoplot(tsales, 
     main = "E-commerce retail sales in the USA", 
     ylab="Values in Billions of dollars",
     xlab='Time [Quarterly date]',col = 1:87)
