# Time series analysis of United States E-commerce retail sales

The time series is performed using R-studio and the initial step is to convert the data into a time-series object. The pattern of the time plot is important as it helps to understand the past behaviour using which we can expect such similarities in future, which can be identified using forecasting method.

Based on the type of forecasting methodology, the time series models are classified into: 

1.	Simple (Three Simple Models): Average Method, Naïve Method, and Seasonal Naive Method
2.	Exponential smoothing: Simple Exponential Smoothing, Holt’s linear Trend Method and Holt-Winters Method
3.	Autoregressive integrated moving average [ARIMA]
4.	Seasonal Arima [SARIMA]. 

Each time series models have various methods which will be analysed to find the suitable model
On comparing the time series models Simple model, Exponential smoothing and ARIMA and SARIMA, we can say that the ARIMA/SARIMA models provide better results both in terms of accuracy. In comparison to exponential smoothing method, the ARIMA/SARIMA model can provide stable forecast for longer periods as well. 
As a result, **SARIMA** is the best model in terms of accuracy, for which we will use the RMSE value as a criterion, as well as efficiency.

