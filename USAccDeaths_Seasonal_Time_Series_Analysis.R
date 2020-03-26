
#############################################################

#USAccDeaths time series analysis

#Lets look at the Data
USAccDeaths
data = USAccDeaths  
plot(data)

# we see some seasonal trend over the period of 12 months or a year

#Lets take the difference to remove the seasonality first and then remove the trend in the data 
# to make it stationary

par(mfrow=c(3,1))
USAccDeaths
data = USAccDeaths
plot(data, col='blue', lwd = 3)
plot(diff(data,12), col='red', lwd = 3)
plot(diff(diff(data,12)), col=' green', lwd = 3)

#Lets save final formated data in acData and look at the ACF and PACF
acData = diff(diff(data,12))
acf(acData)
#Significant adjacent ACF suggests order of MA terms, q < or = 1
#Further, seasonal significant ACF suggests order of season MA term, Q < or = 1

pacf(acData)
#Significant adjacent PACF suggests order of AR terms, p < or = 2
#Further, seasonal significant PACF suggests order of season AR term, P < or = 1


#We selected the below model for simplicity with low AIC
model<- arima(acData, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
model$aic


#Lets look at the forecast from the built model
library(astsa)
model<-sarima(USAccDeaths, 0,1,1,0,1,1,12)
model$ttable
model<- arima(x=USAccDeaths, order = c(0,1,1),
              seasonal = list(order=c(0,1,1), period=12))

library(forecast)
plot(forecast(model))
forecast(model)
# Dark grey represents 80% confidence interval while light gray in the forecast represents 95% CI.

