#########################################
#install the requried package
install.packages('datasets')

#Lets have a look  at the data
plot(BJsales)

# There is upward and down ward trend. Time series is not stationary

#Plot the diff as there is a trend in the above plot making the data non stationary
plot(diff(BJsales))

#Still non stationery, take diff again and plot
plot(diff(diff(BJsales)))

#look at the PACF
pacf(diff(diff(BJsales)))

#Ignoring less significant lags, order of AR term can be 0,1,2 or 3

#Look at the ACF
acf(diff(diff(BJsales)))

#Ignoring less significant lags, order of MA term can be 0 or 1

#Lets look at the AIC and SSE for different combination of model parameters:
d=2 # since we had to take difference two times
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=6){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

# we see model ARIMA(0,2,1) gives lowest AIC value and model ARIMA(1,2,1) gives lowest SSE.
# To make less complex model, ARIMA(0,2,1) is selected

# Lets look at the model parameters
model<-arima(BJsales, order=c(0,2,1))
model

#Lets look at the residuals
par(mfrow=c(2,2))
plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)

#Using tha bove model, lets forecast the time series
install.packages('forecast')
library(forecast)
forecast(model)
plot(forecast(model))







