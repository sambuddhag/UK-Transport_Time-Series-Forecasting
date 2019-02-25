#####Time Series Case Study#####

setwd("C:/Users/sambuddhag/Desktop/ANALYTIXLABS/R Software Installation/R course/UK Outward Passengers Movement case study/")
TSDATA1<-read.csv("TSDATA1.csv")
View(TSDATA1)

### creating time series data for all the five variables

ed.ireland <- ts(TSDATA1$Ireland, start=c(1996,1), end=c(2005,4), frequency=4)
ed.remaining.eu <- ts(TSDATA1$Other.EU.not.Ireland, start=c(1996,1), end=c(2005,4), frequency=4)
ed.rest.eu.med <- ts(TSDATA1$Rest.of.Europe.and.MED, start=c(1996,1), end=c(2005,4), frequency=4)
ed.rest.world <- ts(TSDATA1$Rest.of.World, start=c(1996,1), end=c(2005,4), frequency=4)
ed.total <- ts(TSDATA1$Total, start=c(1996,1), end=c(2005,4), frequency=4)

### Plotting each variables

plot(ed.ireland)
plot(ed.remaining.eu)
plot(ed.rest.eu.med)
plot(ed.rest.world)
plot(ed.total)

### Installing and reading forecast package

install.packages("forecast", dependencies = T)
library(forecast)


### Using stl function (Decomposition method) to forecast

### Decomposing time series into trend, seasonal and irregular components

stl.ireland      <-  stl(ed.ireland,s.window="periodic")
stl.remaining.eu <-  stl(ed.remaining.eu,s.window="periodic")
stl.rest.eu.med  <-  stl(ed.rest.eu.med,s.window="periodic")
stl.rest.world   <-  stl(ed.rest.world,s.window="periodic")
stl.total        <-  stl(ed.total,s.window="periodic")


### Plotting decomposed variables

plot(stl.ireland)
plot(stl.remaining.eu)
plot(stl.rest.eu.med)
plot(stl.rest.world)
plot(stl.total)

### Model accuracy checking (STL function-decomposition)

accuracy(forecast(stl.ireland,h=4))
accuracy(forecast(stl.remaining.eu,h=4))
accuracy(forecast(stl.rest.eu.med,h=4))
accuracy(forecast(stl.rest.world,h=4))
accuracy(forecast(stl.total,h=4))

### Forecasting the number of passengers to travel for next 4 quarters

forecast(stl.ireland, h=4)
forecast(stl.remaining.eu,h=4)
forecast(stl.rest.eu.med,h=4)
forecast(stl.rest.world,h=4)
forecast(stl.total,h=4)

### Plotting forecasted variables by STL(decomposition)

plot(forecast(stl.ireland))
plot(forecast(stl.remaining.eu))
plot(forecast(stl.rest.eu.med))
plot(forecast(stl.rest.world))
plot(forecast(stl.total))

### Exponential models: HOLT-WINTERS METHOD (calculated only for Total values)

## simple exponential

expo <- HoltWinters(ed.total, beta=FALSE, gamma=FALSE)
ls(expo)
require(forecast)
accuracy(expo$fitted, ed.total)

## double exponential 

expo1 <- HoltWinters(ed.total, gamma=FALSE)
accuracy(expo1$fitted, ed.total)

## triple exponential

expo2 <- HoltWinters(ed.total)
accuracy(expo2$fitted, ed.total)

forecast(expo2, 4)

### Using ETS function (smoothening method) to forecast

ets_ireland <- ets(ed.ireland)
ets_remaining.eu <- ets(ed.remaining.eu)
ets_rest.eu.med <- ets(ed.rest.eu.med)
ets_rest.world <- ets(ed.rest.world)
ets_total <- ets(ed.total)

### Model accuracy checking (ETS smoothening)

accuracy(ets_ireland$fitted,ed.ireland)
accuracy(ets_remaining.eu$fitted,ed.remaining.eu)
accuracy(ets_rest.eu.med$fitted,ed.rest.eu.med)
accuracy(ets_rest.world$fitted,ed.rest.world)
accuracy(ets_total$fitted,ed.total)

### Forecasting variables for next 4 quarters

forecast(ets_ireland,h=4)
forecast(ets_remaining.eu,h=4)
forecast(ets_rest.eu.med,h=4)
forecast(ets_rest.world,h=4)
forecast(ets_total,h=4)

### Plotting forecasted values by ets function (Smoothening Technique)

plot(forecast(ets_ireland))
plot(forecast(ets_remaining.eu))
plot(forecast(ets_rest.eu.med))
plot(forecast(ets_rest.world))
plot(forecast(ets_total))

### ARIMA using acf and pacf (Checked only for Total Values)

require(tseries)
require(forecast)

acf(ed.total)
pacf(ed.total)
ndiffs(ed.total)

######### Finding Autocorrelation (only for Total Values)

acf(ed.total)  ##taking P=2
acf(ed.total,5) ##P=1
pacf(ed.total,5) ###taking Q=1

### Checking whether Data is stationary or not by Augmented Dickey-Fuller test

adf.test(ed.total)


### Fit an ARIMA model of order P, D, Q

fitARIMA <- arima(ed.total, order=c(2,1,1))
summary(fitARIMA) 
plot(forecast(fitARIMA, 4))

### Using auto.arima function to forecst

arima_ireland <- auto.arima(ed.ireland)
arima_remaining.eu <- auto.arima(ed.remaining.eu)
arima_rest.eu.med <- auto.arima(ed.rest.eu.med)
arima_rest.world <- auto.arima(ed.rest.world)
arima_total <- auto.arima(ed.total)

### Model accuracy checking (ARIMA)

accuracy(arima_ireland)
accuracy(arima_remaining.eu)
accuracy(arima_rest.eu.med)
accuracy(arima_rest.world)
accuracy(arima_total)

### Forecasting variables for next 4 quarters

forecast(arima_ireland,h=4)
forecast(arima_remaining.eu,h=4)
forecast(arima_rest.eu.med,h=4)
forecast(arima_rest.world,h=4)
forecast(arima_total,h=4)

### Plotting of forecasted values (ARIMA)

plot(forecast(arima_ireland))
plot(forecast(arima_remaining.eu))
plot(forecast(arima_rest.eu.med))
plot(forecast(arima_rest.world))
plot(forecast(arima_total))

##### Accuracy (based on MAPE) is better in ETS method and thus values should be predicted using ETS method.

####### END #########