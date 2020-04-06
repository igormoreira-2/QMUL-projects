##Tutorial 5##
library(ggplot2)
library(forecast)
library(fma)
library(tseries)
rm(list=ls())

#Question 1
? autoplot
? ggseasonplot
? ggmonthplot
? gglagplot
? ggAcf
? ggtsdisplay

autoplot(chicken)
gglagplot(chicken)
ggAcf(chicken)
ggtsdisplay(chicken)

autoplot(beer)
ggseasonplot(beer)
ggmonthplot(beer)
gglagplot(beer)
ggAcf(beer)
ggtsdisplay(beer)

autoplot(eggs)
gglagplot(eggs)
ggAcf(eggs)
ggtsdisplay(eggs)

#Question 2
beer_d <- decompose(beer)
plot(beer_d)
#Before fitting an ARMA model, we need to make sure data is stationary in the mean (no trend, no seasonality) and in the variance
#Thus we need to remove any visible trend and seasonality, using the decompose() function
#(a)
#Fitting AR(1)
beerAR1 <- arima(na.omit(beer_d$random), order = c(1,0,0))
beerAR1
beerAR1_res <- residuals(beerAR1)
plot(beerAR1_res, type="l") #looks stationary
hist(beerAR1_res) 
qqnorm(beerAR1_res)
qqline(beerAR1_res) #residuals seem to be normally distributed
acf(beerAR1_res) #only one lag outside boundaries = indicates stationarity
#Residuals are not similar to WN

#Fitting MA(1)
beerMA1 = arima(na.omit(beer_d$random), order = c(0,0,1))
beerMA1
beerMA1_res <- residuals(beerMA1)
plot(beerMA1_res, type="l")
hist(beerMA1_res) 
qqnorm(beerMA1_res)
qqline(beerMA1_res, distribution = qnorm)
acf(beerMA1_res)
ggAcf(beerMA1_res)

#(b)
#MA(1) seems to be the best model. Residuals are closer to WN. 
#The residuals suggest that the MA(1) model is a better fit for this dataset.

#(c)
AIC(beerAR1)
AIC(beerMA1)
BIC(beerAR1)
BIC(beerMA1)
#AIC and BIC for the MA1 model is the lowest, therefore the chosen model is beerMA1.
#The BIC and the AIC agree that the model MA(1) is more suitable model than AR(1) for this dataset.

#(d)
forecast_AR1 <- forecast(beerAR1, h=4)
plot(forecast_AR1, type="l", xlab="Forecast AR1 Model")

forecast_MA1 <- forecast(beerMA1, h=4)
plot(forecast_MA1, type="l", xlab="Forecast MA1 Model")

#alternative solution
forecast_AR1_1 <- predict(beerAR1, 4)
forecast_MA1_1 <- predict(beerMA1, 4)
plot(as.ts(beer_d$random,rep(NA,4)))
points(as.ts(forecast_AR1_1$pred),col="red")
points(as.ts(forecast_MA1_1$pred),col="blue")

#(e)
#As we do not know the actual monthly beer production for the forecasted months, we
#cannot compare the two models using the forecast. Alternatively, we could have split the data into
#training and testing datasets and then compared the forecast on the test dataset using RMSE and MAE.

#Question 3
#(a)
plot(dowjones)
acf(dowjones)
pacf(dowjones)
tsdisplay(dowjones)
#Looking at the time plot, there is indeed a trend in the dowjones time series: it is not stationary in mean.

#(b)
plot(dowjones - lag(dowjones, -1))
plot(diff(dowjones))
tsdisplay(diff(dowjones))
dj_ARIMA <- arima(dowjones,c(0,1,0))
tsdisplay(dj_ARIMA$residuals)
#There is still a bit of a trend appearing in the residuals so lets increase to second order differencing.

#(c)
plot(lag(dowjones, -1) - lag(dowjones, -2))
dj_ARIMA_1 <- arima(dowjones,c(0,2,0))
tsdisplay(dj_ARIMA_1$residuals)
#The time plot of the residuals looks better now as it looks like noise, and the mean across time seems
#to be well centered around 0.

#(d)
#In the ACF there is a major peak at lag 1 then the following ones get negligeable. Although
#major peaks also appears at lags 11 to 17, lets try the MA(1).
dj_ARIMA_2 <- arima(dowjones, c(0,2,1))
plot(dj_ARIMA_2$residuals)
tsdisplay(dj_ARIMA_2$residuals)

#(e)
AIC(dj_ARIMA)
BIC(dj_ARIMA)
AIC(dj_ARIMA_1)
BIC(dj_ARIMA_1)
AIC(dj_ARIMA_2)
BIC(dj_ARIMA_2)
#Note that indeed at each step we were able to dicrease the AIC and BIC using visualisation tools.

#(f)
auto.arima(dowjones)
#While the computer has identified a model with lower AIC, our model had the lowest BIC. Arguably,
#also, our model had less parameters and therefore was more desireable.

#Question 4
#Chicken dataset
plot(chicken)
chicken_ARIMA <- arima(chicken, c(0,1,0))
tsdisplay(chicken_ARIMA$residuals)

auto.arima(chicken)

#Internet dataset
plot(internet)
tsdisplay(diff(internet))
internet_ARIMA <- arima(internet, c(1,1,1))
tsdisplay(internet_ARIMA$residuals)     

auto.arima(internet)

#Copper dataset
plot(copper)
tsdisplay(diff(copper))
copper_ARIMA <- arima(copper, c(0,1,3))
tsdisplay(copper_ARIMA$residuals)

auto.arima(copper)

#Question 5
#(a)
plot(hsales)
tsdisplay(diff(hsales,12)) #monthly data, so needs to specify the frequency. Still looks non-stationary
tsdisplay((diff(diff(hsales,12)))) #looks stationary

#(b)
best_hs_sARIMA <- auto.arima(hsales, seasonal=TRUE)
hs_ARIMA <- arima(hsales, c(0,1,0), c(0,1,0))

#(c)
tsdisplay(best_hs_sARIMA$residuals)
tsdisplay(hs_ARIMA$residuals)

#Ho Explosive = Non Stationary
adf.test(hs_ARIMA$residuals)
adf.test(best_hs_sARIMA$residuals)
#p-value < 0.5, thus Ho is reject, Ha = is stationary

#Ho absence of serial correlation = is stationary
Box.test(best_hs_sARIMA$residuals, fitdf=5, lag=24, type="Lj") #Ho rejected
Box.test(hs_ARIMA$residuals, fitdf=2, lag=24, type="Lj") #Ho rejected, not stationary

#(d)
plot(forecast(best_hs_sARIMA, 24))
plot(forecast(hs_ARIMA, 24))

#Question 6
#(a)
plot(uselec)
tsdisplay(diff(uselec,12)) #looks roughly stationary

#(b)
best_us_sARIMA <- auto.arima(uselec, seasonal = TRUE)

#(c)
tsdisplay(best_us_sARIMA$residuals) #residuals look good
adf.test(best_us_sARIMA$residuals) #rejects Ho, it is stationary

#(d)
plot(forecast(best_us_sARIMA, 24))
