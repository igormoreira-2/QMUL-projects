#**************************************************************
#MTH783 - Time Series Analysis for Business
#Igor Marques
#**************************************************************

library(fma)
library(ggplot2)
library(forecast)
library(xts)
library(tseries)
library(ggpubr)

rm(list=ls())
setwd('/Users/igorm/OneDrive/Queen Mary University of London/MTH783 - Time Series Analysis for Business/Coursework')
airq = na.omit(read.csv("AirQualityCourseworkNew.csv"), na.strings="?")

#Initial data exploration
summary(airq)
str(airq)

# Convert "Date" to 'Date' class
class(airq$Date)
airq$Date <- as.Date(airq$Date, format = "%d/%m/%Y")
class(airq$Date) #as date now

#Removing data column from dataset
airq_dt <- airq$Date
airq$Date <- NULL

#Converting data frame to time series xts
airq_xts <- as.xts(airq, order.by = airq_dt)

#Exploring the dataset
g1 = ggplot(data = airq_xts, aes(x=index(airq_xts$NOx))) + geom_line(aes(y=NOx), color="red") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
g2 = ggplot(data = airq_xts, aes(x=index(airq_xts$NO2))) + geom_line(aes(y=NO2), color="blue") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g3 = ggplot(data = airq_xts, aes(x=index(airq_xts$Temp))) + geom_line(aes(y=Temp), color="darkgreen") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g4 = ggplot(data = airq_xts, aes(x=index(airq_xts$RH))) + geom_line(aes(y=RH), color="orange") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g5 = ggplot(data = airq_xts, aes(x=index(airq_xts$AH))) + geom_line(aes(y=AH), color="purple") + labs(x="")
ggarrange(g1, g2, g3, g4, g5, ncol=1, nrow=5)

#Missing values (-200) will be replaced by using an interpolation imputer from the forecast library
#First, let's set all -200 values to NA
airq_xts$NOx[airq_xts$NOx==-200] <- NA
airq_xts$NO2[airq_xts$NO2==-200] <- NA
airq_xts$Temp[airq_xts$Temp==-200] <- NA
airq_xts$RH[airq_xts$RH==-200] <- NA
airq_xts$AH[airq_xts$AH==-200] <- NA

#Interpolate missing values in the time series
airq_xts$NOx <- na.interp(airq_xts$NOx)
airq_xts$NO2 <- na.interp(airq_xts$NO2)
airq_xts$Temp <- na.interp(airq_xts$Temp)
airq_xts$RH <- na.interp(airq_xts$RH)
airq_xts$AH <- na.interp(airq_xts$AH)

#Verifying if the missing values have been treated
g1 = ggplot(data = airq_xts, aes(x=index(airq_xts$NOx))) + geom_line(aes(y=NOx), color="red") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
g2 = ggplot(data = airq_xts, aes(x=index(airq_xts$NO2))) + geom_line(aes(y=NO2), color="blue") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g3 = ggplot(data = airq_xts, aes(x=index(airq_xts$Temp))) + geom_line(aes(y=Temp), color="darkgreen") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g4 = ggplot(data = airq_xts, aes(x=index(airq_xts$RH))) + geom_line(aes(y=RH), color="orange") + labs(x="") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
g5 = ggplot(data = airq_xts, aes(x=index(airq_xts$AH))) + geom_line(aes(y=AH), color="purple") + labs(x="")
ggarrange(g1, g2, g3, g4, g5, ncol=1, nrow=5)

summary(airq_xts)

#Splitting data into training and test set
airq_training <- window(airq_xts, start = c("2004-03-11") , end = c("2004-12-31"))
airq_test <- window(airq_xts, start = c("2005-01-01"), end = c("2005-01-31"))

#Verifying lenght of each dataset
length(airq_training$NOx) #=296
length(airq_test$NOx) #=31

#Graphical Analysis
#NOx
(ggplot(data=airq_training$NOx, aes(x = index(airq_training), y=NOx)) 
  + geom_line(color = "darkorchid4") + labs(title = "Daily NOx Concentration - Before Box Cox Transform", x = "Date"))
ggtsdisplay(airq_training$NOx) #Seasonal pattern appearing seems to be weekly

#NO2, Temperature, RH and AH
ggtsdisplay(airq_training$NO2) #(lags represent each day)
ggtsdisplay(airq_training$Temp)
ggtsdisplay(airq_training$RH)
ggtsdisplay(airq_training$AH)

#Seasonal pattern appearing seems to be weekly, thus frequency is set to 7
attr(airq_training, "frequency") <- 7
attr(airq_test, "frequeny") <- 7

#Let's decompose the NOx variable
NOx_dec <- decompose(as.ts(airq_training$NOx))
plot(NOx_dec)
adf.test(na.omit(NOx_dec$random)) #ramdom component seem to be stationary according to adf test - Ho rejected
kpss.test(na.omit(NOx_dec$random)) #random component seem to be stationary according to adf test - Ho not rejected
ggAcf(as.ts(NOx_dec$random))#random components still show some autocorrelation, which is expected

############### MODEL SELECTION ###############

#Finding best model using auto.arima function
m1 <- auto.arima(airq_xts$NOx)
checkresiduals(m1) #there is still some significant autocorrelation
qqnorm(m1$residuals, col="red")
qqline(m1$residuals) #residuals do not look like white noise
m1_pred <- forecast(m1, 31) #forecasting in order to calculate rmse
plot(m1_pred) #really bad forecast
rmse_m1 <- sqrt(sum(((as.numeric(airq_test$NOx)-as.numeric(m1_pred$mean))^2))/length(airq_test$NOx))

#Data Transformations
#We can notice that the amplitude of NOx seasonal variation increases throughout the year
#Box-Cox transformations will be used to stabilize variance

lambda_NOx <- BoxCox.lambda(airq_training$NOx)
NOx_tr <- forecast::BoxCox(airq_training$NOx, lambda_NOx)
(ggplot(data=NOx_tr, aes(x = index(NOx_tr), y=NOx_tr)) 
  + geom_line(color = "darkorchid4") + labs(title = "Daily NOx Concentration - After Box Cox Transform", x = "Date"))

#Finding best model using auto.arima function
m2 <- auto.arima(NOx_tr)
checkresiduals(m2) #there is still some significant autocorrelation
m2_pred <- forecast(m2, 31)
plot(m2_pred) #poor forecast
rmse_m2 <- sqrt(sum(((as.numeric(airq_test$NOx)-as.numeric(InvBoxCox(m2_pred$mean, lambda = lambda_NOx)))^2))
              /length(airq_test$NOx))

#Considering variations of the current model by varying P and Q
m3 <- arima(NOx_tr, c(1,1,1), c(1,0,1))
BIC(m3)
checkresiduals(m3) #great, not autocorellation in the residuals
qqnorm(m3$residuals) #overall a good qqplot, however a few outliers can be noticed at the edges
qqline(m3$residuals)
m3_pred <- forecast(m3, 31)
plot(m3_pred)
rmse_m3 <- sqrt(sum(((as.numeric(airq_test$NOx)-as.numeric(InvBoxCox(m3_pred$mean, lambda = lambda_NOx)))^2))
                /length(airq_test$NOx))

#Dynamic Models
cor(airq_xts) #checking linear correlation between variables - NO2, Temp and RH have a strong relationship with NOx

exp_var_tr <- cbind(airq_training$NO2, airq_training$Temp, airq_training$RH) #dataframe for training set predictors
exp_var_test <- cbind(airq_test$NO2, airq_test$Temp, airq_test$RH) #dataframe for test set predictors
m4 <- auto.arima(NOx_tr, xreg = exp_var_tr)
checkresiduals(m4)
qqnorm(m4$residuals, col="green") #look relatively good, best of all models
qqline(m4$residuals)
(ggplot(m4$residuals, aes(x=index(m4$residuals), y=m4$residuals)) #homoscedastic
  + geom_point()
  + ggtitle("Model 4 Residuals")
  + labs(x="Weekly Data - Mar 2004 to Dec 2004", y="Residuals"))
kpss.test(m4$residuals) #confirmed stationarity
adf.test(na.omit(m4$residuals)) #confirmed stationarity
Box.test(m4$residuals, type="Ljung-Box") #Ho not rejected (absence of serial correlation)

#Now let's forecast using the selected model
m4_pred <- forecast(m4, xreg = exp_var_test)
plot(m4_pred) #great forecast
rmse_m4 <- sqrt((sum((as.numeric(airq_test$NOx) - as.numeric(InvBoxCox(m4_pred$mean, lambda = lambda_NOx)))^2))
                 /length(airq_test$NOx))

#Plotting observed values over predicted values for all model for comparisson
all_pred_df <- data.frame(NOx=as.numeric(airq_test$NOx), 
                         NOx_pred_m4=as.numeric(InvBoxCox(m4_pred$mean, lambda = lambda_NOx)),
                         NOX_pred_m3=as.numeric(InvBoxCox(m3_pred$mean, lambda = lambda_NOx)),
                         NOX_pred_m2=as.numeric(InvBoxCox(m2_pred$mean, lambda = lambda_NOx)),
                         NOX_pred_m1=as.numeric(m1_pred$mean))
ggplot(all_pred_df, aes(x=index(all_pred_df))) + 
  geom_line(aes(y = NOx, colour = "Observed NOx Values"),size=1) + 
  geom_line(aes(y = NOx_pred_m4, colour = "Model 4 Predictions"), size=1) + 
  geom_line(aes(y = NOX_pred_m3, colour = "Model 3 Predictions"), linetype="dashed") +
  geom_line(aes(y = NOX_pred_m2, colour = "Model 2 Predictions"), linetype="dashed") +
  geom_line(aes(y = NOX_pred_m1, colour = "Model 1 Predictions"), linetype="dashed") +
  ggtitle("NOx - Observed vs. Predicted (All Models)") +
  labs(x="Days (January, 2005)", colour="Legend")
