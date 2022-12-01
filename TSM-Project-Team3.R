#--Team 3 Yubing Chen, Yizhuo Hu, Shijia Ren, Yuheng Ma, Zhiyi Guo

library (ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
library(tseries)

# The raw data set we used is about the price of world top 6 commodity from 2000-01 to 2022-03
commodity.2000.2022 <- read.csv("~/Downloads/commodity 2000-2022.csv")

View(commodity.2000.2022)

#----Data Pre-processing----#
#Step 1:taking out wanted data (Gold Section)
commodity_gold <- subset(commodity.2000.2022, Symbol== "Gold")

#**changing datatype of Date column from Character to Date
commodity_gold$Date = as.Date(commodity_gold$Date)

#**Finding the average open price of gold group by year and month
commodity_gold$Year_Month <- format(commodity_gold$Date, format="%Y-%m")
Open_Ave<-aggregate(commodity_gold$Open, list(commodity_gold$Year_Month), FUN=mean)
Open_Ave

#Step 2: Creating a new database and insert all data that we want.#
Open_Price_df <-data.frame(Open_Ave$x)
colnames (Open_Price_df)<-'Gold_open_price'

boxplot(Open_Price_df$Gold_open_price)
hist(Open_Price_df$Gold_open_price)
#**Change the database to time-series database for further prediction and analysis
Open_data_ts<-ts(Open_Price_df, start = c(2000,1), frequency=12)# The start time of this database of January 2000, and the data is recorded monthly.

#**Here we get data we want and also the trend of the data.
autoplot(Open_data_ts)


#Step3: For further prediction, we need to separate our data set into two groups: training set and testing set
Train_set_open = Open_data_ts [1:216] # from 2000-01 to 2017-12
Test_set_open = Open_Price_df[217:267,] # from 2018-01 to 2022-03

#**changing the type these two data set to time series
Train_set_open_ts = ts(Train_set_open, start = c(2000, 1), frequency = 12)
Test_set_open_ts = ts(Test_set_open, start = c(2018, 1), frequency = 12)

autoplot(Train_set_open_ts)
autoplot(Test_set_open_ts)


#----Prediction----#
#Right now, we have 3 time-series data: Open_data_ts, Train_set_open_ts, and Test_set_open_ts. It is time to do prediction. We will use 8 models for prediction.

#Model 1. Mean Forecast.

mean_train <-meanf(Train_set_open_ts, h =51)
mean_train
# ploting
autoplot(Train_set_open_ts) +
  autolayer(meanf(Train_set_open_ts, h=51),
            series="Mean", PI=FALSE) +
  ggtitle("Open Price of Gold") +
  xlab("Year") + ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))
accuracy(mean_train,Test_set_open_ts)
checkresiduals(mean_train)
residual_mean <- residuals.lm(mean_train)
hist(residual_mean,main = "Frequency of Residual (Mean)")


#Model 2. Naive Forecast.
#Model 2. Naive Forecast.
# model building
naive_train <- naive(Train_set_open_ts, h=51)

# ploting
autoplot(naive_train)
autoplot(Train_set_open_ts) +
  autolayer(naive(Train_set_open_ts, h=51),
            series="Naïve", PI=FALSE) +
  ggtitle("Open price of gold") +
  xlab("Year") + ylab("Amount") +
  guides(colour=guide_legend(title="Forecast"))

# Accuracy test
accuracy(naive_train, Test_set_open_ts)
residual_naive <- residuals.lm(naive_train)
hist(residual_naive,main = "Frequency of Residual (Naive)")

# Check residuals
checkresiduals(naive_train)


#Model 3. Seasonal Naive Forecast.
snaive_train <- snaive(Train_set_open_ts, h=51)

# ploting
autoplot(snaive_train)
autoplot(Train_set_open_ts) +
  autolayer(snaive(Train_set_open_ts, h=51),
            series="Naïve", PI=FALSE) +
  ggtitle("Open Price of Gold") +
  xlab("Year") + ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))

# Accuracy test
accuracy(snaive_train, Test_set_open_ts)
residual_snaive <- residuals.lm(snaive_train)
hist(residual_snaive)



#Model 4. ETS.
Y<- ts(Train_set_open, start = c(2000, 1), frequency = 12)
# Remove trends to make it stationary:
Y2<-diff(Y)
# Plot Stationary Model(Y2)
autoplot(Y2)   
#check data month by month with ggseasonplot():
ggseasonplot(Y2)
# Exponential Smoothing State space model --> used for forecasting
fit_ets<-ets(Y2)
# Print summary to analyze data:
print(summary(fit_ets))
checkresiduals(fit_ets)
fcst1<-forecast(fit_ets,h=51)
print(fcst1)
autoplot(fcst1)+ggtitle("Open price of gold")+xlab("Year")+ylab("Price")+guides(colour=guide_legend(title="Forecast"))
checkresiduals(fcst1)
accuracy(fcst1, Test_set_open_ts)  
residual_ets <- residuals.lm(fcst1)
hist(residual_ets,main="Frequency of Residual(ETS)")


#Model 5. Holts-Winter.
# Holt Winter Exponential Smoothing

# Two variations: additive for roughly constant seasonal variation, otherwise multiplicative method

Holt_Winter = window(Train_set_open_ts, start= c(2000,1))
Holt_Winter_data_add = hw(Holt_Winter, seasonal="additive")
Holt_Winter_data_mult = hw(Holt_Winter, seasonal="multiplicative")

autoplot(Holt_Winter_data_add)
autoplot(Holt_Winter_data_mult, main = "Open price of gold")

autoplot(Holt_Winter, series = "original data")+
  autolayer(Holt_Winter_data_add, series = "additive", PI=FALSE)+
  autolayer(Holt_Winter_data_mult, series = "multiplicative", PI=FALSE)+
  ggtitle("Gold Open Price") +
  xlab("Year") + ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))


autoplot(Train_set_open_ts) +
  autolayer(meanf(Train_set_open_ts, h=51),
            series="Mean", PI=FALSE) +
  ggtitle("Forecasts for Gold Open Price") +
  xlab("Year") + ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))
accuracy(mean_train,Test_set_open_ts)
# model summary and performance
Holt_Winter_data_add[["model"]]
accuracy(Holt_Winter_data_mult, Test_set_open_ts)
accuracy(Holt_Winter_data_add, Test_set_open_ts)
residual_Holt_mult <- residuals.lm(Holt_Winter_data_mult)
hist(residual_Holt_mult, main = "Frequency of Residual (Holt-Winter mult)")
residual_Holt_add <- residuals.lm(Holt_Winter_data_add)
hist(residual_Holt_add, main = "Frequency of Residual (Holt-Winter add)")

# Check residuals
checkresiduals(Holt_Winter_data_mult)
checkresiduals(Holt_Winter_data_add)



#Model 6. Moving Averages.
##moving average 
moving_average_train<-ma(Train_set_open_ts, 5)
fmoving_average_train<-forecast(moving_average_train, h=51)
plot(fmoving_average_train)

autoplot(fmoving_average_train, series="Data") +
  autolayer(ma(Train_set_open_ts,5), series="5-MA") +
  xlab("Year") + ylab("Price") +
  ggtitle("Annual Open Price:Gold") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

residual_moving_average <- residuals.lm(fmoving_average_train)
hist(residual_moving_average,main = "Frequency of Residual (Moving Averages)")

# Accuracy test
accuracy(fmoving_average_train, Test_set_open_ts)
checkresiduals(fmoving_average_train)

#Model 7.ARIMA.
#Arima
#Model 8.ARIMA.
arima_train <- auto.arima(Train_set_open_ts)
summary(arima_train)
accuracy(arima_train)
plot(forecast(arima_train, h = 51),xlab = "Year", ylab = "Price", main = "Open Price of Gold")
forecast_arima_train<-forecast(Train_set_open_ts,h=51,model=arima_train)
arima_train1<-ts(arima_train,frequency = 12, start = c(2018,1))

# Accuracy test
accuracy(forecast_arima_train, Test_set_open_ts)

#residual
residual_arima <- residuals.lm(arima_train)
hist(residual_arima,main="Frequency of Residual(arima)")

# Check residuals
checkresiduals(forecast_arima_train)

