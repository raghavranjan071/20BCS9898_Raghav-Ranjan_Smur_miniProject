## Required Packages
install.packages("quantmod")
install.packages("forecast")

library(quantmod)
library(forecast)

## Importing Dataset from Finance Websites...(Yahoo)
getSymbols('GOOG', from = '2019-01-01', to = '2021-01-01')

## Visualize the data
chartSeries(GOOG, subset = 'last 6 months', type = 'candlesticks')
addSMA()

## Extracting columns
Opening <- GOOG[,1]
Highest <- GOOG[,2]
Lowest <- GOOG[,3]
Closing <- GOOG[,4]
Volume <- GOOG[,5]
Adjusted <- GOOG[,6]

## Plotting
par(mfrow = c(2,3))
plot(Opening, main = 'Open Price')
plot(Highest, main = 'High Price')
plot(Lowest, main = 'Low Price')
plot(Closing, main = 'Close Price')
plot(Volume, main = 'Volume')
plot(Adjusted, main = 'Adjusted Price')

## Price Prediction
Price_Predict <- Adjusted

## ACF and PACF
par(mfrow = c(1,2))
Acf(Price_Predict, main = 'Autocorrelation')
Pacf(Price_Predict, main = 'Partial Autocorrelation')

## Test for stationarity
print(adf.test(Price_Predict))

## Return Prediction
Return_GOOG <- 100 * diff(log(Price_Predict))
GOOG_return_train <- Return_GOOG[1:(0.9 * length(Return_GOOG))]
GOOG_return_test <- Return_GOOG[(0.9 * length(Return_GOOG) + 1):length(Return_GOOG)]

## Model fitting
fit <- auto.arima(GOOG_return_train, seasonal = FALSE)
preds <- forecast(fit, h = (length(Return_GOOG) - (0.9 * length(Return_GOOG))))$mean

## Forecasting
test_forecast <- forecast(fit, h = 15)

## Plot forecast
plot(test_forecast, main = "Forecast for Google Stock")

## Evaluate accuracy
accuracy(preds, GOOG_return_test)
