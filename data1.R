library(readr)
library(fGarch)
library(rugarch)
library(forecast)
library(tseries)
data1 <- read_csv("data1.csv", col_types = cols(time = col_date(format = "%Y/%m/%d")))
data2 <- read_csv("data2.csv", col_types = cols(time = col_date(format = "%Y/%m/%d")))
plot(data1$time,data1$number,type='l',xlab="time",ylab="new users")
lm1 <- lm(data1$number~data1$big+data1$small)
plot(data2$time,data2$number,type='l',xlab="time",ylab="new users")
lm2 <- lm(data2$number~data2$big+data2$small)
lm3 <- lm(data2$number~data2$discount)
ts1 <- ts(data1$number,frequency = 12,start = c(2004,1))
plot.ts(ts1)
plot.ts(diff(ts1))
acf(ts1)
pacf(ts1)
number.forecast = auto.arima(ts1)
number.forecast
forecast(number.forecast)
plot(forecast(number.forecast),shadecols='oldstyle')
adf.test(diff(ts1))


#waste
number.forecast.30 = forecast(ts1, h=30)
plot(number.forecast.30)
myspec = ugarchspec(variance.model = list(model='sGARCH', 
                                          garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(3,3), 
                                      include.mean = T),
                    distribution.model = 'std'
)
number.myfit = ugarchfit(myspec, data=diff(ts1))
acf(residuals(number.myfit))
number.forecast1 = ugarchforecast(number.myfit, data = diff(ts1))
number.forecast2 = ugarchforecast(number.myfit)
plot(number.forecast1)
plot(number.forecast2)