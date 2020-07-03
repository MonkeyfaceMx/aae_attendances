# Time series ARIMA models with NHS England data
# Created by Dr Pablo N Perez-Guzman
# Imperial College London, 2020

library(tidyverse)
library(forecast)
library(ggplot2)
library(tseries)

### 1. Load the data for analysis

data <- read.csv("timeseries.csv")

london <- data %>% filter(str_detect(region,"London")) 
london_past <- london %>% filter(as.numeric(date) < 56)
london_present <- london  %>% filter(as.numeric(date) >= 56)

## 2. Create time series object and check stationarity and differencing 

# Historic data 
london_ats <- ts(as.vector(tapply(london_past$type1_attendances, london_past$date, FUN=sum)),
                frequency = 12,start = c(2015,6))
london.diff <- diff(london_ats)

  # Stationarity and differencing
  par(mfrow=c(1,2))
    plot(london_ats)
    plot(london.diff)
  par(mfrow=c(1,1))
  
  # Augmented Dickey-Fuller test
  adf.test(!is.na(london_ats),alternative="stationary",k=0) # Undifferenciated data is stationary
  
  # KPSS unit root test to determine if differencing is required
  library(urca)
  london_ats %>% ur.kpss() %>% summary() # p non significant; differencing not required
  
# Current year observed data
london_ats_actual <- ts(as.vector(tapply(london_present$type1_attendances, london_present$date, FUN=sum)),
                        frequency = 12,start = c(2015,6))


# 3. Build ARIMA model and forecast observed data for 2020
arima_london_ats<-auto.arima(london_ats)
forecast_london_ats <- forecast(arima_london_ats,h=4)
summary(forecast_london_ats)

  # Simple plot
  plot(forecast_london_ats,ylim = c(100000,480000),main="London",ylab="A&E attendances")
  lines(london_ats_actual,lty=2,col="darkblue")

  # Advanced plot 
  autoplot(forecast_london_ats,ylim = c(0,480000),
         main="London",ylab="ED attendances") +
    autolayer(london_ats_actual, series="Observed") + 
    autolayer(forecast_london_ats$mean, series="Forecast") +
    theme_bw() 



