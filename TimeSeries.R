library(readxl)
library(fpp2)

exchange_data <- read_excel("../../ExchangeUSD.xlsx")

#remove "wdy" column
exchange_data <- exchange_data[c(-2, -1)] 

#make the data time series
exchange_data <- ts(exchange_data)

#plot time series
autoplot(exchange_data) + ggtitle("Time Plot: Exchange Rates")

#get the difference to make the data trend-stationary
DIF <- diff(exchange_data)
autoplot(DIF) + ggtitle("Time Plot: Change in Exchange Rates")

#check for any seasonality
ggseasonplot(DIF) + ggtitle("Season Plot: Change in Exchange Rates")
#not seasonal