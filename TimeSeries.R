library(readxl)
library(neuralnet)
library(fpp2)

exchange_data <- read_excel("../../ExchangeUSD.xlsx")

#remove "wdy" column
exchange_data <- exchange_data[c(-2)] 

#make the data time series
exchange_data <- ts(exchange_data)

#scale the data
exchange_data <- apply(exchange_data, 2, scale)

#convert the data back into time series, since the plot functions require as so
exchange_data <- ts(exchange_data)

#plot time series
autoplot(exchange_data) + ggtitle("Time Plot: Exchange Rates")

#get the difference to make the data trend-stationary
DIF <- diff(exchange_data)
autoplot(DIF) + ggtitle("Time Plot: Change in Exchange Rates")

name <- colnames(DIF)
formula <- as.formula(paste(name[name]))
