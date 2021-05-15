library(readxl)
library(neuralnet)
library(fpp2)
library(ggplot2)
library(Metrics)
library(useful)
library(nnfor)
library(forecast)


# import dataset
exchange_data <- read_excel("../../ExchangeUSD.xlsx")
# remove unneeded days column
exchange_data <- exchange_data[c(-2)] 
# convert to time-series, so the date is also in a numeric format
# required for scaling
exchange_data <- ts(exchange_data)
# after converting date to numeric convert back into data frame
exchange_data <- as.data.frame(exchange_data)
# rename col names for easiness
colnames(exchange_data) <- c("date", "rate")


# scale the data
maxs <- apply(exchange_data, 2, max)
mins <- apply(exchange_data, 2, min)
scaled_exchange_data <- as.data.frame(scale(exchange_data, center = mins, scale = maxs - mins))


# plot partial autocorrelation plot to check for optimum order of AR ~ however many orders
# will be tested upon
pacf(scaled_exchange_data[, 2])
# plot the rates to check whether stationary
plot(exchange_data[c(2)])



## TWO HIDDEN LAYERS ##
perform_neuralnet_ar_two <- function(seed, order, new_col_name) {
  # write a function to obtain the optimum order of AR(p) model #
  # will create different input/output matrices, based on the specific order (the lag column changes)
  # will then remove the unneeded date column, split the data into train and test sets, of fixed train set sizes
  # will then create and train a neural net of internal conditions in control form, so only the input/output matrix would affect evaluation
  # will then predict on the test set, and return the RMSE, MAE and MAPE of the predictions
  
  scaled_exchange_data_shifted <- shift.column(scaled_exchange_data, columns = "rate", newNames = new_col_name, len = order, up = F)
  scaled_exchange_data_shifted <- scaled_exchange_data_shifted[c(-1)]
  train <- na.omit(scaled_exchange_data_shifted[0:400, ])
  test_index = 400 + order
  test <- na.omit(scaled_exchange_data_shifted[test_index:500, ])
  
  set.seed(seed) 
  nn_trial <- neuralnet(rate ~ rate_2, data = train, hidden = c(6, 6), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test)
  
  rmse_nn_trial <- round(rmse(test[, 1], nn_trial.test_prediction), digits = 4)
  mae_nn_trial <- round(mae(test[, 1], nn_trial.test_prediction), digits = 4)
  mape_nn_trial <- round(mape(test[, 1], nn_trial.test_prediction), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}
order_1_hidden_2 <- perform_neuralnet_ar_two(104, 1, "rate_1")
order_2_hidden_2 <- perform_neuralnet_ar_two(104, 2, "rate_2")
order_3_hidden_2 <- perform_neuralnet_ar_two(104, 3, "rate_3")
order_4_hidden_2 <- perform_neuralnet_ar_two(104, 4, "rate_4")
order_5_hidden_2 <- perform_neuralnet_ar_two(104, 5, "rate_5")
# AR of order 1 gave the best metrics, as determined in PACF


# Optimal Input vector - AR(1)
scaled_exchange_data_hidden_2_shifted <- shift.column(scaled_exchange_data, columns = "rate", newNames = "rate_1", len = 1, up = F)
scaled_exchange_data_hidden_2_shifted <- scaled_exchange_data_hidden_2_shifted[c(-1)]
train_hidden_2 <- na.omit(scaled_exchange_data_hidden_2_shifted[0:400, ])
test_hidden_2 <- na.omit(scaled_exchange_data_hidden_2_shifted[401:500, ])


perform_neuralnet_calculations_two <- function(seed, hidden, activation, learning) {
  # write a function that will determine the best internal neural net structure and hyper parameters #
  # will train on the input/output matrix of AR(1) and vary inner neural net values
  # will then predict on the test set and return the RMSE, MAE, MAPE of the predictions
  set.seed(seed) 
  nn_trial <- neuralnet(rate ~ rate_1, data = train_hidden_2, hidden = c(hidden, hidden), act.fct = activation, err.fct = "sse", lifesign = "full", learningrate = learning, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test_hidden_2)
  
  rmse_nn_trial <- round(rmse(test_hidden_2[, 1], nn_trial.test_prediction), digits = 4)
  mae_nn_trial <- round(mae(test_hidden_2[, 1], nn_trial.test_prediction), digits = 4)
  mape_nn_trial <- round(mape(test_hidden_2[, 1], nn_trial.test_prediction), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}

# find optimal number of nodes for 2 hidden layers
trial0_metrics <- perform_neuralnet_calculations_two(104, 2, "logistic", 0.08)
trial1_metrics <- perform_neuralnet_calculations_two(104, 3, "logistic", 0.08)
trial2_metrics <- perform_neuralnet_calculations_two(104, 4, "logistic", 0.08)
trial3_metrics <- perform_neuralnet_calculations_two(104, 5, "logistic", 0.08)
trial4_metrics <- perform_neuralnet_calculations_two(104, 6, "logistic", 0.08) 
trial5_metrics <- perform_neuralnet_calculations_two(104, 8, "logistic", 0.08)

# find best learning rate value for above best trial
trial6_metrics <- perform_neuralnet_calculations_two(104, 4, "logistic", 0.04)
trial7_metrics <- perform_neuralnet_calculations_two(104, 4, "logistic", 0.02)
trial8_metrics <- perform_neuralnet_calculations_two(104, 4, "logistic", 0.1)

# find optimal activation function for above best trial
trial9_metrics <- perform_neuralnet_calculations_two(104, 4, "tanh", 0.08)
# Optimal internal network values values for 2 hidden layer network with seed 104: (4, 4), logistic, 0.08, trial2



## SINGLE HIDDEN LAYER ##
perform_neuralnet_ar_one <- function(seed, order, new_col_name) {
  # write a function that does the same exact functionality as perform_neuralnet_ar_two()
  # but with a single hidden layer
  scaled_exchange_data_shifted <- shift.column(scaled_exchange_data, columns = "rate", newNames = new_col_name, len = order, up = F)
  scaled_exchange_data_shifted <- scaled_exchange_data_shifted[c(-1)]
  train <- na.omit(scaled_exchange_data_shifted[0:400, ])
  test_index = 400 + order
  test <- na.omit(scaled_exchange_data_shifted[test_index:500, ])
  
  set.seed(seed) 
  nn_trial <- neuralnet(rate ~ rate_5, data = train, hidden = c(6), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test)
  
  rmse_nn_trial <- round(rmse(test[, 1], nn_trial.test_prediction), digits = 4)
  mae_nn_trial <- round(mae(test[, 1], nn_trial.test_prediction), digits = 4)
  mape_nn_trial <- round(mape(test[, 1], nn_trial.test_prediction), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}
order_1_hidden_1 <- perform_neuralnet_ar_one(104, 1, "rate_1")
order_2_hidden_1 <- perform_neuralnet_ar_one(104, 2, "rate_2")
order_3_hidden_1 <- perform_neuralnet_ar_one(104, 3, "rate_3")
order_4_hidden_1 <- perform_neuralnet_ar_one(104, 4, "rate_4")
order_5_hidden_1 <- perform_neuralnet_ar_one(104, 5, "rate_5")
# AR of order 1 gave best metrics


# Optimal Input vector - AR(1)
scaled_exchange_data_hidden_1_shifted <- shift.column(scaled_exchange_data, columns = "rate", newNames = "rate_1", len = 1, up = F)
scaled_exchange_data_hidden_1_shifted <- scaled_exchange_data_hidden_1_shifted[c(-1)]
train_hidden_1 <- na.omit(scaled_exchange_data_hidden_1_shifted[0:400, ])
test_hidden_1 <- na.omit(scaled_exchange_data_hidden_1_shifted[401:500, ])


perfom_neuralnet_calculations_one <- function(seed, hidden, activation, learning) {
  # write a function that does the same exact functionality as perform_neuralnet_calculations_two()
  # but with a single hidden layer
  set.seed(seed) 
  nn_trial <- neuralnet(rate ~ rate_1, data = train_hidden_1, hidden = c(hidden), act.fct = activation, err.fct = "sse", lifesign = "full", learningrate = learning, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test_hidden_1)
  
  rmse_nn_trial <- round(rmse(test_hidden_1[, 1], nn_trial.test_prediction), digits = 4)
  mae_nn_trial <- round(mae(test_hidden_1[, 1], nn_trial.test_prediction), digits = 4)
  mape_nn_trial <- round(mape(test_hidden_1[, 1], nn_trial.test_prediction), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}

# find optimal number of nodes for 1 hidden layer
trial10_metrics <- perfom_neuralnet_calculations_one(104, 2, "logistic", 0.08)
trial11_metrics <- perfom_neuralnet_calculations_one(104, 3, "logistic", 0.08)
trial12_metrics <- perfom_neuralnet_calculations_one(104, 4, "logistic", 0.08)
trial13_metrics <- perfom_neuralnet_calculations_one(104, 5, "logistic", 0.08)
trial14_metrics <- perfom_neuralnet_calculations_one(104, 6, "logistic", 0.08)
trial15_metrics <- perfom_neuralnet_calculations_one(104, 8, "logistic", 0.08)

# find best learning rate value for above best trial
trial16_metrics <- perfom_neuralnet_calculations_one(104, 4, "logistic", 0.04)
trial17_metrics <- perfom_neuralnet_calculations_one(104, 4, "logistic", 0.02)
trial18_metrics <- perfom_neuralnet_calculations_one(104, 4, "logistic", 0.1)

# find optimal activation function for above best trial
trial19_metrics <- perfom_neuralnet_calculations_one(104, 4, "tanh", 0.08)
# Optimal values for 1 hidden layer network with seed 104: (4), logistic, 0.08, trial12




## BEST SINGLE HIDDEN LAYER NETWORK ##
set.seed(104) 
nn_best_single <- neuralnet(rate ~ rate_1, data = train_hidden_1, hidden = c(4), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
nn_best_single.test_prediction <- predict(nn_best_single, test_hidden_1)
plot(nn_best_single)




## BEST TWO HIDDEN LAYER NETWORK ##
set.seed(104)
nn_best_double <- neuralnet(formula = rate ~ rate_1, data = train_hidden_2, hidden = c(4, 4), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
nn_best_double.test_prediction <- predict(nn_best_double, test_hidden_2)
plot(nn_best_double)




## plot graph of best network ##
plot(test_hidden_1[, 1], type = "l", col = "blue", lwd = 2, xlab = "index", ylab = "Rates")
lines(nn_best_single.test_prediction, type = "l", col = "red", lwd = 2)
title("AR(1) Time Series - Single Hidden Layer")
legend("bottomright", legend = c("Actual", "Predicted"), col = c("blue", "red"), bty = "n", cex=1, lwd = 5, text.font = 7)

plot(test_hidden_2[, 1], type = "l", col = "blue", lwd = 2, xlab = "index", ylab = "Rates")
lines(nn_best_double.test_prediction, type = "l", col = "red", lwd = 2)
title("AR(1) Time Series - Two Hidden Layers")
legend("bottomright", legend = c("Actual", "Predicted"), col = c("blue", "red"), bty = "n", cex=1, lwd = 5, text.font = 7)
