library(readxl)
library(neuralnet)
library(fpp2)
library(ggplot2)
library(Metrics)

exchange_data <- read_excel("../../ExchangeUSD.xlsx")


# remove second unneeded column
exchange_data <- exchange_data[c(-2)] 
# convert date into time series
exchange_data <- ts(exchange_data)
exchange_data <- as.data.frame(exchange_data)
# rename col names for easiness
colnames(exchange_data) <- c("date", "rate")


# scale the exchange rates
maxs <- apply(exchange_data, 2, max)
mins <- apply(exchange_data, 2, min)
scaled_exchange_data <- as.data.frame(scale(exchange_data, center = mins, scale = maxs - mins))


# split into training and test
train <- scaled_exchange_data[0:400,]
test <- scaled_exchange_data[401:500,]


# Two hidden layers
# implement many neural networks with different hyper parameter values
perfom_neuralnet_calculations_two <- function(seed, hidden, activation, learning) {
  # set a seed for reproducible neural networks
  set.seed(seed) 
  # create the neural net and get predictions
  nn_trial <- neuralnet(rate ~ date, data = train, hidden = c(hidden, hidden), act.fct = activation, err.fct = "sse", lifesign = "full", learningrate = learning, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test)
  nn_trial.train_prediction <- predict(nn_trial, train)
  
  # convert the predictions into a data frame with the date column for plotting
  nn_trial_train_prediction_df <- data.frame(train[, 1], nn_trial.train_prediction)
  colnames(nn_trial_train_prediction_df) <- c("date", "rate")
  
  nn_trial_test_prediction_df <- data.frame(test[, 1], nn_trial.test_prediction)
  colnames(nn_trial_test_prediction_df) <- c("date", "rate")
  
  # evaluations using RMSE, MAE and MAPE
  rmse_nn_trial <- round(rmse(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  mae_nn_trial <- round(mae(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  mape_nn_trial <- round(mape(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}

# find optimal number of nodes for 2 hidden layers
trial0_metrics <- perfom_neuralnet_calculations_two(104, 8, "logistic", 0.08)
trial1_metrics <- perfom_neuralnet_calculations_two(104, 6, "logistic", 0.08)
trial2_metrics <- perfom_neuralnet_calculations_two(104, 5, "logistic", 0.08)
trial3_metrics <- perfom_neuralnet_calculations_two(104, 4, "logistic", 0.08)
trial4_metrics <- perfom_neuralnet_calculations_two(104, 3, "logistic", 0.08) 
trial5_metrics <- perfom_neuralnet_calculations_two(104, 2, "logistic", 0.08)

# find best learning rate value for above best trial, use same seed
trial6_metrics <- perfom_neuralnet_calculations_two(104, 6, "logistic", 0.04)
trial7_metrics <- perfom_neuralnet_calculations_two(104, 6, "logistic", 0.02)
trial8_metrics <- perfom_neuralnet_calculations_two(104, 6, "logistic", 0.1)

# find optimal activation function
trial9_metrics <- perfom_neuralnet_calculations_two(104, 6, "tanh", 0.08)
### Optimal values for 2 hidden layer network with seed 104: (3, 3), logistic, 0.08, trial4


# Single hidden layer
perfom_neuralnet_calculations_one <- function(seed, hidden, activation, learning) {
  # set a seed for reproducible neural networks
  set.seed(seed) 
  # create the neural net and get predictions
  nn_trial <- neuralnet(rate ~ date, data = train, hidden = c(hidden), act.fct = activation, err.fct = "sse", lifesign = "full", learningrate = learning, rep = 10, linear.output = T)
  nn_trial.test_prediction <- predict(nn_trial, test)
  nn_trial.train_prediction <- predict(nn_trial, train)
  
  # convert the predictions into a data frame with the date column for plotting
  nn_trial_train_prediction_df <- data.frame(train[, 1], nn_trial.train_prediction)
  colnames(nn_trial_train_prediction_df) <- c("date", "rate")
  
  nn_trial_test_prediction_df <- data.frame(test[, 1], nn_trial.test_prediction)
  colnames(nn_trial_test_prediction_df) <- c("date", "rate")
  
  # evaluations using RMSE, MAE and MAPE
  rmse_nn_trial <- round(rmse(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  mae_nn_trial <- round(mae(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  mape_nn_trial <- round(mape(test[, 2], nn_trial_test_prediction_df[, 2]), digits = 4)
  
  return(c(rmse_nn_trial, mae_nn_trial, mape_nn_trial))
}

# find optimal number of nodes for 1 hidden layer
trial1x_metrics <- perfom_neuralnet_calculations_one(104, 8, "logistic", 0.08)
trial10_metrics <- perfom_neuralnet_calculations_one(104, 6, "logistic", 0.08)
trial11_metrics <- perfom_neuralnet_calculations_one(104, 5, "logistic", 0.08)
trial12_metrics <- perfom_neuralnet_calculations_one(104, 4, "logistic", 0.08)
trial13_metrics <- perfom_neuralnet_calculations_one(104, 3, "logistic", 0.08)
trial14_metrics <- perfom_neuralnet_calculations_one(104, 2, "logistic", 0.08)

# find best learning rate value for above best trial
trial15_metrics <- perfom_neuralnet_calculations_one(104, 3, "logistic", 0.04)
trial16_metrics <- perfom_neuralnet_calculations_one(104, 3, "logistic", 0.02)
trial17_metrics <- perfom_neuralnet_calculations_one(104, 3, "logistic", 0.1)

# find optimal activation function
trial18_metrics <- perfom_neuralnet_calculations_one(104, 3, "tanh", 0.08)
### Optimal values for 1 hidden layer network with seed 104: (3), logistic, 0.08, trial13


# best single hidden layer network, trial 13
set.seed(104) 
nn_best_single <- neuralnet(rate ~ date, data = train, hidden = c(3), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
nn_best_single.test_prediction <- predict(nn_best_single, test)
nn_best_single.train_prediction <- predict(nn_best_single, train)

nn_best_single_train_prediction_df <- data.frame(train[, 1], nn_best_single.train_prediction)
colnames(nn_best_single_train_prediction_df) <- c("date", "rate")

nn_best_single_test_prediction_df <- data.frame(test[, 1], nn_best_single.test_prediction)
colnames(nn_best_single_test_prediction_df) <- c("date", "rate")
plot(nn_best_single)


# best 2 hidden layer network: trial 4
set.seed(104) 
nn_best_double <- neuralnet(rate ~ date, data = train, hidden = c(6, 6), act.fct = "logistic", err.fct = "sse", lifesign = "full", learningrate = 0.08, rep = 10, linear.output = T)
nn_best_double.test_prediction <- predict(nn_best_double, test)
nn_best_double.train_prediction <- predict(nn_best_double, train)

nn_best_double_train_prediction_df <- data.frame(train[, 1], nn_best_double.train_prediction)
colnames(nn_best_double_train_prediction_df) <- c("date", "rate")

nn_best_double_test_prediction_df <- data.frame(test[, 1], nn_best_double.test_prediction)
colnames(nn_best_double_test_prediction_df) <- c("date", "rate")
plot(nn_best_double)


# plot graph of best network
plot(scaled_exchange_data, type = "l", col = "lightblue", lwd = 2)
lines(nn_best_single_train_prediction_df, type = "l", col = "gold", lwd = 2)
lines(nn_best_single_test_prediction_df, type = "l", col = "red", lwd = 2)
title("Actual VS Predicted")

plot(scaled_exchange_data, type = "l", col = "lightblue", lwd = 2)
lines(nn_best_double_train_prediction_df, type = "l", col = "gold", lwd = 2)
lines(nn_best_double_test_prediction_df, type = "l", col = "red", lwd = 2)
title("Actual VS Predicted")
