# Reading the data-set "vehicles.xlsx"
df <- read_excel("../../ExchangeUSD.xlsx")
View(df)

# Dropping unwanted columns
df = subset(df, select = -c(Wdy, `YYYY/MM/DD`))
df$Rate = df$`USD/EUR`
df_copy_ar2 = df

# Renaming the Columns of the Data-frame
df = setNames(df, c("Rate_Original", "Rate_AR1"))
View(df)

# Shifting the Rate_AR1 column rows by one down below
df['Rate_AR1'] <- c(NA, head(df['Rate_AR1'], dim(df)[1] - 1)[[1]])

# Removing the first row from the dataframe because there is a null value present in the Rate_AR1 column
df = na.omit(df)

# normalization
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalized data
df.normalized = data.frame(lapply(df, normalize))
View(df.normalized)

# Creating the index colummn for the dataset
df.normalized$Index = seq.int(nrow(df))

# Creating the Training Data
training_data = df.normalized[1:400,]

# Creating the Testing Data
testing_data = df.normalized[401:499,]
View(testing_data)

# Training a model on the data
set.seed(101)

# Training the model
model <- neuralnet(Rate_Original~Rate_AR1,
                   # hidden=c(6),
                   hidden=c(6,6),
                   data = training_data, 
                   act.fct = "logistic", 
                   linear.output = TRUE,
                   err.fct = "sse", 
                   learningrate = 0.08)

testing_data_actual_rate = data.frame(testing_data$Rate_Original)
predict_result = predict(model, testing_data)
predict_result_2 = predict(model, testing_data_actual_rate)

View(predict_result)

# Plotting the graph
plot(testing_data$Index,testing_data$Rate_Original, 
     main = "Actual VS Predicted", xlab = "Index",
     ylab = "Rate", col = "black", type = "l")
lines(testing_data$Index, predict_result, col="red")

# Evaluating the model
actual = data.frame(testing_data$Rate_Original)
predicted = predict_result

# Calculating the Mean Absolute Error
mae = round(mae(actual$testing_data.Rate_Original, predicted) * 100, digits = 4)
print(paste("Mean Absolute Error: ", mae, " %", sep = ""))

# Calculating the Root Mean Squared Error
rmse = round(rmse(actual$testing_data.Rate_Original, predicted) * 100, digits = 4)
print(paste("Root Mean Squared Error: ", rmse, " %", sep = ""))

# Calculating the Mean Absolute Percentage Error Loss
mape = round(MAPE(actual$testing_data.Rate_Original, predicted) * 100, digits = 4)
print(paste("Mean Absolute Percentage Error Loss: ", mape, " %", sep = ""))