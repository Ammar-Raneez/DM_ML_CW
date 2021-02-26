library(readxl)

#read in the data
vehicle_data <- read_excel("../../vehicles.xlsx")

#data type
class(vehicle_data)

#any missing values?
sum(is.na(vehicle_data))

#separate inputs and outputs
vehicle_data_output_features <- vehicle_data[c("Class")]
vehicle_data_input_features <- vehicle_data[c(-ncol(vehicle_data))]

#normalize the input features
scaled_vehicle_data <- apply(vehicle_data_input_features, 2, scale)

