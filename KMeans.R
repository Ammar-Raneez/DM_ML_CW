library(readxl)
library(ggstatsplot)

#read in the data
vehicle_data <- read_excel("../../vehicles.xlsx")

#data type
class(vehicle_data)

#any missing values?
sum(is.na(vehicle_data))

#separate inputs and outputs
vehicle_data_output_features <- vehicle_data[c("Class")]
vehicle_data_input_features <- vehicle_data[c(-ncol(vehicle_data))]



# DATA PREPROCESSING SECTION #

#normalize the input features
scaled_vehicle_data <- apply(vehicle_data_input_features, 2, scale)

#visualize outliers of data set
boxplot(scaled_vehicle_data, las = 2, col = c("lightgreen", "lightblue"), ylab = "Normalized Values")
#Based on box plot, following columns have outliers
have_outliers <- c("Rad.Ra", "Pr.Axis.Ra", "Max.L.Ra", "Pr.Axis.Rect", "Sc.Var.Maxis", "Sc.Var.maxis", "Skew.Maxis", "Skew.maxis", "Kurt.maxis")

#loop through all the columns that have outliers
for (outlier in have_outliers) {
  number_of_rows = nrow(scaled_vehicle_data)
  #get quantile values
  quant <- quantile(scaled_vehicle_data[1:number_of_rows, outlier], probs = c(0.25, 0.75))
  
  #calculate IQR for the respective columns
  iqr <- IQR(scaled_vehicle_data[1:number_of_rows, outlier])
  
  #An outlier is a point below, lower quartile and above upper quartile
  upper_quartile <- quant[2] + 1.5*iqr
  lower_quartile <- quant[1] - 1.5*iqr
  
  #subset the data set, such that the outliers are filtered out
  scaled_vehicle_data <- subset(
    scaled_vehicle_data, 
    scaled_vehicle_data[1:number_of_rows, outlier] > lower_quartile & 
      scaled_vehicle_data[1:number_of_rows, outlier] < upper_quartile
  ) 
}

#plot box plot again to check whether outliers have been removed
boxplot(scaled_vehicle_data, las = 2, col = c("lightgreen", "lightblue"), ylab = "Normalized Values")



# KMEANS ANALYSIS - MANUAL CLUSTER SIZE #





# KMEANS ANALYSIS - AUTOMATED CLUSTER SIZE #
