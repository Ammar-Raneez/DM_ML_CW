library(readxl)
library(factoextra)
library(NbClust)
#for evaluations
library(caret)
library(ggplot2)
library(cluster)


#read in the data
vehicle_data <- read_excel("../../vehicles.xlsx")
#remove samples column due to it only being a counter
vehicle_data <- vehicle_data[c(-1)] 
#data type
class(vehicle_data)
#summary(vehicle_data)
#any missing values?
sum(is.na(vehicle_data))


## DATA PREPROCESSING SECTION ##
#normalize the input features
scaled_vehicle_data <- apply(vehicle_data[-c(ncol(vehicle_data))], 2, scale)
#bind the class column back into the normalized data set
scaled_vehicle_data <- cbind(scaled_vehicle_data, vehicle_data[c(ncol(vehicle_data))])

#visualize outliers of data set
boxplot(scaled_vehicle_data[-c(ncol(vehicle_data))], las = 2, col = c("lightgreen", "lightblue"), ylab = "Normalized Values")
#Based on box plot, following columns have outliers
have_outliers <- c("Rad.Ra", "Pr.Axis.Ra", "Max.L.Ra", "Pr.Axis.Rect", "Sc.Var.Maxis", "Sc.Var.maxis", "Skew.Maxis", "Skew.maxis", "Kurt.maxis")

#loop through all the columns that have outliers
for (outlier in have_outliers) {
  number_of_rows = nrow(scaled_vehicle_data)
  
  #get quantile values - 25th and 75th percentiles
  quant <- quantile(scaled_vehicle_data[, outlier], probs = c(0.25, 0.75))
  
  #calculate IQR for the respective columns
  iqr <- IQR(scaled_vehicle_data[1:number_of_rows, outlier])
  
  #An outlier is a point below lower quartile and above upper quartile
  upper_quartile <- quant[2] + 1.5*iqr
  lower_quartile <- quant[1] - 1.5*iqr
  
  #subset the data set, such that the outliers are filtered out
  scaled_vehicle_data <- subset(
    scaled_vehicle_data, 
    scaled_vehicle_data[c(-ncol(vehicle_data))][, outlier] > lower_quartile & 
      scaled_vehicle_data[c(-ncol(vehicle_data))][, outlier] < upper_quartile
  ) 
}

#data set can now be split into input and output
scaled_vehicle_data_inputs <- scaled_vehicle_data[c(-ncol(vehicle_data))]
scaled_vehicle_data_output <- scaled_vehicle_data[c(ncol(vehicle_data))]

#plot box plot again to check whether outliers have been removed
boxplot(scaled_vehicle_data_inputs, las = 2, col = c("lightgreen", "lightblue"), ylab = "Normalized Values")


## KMEANS CLUSTER DEFINITION ##
#Manual
manual_cluster_size <- 4
#Elbow Method
fviz_nbclust(scaled_vehicle_data_inputs, kmeans, method = "wss") + labs(subtitle = "Elbow")
#Silhouette Method
fviz_nbclust(scaled_vehicle_data_inputs, kmeans, method = "silhouette") + labs(subtitle = "Silhouette")
#Gap statistic Method
fviz_nbclust(scaled_vehicle_data_inputs, kmeans, method = "gap_stat", verbose = F) + labs(subtitle = "Gap statistic")


## KMEANS ANALYSIS ##
#set seed for reproducible runs
set.seed(101)

#Manual
manual_kmean <- kmeans(scaled_vehicle_data_inputs, centers = manual_cluster_size, nstart = 50)
clusplot(scaled_vehicle_data, manual_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_manual <- table(scaled_vehicle_data_output$Class, manual_kmean$cluster)

set.seed(102)
#Elbow
elbow_kmean <- kmeans(scaled_vehicle_data_inputs, centers = 3, nstart = 50)
clusplot(scaled_vehicle_data, elbow_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_elbow <- table(scaled_vehicle_data_output$Class, elbow_kmean$cluster)

set.seed(103)
#Silhouette & Gap Stat
silhouette_gap_kmean <- kmeans(scaled_vehicle_data_inputs, centers = 2, nstart = 50)
clusplot(scaled_vehicle_data, silhouette_gap_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_silhouette_gap <- table(scaled_vehicle_data_output$Class, silhouette_gap_kmean$cluster)


#table evaluations for each method
#comparison_table_manual
#comparison_table_elbow
#comparison_table_silhouette_gap

#convert to factors, since confusion matrix expects factors
ground_truth_as_factor <- as.factor(scaled_vehicle_data_output$Class)
#predictions_as_factor <- as.factor(manual_kmean$cluster)
#predictions_as_factor <- as.factor(elbow_kmean$cluster)
predictions_as_factor <- as.factor(silhouette_gap_kmean$cluster)

#since the predictions comes out as 1,2,3,4, The categorical data of the ground truth is converted to numerical
ground_truth_as_numeric <- as.numeric(ground_truth_as_factor)
#the data type has to be changed back to factor, since CM can accept only factors
ground_truth_as_factor <- as.factor(ground_truth_as_numeric)
confusionMatrix(data = predictions_as_factor, reference = ground_truth_as_factor, mode = 'prec_recall')