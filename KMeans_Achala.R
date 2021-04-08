library(readxl)
library(factoextra)
library(NbClust)
library(flexclust)
library(knitr)
library(tidymodels)
library(caret)
library(janitor)
library(ggplot2)
library(cluster)
library(tidyverse)
library(funtimes)

#read in the data
vehicle_data <- read_excel("../../vehicles.xlsx") %>% 
  # R variable convention (underscore separation)
  # %>% get result and pass to
  janitor::clean_names() %>% 
  mutate(class = as_factor(class))

#remove samples column due to it only being a counter
vehicle_data <- vehicle_data[c(-1)] 
#data type
class(vehicle_data)
#data view
summary(vehicle_data)
#any missing values?
sum(is.na(vehicle_data))


#determine outliers for each class
vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Van Outliers")

vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Saab Outliers")

vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Opel Outliers")  

vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Bus Outliers")


bus_data <- vehicle_data %>%
  filter(class == "bus") %>%
  mutate(across(1:18, ~squish(.x, quantile(.x, c(.05, .95)))))

van_data <- vehicle_data %>%
  filter(class == "van") %>%
  mutate(across(1:18, ~squish(.x, quantile(.x, c(.05, .95)))))

opel_data <- vehicle_data %>%
  filter(class == "opel") %>%
  mutate(across(1:18, ~squish(.x, quantile(.x, c(.05, .95)))))

saab_data <- vehicle_data %>%
  filter(class == "saab") %>%
  mutate(across(1:18, ~squish(.x, quantile(.x, c(.05, .95)))))


combined_vehicle_data <- bind_rows(list(bus_data, van_data, opel_data, saab_data)) %>%
  arrange()

combined_vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Bus Without Outliers")

combined_vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Van Without Outliers")

combined_vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Opel Without Outliers")

combined_vehicle_data %>%
  pivot_longer(1:18, names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class, value, median)) %>%
  ggplot(aes(class, value, fill = reorder(labels, value))) + 
  geom_boxplot() + 
  labs(title = "Saab Without Outliers")


#remove class column, since all numerical values will be used for training
vehicle_data_inputs <- combined_vehicle_data %>%
  select(-class)

#scale the input features
vehicle_data_inputs <- vehicle_data_inputs %>%
  mutate(across(everything(), scale))


#plot box plot again to check whether outliers have been removed
boxplot(vehicle_data_inputs, las = 2, col = c("lightgreen", "lightblue"), ylab = "Normalized Values")


## KMEANS CLUSTER DEFINITION ##
#Manual
manual_cluster_size <- 4
#Elbow Method
fviz_nbclust(vehicle_data_inputs, kmeans, method = "wss") + labs(subtitle = "Elbow")
#Silhouette Method
fviz_nbclust(vehicle_data_inputs, kmeans, method = "silhouette") + labs(subtitle = "Silhouette")
#Gap statistic Method
fviz_nbclust(vehicle_data_inputs, kmeans, method = "gap_stat", verbose = F) + labs(subtitle = "Gap statistic")


## KMEANS ANALYSIS ##
#set seed for reproducible runs
set.seed(101)

#Manual
manual_kmean <- kmeans(vehicle_data_inputs, centers = manual_cluster_size, nstart = 50)
clusplot(combined_vehicle_data, manual_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_manual <- table(combined_vehicle_data$class, manual_kmean$cluster)

set.seed(102)
#Elbow
elbow_kmean <- kmeans(vehicle_data_inputs, centers = 3, nstart = 50)
clusplot(combined_vehicle_data, elbow_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_elbow <- table(combined_vehicle_data$class, elbow_kmean$cluster)

set.seed(103)
#Silhouette & Gap Stat
silhouette_gap_kmean <- kmeans(vehicle_data_inputs, centers = 2, nstart = 50)
clusplot(combined_vehicle_data, silhouette_gap_kmean$cluster, color = T, shade = T, labels = 4)
comparison_table_silhouette_gap <- table(combined_vehicle_data$class, silhouette_gap_kmean$cluster)


#convert to factors, since confusion matrix expects factors
ground_truth_as_factor <- as.factor(combined_vehicle_data$class)
predictions_as_factor_m <- as.factor(manual_kmean$cluster)
predictions_as_factor_e <- as.factor(elbow_kmean$cluster)
predictions_as_factor_sg <- as.factor(silhouette_gap_kmean$cluster)

#since the predictions comes out as 1,2,3,4, The categorical data of the ground truth is converted to numerical
ground_truth_as_numeric <- as.numeric(ground_truth_as_factor)
#the data type has to be changed back to factor, since CM can accept only factors
ground_truth_as_factor <- as.factor(ground_truth_as_numeric)
confusionMatrix(data = predictions_as_factor_m, reference = ground_truth_as_factor, mode = 'prec_recall')
confusionMatrix(data = predictions_as_factor_e, reference = ground_truth_as_factor, mode = 'prec_recall')
confusionMatrix(data = predictions_as_factor_sg, reference = ground_truth_as_factor, mode = 'prec_recall')