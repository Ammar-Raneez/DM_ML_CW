# Import Required packages
set.seed(500)
library(neuralnet)
library(MASS)

# Boston dataset from MASS
data <- Boston

# Normalize the data\
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins,
                              scale = maxs - mins))

# Split the data into training and testing set
index <- sample(1:nrow(data), round(0.75 * nrow(data)))
train_ <- scaled[index,]
test_ <- scaled[-index,]


# Build Neural Network
nn <- neuralnet(medv ~ crim + zn + indus + chas + nox
                + rm + age + dis + rad + tax +
                  ptratio + black + lstat,
                data = train_, hidden = c(5, 3),
                linear.output = TRUE, stepmax = 1e7)

# Predict on test data
pr.nn <- compute(nn, test_[,1:13])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(data$medv) - min(data$medv))
+ min(data$medv)
test.r <- (test_$medv) * (max(data$medv) - min(data$medv)) +
  min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_)


# Plot regression line
plot(data)
plot(test_$medv, pr.nn_, col = "red",
     main = 'Real vs Predicted')
abline(0, 1, lwd = 2)
