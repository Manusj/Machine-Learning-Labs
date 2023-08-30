library(ggplot2)
# Task 1
dataset_communities = read.csv("communities.csv")
dataset_communities[,-101] = scale(dataset_communities[,-101])
scaled_dataset = as.matrix(dataset_communities)
# Finding covarience matrix of features
covarience_matrix = cov(dataset_communities)
# Finding eigen vectors and values of covarience matrix which are 
# the principle components and the varience of data projected to the principle components 
eigen = eigen(covarience_matrix)
eigen_vectors = as.matrix(eigen$vectors)
# Printing cummulative sum of contribution of eigen values
print(cumsum(eigen$values/sum(eigen$values)))
tranformed_data = scaled_dataset %*% eigen_vectors

# Task 2
pca_res = princomp(scaled_dataset)
summary(pca_res)
pca_loadings = as.data.frame.matrix(pca_res$loadings)
# plotting PC1 Scores
plot(pca_loadings[,1])

# Finding features in PC1 that contribute more than 0.1 
pca_eigen_vector = as.data.frame.matrix(pca_res$loadings)
pc1 = as.data.frame(pca_eigen_vector[,1] , row.names = row.names(pca_eigen_vector))
row.names(pc1)[which(abs(pc1[,1])>0.1)]

# Finding five feature that contribute most in PC1 and printing their values
highest_contributors_pc1 = tail(sort(unlist(abs(pc1), use.names = FALSE)), 5)
row.names(pc1)[which(abs(pc1[,1]) %in% highest_contributors_pc1)]
unlist(pc1, use.names = FALSE)[which(abs(pc1[,1]) %in% highest_contributors_pc1)]

# Finding five feature that contribute most in PC2
pc2 = as.data.frame(pca_eigen_vector[,2] , row.names = row.names(pca_eigen_vector))
highest_contributors_pc2 = tail(sort(unlist(abs(pc2), use.names = FALSE)), 6)
row.names(pc2)[which(abs(pc2[,1]) %in% highest_contributors_pc2)]
unlist(pc2, use.names = FALSE)[which(abs(pc2[,1]) %in% highest_contributors_pc2)]

# Plotting PC scores in the coordiantes of PC1 and PC2
pca_scores$ViolentCrimesPerPop <- dataset_communities$ViolentCrimesPerPop
ggplot(data = pca_scores, aes(x = Comp.1, y = Comp.2, color = ViolentCrimesPerPop)) +
  geom_point(aes(alpha = 1)) + 
  scale_color_gradient(low="green", high="red")

# Task 3
# Scaling responce 
dataset_communities[,101] = scale(dataset_communities[,101])
# Splitting scaled data to 50% training and 50% test
n <- dim(dataset_communities)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train_dataset <- data.frame(dataset_communities[id,])
test_dataset <- data.frame(dataset_communities[-id,])
# Training Linear Regression Model
linearRegression_model <- lm(ViolentCrimesPerPop~.,data = train_dataset)
summary(linearRegression_model)

# Function to compute Mean Sqaured Error
mse <- function(prediction, datatset)
{
  return(mean((datatset$ViolentCrimesPerPop - prediction) ^ 2))
}

train_error_linearRegression_model = mse(predict(linearRegression_model), train_dataset)
test_error_linearRegression_model = mse(predict(linearRegression_model, test_dataset), test_dataset)

print(train_error_linearRegression_model)
print(test_error_linearRegression_model)
print(min(test_dataset$ViolentCrimesPerPop))


# Task 4
train_error_iteration = list()
test_error_iteration = list()
count <- 0

theta_optim <- matrix(0, nrow = 100, ncol = 1)
min_mse_test <- 10
# defining Cost function and storing training and test errors
costFunction <- function(theta, data)
{
  count <<- count + 1
  
  # computing cost based on prediction of current theta
  input_data = as.matrix(data[,-101])
  prediction = input_data %*% theta
  cost = mean((prediction - data[,101])^2)
  
  # computing and storing train and test errors based on current theta
  prediction_train = as.matrix(train_dataset[,-101]) %*% theta
  prediction_test = as.matrix(test_dataset[,-101]) %*% theta
  
  train_error_iteration[count] <<- mse(prediction_train, train_dataset)
  test_error_iteration[count] <<- mse(prediction_test, test_dataset)
  
  # storing theta having minimum value of test set mean squared error
  if(mse(prediction_test, test_dataset) < min_mse_test)
  {
    min_mse_test <<- test_error_iteration[count]
    theta_optim <<- theta
  }

  return(cost)
}

# Using optim function to optimize for theta
theta = matrix(0, nrow = 100, ncol = 1)
optim_theta <- optim(par = theta,
                   fn = costFunction, 
                   data =train_dataset,
                   method="BFGS")

# Plotting dependance of training and test error of iterations
plot(x = c(501:length(train_error_iteration)), y=train_error_iteration[-(1:500)],  
     xlab = "Iterations", ylab = "Mean Squared Error", type = "l", col ="red", ylim=c(0,1))

lines(x = c(501:length(test_error_iteration)), y=test_error_iteration[-(1:500)],  
      xlab = "Iterations", ylab = "Mean Squared Error", type = "l", col ="blue", ylim=c(0,1))

legend("topright" , legend=c("Training Mean Squared error", "Test Squared error"), 
       col=c("red", "blue"), lty=1:1, cex=0.8)

min_value_testError = test_error_iteration[which.min(test_error_iteration)]
abline(h=min_value_testError, lty=3, col ="blue")

min_test_error_iteration = which.min(test_error_iteration)
print(min_test_error_iteration)

# Finding Predictions and computing mean square error for optimal model
prediction_train_optimalModel = as.matrix(train_dataset[,-101]) %*% theta_optim
prediction_test_optimalModel = as.matrix(test_dataset[,-101]) %*% theta_optim

train_error_optimalModel <- mse(prediction_train_optimalModel, train_dataset)
test_error_optimalModel <- mse(prediction_test_optimalModel, test_dataset)

print(train_error_optimalModel)
print(test_error_optimalModel)

