# Part 1 of Assignment 2
# Reading data from provided CSV File
dataset_parkinsons <- read.csv("parkinsons.csv")
# Standardizing data other than subject number by scaling to get mean 0 and standard deviation 1
dataset_parkinsons <- scale(dataset_parkinsons)
# Splitting scaled data to 60% training and 40% test
n <- dim(dataset_parkinsons)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.6))
train_dataset <- data.frame(dataset_parkinsons[id,])
test_dataset <- data.frame(dataset_parkinsons[-id,])


# Part 2 of Assignment 2
# Creating a linear regression model with all voice characteristics as parameters and motor_UPDRS as the output
linearRegression_model <- lm(motor_UPDRS ~ 0+Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+Jitter.DDP
                             +Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5 +Shimmer.APQ11+Shimmer.DDA
                             +NHR+HNR+RPDE+DFA+PPE,data = train_dataset)
summary(linearRegression_model)
# calculating Mean Squared error of train and test sets
testset_predictions <- predict(linearRegression_model, test_dataset)
train_mse <- mean((train_dataset$motor_UPDRS - predict(linearRegression_model)) ^ 2)
test_mse <- mean((test_dataset$motor_UPDRS - testset_predictions) ^ 2)
print(train_mse)
print(test_mse)


# Part 3 
# Defining loglikelihood function that returns the loglikelihood 
# based on sigma(dispersion) and theta(learned) parameters
loglikelihood <- function(sigma, theta){
  n <- dim(train_dataset)[1]
  theta <- t(as.matrix(theta))
  input_data <- t(as.matrix(train_dataset[,7:22]))
  prediction <- theta %*% input_data
  se <- sum((train_dataset$motor_UPDRS - prediction) ^ 2)
  return (-(((n/2)*log(2*pi*(sigma^2)))+((1/(2*(sigma^2)))*se)))
}

# Defining Ridge function that returns the penalized loglikelihood 
# based on lambda(Regularization paramter) and theta(learned) parameters
Ridge <- function(params, lambda){
  sigma <- params[1]
  theta <- params[-1]
  negativemodel_loglikelihood <- -(loglikelihood(sigma, theta))
  return (negativemodel_loglikelihood+(lambda * (sum(theta^2))))
}

# Defining Ridge function that returns optimized value for learned paramters based on cost function defined above 
# based on lambda(Regularization paramter), theta(learned) parameters of linear regression model
# and sigma dispersion of linear regression model
RidgeOpt <- function(lambda, thetha, sigma)
{
  optimization_variables <- c(sigma, thetha)
  optim_par <- optim(par = optimization_variables,
                               fn = Ridge, 
                               lambda =lambda,
                               method="BFGS")
  return (optim_par$par[-1])
}

# Defining DF function that prints degress of freedom 
# based on lambda(Regularization paramter)
DF <- function(lambda){
  x <- as.matrix(train_dataset[,7:22])
  P <- x%*%(solve((t(x) %*% x)+(lambda*diag(16))))%*%t(x)
  print(sum(diag(P)))
}

# Task 4
# getting learned paramters and dispersion(calculated as standard deviation of residuals) of the linear regression model
intial_weights = coefficients(linearRegression_model)
initial_dispersion = sd(residuals(linearRegression_model))
# finding optimized paramters when lambda = 1,100 and 1000
optimisedWeight_lambda1 = t(as.matrix(RidgeOpt(1, intial_weights, initial_dispersion)))
optimisedWeight_lambda100 = t(as.matrix(RidgeOpt(100, intial_weights, initial_dispersion)))
optimisedWeight_lambda1000 = t(as.matrix(RidgeOpt(1000, intial_weights, initial_dispersion)))

train_data_martix = t(as.matrix(train_dataset[,7:22]))
test_data_matrix = t(as.matrix(test_dataset[,7:22]))

# finding predictions based on optimized paramters when lambda = 1,100 and 1000
predictions_lambda1_traindata = (optimisedWeight_lambda1 %*% train_data_martix)
predictions_lambda1_testdata = (optimisedWeight_lambda1 %*% test_data_matrix)
predictions_lambda100_traindata = (optimisedWeight_lambda100 %*% train_data_martix)
predictions_lambda100_testdata = (optimisedWeight_lambda100 %*% test_data_matrix)
predictions_lambda1000_traindata = (optimisedWeight_lambda1000 %*% train_data_martix)
predictions_lambda1000_testdata = (optimisedWeight_lambda1000 %*% test_data_matrix)

# calculating mean sqaure error baed on predictions when lambda = 1,100 and 1000
trainingMse_lambda1 = mean((train_dataset$motor_UPDRS - predictions_lambda1_traindata) ^ 2)
testMse_lambda1 = mean((test_dataset$motor_UPDRS - predictions_lambda1_testdata) ^ 2)
trainingMse_lambda100 = mean((train_dataset$motor_UPDRS - predictions_lambda100_traindata) ^ 2)
testMse_lambda100 = mean((test_dataset$motor_UPDRS - predictions_lambda100_testdata) ^ 2)
trainingMse_lambda1000 = mean((train_dataset$motor_UPDRS - predictions_lambda1000_traindata) ^ 2)
testMse_lambda1000 = mean((test_dataset$motor_UPDRS - predictions_lambda1000_testdata) ^ 2)

# prining results of mean square error and degrees of freedom when lambda = 1,100, 1000
print(trainingMse_lambda1)
print(testMse_lambda1)
DF(1)
print(trainingMse_lambda100)
print(testMse_lambda100)
DF(100)
print(trainingMse_lambda1000)
print(testMse_lambda1000)
DF(1000)