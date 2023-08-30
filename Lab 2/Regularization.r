library(caret)
library(glmnet)
library(tidyr)

# Reading data from provided CSV file
tecator <- read.csv("tecator.csv", header = TRUE, sep = ",")

# splitting the data into 50% training and 50% test sets
n <- dim(tecator)[1]
set.seed(12345)
id <- sample(1 : n, floor(n * 0.50))
train_dataset <- tecator[id, ]
test_dataset <- tecator[-id, ]

# Task 1
# Creating linear regression to the training data
linearRegression_model <- lm(Fat~., data = train_dataset[,2:102])
summary(linearRegression_model)

# Calculating Mean Squared error of train and test sets
testset_predictions <- predict(linearRegression_model, test_dataset[,2:102])
train_mse <- mean((train_dataset$Fat - predict(linearRegression_model)) ^ 2)
test_mse <- mean((test_dataset$Fat - testset_predictions) ^ 2)
print(train_mse)
print(test_mse)

# Task 3
# Predictor and outcome variables
x <- as.matrix(train_dataset[,2:101])
y <- train_dataset$Fat

# Creating LASSO regression model
lasso_model <- cv.glmnet(x, y, alpha = 1)

# finding lambda value in which lasso model has three features
lambda_lasso = lasso_model$lambda[which(lasso_model$glmnet.fit$df == 3)]
print(lambda_lasso)
lasso_model_three_features = glmnet(x, y, alpha = 1, lambda = lambda_lasso[1])

plot(lasso_model$glmnet.fit , "lambda", label=TRUE)

# Task 4
# Creating RIDGE Regression
ridge_model <- cv.glmnet(x, y, alpha = 0)
plot(ridge_model$glmnet.fit , "lambda", label=TRUE)

# finding lambda value in which lasso model has three features
lambda_ridge = ridge_model$lambda[which(ridge_model$glmnet.fit$df == 3)]

# Task 5
plot(lasso_model)
min_lambda = lasso_model$lambda.min
print(min_lambda)
min_lambda_lasso_model = glmnet(x, y, alpha = 1, lambda = min_lambda)
coef(min_lambda_lasso_model)

ynew_test=predict(lasso_model, newx=as.matrix(test_dataset[, 2:101]), type="response", s = "lambda.min")
plot(test_dataset$Fat, ynew_test)
abline(a=0, b= 1)

