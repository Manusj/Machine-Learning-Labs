# Task 1
library(kknn)
optdigits <- read.csv("optdigits.csv", header = FALSE, sep = ",")
n = dim(optdigits)[1]
set.seed(12345)

# splitting into train, test and validation dataset
train.rows <- sample(1 : n, floor(n * 0.50))
train.data <- optdigits[train.rows, ]
valid.rows <- setdiff(1 : n, train.rows)
set.seed(12345)
valid <- sample(valid.rows, floor(n * 0.25))
valid.data = optdigits[valid, ]
test.rows <- setdiff(valid.rows, valid)
test.data = optdigits[test.rows, ]
ƒ
# Task 2
missclass=function(X,X1)
{
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

# fitting model based on training and testing model with training data itself and calculating misclassifications rate of training data
target_category <- as.factor(optdigits[train.rows, 65])
train_model <- kknn(target_category~., train = train.data, test = train.data, k = 30, kernel = "rectangular")
train_pred <- as.integer(predict(train_model)) # round off to 0 or 1predict(train_pred)
train_confusionmatrix <- table(train.data[,65], train_pred)
train_confusionmatrix
train_misclassification_error = missclass(train.data[,65], train_pred)
train_misclassification_error

# fitting model based on training and testing model with test data itself and calculating misclassifications rate of test data
target_category <- as.factor(optdigits[train.rows, 65])
test_model <- kknn(target_category~., train = train.data, test = test.data, k = 30, kernel = "rectangular")
test_pred <- as.integer(predict(test_model)) # round off to 0 or 1predict(test_pred)
test_confusionmatrix <- table(test.data[,65], test_pred)
test_confusionmatrix
test_misclassification_error = missclass(test.data[,65], test_pred)
test_misclassification_error

# Task 3
# Finding easy prediction with with probability same as 8
easy_perdiction_8 = which(predict(train_model)==8)
# Finding hard prediction with with highest and lowest prediction of 8 
# i.e prediction having values just greater than 7.5 and predictions having probablity just less than 8.5 
hard_perdictions_min_8 = which(predict(train_model)==min(predict(train_model)[predict(train_model)>7.5]))
hard_perdictions_max_8 = which(predict(train_model)==max(predict(train_model)[predict(train_model)<8.5]))


# displaying images as heat map
imageVector = as.matrix(train.data[54,1:64])
imageMatrix = matrix(imageVector,8,8,byrow = TRUE)
heatmap(imageMatrix, Rowv = NA, Colv = NA, scale = "row")

# Fitting kknn model and predicting it based on validation data and calculating valdation data misclassification error
target_category <- optdigits[train.rows, 65]
valid_model <- kknn(target_category~., train = train.data, test = valid.data, k = 30, kernel = "rectangular")
valid_pred <- as.integer(predict(valid_model)+0.5) # round off to 0 or 1predict(train_pred)
valid_confusionmatrix <- table(valid.data[,65], valid_pred)
valid_confusionmatrix
valid_misclassification_error = missclass(valid.data[,65], valid_pred)
valid_misclassification_error

# Task 4
# Finding training data misclassifcation error for models with k = 1,...30
k_missclassifcationError_train = list()

for (i in 1:30) {
  train_model_k <- kknn(target_category~., train = train.data, test = train.data, k = i, kernel = "rectangular")
  train_predict_k <- as.integer(predict(train_model_k)+0.5)
  k_missclassifcationError_train[i] <- missclass(train.data[,65], train_predict_k)
}

# Finding validation data misclassifcation error and cross entropy for models with k = 1,...30
k_missclassifcationError_valid = list()
k_crossEntropy_valid = list()

for (i in 1:30) {
  valid_model_k <- kknn(target_category~., train = train.data, test = valid.data, k = i, kernel = "rectangular")
  valid_predict_k <- as.integer(predict(valid_model_k)+0.5)
  valid_predict_prob_k = predict(valid_model_k)
  k_missclassifcationError_valid[i] <- missclass(valid.data[,65], valid_predict_k)
  # Task 5
  k_crossEntropy_valid[i] = -sum(valid.data[,65]*log(valid_predict_prob_k + 10^(-15)))
}

# Task 4
# plotting train and validation misclassification error vs value of K graph
plot(x = c(1:30), y=k_missclassifcationError_train,  xlab = "K", ylab = "Error", type = "b", col ="red")
lines(x = c(1:30), y=k_missclassifcationError_valid,  xlab = "K", ylab = "Error", type = "b", col ="blue")
legend(1,0.18 , legend=c("Training Misclassification error", "Test Misclassification error"), col=c("red", "blue"), lty=1:1, cex=0.8)

# Task 5
# plotting valdaition data cross entropy error vs value of K graph
plot(x = c(1:30), y=k_crossEntropy_valid,  xlab = "K", ylab = "Cross Entropy", type = "b", col ="green")
