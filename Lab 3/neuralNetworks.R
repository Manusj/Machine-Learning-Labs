library(neuralnet)
set.seed(1234567890)

# Task 1
var <- runif(500, 0, 15)
mydata <- data.frame(var, Sin=sin(var)) 
# Training data set
train <- mydata[1:25,] 
# Test data set
test <- mydata[26:500,] 

# Random initialization of the weights in the interval [-1, 1]

nn <- neuralnet(Sin ~ var, train, hidden = 10, linear.output=TRUE)

pred_train <- predict(nn, newdata=train)
pred_test <- predict(nn, newdata=test)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train[,1],train[,2], cex=3, xlab="Sample Value", ylab = "Sine of Sample Values")
points(test[,1], test[,2], col = "blue", cex=1)
points(test[,1], pred_test, col="red", cex=1)
legend("bottomleft",fill=c("black","blue","red"), legend=c("Train data point","Test data points"
                                                           ,"Predication Points"),bty="n")

mse <- function(prediction, observation) {
  return (mean((observation - prediction)^2))
}

print(mse(pred_train,train[,2]))
print(mse(pred_test,test[,2]))


# Task 2

linear <- function(x) x 

relu <- function(x) ifelse(x>=0,x,0)

softplus <- function(x) log(1+exp(x))


nn_linear <- neuralnet(Sin ~ var, train, hidden = 10, linear.output=TRUE, act.fct = linear)
predict_test_linear <- predict(nn_linear, newdata=test)

nn_relu <- neuralnet(Sin ~ var, train, hidden = 10, linear.output=TRUE, act.fct = relu)
predict_test_relu <- predict(nn_relu, newdata=test)

nn_softplus <- neuralnet(Sin ~ var, train, hidden = 10, linear.output=TRUE, act.fct = softplus)
predict_test_softplus <- predict(nn_softplus, newdata=test)

plot(test[,1], test[,2], cex = 2, xlab="Sample Value", ylab = "Sine of Sample Values")
points(test[,1], predict_test_linear, col = "blue")
points(test[,1], predict_test_relu, col = "red")
points(test[,1], predict_test_softplus, col = "green")
legend("bottomleft",fill=c("black","blue","red", "green"), legend=c("Actual Test data point",
                          "Linear Prediction","Relu Prediction","Softplus Prediction"),bty="n")

# Task 3
set.seed(1234567890)
var <- runif(500, 0, 50)
newdata_test <- data.frame(var, Sin=sin(var)) 
newprediction <- predict(nn, newdata=newdata_test)
plot(newdata_test[,1], newdata_test[,2], ylim=c(-3,3), xlab="Sample Value", ylab = "Sine of Sample Values")
points(newdata_test[,1], newprediction, col = "blue")
legend("bottomleft",fill=c("black","blue"), legend=c("Actual Test data point","Prediction Data Points"),bty="n")
grid(50,10)

# Task 4
plot(nn)
print(nn$weights)
sig <- function(x)
{
  return(1/(1+exp(-(x))))
}


# Task 5
data <- runif(500, 0, 10)
data_test <- data.frame(data, Sin=sin(data))
nn_reversePrediction <- neuralnet(data ~ Sin, data_test, hidden = 10, threshold = 0.1)
newprediction <- predict(nn_reversePrediction, newdata=data_test)
plot(data_test[,2], data_test[,1], ylab="Sample Value", xlab = "Sine of Sample Values")
points(data_test[,2], newprediction, col = "blue")
legend("topleft",fill=c("black","blue"), legend=c("Actual  points","Prediction  Points"),bty="n")
