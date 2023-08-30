#Task 1
# Reading the dataset and plotting the scatter plot based in on age and glucose concentration
DF=read.csv("pima-indians-diabetes.csv")
plot(DF[,2], DF[,8], main="Scatterplot",xlab="Glucose Concentration ", ylab="Age", col= ifelse(DF[,9]==1, 'red', 'green'))
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")

#Task 2
# Fitting the logistic regression model using glm and plotting the scatter plot based in the predictions of the model
logistic <- glm(DF[,9] ~ DF[,2]+DF[,8], data=DF, family="binomial")
summary(logistic)
plot(DF[,2], DF[,8], main="Scatterplot", xlab="Glucose Concentration ", ylab="Age", col= ifelse(predict(logistic, type = "response")>0.5, 'red', 'green'))
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")
missclass=function(X,X1)
{
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
# Finding misclassifcation error of model
olddata <- predict(logistic, type = "response")
a <- predict(logistic, type = "response")
a <- ifelse(a>0.5, 1, 0)

resulterror <- missclass(a, DF[,9])
print(resulterror)
print(summary(logistic))


# Task 3
# Plotting the decision boundary
tetha = coefficients(logistic)
intercept = -(tetha[1]/tetha[3])
slope = -(tetha[2]/tetha[3])
abline(a=intercept, b = slope, col="blue")
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")



# task 4
# plotting scatterplots with r = 0.2 and r = 0.8
a <- predict(logistic, type = "response")
a <- ifelse(a>0.2, 1, 0)
plot(DF[,2], DF[,8], main="Scatterplot", xlab="Glucose Concentration ", ylab="Age", col= ifelse(predict(logistic, type = "response")>0.2, 'red', 'green'))
plot(DF[,2], DF[,8], main="Scatterplot", xlab="Glucose Concentration ", ylab="Age", col= ifelse(predict(logistic, type = "response")>0.8, 'red', 'green'))
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")

# Task 5
# creating new data entires for basis expansion
DF[,10] = DF[,2]^4
DF[,11] = (DF[,2]^3)*DF[,8]
DF[,12] = (DF[,2]^2)*(DF[,8]^2)
DF[,13] = DF[,2]*(DF[,8]^3)
DF[,14] = DF[,2]^4

# fitting new model with added paramters form basis expansion
log_basisexpansion <- glm(DF[,9] ~ DF[,2]+DF[,8]+DF[,10]+DF[,11]+DF[,12]+DF[,13]+DF[,14], data=DF, family="binomial")
summary(log_basisexpansion)
bas <- predict(log_basisexpansion, type = "response")
bas <- ifelse(bas>0.5, 1, 0)
# plotting scatter plot based in new values predicted from newly fiited basis expansion model
plot(DF[,2], DF[,8], main="Scatterplot", xlab="Glucose Concentration ", ylab="Age", col= ifelse(predict(log_basisexpansion, type = "response")>0.5, 'red', 'green'))
legend("topleft",fill=c("red","green"), legend=c("diabetic","non diabetic"),bty="n")
basisexpansionerror <- missclass(bas, DF[,9])
print(basisexpansionerror)
print(resulterror)




 






