# Task 1
# Importing dataset
dataset_banks = read.csv2("bank-full.csv", stringsAsFactors=TRUE)
# Removing duration from dataset
dataset_banks = dataset_banks[,-12]

# Splitting into 40% training, 30% validation and 30% Test Set
n=dim(dataset_banks)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=dataset_banks[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=dataset_banks[id2,]
id3=setdiff(id1,id2)
test=dataset_banks[id3,]

# Task 2

library(tree)

missclass=function(X,X1)
{
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

# Fitting decision tree with default settings and finding test and validation misclassifcation rates
fit_decisionTree_default = tree(y~., data=train)

trainPredict_decisionTree_default=predict(fit_decisionTree_default, newdata=train, type="class")
validPredict_decisionTree_default=predict(fit_decisionTree_default, newdata=valid, type="class")
misclass_error_default_train = missclass(train$y, trainPredict_decisionTree_default)
misclass_error_default_valid = missclass(valid$y, validPredict_decisionTree_default)
print(misclass_error_default_train)
print(misclass_error_default_valid)

# Fitting decision tree with smallest node size 7000 and finding test and validation misclassifcation rates
fit_decisionTree_nodesize = tree(y~., data=train, control = tree.control(nobs = length(train$y) ,minsize = 7000))

trainPredict_decisionTree_nodesize=predict(fit_decisionTree_nodesize, newdata=train, type="class")
validPredict_decisionTree_nodesize=predict(fit_decisionTree_nodesize, newdata=valid, type="class")
misclass_error_nodesize_train = missclass(train$y, trainPredict_decisionTree_nodesize)
misclass_error_nodesize_valid = missclass(valid$y, validPredict_decisionTree_nodesize)
print(misclass_error_nodesize_train)
print(misclass_error_nodesize_valid)

# Fitting decision tree with minimum deviance = 0.0005 and finding test and validation misclassifcation rates
fit_decisionTree_devience = tree(y~., data=train, control = tree.control(nobs = length(train$y) ,mindev = 0.0005))

trainPredict_decisionTree_deviense=predict(fit_decisionTree_devience, newdata=train, type="class")
validPredict_decisionTree_deviense=predict(fit_decisionTree_devience, newdata=valid, type="class")
misclass_error_deviense_train = missclass(train$y, trainPredict_decisionTree_deviense)
misclass_error_deviense_valid = missclass(valid$y, validPredict_decisionTree_deviense)
print(misclass_error_deviense_train)
print(misclass_error_deviense_valid)

# Task 3

trainScore=rep(0,50)
testScore=rep(0,50)

for(i in 2:50) { 
  prunedTree=prune.tree(fit_decisionTree_devience,best=i) 
  pred=predict(prunedTree, newdata=valid,type="tree") 
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:50, trainScore[2:50], type="b", col="red", ylim=c(7500,12000), xlab = "Number of leaf nodes", ylab = "Deviance")
points(2:50, testScore[2:50], type="b", col="blue")
legend("topright",fill=c("red","blue"), legend=c("train","validation"),bty="n")

opt.size <- which.min(testScore[-1]) + 1
print(opt.size)
opt_tree <- prune.tree(fit_decisionTree_devience,best=opt.size) 
plot(opt_tree)
text(opt_tree, pretty=0)
opt_tree
summary(opt_tree)

# Task 4

# Estimating Confusion Matrix
opt_tree_predictions <- predict(opt_tree, newdata=test,type="class") 
confusion_matrix_optimalModel <- table(test$y, opt_tree_predictions)
print(confusion_matrix_optimalModel)

accuracy = (confusion_matrix_optimalModel[1,1] + confusion_matrix_optimalModel[2,2])/(sum(confusion_matrix_optimalModel))
#f1_score = F1_Score(test$y,opt_tree_predictions)
print(accuracy)
#print(f1_score)

# Task 5
opt_tree_predictions <- predict(opt_tree, newdata=test,type="vector")
head(opt_tree_predictions)

newpred <- ifelse((opt_tree_predictions[,1]/5)>opt_tree_predictions[,2], 0,1)
confusion_matrix_newPred <- table(test$y, newpred)
print(confusion_matrix_newPred)
misclas_rate <- missclass(test$y, newpred)
misclas_rate

# Task 6
logistic_model <- glm(y~., data=train, family="binomial")
logistic_prediction <- predict(logistic_model, newdata = test, type="response")
opt_tree_predictions <- predict(opt_tree, newdata=test,type="vector")

tpr_logistic = list()
fpr_logistic = list()
tpr_tree = list()
fpr_tree = list()
for(i in c(1:19)){
  confusion_matrix_newPred_optimTree <- matrix(c(0,0,0,0), nrow = 2, ncol = 2)
  
  
  pi_value = i*0.05
  newpred_logistic <- ifelse(logistic_prediction>pi_value,1,0)
  newpred_optimTree <- ifelse(opt_tree_predictions[,2]>pi_value,1,0)

  confusion_matrix_newPred_logistic <- table(test$y, newpred_logistic)
  confusion_matrix_newPred_optimTree <- table(test$y, newpred_optimTree)

  tpr_logistic[i] <- confusion_matrix_newPred_logistic[2,2]/(confusion_matrix_newPred_logistic[2,1]+confusion_matrix_newPred_logistic[2,2])
  fpr_logistic[i] <- confusion_matrix_newPred_logistic[1,2]/(confusion_matrix_newPred_logistic[1,1]+confusion_matrix_newPred_logistic[1,2])
  
  if(dim(confusion_matrix_newPred_optimTree)[2]==2)
  {
    tpr_tree[i] <- confusion_matrix_newPred_optimTree[2,2]/(confusion_matrix_newPred_optimTree[2,1]+confusion_matrix_newPred_optimTree[2,2])
    fpr_tree[i] <- confusion_matrix_newPred_optimTree[1,2]/(confusion_matrix_newPred_optimTree[1,1]+confusion_matrix_newPred_optimTree[1,2])
  }
  else
  {
    tpr_tree[i] = 0
    fpr_tree[i] = 0
  }

}
plot(fpr_logistic,tpr_logistic, type = "b", main = "Logistic model", xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(fpr_tree,tpr_tree, type = "b", main = "Optimal Tree", xlab = "False Positive Rate", ylab = "True Positive Rate")

#area_under_curve_logistic = Area_Under_Curve(unlist(fpr_logistic, use.names = FALSE), unlist(tpr_logistic, use.names = FALSE))
#area_under_curve_tree = Area_Under_Curve(unlist(fpr_tree, use.names = FALSE), unlist(tpr_tree, use.names = FALSE))
#print(area_under_curve_logistic)
#print(area_under_curve_tree)
