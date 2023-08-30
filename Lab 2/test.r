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


fit=tree(y~., data=train)

trainScore=rep(0,6)
testScore=rep(0,6)
for(i in 2:6) { 
  prunedTree=prune.tree(fit,best=i) 
  pred=predict(prunedTree, newdata=valid,type="tree") 
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:6, trainScore[2:6], type="b", col="red", ylim=c(8200,12000) )
points(2:6, testScore[2:6], type="b", col="blue")