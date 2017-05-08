##Load all the rquired packages

require(ISLR)
require(tree)
require(randomForest)
require(MASS)

##Check dimensions of the data set
dim(Boston)

train = sample(1:nrow(Boston),400)

##Run a simple random forest model 
rf= randomForest(Boston$medv~.,Boston,subset = train)

#Check the random forest result 
rf
#mtry is tuning variable. 4 Preds are used to build each tree

##Variables to store th out of bag estimate and the test error
oob.err= double(13)
test.err = double(13)

##Run random forest for mtry values ranging from 1 to 13(#of predictors)
for(i in 1:13){
  rf=randomForest(Boston$medv~.,Boston,subset = train,mtry = i,ntree =1000)
  oob.err[i] = rf$mse[1000]
  pred=predict(rf,Boston[-train,])
  test.err[i]= with(Boston[-train,],mean((Boston$medv-pred)^2))
  cat(i,"->")
}

##Plot the MSE values for different mtry values
matplot(1:13,cbind(test.err,oob.err),type= "b",col = c("red","blue"),ylab="MSE")

#sUse of Boosting Algorithm
set.seed(101)

##View the dataset
View(Boston)
summary(Boston)

##Load gradient boosting library
require(gbm)

##Fit a model with 10000 trees and depth of 4
boston1.boost= gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)

##Observe the results
summary(boston1.boost)
plot(boston1.boost,i= "rm")#shows the relation between each variable and output

##create alist of trees from 100 to 10000 increasing by 100
n.trees= seq(from=100 ,to=10000,by=100)

##predict the target value using different number of trees created above
pred1= predict(boston1.boost,newdata = Boston[-train,],n.trees = n.trees)

##Calcuating the error
Err= with(Boston[-train,],apply(((Boston$medv-pred1)^2),2,mean))

##Plot the error rate for value of ntree
plot(n.trees,Err,pch=19)

##draw a vertical line to indicate lowest test error
abline(h=min(test.err),col="red")
