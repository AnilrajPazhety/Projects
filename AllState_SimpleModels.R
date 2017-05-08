library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)


SEED = 240768

LETTERS
LETTERS_AY <- LETTERS[-length(LETTERS)]
LETTERS_AY
LETTERS702 <- c(LETTERS_AY, sapply(LETTERS_AY, function(x) paste0(x, LETTERS_AY)), "ZZ")
LETTERS702

train<- fread("train_claim.csv",showProgress = TRUE)
test<- fread("test.csv")

#train_ids <- train[,ID, with = FALSE][[ID]]
trids <- train$id
#test_ids <- test[,ID, with = FALSE][[ID]]
tsids <- test$id
#y_train = log(train[,TARGET, with = FALSE])[[TARGET]]
y_train1 = log(train$loss)
#test1s <- test[,ID, with = FALSE][["id"]]
plot(y_train1)

log(head(train$loss))

test$loss <- 0

train_test <- rbind(train,test)

train_test$id <- NULL
features <- setdiff(names(train),c("id","loss"))
i<-1
for(f in features){
  
  if(class(train_test[[f]])=="character"){
    levels <- intersect(LETTERS702,unique(train_test[[f]]))#ordering
    labels <- match(levels, LETTERS702)
    train_test[[f]] <- as.integer(factor(train_test[[f]],levels = levels,labels = labels))
    
    
  }
  
}

#trainCl <- subset(train_test,select = -c(loss))
#trainCl <- trainCl[1:188318,]
#correlations <- cor(trainCl)
#table(correlations)

#corrplot(correlations>0.9, method="square", order="hclust")

x_train <- train_test[1:nrow(train),]
x_test <- train_test[(nrow(train) + 1) : nrow(train_test),]

#Mutual Information

library(infotheo)
rf.label <- x_train$loss
i<- 1
#mutinformation(discretize(rf.label),discretize(x_train$cat1))
#mutinformation(discretize(rf.label),discretize(x_train[[1]]))

mval <- data.frame(Value=rep(0,130))
cName <- data.frame(Name=rep("None",130))
l <- vector("list", 131)
mutvalindex <- vector("list", 130)
length(features)
for(f in features){
  
 # l[[i]]<-(mutinformation(discretize(rf.label),discretize(x_train[[f]])))
   
  #if(l[[i]] >= 0.01){
   # mutvalindex <- i
    
 # }
  
  mval$Value[i] <- (mutinformation(discretize(rf.label),discretize(x_train[[f]]))) 
  cName$Col[i] <- f
  i <- i + 1

  
}


cName$Name <- NULL

#indexCorr <- (which(mval$Value >= 0.01))
test_corr <- cbind(cName,mval)


t <-test_corr[which(test_corr$Value > 0.01),]
table(t)
x_newtrain <- subset(x_train,select = t$Col)
x_newtest <- subset(x_test,select = t$Col)
names(x_test)
#names(x_newtrain)
#subt <- x_train[,colnames(x_train) %in% t$Col]
#RandomForests
#########

library(randomForest)

set.seed(240768)

rf.1 <-randomForest(x=x_newtrain[1:40000,],y=rf.label[1:40000],importance = TRUE ,ntree = 1000)
rf.1
varImpPlot(rf.1)



rf.preds = predict(rf.1,x_newtest)

#Submit
submission <- data.frame(ID=tsids,loss=rf.preds)
#submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
#xg.preds = predict(gbdt,dtest)
#write.csv(submission,'xgb_starter_v2.sub.csv',row.names = FALSE)


write.csv(submission,file = "AllStateClaim_11172016rf.csv",row.names = TRUE)
print(1)
require(entropy)
disc=discretize2d(rf.label, x_train$cat1, numBins1=16, numBins2=16)
mutualInfo=mi.empirical(disc)


sum(is.na(train_test))

#Using all Variables
lm1 = lm(x_train$loss~.,data=x_train)
summary(lm1)

#Using leaps and linear model

require(leaps)

reg.fit.all <-  regsubsets(loss~.,data = x_train,method="backward",nvmax=130)
regsum <- summary(reg.fit.all)
regsum$rsq

#Ridge  and lasso
 
require(glmnet) 
x_train_pred <- subset(x_train, select = -loss)
x_test_pred <- subset(x_test, select = -loss)
grid= 10^ seq(10,-2,length=100)
ridge.mod = glmnet(x=as.matrix(x_train_pred),y=x_train$loss,alpha=0,lambda= grid)

ridge.pred = predict(ridge.mod,as.matrix(x_test_pred))


submit.df <- data.frame(ID=tsids,loss=ridge.pred)

write.csv(submit.df,file = "AllStateClaim_11022016.csv",row.names = TRUE)

#PCR and PLS
require(pls)
pls.fit = plsr(loss~., data=x_train,scale = TRUE,ncomp=4)
summary(pls.fit)

pls.pred <- predict(pls.fit,x_test,ncomp=10)

y<-x_newtrain$cat1
x_newtrain$loss <- x_train$loss
x_newtrain$cat1<-NULL
x_newtest$cat1<-NULL
x_newtest$loss <- 0
pls.fit.1 = plsr(loss~., data=x_newtrain,scale = TRUE,ncomp=10)
summary(pls.fit)

pls.pred <- predict(pls.fit.1,x_newtest,ncomp=10)


#CV using Random Forest
library(caret)
library(doSNOW)
rf.label <- x_newtrain$loss
set.seed(14587)
set.seed(123)
#seeds <- vector(mode = "list", length = 11)#length is = (n_repeats*nresampling)+1
#for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 3) #(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)

#seeds[[11]]<-sample.int(1000, 1)#for the last model
cv.3.folds <-createMultiFolds(rf.label,k=3,times = 10)
ctrl <- trainControl(method = "repeatedcv",number=3,repeats=10,index=cv.3.folds)

cl <- makeCluster(4,type = "SOCK")
registerDoSNOW(cl)



pls.tune1 <- train(x_newtrain[1:900,],rf.label[1:900],method="rf",tunelength=20,trControl=ctrl)
stopCluster(cl)

sum(is.na(rf.label))
pls.tune1
rf.preds <- predict(pls.tune1,x_newtest)

pls.tune <- train(x_train_pred,x_train$loss,method="pls",tunelength=20,trControl=ctrl,preProc=c("center","scale"))

pls.tune
validationplot(pls.fit,val.type = "R2")

#Submit
submit.df <- data.frame(loss=pls.pred,ID=tsids)

write.csv(submit.df,file = "AllStateClaim_110320164plsr.csv",row.names = TRUE)

plot(pls.pred)
#Correlations
require(caret)
corThresh <- 0.9

toohigh <- findCorrelation(cor(x_train),corThresh)
corrPred <- names(x_train)[toohigh]
toohigh
X_train_filtered <- x_train
X_test_filtered <- x_test
X_train_filtered[,105] <- NULL

X_test_filtered[,130] <- NULL
X_train_filtered$loss <- x_train$loss

X_test_filtered$loss <- 0
names(x_train)

require(MASS)
set.seed(240768)
ctrl <- trainControl(method= "cv",number=10)
lmfiltered <- train(X_train_filtered,X_train_filtered$loss,method= "lm",trControl = ctrl)
summary(lmfiltered)
Preds<- predict(lmfiltered,X_test_filtered)
summary(Preds)
table(X_train_filtered)
