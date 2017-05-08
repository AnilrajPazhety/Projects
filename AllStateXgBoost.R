library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)

ID = 'id'
TARGET = 'loss'
SEED = 240768

LETTERS
LETTERS_AY <- LETTERS[-length(LETTERS)]
LETTERS_AY
LETTERS702 <- c(LETTERS_AY, sapply(LETTERS_AY, function(x) paste0(x, LETTERS_AY)), "ZZ")


train<- fread("train_claim.csv",showProgress = TRUE)
test<- fread("test.csv")

#train_ids <- train[,ID, with = FALSE][[ID]]
trids <- train$id
#test_ids <- test[,ID, with = FALSE][[ID]]
tsids <- test$id
#y_train = log(train[,TARGET, with = FALSE])[[TARGET]]
y_train1 = log(train$loss)
#test1s <- test[,ID, with = FALSE][["id"]]

log(head(train$loss))

test$loss <- 0

train_test <- rbind(train,test)

features <- setdiff(names(train),c("id","loss"))

levels1 <- intersect(LETTERS702, unique(train_test[[113]]))

(levels1)


levels <- unique(train_test[[113]])
levels
levels <- sort(levels)
levels

length(match(levels,levels1))
length(levels1)
labels <- match(levels, LETTERS702)
labels
for(f in features){
  
  if(class(train_test[[f]])=="character"){
   levels <- intersect(LETTERS702,unique(train_test[[f]]))#ordering
   labels <- match(levels, LETTERS702)
 train_test[[f]] <- as.integer(as.character(factor(train_test[[f]],levels = levels,labels = labels)))
    
    
  }
  
}

#trainCl <- subset(train_test,select = -c(loss))
#trainCl <- trainCl[1:188318,]
#correlations <- cor(trainCl)
#table(correlations)

#corrplot(correlations>0.9, method="square", order="hclust")

x_train <- train_test[1:nrow(train),]
x_test <- train_test[(nrow(train) + 1) : nrow(train_test),]
TARGET <- 'loss'

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]


dtrain <- xgb.DMatrix(as.matrix(x_train),label=y_train)

dtest <- xgb.DMatrix(as.matrix(x_test))

xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

# For all those R users that want a competitive starter
# a shameless port of Faron's super python script to R
# https://www.kaggle.com/mmueller/allstate-claims-severity/yet-another-xgb-starter/code
# scores 1128 on public leaderboard but produced 1126 on my local run

library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)

ID = 'id'
TARGET = 'loss'
SEED = 0

TRAIN_FILE = "../input/train.csv"
TEST_FILE = "../input/test.csv"
SUBMISSION_FILE = "../input/sample_submission.csv"


train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]

train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]

ntrain = nrow(train)
train_test = rbind(train, test)

features = names(train)

for (f in features) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}


x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]

#Using all Variables
lm1 = lm(x_train$loss~.,data=x_train)
summary(lm1)

#Using leaps

require(leaps)

impVars <-  regsubsets(loss~.,data = x_train,method = "backward")

summary(impVars)
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))


xgb_params = list(
  seed = 150,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(y,yhat )
  return (list(metric = "error", value = err))
}



res = xgb.cv(xgb_params,
             dtrain,
             nrounds=50,
             nfold=4,
             early_stopping_rounds=5,
             print_every_n = 10,
             verbose= 1,
             feval=xg_eval_mae,
             maximize=FALSE)

best_nrounds = min(res$train.error.mean)

best_nrounds = which.min(res[,train.error.mean==194.282823])
best_nrounds
cv_mean = res$evaluation_log$test_error_mean[1]
cv_std = res$evaluation_log$test_error_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

gbdt = xgb.train(xgb_params, dtrain, 50)
gbdt

submission = fread(test.csv, colClasses = c("integer", "numeric"))
test$loss = exp(predict(gbdt,dtest))

test$loss
write.csv(test,'xgb_starter_v2.sub.csv',row.names = FALSE)

maeCl <- xg_eval_mae(test$loss,dtest)
maeCl

lm2=lm(loss~cat44+cat57+cat79+cat80+cat101+cat111+cat114+cont1+cont2+cont3+cont7+cont14+cont10+cont8+cont9,data= x_train)
summary(lm2)

#Correlations
require(caret)
corThresh <- 0.9

toohigh <- findCorrelation(cor(x_train),corThresh)
corrPred <- names(x_train)[toohigh]
corrPred
X_train_filtered <- x_train[,-toohigh]

View(X_train_filtered)

require(MASS)
set.seed(240768)
ctrl <- trainControl(method= "cv",number=10)
lmfiltered <- train(X_train_filtered,train$loss,method= "lm",trControl = ctrl)
summary(X_train_filtered)
table(X_train_filtered)
