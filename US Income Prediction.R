##Load packages and data

library(data.table)
library(xgboost)
train <- fread("train.csv",na.strings=c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings=c(""," ","?","NA",NA))

##Analyse the data
dim(train)
str(train)
View(train)

dim(test)
str(test)
View(test)

##The target variable income level require encoding to be done
unique(train$income_level)
unique(test$income_level)

train$income_level=ifelse(train$income_level=="+50000",1,0)

test$income_level=ifelse(test$income_level=="50000+.",1,0)

##What is the disbtribution of classes in target variables
round(prop.table(table(train$income_level))*100)

##The structure of the data needs to be changed to perform any operations
##Convert into to numeric and character classes to factors

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

ab<-train[,factcols := lapply(.SD,as.factor),.SDcols=factcols]
train[,numcols := lapply(.SD,as.numeric),.SDcols=numcols]

test[,factcols := lapply(.SD,factor),.SDcols=factcols]
test[,numcols := lapply(.SD,as.numeric),.SDcols=numcols]


##Verifying the conversion

str(train$marital_status)
str(test)
head(train)


#Subsetting categorical variables
cat_train <- train[,factcols,with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#Subsetting numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]


##Analyze the numerical data using plots
##Loading the libraries

library(ggplot2)
library(plotly)

##Function to plot

pltfn= function(a){
  ggplot(num_train,aes(x=a,y=..density..))+geom_histogram(fill="blue",color="red",alpha = 0.5,bins = 100)+geom_density()
  ggplotly()

}

##Plot Age
pltfn(num_train$age)

##From the above plot it can be inferred that the age in data set has a declining trend
#with a concentration in the middle
##Plot capital losses
pltfn(num_train$capital_losses)

## Add income variable to cat_train
num_train$income_level <- cat_train$income_level
##Create scatterplot with income level


str(train$income_level)

ggplot(data=train,aes(x=education,fill=as.factor(income_level)))+geom_bar(width=0.7)+ facet_wrap(~race)+ggtitle("Age/Race and Income Level")+
  xlab("Education")+
  ylab("Total count")+
  labs(fill="Income Level")

##The class imbalance is very evident from the below two graphs

ggplot(data=train,aes(x=education,fill=as.factor(income_level)))+geom_bar(width=0.7)+ facet_wrap(~race)+ggtitle("Age/Race and Income Level")+
  xlab("Education")+
  ylab("Total count")+
  labs(fill="Income Level")



ggplot(data=train,aes(x=education,fill=as.factor(income_level)))+geom_bar(width=0.7)+ facet_wrap(~class_of_worker)+ggtitle("Age/class of worker and Income Level")+
  xlab("Education")+
  ylab("Total count")+
  labs(fill="Income Level")

##Analyzing the conitnous variables-Individuals with age>20 can be put in the lowerincome bracket
ggplot(data=num_train,aes(x=age,y=capital_losses))+geom_point(aes(colour=as.factor(num_train$income_level)))
ggplot(data=num_train,aes(x=age,y=dividend_from_Stocks))+geom_point(aes(colour=as.factor(num_train$income_level)))

##Weeks worked in a year looks important
ggplot(data=num_train,aes(x= weeks_worked_in_year,y=wage_per_hour))+geom_point(aes(colour= as.factor(income_level)))
ggplot(data=num_train,aes(x= weeks_worked_in_year,y=wage_per_hour))+ geom_jitter()+geom_point(aes(colour= as.factor(income_level)))


##Analyzing the categorical variables visually

##It can be inferred that class of workers donot provide a clear insight 
##Creating a function to plot graphs using categorical variables

plotcat=function(a){
ggplot(cat_train,aes(x=a,fill=as.factor(cat_train$income_level)))+geom_bar(width=0.5)+theme(axis.text.x=element_text(angle=60,hjust=1))
}


#Industry code above 25 shows a relatively higher proportion for higher income level
plotcat(cat_train$industry_code)

##Private class of worker has a higher presence of high income levels
plotcat(cat_train$class_of_worker)

##MOre than 50% of Occupation code has values 0 indicating a high left skewness
plotcat(cat_train$occupation_code)

##Education shows a clear pattern as expected. Children and students below 9th grade are in lower income group
plotcat(cat_train$education)
##Memory Allocation issues
#train[cat_train$class_of_worker %in% c("Without pay")] <-"Others"
#"Federal government ","Local government","Never worked ","Self-employed-incorporated","Self-employed-not incorporated","State government","Without pay"


##Only one sub -class shown high income level
plotcat(cat_train$enrolled_in_edu_inst_lastwk)


##Married with civilan spouse present has relatively higher proportion of high income levels
plotcat(cat_train$marital_status)

##NOthing significant in the major industry code
plotcat(cat_train$major_industry_code)

##There is visible patter here with some sub classes with no high income levels
plotcat(cat_train$major_occupation_code)


##Data Cleansing

table(is.na(num_train))
table(is.na(num_test))

##Check for correlaiton among variables
library(caret)
library(corrplot)

##Only one variable is highly correlated
corrplot(cor(num_train),order="hclust")
x=findCorrelation(x=cor(num_train),cutoff = 0.7)

##removing the highly correlated variables from num_train

num_train1<-num_train[,-x,with=FALSE]

##Find out if any missing values exist in categorical variables
(prop.table(table(is.na(cat_train)))*100)

##Find out missing values per column

mcatcoltrain <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})
mcatcoltest <- sapply(cat_test, function(x){sum(is.na(x))/length(x)})


##Select columns with missing values less than 5%

cat_train1 <- subset(cat_train,select=mcatcoltrain<0.05)

cat_test1 <- subset(cat_test,select=mcatcoltest<0.05)

##Set NA values as unavailable

cat_train2 <- cat_train1[,names(cat_train1):= lapply(.SD,as.character),.SDcols=names(cat_train1)]

for(i in seq_along(cat_train2))
{
  
  set(cat_train2,i=which(is.na(cat_train2[[i]])),j=i,value="Unavailable")
}

cat_train2 <- cat_train2[,names(cat_train2):= lapply(.SD,as.factor),.SDcols=names(cat_train2)]
str(cat_train2)

##Performing the NA to Unavailable operation on test set
cat_test2 <- cat_test1[,names(cat_test1):= lapply(.SD,as.character),.SDcols=names(cat_test1)]
for(i in seq_along(cat_test2)){
  
  set(cat_test2,i=which(is.na(cat_test2[[i]])),j=i,value="Unavailable")
}

cat_test2 <- cat_test2[,names(cat_test2):= lapply(.SD,as.factor),.SDcols=names(cat_test2)]


##As observed in the visualizations there is severe imbalance in the classes
##hence it is a better option to combine subclasses with the low frequencies

##Combining low frequency subclass for train set

##Cut off as 5%
p=0.05

for(i in colnames(cat_train2)){

 combine <- names(which(prop.table(table(cat_train2[[i]])) < p))
 levels(cat_train2[[i]])[levels(cat_train2[[i]]) %in% combine] <- "Other"  
}

##Combining low frequency subclass for test set

for(i in colnames(cat_test2)){
  
  combine <- names(which(prop.table(table(cat_test2[[i]])) < p))
  levels(cat_test2[[i]])[levels(cat_test2[[i]]) %in% combine] <- "Other"  
}


##Verifying the conversion operations done earlier on train and test data
View(cat_train2)
View(cat_test2)
##Binning Numerical Variables

num_train1[,.N,age][order(age)]

num_train1$age = cut(x=num_train1$age,breaks = c(0,30,60,90),labels= c("young","adult","old"),include.lowest = T)

num_test$age = cut(x=num_test$age,breaks = c(0,30,60,90),labels= c("young","adult","old"),include.lowest = T)

##Verifying the binning

table(num_train1$age)
##Bining all numeric variables into 0 and greater than zero bins

#Train

num_train1[,.N,wage_per_hour][order(wage_per_hour)]
num_train1$wage_per_hour = as.factor(ifelse(num_train1$wage_per_hour==0,"Zero","Morethanzero"))

num_train1[,.N,capital_gains][order(capital_gains)]
num_train1$capital_gains = as.factor(ifelse(num_train1$capital_gains==0,"Zero","Morethanzero"))

num_train1[,.N,capital_losses][order(capital_losses)]
num_train1$capital_losses = as.factor(ifelse(num_train1$capital_losses==0,"Zero","Morethanzero"))

num_train1[,.N,dividend_from_Stocks][order(dividend_from_Stocks)]
num_train1$dividend_from_Stocks= as.factor(ifelse(num_train1$dividend_from_Stocks==0,"Zero","Morethanzero"))

##Test
num_test$wage_per_hour = as.factor(ifelse(num_test$wage_per_hour==0,"Zero","Morethanzero"))
num_test$capital_gains = as.factor(ifelse(num_test$capital_gains==0,"Zero","Morethanzero"))
num_test$capital_losses = as.factor(ifelse(num_test$capital_losses==0,"Zero","Morethanzero"))
num_test$dividend_from_Stocks= as.factor(ifelse(num_test$dividend_from_Stocks==0,"Zero","Morethanzero"))

##Verifying the conversions
View(num_train1)
View(num_test)


library(mlr)
summarizeColumns(cat_train2)[,c("name","nlevs")]
summarizeColumns(cat_test2)[,c("name","nlevs")]

##Removing the target variable-income_level which was added for analysis
num_train1$income_level <- NULL

##Modeling  

install.packages('mlr', repo='http://cran.fiocruz.br/')
getOption('repos')
install.packages('mlr', repo='http://nbcgib.uesc.br/mirrors/cran/')


dc_train <- cbind(num_train1,cat_train2)
dc_test <-cbind(num_test,cat_test2)

#remove unwanted files
rm(admission,cat_test2,cat_train,cat_train2,num_test,cat_test,cat_test1,train,tst) #save memory


##Create tasks

##Converting integer variables to numerics for use in modeling

dc_train$num_person_Worked_employer <-as.numeric(dc_train$num_person_Worked_employer)
dc_test$num_person_Worked_employer <-as.numeric(dc_test$num_person_Worked_employer)

train.task <- makeClassifTask(data=dc_train,target="income_level")
test.task <- makeClassifTask(data=dc_test,target="income_level")

##Remove variables with zero variables

train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)


##Understand which variables are important

var_imp <- generateFilterValuesData(train.task,method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = T)


##Data Balancing operations
train.under <- undersample(train.task,rate=0.1)##retain only 10% of majority class
table(getTaskTargets(train.under))


train.over <- oversample(train.task,rate=15)##add 15 times the minority clas
table(getTaskTargets(train.over))


system.time(
  
  train.smote <- smote(train.task,rate = 10,nn = 3)
)

listLearners("classif","twoclass")[c("class","package")]


##Naive Baiyes

nb_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
nb_learner$par.vals <-list(laplace=1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

func_cv=function(a){
  
  cv_val <- resample(learner = nb_learner,resampling = folds ,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn))
  cv_val$aggr
}

func_cv((train.over))
func_cv((train.under))
func_cv((train.smote))

##Train.smote gives the highest true positive and true negative rates

##Train the model and make predictions using train.smote

nb_fit <- train(nb_learner,train.smote)
nb_preds <- predict(nb_fit,test.task)


##Measure Classfication Accuracy

nb_classpreds <- nb_preds$data$response

nb_acc<-confusionMatrix(dc_test$income_level,nb_classpreds)

##Using xgboost for better accuracy


set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50
)

#define hyperparameters for tuning

xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)


#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)


#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)


#top 20 features
filtered.data <- filterFeatures(train.task,method = "information.gain",abs = 20)
#train
xgb_boost <- train(xgb_new,filtered.data)

#xgboost AUC 
xgb_prob <- setPredictType(learner = xgb_new,predict.type = "prob")

#train model
xgmodel_prob <- train(xgb_prob,train.task)

#predict
predict.xgprob <- predict(xgmodel_prob,test.task)

#predicted probabilities
predict.xgprob$data[1:10,]

##AUC Curve

df <- generateThreshVsPerfData(predict.xgprob,measures = list(fpr,tpr))

plotROCCurves(df)

#set threshold as 0.4
pred2 <- setThreshold(predict.xgprob,0.4)
confusionMatrix(d_test$income_level,pred2$data$response)