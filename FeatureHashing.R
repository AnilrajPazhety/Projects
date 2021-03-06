##Feature Hashing##
library(stringr)

train <- read.csv("C:/Users/AnilNeha/Desktop/Test Data/dataset_diabetes/diabetic_data.csv",stringsAsFactors = FALSE)

##Looking at the data types
str(train)
summary(train)

##removing unique id values from the dataframe
diabetes <- subset(train,select = -c(encounter_id, patient_nbr))

#Convert all "?" to NAs
diabetes[diabetes == "?"] <- NA
#Verify conversion
str(diabetes)

#Remove Predictors with zero variance
diabetes <- diabetes[sapply(diabetes, function(x)length(levels(factor(x,exclude = NA)))>1)]
dim(diabetes)

#Format Response Variable
diabetes$readmitted = ifelse(diabetes$readmitted=="<30",1,0)

##Assign the response variable to a new varible for future use
outcomeName <- diabetes$readmitted

#Use of Dummy Variables

require(caret)
diabetes_dummy <- diabetes

dmy <- dummyVars(~.,diabetes_dummy)
diabetes_dummy <- predict(dmy,newdata = diabetes_dummy)

diabetes_dummy <- data.frame(diabetes_dummy)
diabetes_dummy[is.na(diabetes_dummy)] <- 0


##Loading the required packages for modeling
library(glmnet)

##set seed for reproducable results

set.seed(101786)

##Creating train and test set
split <- sample(nrow(diabetes_dummy),floor(0.5*nrow(diabetes_dummy)))
test.diabetes <- diabetes_dummy[-split,]
train.diabetes <- diabetes_dummy[split,]

##removing target variable from list of predictors
predictorNames <- setdiff(names(diabetes_dummy),outcomeName)

glmnetMod <- cv.glmnet(sparse.model.matrix(~.,data=train.diabetes[,predictorNames]),train.diabetes[,outcomeName],family= "binomial",type.measure = "auc")

glmpred <- predict(glmnetMod,sparse.model.matrix(~.,test.diabetes[-outcomeName]),s = "lamda.min")


#Hashing

library(FeatureHashing)

##assigning original dataframe to a new dataframe to be hashed
diabetes_hash <- diabetes

##Assigning all NA's to 0
diabetes_hash[is.na(diabetes_hash)] <- 0

##Create train and test set
split1 <- sample(nrow(diabetes_hash),floor(0.5*nrow(diabetes_hash)))
train.hash <- diabetes_hash[split1,]
test.hash <- diabetes_hash[-split1,]

##creating hashed matrix 

train_hashed <- hashed.model.matrix(~.,data=train.hash[-train.hash$readmitted],hash.size = 2^2,transpose = FALSE)
train_hashed <- as(train_hashed,"dgCMatrix")
test_hashed <- hashed.model.matrix(~.,data=test.hash[-train.hash$readmitted],hash.size = 2^6,transpose = FALSE)
test_hashed <- as(test_hashed,"dgCMatrix")

##Modeling using binomial family for predicting readmission
glm.hashed <- cv.glmnet(train_hashed,train.hash[,train.hash$readmitted],family ="binomial",type.measure = "auc")
pred.hash <- predict(glm.hashed,test_hashed,s= "lambda.min")
 
##Verifyin the AUC curve value for usefulness of the fit
auc(test_hashed[,test.hash$readmitted,pred.hash])

