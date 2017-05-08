#Load Raw Data

train <- read.csv("train.csv",header = TRUE)
test <-  read.csv("test.csv",header = TRUE)

#Creating a new dataframe with the response variable added to the test set
#and setting the default value to None
test.survived <- data.frame(Survived=rep("None",nrow(test)),test[,])

#Combining Train and newly created test set
data.combined <- rbind(train,test.survived)

#Checking the structure of Data.combined dataframe
str(data.combined)

#Survived and Pclass needs to be converted to factor for further analysis
data.combined$ Survived <- as.factor(data.combined$ Survived)
data.combined$ Pclass <- as.factor(data.combined$ Pclass)

#Verifying the conversion
str(data.combined)

#Understanding the spread of the two variables in the combined data set
table(data.combined$ Survived)
table(data.combined$Pclass)

#Starting exploratory data analysis with Pclass variable in train set

#Loading the ggplot2 library for visual analysis
library(ggplot2)

#Checking the data types for train set
str(train)

#Pclass needs to be converted to factor 
train$Pclass <- as.factor(train$Pclass)

#Taking a look at number passengers who survived or perished across passenger classes

ggplot(train,aes(x = Pclass,fill=factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("TotalCount") + 
  labs(fill="Survived")

#From the plot we can observe that pclass variable has some predictive

#Taking a look at the next variable-$Name
#Name is factor variable . Converting it to a string format would help in extracting 
#useful information. We would also look at a few values using head function

head(as.character(train$Name))

#Find out the number of unique names in combined data set
length(unique(as.character(data.combined$Name)))

#Creating a character set which contains all the duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
#Checking the records which contain the same Names to check if they are duplicates
data.combined[which(data.combined$ Name %in% dup.names),]

#Loading the stringr library for string manipulations
library(stringr)

#Creating separate dataframes using title in the passenger names
misses <- data.combined[(grep("Miss",data.combined$Name)),]
misses[1:5,]

mrses <-data.combined[which(str_detect(data.combined$Name,"Mrs")),]
mrses[1:5,]

#Creating a dataframe for male passengers

males <-data.combined[(data.combined$Sex == "male"),]
males[1:5,]

#There is a pssibility to add new title to younger males and females
#This would help us to look at the predictive power of age and gender combined

#Creating a function to assign a title  variable to each passenger on age and gender

extractTitle <- function(name)
  {
  name <- as.character(name)
  
  if(length(grep("Miss",name))>0)
    { return("Miss.")
  }else if(length(grep("Mrs",name))>0)
    { return("Mrs.")}
  else if(length(grep("Master",name))>0){ return("Master.")}
  else if(length(grep("Mr",name))>0){ return("Mr.")}
  else {return("Other")}
  
}


#Create a new title object 
titles <- NULL

for(i in 1:nrow(data.combined)){
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
  
}

#Assigning the variable to combined data set
data.combined$title <- as.factor(titles)

#Validating conversion by looking at first few records
data.combined[1:5,]

#We will take a look at the title values in the train set to see the predictive power
ggplot(data.combined[1:891,],aes(x=title,fill=Survived))+
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass") +
  xlab("Title")+
  ylab("Total COunt")+
  labs(fill = "Survived")

#The title variable has a story to tell. Hence,we can use this as one of the predictor 
#variables in our model

#Lets check the distribution of males and females
table(data.combined$Sex)

#Survival Rate based on Gender and Pclass

ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived))+
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total count")+
  labs(fill="Survived")


summary(data.combined$Age)

#Survival Rate based on Age vs Gender and Pclass

ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth=10)+
  facet_wrap(~ Sex + Pclass)+
  ggtitle("Pclass")+
  xlab("Age")+
  ylab("Total count")+
  labs(fill="Survived")

#Based on the above plot,there is an observable pattern.Young boys and girls in higher
#classes survived

#Lets separate out young boys and girls 

boys <-data.combined[which(data.combined$ title=="Master."),]
summary(boys$Age)



misses <-data.combined[which(data.combined$ title=="Miss."),]
summary(misses$Age)

#Examining the pattern for survival in misses df based on Age and Pclass
ggplot(misses[misses$Survived!="None",],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth=10)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Age for Miss by Pcall")+
  ylab("Total count")+
  labs(fill="Survived")

#Lets try to check number of  misses who travelled alone who are young girls and adult females
misses.alone <-misses[which(misses$SibSp == 0 & misses$Parch== 0),]
table(misses.alone$Age)
summary(misses$Age)

#The number of females who can be bucketed as young girls
length(unique(misses.alone$Age <=14.5))

#Lets take a look at the number of Siblings
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)
str(data.combined)

#Examining if siblings/Spouse can be used in our model
ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass + title)+
  ggtitle("Test")+
  xlab("Siblings")+
  ylab("Total COunt")+
  labs(fill="Survived")
  
#It doesnot offer us any additional information 

#Lets check the Parch variable
summary(data.combined$Parch)

#Converting Parch into a factor
data.combined$Parch <- as.factor(data.combined$Parch)

#Examining if #Parent/Child can be used in our mode
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass + title)+
  ggtitle("Test")+
  xlab("ParentChild")+
  ylab("Total COunt")+
  labs(fill="Survived")

#Individually both Sibsp and Parch variable donot offer anything substantial
#We will combine Sibsp and Parch to create a new variable for family size
#and append it to the combined data set

temp.sibsp <- c(train$SibSp,test$SibSp)
temp.parch <- c(train$Parch,test$Parch)

data.combined$familysize <- as.factor(temp.parch+temp.sibsp+1)

#Now lets look at the newly created variable family size

ggplot(data.combined[1:891,],aes(x=familysize,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass + title)+
  ggtitle("Test")+
  xlab("Family Size")+
  ylab("Total COunt")+
  labs(fill="Survived")

#We will park for now and see if this variable helps in fine tuning the model later on

#Examining the next variable Ticket
str(data.combined$Ticket)

#Ticket needs to be converted to character to extract some meaningful info
data.combined$Ticket <- as.character(data.combined$Ticket)

#Taking a look at the first few Ticket strings
data.combined$Ticket[1:20]

#Looks like the first character has some meaning
#Lets pull it out from ticket string and convert it to a factor for further analysis
ticket.first.char <- ifelse(data.combined$Ticket == ""," ",substr(data.combined$Ticket,1,1))
data.combined$ticket.first.char <- as.factor(ticket.first.char)

#Lets plot ticket first character vs Pclass and Title
  ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+title)+
  ggtitle("Ticket Analysis")+
  xlab("First Character")+
  ylab("Total COunt")+
  labs(fill="Survived")

#We will park for now and see if this variable helps in fine tuning the model later on
 
#Lets take a look at the Fare Variable
  summary(data.combined$Fare)
  length(unique(data.combined$Fare))
  
  ggplot(data.combined[1:891,],aes(x= Fare,fill=Survived))+
  geom_bar(width=10)+
  ggtitle("Fare Analysis")+
  xlab("Fare")+
  ylab("Total COunt")
  
#Lets check Fare variable vs Pclass and Title
  
  ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+title)+
  ggtitle("Fare Comparison")+
  xlab("Fare")+
  ylab("Total COunt")+
  ylim(0,50)
  labs(fill="Survived")
    
# The plot doesnot provide a convincing picture.Hence,we move on to the next variable
# Cabin
    
  str(data.combined$Cabin)
  summary(data.combined$Cabin)

#Cabin varibale needs to be converted to character to check for any specific patterns

  data.combined$Cabin <-as.character(data.combined$Cabin)  
  
#Check for the first 100 entries  
  data.combined$Cabin[1:100]
  
#There many unassigned or blank entries.Lets assign "U" to such entries
    
  data.combined[which(data.combined$Cabin==""),"Cabin"]<- "U"
  data.combined$Cabin[1:100]

#From the Cabin entries,it can observed that the first character has some predictive
#power.We will extract the first character from cabin variable and assign it to a new
# Object
  
  cabin.first.char<-as.factor(substr(data.combined$Cabin,1,1))
  str(cabin.first.char)
  
#Check for the total number for level in cabin.first.char variable
  levels(cabin.first.char)

#There are only 9 levels,hence we convert it to factor
  data.combined$cabin.first.char <- as.factor(cabin.first.char)
  
#We can now plot cabin.first.char with pclass and title to check for any patterns  

  ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+title)+
  ggtitle("Test")+
  xlab("Cabin First Character")+
  ylab("Total COunt")+ labs(fill="Survived")
 
#This plot tells reiterates our earlier findings
    
#Delete if it doesnot make sense
  #data.combined$Cabin.multiple <- as.character(ifelse(str_detect(data.combined$cabin," "),"Y","N"))
  #data.combined$Cabin.multiple[1:100]
  
#Lets examine the next variable Embarked
  str(data.combined$Embarked)
    
  ggplot(data.combined[1:891,],aes(x=Embarked,fill=Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+title)+
  ggtitle("Embarked Analysis")+
  xlab("Embarked")+
  ylab("Total COunt")+
  labs(fill="Survived")

#This plot doesnot carry enough information for us to draw any relation between 
#Embarked variable and Survivability

### Data MOdeling ###
    
#Based on our exploratory data analysis we shortlist Pclass and title for our model
#Loading Random Forest library 
  
  library(randomForest)
  rf.train.1 <- data.combined[1:891,c("Pclass","title")]
  rf.label <- as.factor(train$Survived)
    
    set.seed(1234)
    
    rf.1 <-randomForest(x=rf.train.1,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.1
    varImpPlot(rf.1)
    
    rf.train.2 <- data.combined[1:891,c("Pclass","title","SibSp")]
  
    set.seed(1234)
    
    rf.2 <-randomForest(x=rf.train.2,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.2
    varImpPlot(rf.2)
    
    rf.train.3 <- data.combined[1:891,c("Pclass","title","Parch")]
    
    set.seed(1234)
    
    rf.3 <-randomForest(x=rf.train.3,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.3
    varImpPlot(rf.3)
    
        
    rf.train.4 <- data.combined[1:891,c("Pclass","title","Parch","SibSp")]
    
    set.seed(1234)
    
    rf.4 <-randomForest(x=rf.train.4,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.4
    varImpPlot(rf.4)
    
    rf.train.5 <- data.combined[1:891,c("Pclass","title","familysize")]
    
    set.seed(1234)
    
    rf.5 <-randomForest(x=rf.train.5,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.5
    varImpPlot(rf.5)
    
    rf.train.6 <- data.combined[1:891,c("Pclass","title","SibSp","familysize")]
    
    set.seed(1234)
    
    rf.6 <-randomForest(x=rf.train.6,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.6
    varImpPlot(rf.6)
    
    rf.train.7 <- data.combined[1:891,c("Pclass","title","Parch","familysize")]
    
    
    set.seed(1234)
    
    
    rf.7 <-randomForest(x=rf.train.7,y=rf.label,importance = TRUE ,ntree = 1000)
    rf.7
    varImpPlot(rf.7)
 
 
   test.submit.df <- data.combined[892:1309,c("Pclass","title","familysize")]
   
   rf.5.preds <- predict(rf.5,test.submit.df)
   str(rf.5.preds)
   
   table(rf.5.preds)
   
   submit.df <- data.frame(PassengerID=rep(892:1309),Survived=rf.5.preds)
   
   write.csv(submit.df,file = "Kaggle_Anil.csv",row.names = TRUE)
   
   library(caret)
   library(doSNOW)
   
   set.seed(2348)
   cv.10.folds <-createMultiFolds(rf.label,k=10,times = 10)
   table(rf.label[cv.10.folds[[33]]])
   
   ctrl.1 <- trainControl(method = "repeatedcv",number = 10,repeats=10,index = cv.10.folds)
   cl <- makeCluster(4,type = "SOCK")
   registerDoSNOW(cl)
   
   set.seed(23482)
   rf.5.cv.1 <-train(x=rf.train.5,y=rf.label,method="rf",tuneLength =3 ,ntree=1000,trcontrol=ctrl.1)
  
   stopCluster(cl)
   rf.5
   rf.5.cv.1
   set.seed(22582)
   
   cv.3.folds <-createMultiFolds(rf.label,k=3,times=10)
   ctrl.3 <-trainControl(method = "repeatedcv",number=3,repeats=10,index=cv.3.folds)
   
   cl <- makeCluster(4,type = "SOCK")
   registerDoSNOW(cl)
   
   rf.5.cv.3 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trcontrol=ctrl.3)
   
   stopCluster(cl)
   rf.5.cv.3
   
   
   #Using the CART Algorithm for further Exploratory Modeling
   #Installing Packages
   require(rpart)
   require(rpart.plot)
   
   rpart.cv <-function(seed,training,labels,ctrl){
     
     cl <- makeCluster(4,type = "SOCK")
     registerDoSNOW(cl)
     
     set.seed(seed)
     
     rpart.cv <- train(x=training,y=labels,method="rpart",trControl = ctrl,tuneLength= 30)
     #Stop Cluster
     stopCluster(cl)
     return(rpart.cv)
   }
   
   features <- c("Pclass","title","familysize")
   rpart.train.1 <- data.combined[1:891,features]

   rpart.1.cv.1 <- rpart.cv(44786,rpart.train.1,rf.label,ctrl.3)
   rpart.1.cv.1
   
   prp(rpart.1.cv.1$finalModel,type=0 ,extra=1,under=TRUE)
   
   
   table(data.combined$title)
   data.combined[1:5,"Name"]
   
   name.splits <-str_split(data.combined$Name, ",")
   name.splits[1]
   last.names <- sapply(name.splits, "[",1)
   last.names[1:10]
   name.splits[1]
   data.combined$last.name <- last.names
 
   
   name.splits <-str_split(sapply(name.splits, "[",2), " ")
   name.splits[1]
   
   title.names <- sapply(name.splits, "[",2)
   title.names[1:10]
   unique(title.names)
   table(title.names) 
   
   data.combined[which(title.names == "the"),]
   data.combined[which(title.names == "Dona."),]
   title.names[title.names %in% c("Dona.","the")] <- "Lady."
   title.names[title.names %in% c("Ms.","Mlle.")] <- "Miss."
   title.names[title.names %in% c("Mme.")] <- "Mrs."
   title.names[title.names %in% c("Jonkheer.","Don.")] <- "Sir."
   title.names[title.names %in% c("Capt.","Col.","Major.")] <- "Officer"
   table(title.names)
   
   data.combined$new.title <- as.factor(title.names)
   
   ggplot(data.combined[1:891,],aes(x=new.title,fill= Survived))+
     geom_bar(width=1)+
     facet_wrap(~Pclass)+
     ggtitle("Survivability based on new.title and Pclass")
   
   data.combined$new.title[data.combined$new.title %in% c("Lady.")] <- "Mrs."
   data.combined$new.title[data.combined$new.title %in% c("Sir.","Dr.","Officer","Rev.")] <- "Mr."
   
   table(data.combined$new.title)
   
   ggplot(data.combined[1:891,],aes(x=new.title,fill= Survived))+
     geom_bar()+
     facet_wrap(~Pclass)+
     ggtitle("Survivability based on new.title and Pclass")
   
   features <- c("Pclass","new.title","familysize")
   rpart.train.2 <- data.combined[1:891,features]
   
   rpart.2.cv.1 <- rpart.cv(44786,rpart.train.2,rf.label,ctrl.3)
   rpart.2.cv.1
   
   prp(rpart.2.cv.1$finalModel,type=0 ,extra=1,under=TRUE)
   
   #Explore Mr in 1st Class
   
   survived.first.mr <- data.combined[data.combined$new.title == "Mr." & data.combined$Pclass=="1",]
   summary(survived.first.mr)
   
   survived.first.mr[survived.first.mr$Sex== "female",]
   
   #One female record is incorrectly classified into Mr. Category.Hence,needs to be 
   #corrected
   
   incorrect.Mr <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female")
   data.combined$new.title[incorrect.Mr] <- "Mrs."
   
   #Check for anyother incorrect classification
    
   length(which(data.combined$Sex == "female" & (data.combined$new.title=="Master."| data.combined$new.title== "Mr.")))
   
   #Refresh dataframe
   
   survived.first.mr <- data.combined[data.combined$new.title == "Mr." & data.combined$Pclass=="1",]
   
   summary(survived.first.mr[survived.first.mr$Survived=="1",])
   View(survived.first.mr[survived.first.mr$Survived=="1",])
   
   #Visualize survivability fir 1st class "Mr." by Fare
   
   ggplot(survived.first.mr,aes(x=Fare,fill= Survived))+
  geom_density(alpha=0.5)   +
     ggtitle("Survivability of 1st class adult males based on Fare")
   
   length(data.combined$Ticket)
   indexes= which(data.combined$Ticket==data.combined$Ticket[259])
   length(indexes)
   travellersperticket<-NULL
   avg.ticketFare <- NULL
   #travellersperticket[1]
   ticket.party.size <- NULL
   avgFare <- NULL
  # indexes= which(data.combined$Ticket==data.combined$Ticket[1])
  # length(indexes)
   #head(travellersperticket)
   data.combined$Fare[259]
   
   for(i in 1: length((data.combined$Ticket))){
     
    indexes= which(data.combined$Ticket==data.combined$Ticket[i])
   # travellersperticket[i] <- length(indexes)
     avg.ticketFare <- (data.combined$Fare[indexes[1]]/ length(indexes))
     
     for(j in 1:length(indexes)){
       ticket.party.size[indexes[j]] <- length(indexes)
       avgFare[indexes[j]] <- avg.ticketFare
       
     }
   
   }
   
   ticket.party.size[939]
   
   data.combined$travellersperticket <-  ticket.party.size
   data.combined$avgticketfare <-  avgFare
   
   #Refresh dataframe
   
   survived.first.mr <- data.combined[data.combined$new.title == "Mr." & data.combined$Pclass=="1",]
   
   summary(survived.first.mr)
   
  summary(data.combined$avgticketfare)
  
  #One record in pclass=3 has missing data and needs to be imputed
  
  data.combined[which(is.na(data.combined$avgticketfare)),]
  
  #Find all records with values and Calculate the median value for them
  
  indexes <- with(data.combined,which(Pclass == "3" & title == "Mr." & travellersperticket =="1"& Ticket != "3701"))
    
  summary(data.combined$Fare[indexes]) 
  
  #Take the median value as it is robust to outliers
  data.combined[is.na(data.combined$avgticketfare),"avgticketfare"] <- 7.854
  summary(data.combined$avgticketfare)
 # data.combined <- subset(data.combined,select = -avgtickefate)

  #Normalize Data using preproc
  library(caret)
   preproc.data <- data.combined[,c("travellersperticket","avgticketfare")]
   preProc<- preProcess(preproc.data,method = c ("center","scale"))
   summary(data.combined$travellersperticket)
  
   norm.data <- predict(preProc,preproc.data)
   summary(norm.data$travellersperticket)
   data.combined$prepocpassperticket <- norm.data$travellersperticket
   data.combined$avgticketfare <-norm.data$avgticketfare
   #Fit the model again using new features
   features <- features <- c("Pclass","new.title","familysize","travellersperticket","avgticketfare")
   rf.train.3 <-data.combined[1:891,features]
   
   rpart.3.cv.1 <- rpart.cv(44786,rf.train.3,rf.label,ctrl.3)
   rpart.3.cv.1 
   
   prp(rpart.3.cv.1$finalModel,type=0,extra=1,under= TRUE)
   
   test.submit.df <- data.combined[892:1309,features]
   rpart.3.preds <- predict(rpart.3.cv.1$finalModel,test.submit.df,type = "class")
   table(rpart.3.preds)
   
   submit.df <- data.frame(PassengerId = rep(892:1309),Survived = rpart.3.preds)
   
   write.csv(submit.df,file="Rpart_11172016rpart.csv",row.names = FALSE)
   
  
   ##Random Forest
   
   features <-c("Pclass","new.title","travellersperticket","avgticketfare")
   rf.tempData <- data.combined[1:891,features]
   set.seed(111786)
  
    
   summary(test.submit.df$avgticketfare)
   rf.model <- randomForest(rf.tempData,rf.label,ntree = 1000)
   
   rf.model
   
   test.submit.df <- data.combined[892:1309,features]
   test.submit.df$avgticketfare[which(is.na(test.submit.df$avgticketfare))] <-8.662
   rf.preds <- predict(rf.model,test.submit.df)
   table(rf.preds)
   
   submit.df <- data.frame(PassengerId = rep(892:1309),Survived = rf.preds)
   
   write.csv(submit.df,file="Rpart_RF_11172016.csv",row.names = FALSE)
   
   ##Mutual Information for feature selection
   
   library(infotheo)
   mutinformation(rf.label,data.combined$Pclass[1:891])
   mutinformation(rf.label,data.combined$title[1:891])
   mutinformation(rf.label,discretize(data.combined$avgticketfare[1:891]))
   mutinformation(rf.label,data.combined$new.title[1:891])
   
   library(Rtsne)
   most.correct <- data.combined[data.combined$new.title != "Mr.",]
   indexes <- which(most.correct$Survived !="None")
   
   tsne.1 <- Rtsne(most.correct[,features],check_duplicates = FALSE)
   
   ggplot(NULL,aes(x= tsne.1$Y[indexes,1],y = tsne.1$Y[indexes,2],color=most.correct$Survived[indexes]))+
     
    geom_point() + labs(color = "Survived") 
   
   
   condinformation(most.correct$Survived[indexes],discretize(tsne.1$Y[indexes,]))
   
   condinformation(rf.label,data.combined[1:891,c("new.title","Pclass")])
   
   ##Most error in adult males
   
   mister <- data.combined[data.combined$new.title == "Mr.",]
   indexes1 <- which(mister$Survived != "None")
   
   tsne.2 <- Rtsne(mister[,features],check_duplicates = FALSE)
   
   ggplot(NULL,aes(x= tsne.2$Y[indexes,1],y = tsne.2$Y[indexes,2],color=mister$Survived[indexes]))+
     
     geom_point() + labs(color = "Survived") 
   
   
   condinformation(most.correct$Survived[indexes],discretize(tsne.1$Y[indexes,]))
   
   condinformation(mister$Survived[indexes],discretize(tsne.2$Y[indexes,]))
   
   ##tsne for al dataset
   
   tsne.3<- Rtsne(data.combined[,features],check_duplicates = FALSE)
   
   ggplot(NULL,aes(x= tsne.3$Y[1:891,1],y = tsne.3$Y[1:891,2],color=data.combined$Survived[1:891]))+
     
     geom_point() + labs(color = "Survived") 
   
   
   condinformation(data.combined$Survived[1:891],discretize(tsne.3$Y[1:891,2]))
   
   summary(tsne.3$Y[,1])
  
    #Add tsne variables to the model
   
   data.combined$tsne.x = tsne.3$Y[,1]
   data.combined$tsne.y = tsne.3$Y[,2]
   names(data.combined)
   ##Using Random Forest again with RTSNE
   
   features.tsne <-c("Pclass","new.title","prepocpassperticket","avgticketfare","familysize")
   tsne.4<- Rtsne(data.combined[,features.tsne],check_duplicates = FALSE)
   
   data.combined$tsne.x = tsne.4$Y[,1]
   data.combined$tsne.y = tsne.4$Y[,2]
   condinformation(data.combined$Survived[1:891],discretize(tsne.4$Y[1:891,2]))
   
   rf.tempData <- data.combined[1:891,features.tsne]
   set.seed(111786)
   rf.model.tsne <- randomForest(rf.tempData,rf.label,ntree = 1000)
   rf.model.tsne
   
   test.submit.df <- data.combined[892:1309,features.tsne]
   rf.tsne <- predict(rf.model.tsne,test.submit.df)
   table(rf.preds)
   
   submit.df <- data.frame(PassengerId = rep(892:1309),Survived = rf.preds)
   
   write.csv(submit.df,file="RTSNEPrePoc__11172016.csv",row.names = FALSE)
   
