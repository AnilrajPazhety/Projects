require(ISLR)
#Taking Stock Market Data
names(Smarket)
summary(Smarket)

#PLotting to check for any high level correlation
pairs(Smarket,col=Smarket$Direction)

#Fitting a binomial distribution
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data =Smarket,family=binomial)
summary(glm.fit)

#Predict using glm.fit
glm.probs=predict(glm.fit,type = "response")
glm.probs[1:5]

#Assigning direction based on response >0.5
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)

#Check how the predictions stand against actual Directions
table(glm.pred,Direction)

#Make training and test sets

train = Year <2005

#Training using records prior to 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data =Smarket,family=binomial,subset = train)

#Making predictions on records post 2005
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")

#Extracting values for direction variable from test set
Direction.2005 =Smarket$Direction[!train]

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#Fit a simpler model

glm.fit=glm(Direction~Lag1+Lag2,data =Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
