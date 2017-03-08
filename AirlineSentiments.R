library(ggplot2)
library(dplyr)
library(GGally)##Extension of ggplot2
library(stringr)


##reading the file
tweets <- read.csv("Tweets.csv",header = TRUE)
str(tweets)


##checking the number for '@' in  particular tweet
tweets$at_count <- sapply(tweets$text, function(x) str_count(x, '@'))
maxAt <- max(tweets$at_count)


##Checking the proportion of each sentiments
prop.table(table(tweets$airline_sentiment))

##Creating a dataframe containing proportion of each sentiment for each airline
tweet <- as.data.frame(prop.table(table(tweets$airline_sentiment,tweets$airline)))

##Assigning names to the columns for tweet dataframe
colnames(tweet) <- c("Sentiment","Airline","Percent")


##Creating a plot with sentiments for each airline
ggplot(tweet[,],aes(x=Airline,y=Percent,fill=as.factor(Sentiment)))+
geom_bar(stat = "identity" ,alpha = 0.5)

table(tweets$negativereason)
prop.table(table(tweets$negativereason))


#tweet <- as.data.frame(prop.table(table(tweets$negativereason)))

##Creating a dataframe to capture sentiments as per time zones
tweet <- as.data.frame(prop.table(table(tweets$airline_sentiment,tweets$user_timezone)))

##Assigning names to the columns for tweet dataframe
colnames(tweet) <- c("Sentiment","TimeZone","Frequency")
tweet[order(tweet$Frequency,decreasing = T),]

##Calculating mean for the count of each sentiment
meantweets <- aggregate(tweet$Frequency,list(tweet$Sentiment),mean)
#meantweets2<- with(tweet,mean(tweet$Frequency))

##Looking at the retweet count
table(tweets$retweet_count)

##Subsetting tweets which have been retwitted more than 5 times
retweets <- tweets[tweets$retweet_count > 5,]

##Creating a dataframe with sentiments retweeted for each airline
retweets1 <- as.data.frame(prop.table(table(retweets$airline,retweets$airline_sentiment)))
retweets1$Category <- c(rep("MaxRetweets",3))

##Assigning names to the columns for tweet dataframe
colnames(retweets1) <- c("Airline","Sentiment","Frequency","Category")


##Creating a dataframe for all tweets
retweets2 <- as.data.frame(prop.table(table(tweets$airline,tweets$airline_sentiment)))
colnames(retweets2) <- c("Airline","Sentiment","Frequency")   
retweets2$Category <- c(rep("AllRetweets",3))

##Combining both the dataframes retweets1 and retweets2
cTweets <- rbind(retweets1,retweets2)

##Plot categorize ratio of tweets to show sentiments
ggplot(cTweets,aes(x=Category,y=Frequency,fill=Sentiment))+
#  facet_wrap(~Category)+
  geom_bar(stat = 'identity',width=0.5)+
    ggtitle("Overall Sentiments")+
  xlab("Category")+
  ylab("Frequency")+
  labs(fill="Sentiment")


