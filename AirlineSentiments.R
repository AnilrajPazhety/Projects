library(ggplot2)
library(dplyr)
library(GGally)
library(stringr)

tweets <- read.csv("Tweets.csv",header = TRUE)
str(tweets)

tweets$at_count <- sapply(tweets$text, function(x) str_count(x, '@'))
maxAt <- max(tweets$at_count)

prop.table(table(tweets$airline_sentiment))
prop.table(table(Sent$airline))

prop.table(table(tweets$airline_sentiment))
tweet <- as.data.frame(prop.table(table(tweets$airline_sentiment,tweets$airline)))
colnames(tweet) <- c("Sentiment","Airline","Percent")

ggplot(tweet[,],aes(x=Airline,y=Percent,fill=as.factor(Sentiment)))+
geom_bar(stat = "identity" ,alpha = 0.5)

table(tweets$negativereason)
prop.table(table(tweets$negativereason))


tweet <- as.data.frame(prop.table(table(tweets$negativereason)))

tweet <- as.data.frame(prop.table(table(tweets$airline_sentiment,tweets$user_timezone)))

colnames(tweet) <- c("Sentiment","TimeZone","Frequency")
tweet[order(tweet$Frequency,decreasing = T),]

meantweets <- aggregate(tweet$Frequency,list(tweet$Sentiment),mean)
meantweets <- with(tweet,mean(tweet$Frequency))
meantweets

table(tweets$retweet_count)

retweets <- tweets[tweets$retweet_count > 5,]
retweets1 <- as.data.frame(prop.table(table(retweets$airline,retweets$airline_sentiment)))
retweets1$Category <- c(rep("MaxRetweets",3))

colnames(retweets1) <- c("Airline","Sentiment","Frequency","Category")

retweets2 <- as.data.frame(prop.table(table(tweets$airline,tweets$airline_sentiment)))
colnames(retweets2) <- c("Airline","Sentiment","Frequency")   
retweets2$Category <- c(rep("AllRetweets",3))

cTweets <- rbind(retweets1,retweets2)
ggplot(cTweets,aes(x=Category,y=Frequency,fill=factor(Sentiment)))+
    geom_bar(stat = 'identity',alpha=0.6)


ggplot(cTweets,aes(x=Category,y=Frequency,fill=Sentiment))+
#  facet_wrap(~Category)+
  geom_bar(stat = 'identity',width=0.5)+
    ggtitle("Overall Sentiments")+
  xlab("Category")+
  ylab("Frequency")+
  labs(fill="Sentiment")


