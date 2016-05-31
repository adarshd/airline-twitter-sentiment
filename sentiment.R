### author = list(name="Shoaib Najeeb Arayilakath", )

### Setting the working firectory
setwd("/Users/552143/Desktop/airline-twitter-sentiment")

### Import the tweet data
tweets <- read.csv("data/Tweets.csv", stringsAsFactors=FALSE)

### Exploratory data analysis
## Sentiment levels and their frequencies
table(tweets$airline_sentiment)

## Filter out the negative sentiments
tweets_negative <- tweets[tweets$airline_sentiment == "negative",]

# most common reason for negative comment
sort(table(tweets_negative$negativereason), decreasing = TRUE)[1]

# airline having most negative tweets
sort(table(tweets_negative$airline), decreasing = TRUE)[1]

# find the tweet which was retweeted the most (negative sentiments)
tweets_negative[which.max(tweets_negative$retweet_count),]

## Aggregate the sentiments for different airlines
sentiment_1 <- tweets[,c("airline", "airline_sentiment")]
sentiment_1$Count <- 1
sentiment_aggr <- aggregate(sentiment_1$Count, by = list(sentiment_1$airline, sentiment_1$airline_sentiment), sum)
names(sentiment_aggr) <- c("airline", "airline_sentiment", "Sentiment_count")

# transform data into wide form
require(reshape2)
sentiment_aggr_wide <- dcast(data = sentiment_aggr, airline ~ airline_sentiment, value.var = "Sentiment_count")
