### author = list(name="Shoaib Najeeb Arayilakath", email="shoaibnajeeb@gmail.com")

### Setting the working firectory
setwd("/home/sna/Desktop/airline-twitter-sentiment") #/Users/552143/Desktop/airline-twitter-sentiment

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

### Working with dates
tweets$tweet_created_1 <- as.POSIXct(strftime(tweets$tweet_created, format = "%Y-%m-%d %H:%M:%S %z"))
tweets$tweet_created_day <- as.Date(tweets$tweet_created, format = "%Y-%m-%d")

### Working with tweet text
extract_hash_tags <- function(txt) {
  htags <- strsplit(txt, split = " ")[[1]][grep("^@", strsplit(txt, split = " ")[[1]])]
  txt_1 <- strsplit(txt, split = " ")[[1]][-grep("^@", strsplit(txt, split = " ")[[1]])]
  #return(htags)
  return(paste0(txt_1, collapse = " "))
}
tweets$num_hash_tags <- sapply(tweets$text, function(x) length(extract_hash_tags(x)))
tweets$text_no_tags <- sapply(tweets$text, extract_hash_tags)
tweets$text_1 <- gsub("[[:punct:]]", "", tweets$text_no_tags)
tweets$text_1 <- gsub("[[:digit:]]", "", tweets$text_1)
tweets$text_1 <- gsub("http\\w+", "", tweets$text_1)
tweets$text_1 <- gsub("^\\s+|\\s+$", "", tweets$text_1)
tweets$text_1 <- tolower(tweets$text_1)

require(tm)
tweets_corpus <- Corpus(VectorSource(tweets[,"text_1"]))

# remove generic and custom stopwords
stopwords <- c(stopwords("english"), "prolife", "prochoice")
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords)
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)

tweets_dtm <- TermDocumentMatrix(tweets_corpus)
inspect(tweets_dtm[,1:5])
feature_list <- findFreqTerms(tweets_dtm, lowfreq=250)
tweets_dtm <- t(tweets_dtm)

View(as.matrix(tweets_dtm)
