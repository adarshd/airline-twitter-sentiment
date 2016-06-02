### author = list(name="Shoaib Najeeb Arayilakath", email="shoaibnajeeb@gmail.com")

### Lib Requirements
install.packages("RSQLite")
install.packages("randomForest")
install.packages("caret")
install.packages("reshape2")
install.packages("tm")
install.packages("Metrics")

### Setting the working firectory
#setwd("/home/sna/Desktop/airline-twitter-sentiment")
setwd("/Users/552143/Desktop/airline-twitter-sentiment")

### Import the tweet data
## Import tweet data from csv
tweets <- read.csv("data/Tweets.csv", stringsAsFactors=FALSE)

## Import tweet data from sqlite
require(RSQLite)
conn = dbConnect(SQLite(), dbname="data/database.sqlite")
dbListTables(conn)

tweets_ql <- dbGetQuery( conn, "select * from Tweets")

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
  return(htags)
}

extract_text <- function(txt) {
  txt_1 <- strsplit(txt, split = " ")[[1]][-grep("^@", strsplit(txt, split = " ")[[1]])]
  return(paste0(txt_1, collapse = " "))
}

## Cleaning up tweets using base R
tweets$num_hash_tags <- sapply(tweets$text, function(x) length(extract_hash_tags(x)))
tweets$text_no_tags <- sapply(tweets$text, extract_text)
tweets$text_1 <- gsub("[[:punct:]]", "", tweets$text_no_tags) #remove punctuaions
tweets$text_1 <- gsub("[[:digit:]]", "", tweets$text_1) #remove digits/numbers
tweets$text_1 <- gsub("http\\w+", "", tweets$text_1) #remove hyper links
tweets$text_1 <- gsub("^\\s+|\\s+$", "", tweets$text_1) #remove leading and laggin whitespaces
tweets$text_1 <- tolower(tweets$text_1) #convert to lower case

require(tm)
tweets_corpus <- Corpus(VectorSource(tweets[,"text_1"])) #create word corpus

## Cleaning up tweets using tm
stopwords <- c(stopwords("english"), "prolife", "prochoice")
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords) #remove stop words
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace) #remove whitespaces

tweets_dtm <- TermDocumentMatrix(tweets_corpus) #create term document
feature_list <- findFreqTerms(tweets_dtm, lowfreq=200) 
tweets_dtm <- t(tweets_dtm)

tweets_df <- as.data.frame(inspect(tweets_dtm[,feature_list]))
tweets_df$Sentiments <- tweets$airline_sentiment
tweets_df$Sentiments <- as.factor(tweets_df$Sentiments)

## Splitting data into train and test
require(caret)
names(tweets_df)[grep("next", names(tweets_df))] <- "next1"
inx <- createDataPartition(y = tweets_df$Sentiments, times = 1, p = 0.7, list = FALSE)
train_tw <- tweets_df[inx,]
test_tw <- tweets_df[-inx,]

## Training rf model
set.seed(786)
require(randomForest)
fmla <- formula("Sentiments ~ .") #formula object

rftree <- randomForest(fmla, data=train_tw, importance=TRUE, 
                       ntree=1000, mtry = 36, #sampsize = 50000, 
                       do.trace = TRUE)

## Evaluating the model
require(Metrics)

preds <- predict(rftree, test_tw) #predicting for new data
result <- data.frame(Labeled=as.character(test_tw$Sentiments), Predicted=as.character(preds), stringsAsFactors = FALSE) #comparing results
c_mat <- table(Original=result$Labeled, Predicted=result$Predicted)

ce(result$Labeled, result$Predicted)
  
### Clear Workspace
rm(list = ls(all = TRUE)) 
gc(reset=TRUE)




