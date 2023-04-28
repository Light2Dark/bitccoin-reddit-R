# # Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("syuzhet") # for sentiment analysis
# install.packages("ggplot2") # for plotting graphs

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Read the csv file for June 2022 Comments from r/Bitcoin
raw_data <- read.csv("reddit-r-bitcoin-data-for-jun-2022-comments.csv", header=T)

# pre-processing 

# setting data type for date using POSIX, a date-time conversion function, converts created_utc field to standard date type
raw_data$created_utc <- as.POSIXct(raw_data$created_utc, origin="1970-01-01")
# creating new date column
raw_data$Date <- as.Date(raw_data$created_utc)

# Setting date to the last day only
raw_data <- raw_data[raw_data$Date == "2022-06-30", ]

# only looking at first 500 rows, to speed up processing
raw_data <- raw_data[0:500,]

# comments only
comments <- raw_data["body"]
# karma score
score <- raw_data["score"]

# removing all [deleted] and [removed] comments
comments<-comments[!(comments$body=="[deleted]" | comments$body=="[removed]"),]
# replace uppercase with lowercase letters
comments <- tolower(comments)
# convert encoding to UTF-8, eg emojis will be converted to text
comments <- iconv(comments,"WINDOWS-1252","UTF-8")
# remove non-words like numbers and extra spaces, keep ' marks
comments <- gsub("[^A-Za-z ']", "", comments)
comments <- gsub("  ", "", comments)
# remove empty rows
comments <- comments[comments != ""]

# reversing the dataset so that we have oldest to latest comments
# used for when we plot the trend of sentiment over time (time being the oldest -> latest comments)
comments <- rev(comments)

# syuzhet sentiment score for each row of comments
syuzhet_vector <- get_sentiment(comments, method="syuzhet")
# bing sentiment score
bing_vector <- get_sentiment(comments, method="bing")
# affin sentiment score
afinn_vector <- get_sentiment(comments, method="afinn")

# compare the first row of each vector
print (rbind(
  head(syuzhet_vector),
  head(bing_vector),
  head(afinn_vector)
))

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
comments_nrc<-get_nrc_sentiment(comments)

# transpose or inverses data frame of comments_nrc
td<-data.frame(t(comments_nrc))
# The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[1:300]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

# creating pdf file
pdf(file="reddit_comments_sentiment_analysis.pdf", width=9.5, height=7.5)

# Plotting the count of words with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="auto", fill=sentiment, ylab="count")+ggtitle("General Emotion word count of Reddit comments on 30th June")

# Plotting the count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(comments_nrc[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.8, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage",
  col = topo.colors(8)
)

# we can plot the sentiment trend-line by plotting a smooth curve
# using discrete cosine transformation which is better to represent edge values compared to Fourier transform method
dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
# emotional valence refers to the normalized emotional value (-1 being negative & 1 being positive)
plot(
  dct_values, 
  type ="l", 
  main ="Sentiment Trend of Reddit comments using DCT Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "blue"
)

# Specific Emotion Analysis over time
# nrc_sentiment
comments_nrc<-get_nrc_sentiment(comments)

# creating a function to smoothly scale our values of sentiment
# discrete cosine transformation is better to prepresent edge values compared to Fourier Transform method
get_dct_values <- function(data) {
  # the values are scaled from -1 to 1, so that comparison can be done effectively
  dct_values <- get_dct_transform(
    data, 
    low_pass_size = 5, 
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = T
  )
  return(dct_values)
}

# function to plot out the dct values of sentiment against time
plot_trend <- function(data, plot_title, line_colour) {  
  # calls get_dct_values function
  data <- get_dct_values(data)
  # plots dct_values against time to show sentiment trend
  plot(
    data, 
    type ="l", 
    main =plot_title, 
    xlab = "Narrative Time", 
    ylab = "Emotional Valence", 
    col = line_colour
  )
}

# plot sad sentiment trend against time
plot_trend(comments_nrc$sad, "Sad Trend of Reddit comments", "blue")

# apply dct function on whole nrc vectors
dct_sentiment_nrc <- lapply(comments_nrc, get_dct_values)
# convert to dataframe
dct_sentiment_nrc <- data.frame(dct_sentiment_nrc)


# firstly, add row_number as column to make it an identifier variable
dct_sentiment_nrc$row_num <- row.names(dct_sentiment_nrc)

# plotting all sentiments trend against narrative time
# setting  colours
colours = c("red", "orange", "lightgreen", "darkgreen", "blue", "darkblue", "violet", "gray", "purple", "black")
# matplot plots all the columns in dataframe and the x-value is the row_num which represents narrative time
matplot(dct_sentiment_nrc$row_num, dct_sentiment_nrc[,0:8], type="l", col=colours, xlab="Narrative Time", ylab="Emotional Valence", main="All emotion sentiments over time", lwd=2.0)
legend("topright", legend=(names(dct_sentiment_nrc)[0:8]), col=colours, pch=1)


# Mixed Messages - Emotional ambiguity or contradiction in texts
# the mixed_messages function identifies sentences which have contradicting statements
# emotional entropy is a measure of the unpredictability and surprise in the sentences

# pre-processes the comments using syuzhet get_sentences function
comments_sents <- get_sentences(comments)
# form a dataframe so that we can apply a function to the whole dataset
comments_sents <- data.frame(comments_sents)
# applying the mixed_messages function to the whole dataset of comments
test <- lapply(comments_sents, mixed_messages)
# do.call is another way to call a function, where we call rbind and pass in comments_mixed as the arg
# rbind wil combine the rows of the comments_mixed dataframe
entropes <- do.call(rbind, test)
# forming a dataframe, and making sure the strings are not factors datatype
out <- data.frame(entropes, comments_sents, stringsAsFactors = FALSE)
# a double plot on the dataframe, one shows the individual columns in the dataframe, and the other is a simplified plot
simple_plot(out$entropy,title = "Emotional Entropy in Reddit Comments",legend_pos = "topleft")

# closing pdf writer
dev.off()