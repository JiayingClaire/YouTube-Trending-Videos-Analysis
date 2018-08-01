# YouTube-Trending-Videos-Analysis

# load and review data
```{r}
usvideos <- read.csv("~/Desktop/5200 AAFM/Group Project/USvideos.csv")
View(usvideos)
names(usvideos)
str(usvideos)
summary(usvideos)
```

# Deal with video repetitions of the data
```{r}
sum(duplicated(usvideos$title))
sum(duplicated(usvideos$channel_title))
usvideos <- usvideos[!duplicated(usvideos$title), ]
usvideos <- usvideos[!duplicated(usvideos$channel_title), ]
sum(duplicated(usvideos$title))
sum(duplicated(usvideos$channel_title))
```

# Seperate the date and time
```{r}
library(tidyr)
usvideos <- separate(usvideos, publish_time, into = c("release_date", "release_time"), sep = "T")
names(usvideos)
usvideos[1:5,6:7]
```

# Remove the columns of "category_id" and "video_error_or_removed"
```{r}
usvideos$category_id <- NULL
usvideos$video_error_or_removed <- NULL
names(usvideos)
```

# Remove minutes, seconds and the following ".000Z" (only remain the hours of "release_time")
```{r}
library(stringr)
usvideos$release_time <- str_sub(usvideos$release_time, 1, str_length(usvideos$release_time)-11)
usvideos$release_time[1:5]
```

# Add "release_weekday" column to analyze if the number of views is related to the day of week as well
```{r}
usvideos$release_weekday <- weekdays(as.Date(usvideos$release_date))
usvideos$release_weekday[1:5]
```

# Utilize liner regression models to learn the existence and strength of the correlations between: dependent variable: number of views, and independent variables: number of likes, number of dislikes, number of comments.

# Regression Model1: views VS. likes
```{r}
regModel1 = lm(views~likes,usvideos)
summary(regModel1)
```

# Plot regression line1: views VS. likes
```{r}
library(ggplot2)
ggplot(data=usvideos,aes(x=likes,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

# Regression Model2: views VS. dislikes
```{r}
regModel2 = lm(views~dislikes,usvideos)
summary(regModel2)
```

# Plot regression line2: views VS. dislikes
```{r}
ggplot(data=usvideos,aes(x=dislikes,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

# Regression Model3: views VS. comment_count
```{r}
regModel3 = lm(views~comment_count,usvideos)
summary(regModel3)
```

# Plot regression line3: views VS. comment_count
```{r}
ggplot(data=usvideos,aes(x=comment_count,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

# Model with many independent variables to verify the correlations and strengths 
```{r}
model6 = lm(views~likes+dislikes+comment_count+release_time+release_weekday,data=usvideos)
summary(model6)
```

# Additional analysis: 
# Through online research, some “experts” claimed that one should add as many tags as you can think of to get more views. 
# Also long video descriptions can help get more views. 
# So, apply linear regression to verify (results show no correlations)

# views VS. tag length
```{r}
taglength <- nchar(as.character(usvideos$tags))
taglength[1:5]

regModel7 = lm(views~taglength,usvideos)
summary(regModel7)
```

# views VS. description length
```{r}
deslength <- nchar(as.character(usvideos$description))
deslength[1:5]

regModel8 = lm(views~deslength,usvideos)
summary(regModel8)
```

# Explore relationship between views VS. release day and time

# Barchart with release_weekday
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos)+
  geom_bar(stat='summary',fun.y='mean')+ggtitle('choose the best weekday to release your video')+coord_flip()
```

# Barchart with release_weekday based on different time slot 
# 9:00 AM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "09",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 9:00AM')+coord_flip()
```

# 11:00 AM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "11",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 11:00AM')+coord_flip()
```

# 13:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "13",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 13:00PM')+coord_flip()
```

# 15:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "15",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 15:00PM')+coord_flip()
```

# 17:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "17",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 17:00PM')+coord_flip()
```

# 19:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "19",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 19:00PM')+coord_flip()
```

# 21:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "21",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 21:00PM')+coord_flip()
```

# 23:00 PM
```{r}
ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "23",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 23:00PM')+coord_flip()
```

# Barchart with release_time
```{r}
ggplot(aes(x=reorder(release_time, views), y=views, color=factor(release_time)),data=usvideos)+
  geom_bar(stat='summary',fun.y='mean')+ggtitle('choose the best time slot to release your video')+coord_flip()
```

# Barchart with release_time based on different release_weekday 
# Monday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Monday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Monday')+coord_flip()
```

# Tuesday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Tuesday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Tuesday')+coord_flip()
```

# Wednesday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Wednesday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Wednesday')+coord_flip()
```

# Thursday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Thursday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Thursday')+coord_flip()
```

# Friday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Friday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Friday')+coord_flip()
```

# Saturday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Saturday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Saturday')+coord_flip()
```

# Sunday
```{r}
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Sunday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Sunday')+coord_flip()
```

# Apply the text-mining methods

# Explore the general sentiments of YouTube trending list videos
```{r}
library(tidytext)
library(dplyr)
library(twitteR)
library(ROAuth)
library(rlang)

get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()
```

# Extract the emotions described in the title, tag and descriptions of YouTube trending list videos
```{r}
get_sentiments('nrc')%>%
  group_by(sentiment)%>%
  count()
```

# Use wordcloud to extract the keywords of trending video titles
# Install and load the packages
```{r}
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud") 
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
```

# Import the title text, and load the title text as a corpus
```{r}
titletext <- usvideos$title
titledocs <- Corpus(VectorSource(titletext))
```

# Further clean the title content:
# Convert the text to lower case
# Remove numbers
# Remove common english stopwords
# Remove all the punctuations
```{r}
titledocs <- tm_map(titledocs, content_transformer(tolower))
titledocs <- tm_map(titledocs, removeNumbers)
titledocs <- tm_map(titledocs, removeWords, stopwords("en"))
titledocs <- tm_map(titledocs, removeWords, c("video","day","full","one","-","make","john","audio"))
titledocs <- tm_map(titledocs, removePunctuation)
```

# Build a term-document matrix
```{r}
dtm <- TermDocumentMatrix(titledocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
```

# Generate the Word cloud for key words of trending video title
```{r}
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

# Use similar processes to generate a wordcloud to extract the keywords of trending video channels 
```{r}
channeltext <- usvideos$channel_title
channeldocs <- Corpus(VectorSource(channeltext))

channeldocs <- tm_map(channeldocs, content_transformer(tolower))
channeldocs <- tm_map(channeldocs, removeNumbers)
channeldocs <- tm_map(channeldocs, removeWords, stopwords("en"))
channeldocs <- tm_map(channeldocs, removeWords, c("channel","chris","david","john","mike","tom","late","abc","ryan","johnson","ben","kevin","taylor"))
channeldocs <- tm_map(channeldocs, removePunctuation)

dtm <- TermDocumentMatrix(channeldocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
```

# Generate the Word cloud for key words of trending video channel
```{r}
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

# Use similar processes to generate a wordcloud to extract the keywords of trending video tags 
```{r}
tagstext <- usvideos$tags
tagsdocs <- Corpus(VectorSource(tagstext))

tagsdocs <- tm_map(tagsdocs, content_transformer(tolower))
tagsdocs <- tm_map(tagsdocs, removeNumbers)
tagsdocs <- tm_map(tagsdocs, removeWords, stopwords("en"))
tagsdocs <- tm_map(tagsdocs, removeWords, c("none","make","video","like","things","box","one","get"))
tagsdocs <- tm_map(tagsdocs, removePunctuation)

dtm <- TermDocumentMatrix(tagsdocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
```

# Generate the Word cloud for key words of trending video tags
```{r}
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

# Use similar processes to generate a wordcloud to extract the keywords of trending video descriptions
```{r}
destext <- usvideos$description
desdocs <- Corpus(VectorSource(destext))

desdocs <- tm_map(desdocs, content_transformer(tolower))
desdocs <- tm_map(desdocs, removeNumbers)
desdocs <- tm_map(desdocs, removeWords, stopwords("en"))
desdocs <- tm_map(desdocs, removeWords, c("video","one","get","channels","videos","like","will","can","make","just","see","use","know","watch","channel","subscribe","time","►","every","day","way","available","made","'s","also","want","people","full","follow","check","things","please","-","links","find","got","year","used","thanks","years","twitter","facebook","instagram","youtube","►","’s","back","-","website","camera","big","little","series"))
desdocs <- tm_map(desdocs, removePunctuation)

dtm <- TermDocumentMatrix(desdocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
```

# Generate the Word cloud for key words of trending video descriptions
```{r}
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```
