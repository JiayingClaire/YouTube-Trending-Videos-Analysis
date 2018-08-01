```{r}
# YouTube-Trending-Videos-Analysis
# load and review data

usvideos <- read.csv("~/Desktop/5200 AAFM/Group Project/USvideos.csv")
View(usvideos)
names(usvideos)
str(usvideos)
summary(usvideos)
```

```{r}
# Deal with video repetitions of the data

sum(duplicated(usvideos$title))
sum(duplicated(usvideos$channel_title))
usvideos <- usvideos[!duplicated(usvideos$title), ]
usvideos <- usvideos[!duplicated(usvideos$channel_title), ]
sum(duplicated(usvideos$title))
sum(duplicated(usvideos$channel_title))
```

```{r}
# Seperate the date and time

library(tidyr)
usvideos <- separate(usvideos, publish_time, into = c("release_date", "release_time"), sep = "T")
names(usvideos)
usvideos[1:5,6:7]
```

```{r}
# Remove the columns of "category_id" and "video_error_or_removed"

usvideos$category_id <- NULL
usvideos$video_error_or_removed <- NULL
names(usvideos)
```

```{r}
# Remove minutes, seconds and the following ".000Z" (only remain the hours of "release_time")

library(stringr)
usvideos$release_time <- str_sub(usvideos$release_time, 1, str_length(usvideos$release_time)-11)
usvideos$release_time[1:5]
```

```{r}
# Add "release_weekday" column to analyze if the number of views is related to the day of week as well

usvideos$release_weekday <- weekdays(as.Date(usvideos$release_date))
usvideos$release_weekday[1:5]
```

```{r}
# Utilize liner regression models to learn the existence and strength of the correlations between: dependent variable: number of views, and independent variables: number of likes, number of dislikes, number of comments.

# Regression Model1: views VS. likes

regModel1 = lm(views~likes,usvideos)
summary(regModel1)
```

```{r}
# Plot regression line1: views VS. likes

library(ggplot2)
ggplot(data=usvideos,aes(x=likes,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

```{r}
# Plot regression line1: views VS. likes

regModel2 = lm(views~dislikes,usvideos)
summary(regModel2)
```

```{r}
# Plot regression line1: views VS. likes

ggplot(data=usvideos,aes(x=dislikes,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

```{r}
# Regression Model3: views VS. comment_count

regModel3 = lm(views~comment_count,usvideos)
summary(regModel3)
```

```{r}
# Regression Model3: views VS. comment_count

ggplot(data=usvideos,aes(x=comment_count,y=views))+
  geom_point()+
  geom_smooth(method='lm')
```

```{r}
# Model with many independent variables to verify the correlations and strengths 

model6 = lm(views~likes+dislikes+comment_count+release_time+release_weekday,data=usvideos)
summary(model6)
```

```{r}
# Additional analysis: 
# Through online research, some “experts” claimed that one should add as many tags as you can think of to get more views. 
# Also long video descriptions can help get more views. 
# So, apply linear regression to verify (results show no correlations)

# views VS. tag length

taglength <- nchar(as.character(usvideos$tags))
taglength[1:5]

regModel7 = lm(views~taglength,usvideos)
summary(regModel7)
```

```{r}
# views VS. description length

deslength <- nchar(as.character(usvideos$description))
deslength[1:5]

regModel8 = lm(views~deslength,usvideos)
summary(regModel8)
```

```{r}
# Explore relationship between views VS. release day and time

# Barchart with release_weekday

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos)+
  geom_bar(stat='summary',fun.y='mean')+ggtitle('choose the best weekday to release your video')+coord_flip()
```

```{r}
# Barchart with release_weekday based on different time slot 
# 9:00 AM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "09",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 9:00AM')+coord_flip()
```

```{r}
# 11:00 AM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "11",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 11:00AM')+coord_flip()
```

```{r}
# 13:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "13",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 13:00PM')+coord_flip()
```

```{r}
# 15:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "15",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 15:00PM')+coord_flip()
```

```{r}
# 17:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "17",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 17:00PM')+coord_flip()
```

```{r}
# 19:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "19",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 19:00PM')+coord_flip()
```

```{r}
# 21:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "21",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 21:00PM')+coord_flip()
```

```{r}
# 23:00 PM

ggplot(aes(x=reorder(release_weekday,views), y=views, color=factor(release_weekday)),data=usvideos[usvideos$release_time == "23",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best weekday to release your video @ 23:00PM')+coord_flip()
```

```{r}
# Barchart with release_time

ggplot(aes(x=reorder(release_time, views), y=views, color=factor(release_time)),data=usvideos)+
  geom_bar(stat='summary',fun.y='mean')+ggtitle('choose the best time slot to release your video')+coord_flip()
```

```{r}
# Barchart with release_time based on different release_weekday 
# Monday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Monday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Monday')+coord_flip()
```

```{r}
# Tuesday
ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Tuesday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Tuesday')+coord_flip()
```

```{r}
# Wednesday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Wednesday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Wednesday')+coord_flip()
```

```{r}
# Thursday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Thursday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Thursday')+coord_flip()
```

```{r}
# Friday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Friday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Friday')+coord_flip()
```

```{r}
# Saturday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Saturday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Saturday')+coord_flip()
```

```{r}
# Sunday

ggplot(aes(x=reorder(release_time,views), y=views, color=factor(release_time)),data=usvideos[usvideos$release_weekday == "Sunday",])+
  geom_bar(stat='summary',fun.y='mean')+
  ggtitle('choose the best time slot to release your video on Sunday')+coord_flip()
```

```{r}
# Apply the text-mining methods
# Explore the general sentiments of YouTube trending list videos

library(tidytext)
library(dplyr)
library(twitteR)
library(ROAuth)
library(rlang)

get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()
```

```{r}
# Extract the emotions described in the title, tag and descriptions of YouTube trending list videos

get_sentiments('nrc')%>%
  group_by(sentiment)%>%
  count()
```

```{r}
# Use wordcloud to extract the keywords of trending video titles
# Install and load the packages

install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud") 
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
```

```{r}
# Import the title text, and load the title text as a corpus

titletext <- usvideos$title
titledocs <- Corpus(VectorSource(titletext))
```

```{r}
# Further clean the title content:
# Convert the text to lower case
# Remove numbers
# Remove common english stopwords
# Remove all the punctuations

titledocs <- tm_map(titledocs, content_transformer(tolower))
titledocs <- tm_map(titledocs, removeNumbers)
titledocs <- tm_map(titledocs, removeWords, stopwords("en"))
titledocs <- tm_map(titledocs, removeWords, c("video","day","full","one","-","make","john","audio"))
titledocs <- tm_map(titledocs, removePunctuation)
```

```{r}
# Build a term-document matrix

dtm <- TermDocumentMatrix(titledocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
```

```{r}
# Build a term-document matrix

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

```{r}
# Use similar processes to generate a wordcloud to extract the keywords of trending video channels 

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

```{r}
# Use similar processes to generate a wordcloud to extract the keywords of trending video channels 

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

```{r}
# Use similar processes to generate a wordcloud to extract the keywords of trending video channels 

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

```{r}
# Generate the Word cloud for key words of trending video tags

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

```{r}
# Use similar processes to generate a wordcloud to extract the keywords of trending video descriptions

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

```{r}
# Generate the Word cloud for key words of trending video descriptions

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```
