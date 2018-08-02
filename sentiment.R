
sentiment = function(novel){

library(tm)
library(dplyr)
library(tidytext)
#load meta data
#novel <-Corpus(DirSource(directory = "novels//Love_Stories",recursive = TRUE))
n <-length(novel)
text = unlist(novel)
text = text[1:n]
data = data.frame(text=text,file=names(novel))
data$text = as.character(data$text)
words = data %>% unnest_tokens(word,text) 
sentiment = words %>% inner_join(get_sentiments("nrc"),by="word")

positive = sentiment %>% filter(sentiment %in% "positive")%>% count(file)
negative = sentiment %>% filter(sentiment %in% "negative") %>% count(file)
anger = sentiment %>% filter(sentiment %in% "anger") %>% count(file)
anticipation = sentiment %>% filter(sentiment %in% "anticipation") %>% count(file)
disgust = sentiment %>% filter(sentiment %in% "disgust") %>% count(file)
fear = sentiment %>% filter(sentiment %in% "fear") %>% count(file)
joy = sentiment %>% filter(sentiment %in% "joy") %>% count(file)
sadness = sentiment %>% filter(sentiment %in% "sadness") %>% count(file)
surprise =  sentiment %>% filter(sentiment %in% "surprise") %>% count(file)
trust =  sentiment %>% filter(sentiment %in% "trust") %>% count(file)

sentiment_words = data.frame(file= positive$file,positive=positive$n,negative=negative$n,anger=anger$n,anticipation=anticipation$n,disgust=disgust$n,fear=fear$n,joy=joy$n,sadness=sadness$n,trust=trust$n,surprise=surprise$n)

index = c(1:n)
for (i in c(1:n)) {
  index[i] = match(data$file[i],sentiment_words$file)
}
senti =sentiment_words[index,]
senti = senti[-1]
return(senti)
}