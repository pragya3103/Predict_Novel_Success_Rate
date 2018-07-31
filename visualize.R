rm(list = ls())
library(dplyr)
library(corpus)
library(tm)
library(tidytext)
library(ggplot2)

meta <-read.csv("meta.csv")
novel <-Corpus(DirSource(directory = "novels",recursive = TRUE))
sample <-sample(novel,100)
n=length(sample)
text <-unlist(sample)
text <-text[-(n+1)]
data <-data.frame(text=text,file=names(sample))
data <-merge(data,meta,"file")
data <-data[unique(data$file),]
data$text <-as.character(data$text)

genre_data <-data %>% group_by(genre) %>%summarise(count=unique(length(title)))%>%arrange(desc(count))
plot_genre <-wordcloud2::wordcloud2(genre_data,size = 0.3,shape = "star")

words <-data %>% unnest_tokens(word,text)
word_count <-words %>% count(genre)

genre_word_plot <-word_count %>% arrange(desc(n)) %>% top_n(n=10)%>%ggplot(aes(x=factor(genre,levels = genre),y=n))+  
      geom_col(col="Red",fill="yellow",size=1)+labs(x="genre",y="word_count",title="words per genre")

      