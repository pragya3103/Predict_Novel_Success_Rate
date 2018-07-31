rm(list = ls())
#load the libraries
library(tm)
library(NLP)
library(corpus)
library(data.table)
#clean corpus function
clean_corpus<-function(mycorpus){
  mycorpus<-tm_map(mycorpus,tolower)
  mycorpus<-tm_map(mycorpus,removePunctuation)
  mycorpus<-tm_map(mycorpus,removeWords,stopwords('english'))
  mycorpus<-tm_map(mycorpus,removeNumbers)
  mycorpus<-tm_map(mycorpus,stemDocument)
  mycorpus = tm_map(mycorpus, stripWhitespace)
}



Science <-Corpus(DirSource(directory = "D:\\work\\ml\\novel\\novels\\Science_Fiction",recursive = TRUE))
Science <-sample(Science,5)
clean_corpus(Science)
filename <-data.frame(file=names(Science))
sciencedf <- data.frame(text=unlist(Science),genre="Science")
Love_Story <- Corpus(DirSource(directory = "D:\\work\\ml\\novel\\novels\\Love_Stories",recursive = TRUE))
Love_Story <-sample(Love_Story,5)
clean_corpus(Love_Story)
x <-data.frame(file=names(Love_Story))
lovedf <- data.frame(text=unlist(Love_Story),genre="love_Story")
novel <-rbind(sciencedf,lovedf)
filename <-rbind(filename,x)
novel <-novel[-which(novel$text=='en'),]
novel$file <-filename
 data <-merge(novel,metadata,intersect(names(novel),names(metadata))) 
merge(novel,metadata)->y
 