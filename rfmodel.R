rm(list = ls())

library(tm)
library(randomForest)

library(corpus)

#load meta data
meta <-read.csv("pj2.csv")
meta <-meta[-1]
novel <-Corpus(DirSource(directory = "novels//Adventure_Stories",recursive = TRUE))
words_per_sent = Average_word_per_sen(novel)
sentiment_words = sentiment(novel)
n <-length(novel)
filename <-as.data.frame(names(novel))
names(filename)<-"file"
#merge files and data
for(i in c(1:n)){
  filename$rate[i] <-meta$rate[meta$file==as.character(filename$file[i])]
}
filename$rate[which(filename$rate==1)] <-"FAILURE"
filename$rate[which(filename$rate==2)] <-"SUCCESS"

novel = tm_map(novel,tolower)
novel = tm_map(novel,removePunctuation)
novel = tm_map(novel,removeNumbers)
novel = tm_map(novel,stripWhitespace)
novel = tm_map(novel,stemDocument)


Adventdm = DocumentTermMatrix(novel,control = list(stopwords=TRUE))
Adventdm = removeSparseTerms(Adventdm,0.999)

freq <-slam::col_sums(Adventdm)
Adventmat = data.matrix(Adventdm)
Adventdata <-as.data.frame(Adventmat)
Adventdata <-rbind(Adventdata,freq)
nrow <-nrow(Adventdata)
Advent <-Adventdata[order(Adventdata[nrow,],decreasing = TRUE)]
Advent <-Advent[1:(nrow-1),]
Adventdata <-Advent[,1:200]
data = cbind(Adventdata,words_per_sent,sentiment_words)

train <-n*0.8
test <-n*0.2

training_set <-data[1:train,]
test_set <-data[(train+1):n,]
filename$rate <-as.factor(filename$rate)
actual <-filename$rate[(train+1):n]
classifier = randomForest(x = training_set,
                          y = filename$rate[1:train],
                          ntree = 10)
y_pred = predict(classifier, newdata = test_set)
cm = table(actual, y_pred)
accuracy <-(cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
save(classifier,file = "randf")
load("randf")
