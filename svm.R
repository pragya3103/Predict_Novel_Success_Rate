rm(list = ls())

library(tm)
library(RTextTools)

library(corpus)

#load meta data
meta <-read.csv("pj2.csv")
meta <-meta[-1]
novel <-Corpus(DirSource(directory = "novels",recursive = TRUE))
sample <-sample(novel,100)
n <-length(sample)
filename <-as.data.frame(names(sample))
names(filename) <-"file"
for(i in c(1:n)){
  filename$rate[i]<-meta$rate[which(meta$file==as.character(filename$file[i]))]
}
filename$rate[which(filename$rate==2)] <-"SUCCESS"
filename$rate[which(filename$rate==1)] <-"FAILURE"

novelsample <-text_split(sample,units = "sentences",size = 50)
novelsample <-novelsample[which(novelsample$index==1),]
noveltext <-Corpus(VectorSource(novelsample$text))
clean_corpus(noveltext)

noveltdm <-DocumentTermMatrix(noveltext)
noveltdm <-removeSparseTerms(noveltdm,0.999)
novelmatrix <-data.matrix(noveltdm)

container <-create_container(novelmatrix,filename$rate,trainSize = 1:80,virgin = FALSE)
model <-train_model(container,"SVM")



test <-Corpus(DirSource(directory = "novels//Love_Stories",recursive = TRUE))
sampletest <-sample(test,10)
clean_corpus(sampletest)
testtdm <-DocumentTermMatrix(sampletest)
testtdm <-removeSparseTerms(testtdm,0.99)
testmatrix <-data.matrix(testtdm)
testmatrix <-create_matrix(testmatrix,originalMatrix = novelmat)
predict(model,newdata = testmatrix)




