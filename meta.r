rm(list = ls())
library(data.table)

meta<-as.data.frame(readLines("novel_meta.txt"))
names(meta)<-"pj"
meta<-as.data.frame(tstrsplit(meta$pj,","))
names(meta)<-c("1","2","3","4","5","6","7","8")

rate <-as.data.frame(meta$'1')
names(rate)<-"rate"
rate <-as.data.frame(tstrsplit(rate$rate,':'))
names(rate)<-c("rate","filename","file")
rate <-rate[,-2]

meta <-cbind(meta,rate)
meta <-meta[,-1]
title<-meta$`2`
title <-as.data.frame(gsub('Title:','',title))
names(title)<-"title"
meta <-cbind(title,meta)
meta <-meta[,-2]
meta <-meta[-which(is.na(meta$title)),]
metadata <-data.frame(file=meta$file,rate=meta$rate,title=meta$title)
metadata$file <-gsub((' '),'',metadata$file)
metadata$genre <-"Adventure"
metadata$genre[101:200] <-"Mystery"
metadata$genre[201:300] <-"Fiction"
metadata$genre[301:400] <-"Historical"
metadata$genre[401:500] <-"Love"
metadata$genre[501:600] <-"Poetry"
metadata$genre[601:700] <-"Scientific_Fiction"
metadata$genre[701:799] <-"Short_Stories"

write.csv(metadata,file = "meta.csv")
