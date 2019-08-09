install.packages("rtweet")
install.packages("tm")
install.packages("NLP")
install.packages("wordcloud2")
install.packages("twitteR")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("xlsx")
install.packages("devtools")


library(rtweet)
library(NLP)
library(tm)
library(RCOlorBrewer)
library(wordcloud2)
library(twitteR)
library(ggplot2)
library(devtools)

library(xlsx)
library(dplyr)
library(magrittr)
library(ggpubr)

#api
consumer_key <- "ODsIVPNdvvHTZni1D1kw2cL7R"
consumer_secret <- "MlkJhH6ZmhTnsWnkYmqDTp33kRxtywFEQaxo71ItfKxBwS18E5"
access_token    <- "1144113724946432001-inqUzSkNP0kykG4ol6WfKwsK0oLqIZ"
access_secret   <- "K3lcrsrTVeRce0t2stz9hXWxCMLi8NDWTVRSWL6rOIGGq"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#                   === KEYWORD ===
#pencapaian presiden jokowi = 146
#kinerja presiden jokowi = 62
#prestasi pemerintahan jokowi = 8
#pembangunan pemerintahan jokowi = 182
#kemajuan pemerintahan jokowi = 13
#program kerja pemerintahan jokowi = 9

#tw = searchTwitter('pembangunan pemerintahan jokowi', n = 10000, retryOnRateLimit = 10e3)
tw = searchTwitter('pencapaian presiden jokowi', n = 10000)
tw2 = searchTwitter('pembangunan pemerintahan jokowi', n = 10000)
tw3 = searchTwitter('kinerja presiden jokowi', n = 10000)
tw4 = searchTwitter('prestasi pemerintahan jokowi', n = 10000)
tw5 = searchTwitter('kemajuan pemerintahan jokowi', n = 10000)
tw6 = searchTwitter('program kerja pemerintahan jokowi', n = 10000)

head(tw,10)

#save data ke data frame
tw.df = twListToDF(tw)
tw2.df = twListToDF(tw2)
tw3.df = twListToDF(tw3)
tw4.df = twListToDF(tw4)
tw5.df = twListToDF(tw5)
tw6.df = twListToDF(tw6)

#menggabungkan data twett
data = tw.df
data = rbind(data, data.frame(tw2.df))
data = rbind(data, data.frame(tw3.df))
data = rbind(data, data.frame(tw4.df))
data = rbind(data, data.frame(tw5.df))
data = rbind(data, data.frame(tw6.df))

#cleaning data
cleandata = unique(data, by="id")

#simpen data ke csv
write.csv(cleandata, file ="data_twett.csv", row.names =T)

#presentasi run dari sini############

cleandata = read.csv("data_twett.csv")

datframe = data.frame(cleandata)

##lanjut ke asosiasi
## hanya ambil data tweet saja
komen <- datframe$text
komenc <- Corpus(VectorSource(komen))


#cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)
removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)
replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)
removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)
removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)
removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)
removetitik3 <- function(y) gsub("p…", "", y)
twitclean <- tm_map(twitclean, removetitik3)
removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)
removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)
remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)

#cleaning kata hubung
myStopwords = readLines("stopword-id.txt")
twitclean <- tm_map(twitclean,removeWords,myStopwords)
twitclean <- tm_map(twitclean , removeWords, 
                    c('pak','bpk','bilang','serta','untuk','dan',''))

#matrix kata 
{
  dtm <- TermDocumentMatrix(twitclean)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
}

d_freq_word = data.frame(d)

write.csv(d_freq_word, file ="d.csv", row.names =T)

temp = d[12:20,]

dfhead = data.frame(head(d, n=40))

ggplot(data=temp, aes(x=reorder(word, -freq), y=freq)) +
  geom_bar(stat="identity", width=0.5)

wordcloud2(dfhead,shape = "cloud",
           backgroundColor = "black",
           color = 'random-light' ,
           size = 0.5)
