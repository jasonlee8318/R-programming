#wordcloud
install.packages("wordcloud")
install.packages("KoNLP")
install.packages("RColorBrewer")


library(wordcloud)
library(KoNLP)
library(RColorBrewer)

setwd("C:/Users/etern")

data <- readLines("ceoletter_1.txt")
data2 <- readLines("ceoletter_2.txt")

data <- sapply(data, extractNoun, USE.NAMES = F)
data2 <- sapply(data2, extractNoun, USE.NAMES = F)

data_unlist <- unlist(data)
data2_unlist <- unlist(data2)

data_unlist_f <- Filter(function(x){nchar(x)>=2}, data_unlist)
data2_unlist_f <- Filter(function(x){nchar(x)>=2}, data2_unlist)

wordcount1 <- table(data_unlist_f)
wordcount2 <- table(data2_unlist_f)

word_count_top1 <- head(sort(wordcount1, decreasing = T),50)
word_count_top2 <- head(sort(wordcount2, decreasing = T),50)

pal <- brewer.pal(7,"Set3")

wordcloud(words = names(word_count_top1),
           freq = word_count_top1,
           min.freq=5,
           max.words = 30,
           random.order = FALSE,
           rot.per = 0.1,
           scale=c(3,0.3),
           colors = pal)
           
wordcloud(words = names(word_count_top2),
           freq = word_count_top2,
           min.freq=5,
           max.words = 30,
           random.order = FALSE,
           rot.per = 0.1,
           scale=c(3,0.3),
           colors = pal)