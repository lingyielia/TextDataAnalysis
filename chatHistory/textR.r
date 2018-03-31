
rm(list = ls())
getwd()
setwd("/Users/Lingyi/TAD/chat")

library("quanteda")

content <- character()
file <- readChar("messages.txt", file.info("messages.txt")$size) 

content <- dfm(file, remove = stopwords("english"), remove_punct = TRUE)
#topfeatures(content)

textplot_wordcloud(content, max.words = 200)
