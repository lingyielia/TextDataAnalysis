
# Clear Global Environment and Set working directory
rm(list = ls())
getwd()
setwd("/Users/Lingyi/TAD/HW1")
set.seed(8888)

library("quanteda")
library(quanteda.corpora)
library(tidyverse)
library(stringr)
library(tm)
library(boot)

#loading sotu data
data("data_corpus_sotu", package = "quanteda.corpora")

# ndoc identifies the number of documents in a corpus
ndocs <- ndoc(data_corpus_sotu)
ndocs

class(data_corpus_sotu)

# inspect the document-level variables
head(docvars(data_corpus_sotu))

presDfm <- dfm(data_corpus_sotu, 
               groups = c("FirstName", "President"), verbose = FALSE)
head(docnames(presDfm), 20)

#subset all speeches by Franklin D. Roosevelt
Rooseveltcorpus <- corpus_subset(data_corpus_sotu, FirstName == "Franklin D.")
summary(Rooseveltcorpus)

#Find the speeches of 1936 and 1945
Rooseveltcorpus_1936 <- Rooseveltcorpus[3]
Rooseveltcorpus_1945 <- Rooseveltcorpus[ndoc(Rooseveltcorpus)]

#tokenize
Roosevelttokens_1936 <- tokens(Rooseveltcorpus_1936, remove_punct = TRUE)
Roosevelttokens_1945 <- tokens(Rooseveltcorpus_1945, remove_punct = TRUE)

#calculate the TTR
Roosevelttokens_1936_TTR <- textstat_lexdiv(dfm(
    Roosevelttokens_1936, tolower = FALSE, 
                          remove_punct = TRUE), measure = "TTR")
#The same as below
#Roosevelttokens_1936_TTR <- ntype(Roosevelttokens_1936) / lengths(Roosevelttokens_1936)

#calculate the TTR
Roosevelttokens_1945_TTR <- textstat_lexdiv(dfm(
    Roosevelttokens_1945, tolower = FALSE, 
                          remove_punct = TRUE), measure = "TTR")
#The same as below
#Roosevelttokens_1945_TTR <- ntype(Roosevelttokens_1945) / lengths(Roosevelttokens_1945)

print(Roosevelttokens_1936_TTR)
print(Roosevelttokens_1945_TTR)
Roosevelttokens_1936
#calculate the cosine distance. 
#no pre-processing other than to remove the punctuation
Roosevelt2Dfm <- dfm(c(Rooseveltcorpus_1936, Rooseveltcorpus_1945), 
                     stem = FALSE, 
                     tolower = FALSE, 
                     remove_punct = TRUE)

RooseveltSimil <- textstat_simil(Roosevelt2Dfm, 
                                 margin = "documents", 
                                 method = "cosine")

as.matrix(RooseveltSimil)

#(i)


#(ii)
Roosevelttokens_1936_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1936, 
                    stem = TRUE, 
                    tolower = FALSE, 
                    remove_punct = TRUE), measure = "TTR")

Roosevelttokens_1945_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1945, 
                    stem = TRUE, 
                    tolower = FALSE, 
                    remove_punct = TRUE), measure = "TTR")

print(Roosevelttokens_1936_TTR)
print(Roosevelttokens_1945_TTR)

#(iii)
Roosevelt2Dfm <- dfm(c(Rooseveltcorpus_1936, Rooseveltcorpus_1945), 
                     stem = TRUE, 
                     tolower = FALSE, 
                     remove_punct = TRUE)

RooseveltSimil <- textstat_simil(Roosevelt2Dfm, 
                                 margin = "documents", 
                                 method = "cosine")
as.matrix(RooseveltSimil)

#(i)


#(ii)
Roosevelttokens_1936_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1936, 
                    stem = FALSE, 
                    tolower = FALSE, 
                    remove_punct = TRUE,
                    remove = stopwords("english")), measure = "TTR")

Roosevelttokens_1945_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1945, 
                    stem = FALSE, 
                    tolower = FALSE, 
                    remove_punct = TRUE,
                    remove = stopwords("english")), measure = "TTR")

print(Roosevelttokens_1936_TTR)
print(Roosevelttokens_1945_TTR)

#(iii)
Roosevelt2Dfm <- dfm(c(Rooseveltcorpus_1936, Rooseveltcorpus_1945), 
                     stem = FALSE, 
                     tolower = FALSE, 
                     remove_punct = TRUE,
                     remove = stopwords("english"))

RooseveltSimil <- textstat_simil(Roosevelt2Dfm, 
                                 margin = "documents", 
                                 method = "cosine")
as.matrix(RooseveltSimil)

#(i)


#(ii)
Roosevelttokens_1936_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1936, 
                    stem = FALSE, 
                    tolower = TRUE, 
                    remove_punct = TRUE), measure = "TTR")

Roosevelttokens_1945_TTR <- textstat_lexdiv(dfm(Roosevelttokens_1945, 
                    stem = FALSE, 
                    tolower = TRUE, 
                    remove_punct = TRUE), measure = "TTR")

print(Roosevelttokens_1936_TTR)
print(Roosevelttokens_1945_TTR)

#(iii)
Roosevelt2Dfm <- dfm(c(Rooseveltcorpus_1936, Rooseveltcorpus_1945), 
                     stem = FALSE, 
                     tolower = TRUE, 
                     remove_punct = TRUE)

RooseveltSimil <- textstat_simil(Roosevelt2Dfm, 
                                 margin = "documents", 
                                 method = "cosine")
as.matrix(RooseveltSimil)

#print(ntype(Roosevelttokens_1936))
#print(lengths(Roosevelttokens_1936))

#print(ntype(Roosevelttokens_1945))
#print(lengths(Roosevelttokens_1945))

print(paste0("Roosevelt-1936 type: ", 
             ntype(Roosevelttokens_1936),
             "; lengths: ",
             lengths(Roosevelttokens_1936)))
print(paste0("Roosevelt-1945 type: ", 
             ntype(Roosevelttokens_1945),
             "; lengths: ",
             lengths(Roosevelttokens_1945)))

cat(paste0("Roosevelt-1936 type: ", 
             ntype(Roosevelttokens_1936),
             "; lengths: ",
             lengths(Roosevelttokens_1936)),paste0("\nRoosevelt-1945 type: ", 
             ntype(Roosevelttokens_1945),
             "; lengths: ",
             lengths(Roosevelttokens_1945)))

#splitting up strings by word
words_1936 <- str_split(Rooseveltcorpus_1936, boundary("word"))[[1]]
words_1945 <- str_split(Rooseveltcorpus_1945, boundary("word"))[[1]]
words_1936Roosevelttokens_1945length(words_1945)
length(Roosevelttokens_1945[[1]])

#This function calculate the number of words (NoW) before hetting TTR=0.72
#record the number in a list and start calculate again

NoW_Calculate <- function(words) {
    if (length(words) == 0) {
        return("Empty input!") 
    } else {
        wordlist <- character()
        word_count_list <- vector()
        word_count <- 0
        wordType <- 0
        wordToken <- 0

        for (i in words) {
            if (i %in% wordlist) {
                wordToken <- wordToken + 1
                word_count <- word_count + 1
            } else {
                wordType <- wordType + 1
                wordToken <- wordToken + 1
                wordlist <- c(wordlist, i)
                word_count <- word_count + 1
            }
            ttr = wordType / wordToken
            #print(ttr)
            if (ttr <= 0.72) {
                #print("hitting 0.72")
                word_count_list <- append(word_count_list, word_count-1)
                word_count <- 1
                wordType <- 1
                wordToken <- 1
                wordlist <- character()
                wordlist <- c(wordlist, i)
            } 
        }
        #word_count_list <- append(word_count_list, word_count)
        word_count_list
    }
    
}

#This function takes in the tokenized text
#divide the text into 25 parts
#calculate the average MTLD of these 25 parts

MTLD <- function(text) {
    listForNoW <- vector()

    chunk_length <- length(text) %/% 25

    for (num in 1:24) {
        start <- 1 + (num-1) * chunk_length
        end <- num * chunk_length
        words <- text[start:end]
        listForNoW <- c(listForNoW, NoW_Calculate(words))
    }
    start <- 1 + 24 * chunk_length
    end <- length(text)
    listForNoW <- c(listForNoW, NoW_Calculate(words))
    mean(listForNoW)
}

MTLD(words_1936)

MTLD(words_1945)

MTLD(Roosevelttokens_1936[[1]])

MTLD(Roosevelttokens_1945[[1]])

print(paste0("MTLD of Roosevelt_1936: ",
            MTLD(Roosevelttokens_1936[[1]])))
print(paste0("MTLD of Roosevelt_1945: ",
            MTLD(Roosevelttokens_1945[[1]])))
```{r}
library(koRpus)

Roosevelttext_1936 <- texts(Rooseveltcorpus_1936)
RooseveltMTLD <- tokenize(Roosevelttext_1936, format='obj', lang = 'en')
gc()
result1936 <- lex.div(RooseveltMTLD, measure = "MTLD", char = "MTLD")

Roosevelttext_1945 <- texts(Rooseveltcorpus_1945)
RooseveltMTLD <- tokenize(Roosevelttext_1945, format='obj', lang = 'en')
gc()
result1945 <- lex.div(RooseveltMTLD, measure = "MTLD", char = "MTLD")

cat(paste0(" ", "MTLD of Roosevelt_1936: ", result1936, "\n",
    paste0("MTLD of Roosevelt_1945: ", result1945)))
```
Roosevelttext_1936 <- texts(Rooseveltcorpus_1936)
RooseveltMTLD <- tokenize(Roosevelttext_1936, format='obj', lang = 'en')
gc()
lex.div(RooseveltMTLD, measure = "MTLD", char = "MTLD")

Roosevelttext_1945 <- texts(Rooseveltcorpus_1945)
RooseveltMTLD <- tokenize(Roosevelttext_1945, format='obj', lang = 'en')
gc()
lex.div(RooseveltMTLD, measure = "MTLD", char = "MTLD")

sentence1 <- "Mr. and Mrs. Dursley, of number four, Privet Drive, were proud to say that they were normal, thank you very much."
sentence2 <- "The Dursleys had everything they wanted, but they also had a secret, and their greatest fear was that somebody would discover it."

two_sentence <- c(sentence1, sentence2)

two_sentence_dfm <- dfm(two_sentence,
                     stem = TRUE, 
                     tolower = TRUE, 
                     remove_punct = TRUE,
                     remove = stopwords("english"))

as.matrix(two_sentence_dfm)

two_sentence_dfm

summary(two_sentence_dfm)

print(two_sentence_dfm,
  show.summary = TRUE)

sent1 <- c(1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)
sent2 <- c(0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)

sqrt(sum((sent1-sent2)^2))

sum(abs(sent1-sent2))

sum(sent1 * sent2)

sqrt(sum((sent1)^2))

sqrt(sum((sent2)^2))

sum(sent1 * sent2) / (sqrt(sum((sent1)^2)) * sqrt(sum((sent2)^2)))

getwd()
#setwd("/Users/Lingyi/TAD/HW1")

function_word <- c("a", "all", "also", "an", "and", "any", "are", "as", "at",
              "be", "been", "but", "by", "can", "do", "down", "even", "every",
              "for", "from", "had", "has", "have", "her", "his", "if", "in",    
              "into", "is", "it", "its", "may", "more", "must", "my", "no",
              "not", "now", "of", "on", "one", "only", "or", "our", "should",
              "so", "some", "such", "than", "that", "the", "their", "then", "there",
              "things", "this", "to", "up", "upon", "was", "were", "what", "when",
              "which", "who", "will", "with", "would", "your")

#all files we will use
filenames <- c("austen_emma.txt", "austen_northanger.txt", "austen_persuasion.txt",
               "austen_pride.txt", "austen_sense.txt", "dickens_bleak.txt",
               "dickens_copperfield.txt", "dickens_great.txt", "dickens_oliver.txt",
               "dickens_tale.txt", "mystery_text.txt")

#import all the 11 pieces of documents
content <- character()
for (i in 1:11) {
    file <- readChar(filenames[i], file.info(filenames[i])$size) #readChar allocates space for the number of bytes you specify
    content <- c(content, file)
}

#name of the 11 pieces of documents
textName <- character()
for (i in 1:11) {
    Name <- str_sub(filenames[i], 1, -5)
    textName <- c(textName, Name)
}

#store name and content into data.frame
corpus_df <- data.frame(textName, content,
                     stringsAsFactors = FALSE)

# tokenize all documents
for (i in 1:11) {
  corpus_df$token[[i]] <- tokens(
      char_tolower(corpus_df$content[i]), remove_punct = TRUE)
  
  #store the length of each document
  corpus_df$doclength[i] <- length(corpus_df$token[[i]][[1]])
}



#create 3 containers for divided blocks of austen, dickens, and the unknown one
block_austen <- list()
block_dickens <- list()
block_unknown <- list()
for (i in 1:11) {
    if (substr(corpus_df$textName[i], start=1, stop=3) == "aus") {
        for (j in 1:(corpus_df$doclength[i] %/% 1700)) {
            block <- paste(
                corpus_df$token[[i]][[1]][(1700*(j-1)+1):(1700*j)], 
                collapse = " ")
            block_austen <- c(block_austen, block)
        }
    } else if (substr(corpus_df$textName[i], start=1, stop=3) == "dic") {
        for (j in 1:(corpus_df$doclength[i] %/% 1700)) {
            block <- paste(
                corpus_df$token[[i]][[1]][(1700*(j-1)+1):(1700*j)], 
                collapse = " ")
            block_dickens <- c(block_dickens, block)
        }    
    } else {
        for (j in 1:(corpus_df$doclength[i] %/% 1700)) {
            block <- paste(
                corpus_df$token[[i]][[1]][(1700*(j-1)+1):(1700*j)], 
                collapse = " ")
            block_unknown <- c(block_unknown, block)
        }
    }    
}

#convert the block list I got into DFM
getDFM <- function (blocks, auther) {
    #turn list of characters back into corpus
    corp <- Corpus(VectorSource(blocks))
    corp <- corpus(corp)
    docvars(corp, "auther") <- auther
    
    block_token <- tokens(corp)

    #keep the function words I need
    left <- tokens_select(block_token, function_word, selection = "keep")
    function_dfm <- dfm(left)
    function_dfm    
}

block_austen_dfm <- getDFM(block_austen, "austen")
print(block_austen_dfm)

block_dickens_dfm <- getDFM(block_dickens, "dickens")
print(block_dickens_dfm)

block_unknown_dfm <- getDFM(block_unknown, "unknown")
print(block_unknown_dfm)

summary(block_unknown_dfm)

summary(block_unknown_dfm)[["Length"]]

cat(paste0(" Austen: ", 
           as.integer(summary(block_austen_dfm)[["Length"]])/69, " documents\n"),
    paste0("Dickens: ", 
           as.integer(summary(block_dickens_dfm)[["Length"]])/69, " documents\n"),
    paste0("Unknown: ", 
           as.integer(summary(block_unknown_dfm)[["Length"]])/69, " documents\n"))

plot(log10(topfeatures(block_austen_dfm/329, n = 69)), type = "b", col="red", 
     xlab="Feature index", ylab="frequency")
points(log10(topfeatures(block_dickens_dfm/698, n = 69)), type = "b", col="blue")
points(log10(topfeatures(block_unknown_dfm/55, n = 69)), type = "b", col='green')
legend("topright", legend=c("Austen", "Dickens", "Unknown"),
       col=c("red", "blue", "green"), cex=0.8, inset=.02, bty="n", pch = 21:21)
title("Feature frequencies in log space")

corp <- Corpus(VectorSource(content[1:10])) #choose the first 10 texts
corp <- corpus(corp)
token10 <- tokens(corp, remove_numbers = FALSE, remove_punct = TRUE)
dfm10 <- dfm(token10, tolower = TRUE, stem = TRUE)

plot(log10(1:100), log10(topfeatures(dfm10, n = 100)), type = "p", 
     xlab="Feature index (log10)", ylab="frequency (log10)")
title("Zipf's Law Demonstrated in Dickens and Austen's Text")

corp <- Corpus(VectorSource(content[1:10]))
corp <- corpus(corp)
token10 <- tokens(corp, remove_numbers = FALSE, remove_punct = TRUE)
dfm10 <- dfm(token10, tolower = TRUE, stem = TRUE)

#calculate values of T and M
T_token <- token10 %>% lengths() %>% unlist() %>% sum()
M_type <- dfm10 %>% ntype() %>% unlist() %>% sum()

k <- 44

#find the optimal value of b
#as M = k * T^b
#so b = log10(M/k) / log10(T)
b <- log10(M_type / k) / log10(T_token)
b

dickens_tale <- corpus_df %>% select(content) %>% filter(
    textName == "dickens_tale")
austen_pride <- corpus_df %>% select(content) %>% filter(
    textName == "austen_pride")

corp_dickens_tale <- corpus(Corpus(VectorSource(dickens_tale)))
corp_austen_pride <- corpus(Corpus(VectorSource(austen_pride)))

kwic(corp_dickens_tale, "poor", 5)

kwic(corp_austen_pride, "poor", 5)

kwic(corp_dickens_tale, "marriage", 5)

kwic(corp_austen_pride, "marriage", 5)

kwic(corp_dickens_tale, "work", 5)

kwic(corp_austen_pride, "work", 5)

kwic(corp_dickens_tale, "death", 5)

kwic(corp_austen_pride, "death", 5)

head(docvars(data_corpus_ukmanifestos))

textstat_readability(
    texts(corpus_subset(data_corpus_ukmanifestos, 
                        Party == "Lab"), groups = "Year"), "Flesch")

Document_Lab <- corpus_subset(data_corpus_ukmanifestos, Party == "Lab")
class(Document_Lab)

sentences <- tokens(Document_Lab, what = "sentence")
str(sentences)

class(sentences)

length(sentences)

length(sentences[[10]])
sentences[[1]]
Document_Lab[["Year"]]$Year[1]

Document_Lab[["Year"]]$Year

#store sentences in a data.frame
df_sentences <- data.frame(year = numeric(), 
                       sentence = character(), 
                       stringsAsFactors=FALSE)

count <- 0

for (i in 1:length(sentences)) {
    print(i)
    print(length(sentences[[i]]))
    count <- count + length(sentences[[i]])
    
    df_temp <- data.frame(year = numeric(), 
                       sentence = character(), 
                       stringsAsFactors=FALSE)
    
    for (j in 1:length(sentences[[i]])) {
        
        df_temp[j,1] <- Document_Lab[["Year"]]$Year[i]
        df_temp[j,2] <- sentences[[i]][j]
    }
    df_sentences <- rbind(df_sentences, df_temp)
}

str(df_sentences)

count
#some are numbers or headings, filter them out
df <- filter(
    df_sentences, grepl("^\\?", df_sentences$sentence)==FALSE & grepl(
        "^\\d", df_sentences$sentence)==FALSE & ntoken(
        df_sentences$sentence)>2)
df <- filter(df_sentences, grepl(
        "^\\d", df_sentences$sentence)==FALSE & (grepl(     #NOT start with numbers
        "\\.$", df_sentences$sentence)==TRUE | grepl(       #(end with .
        "\\?$", df_sentences$sentence)==TRUE | grepl(       #or end with ?
        "\\!$", df_sentences$sentence)==TRUE) & ntoken(     #or end with !)
        df_sentences$sentence)>2)                           #length longer than 2

str(df)

iters <- 50

#create a data.frame storing FRE
year_FRE <- data.frame(matrix(ncol = 16, nrow = iters))
colnames(year_FRE) <- names(table(Document_Lab[["Year"]]$Year))


# run the bootstrap
for(i in 1:iters) {
  
  sentences_grouped <- group_by(df, year)
  
  # take a sample of 100 sentences per level (year)
  bootstrap_sample <- sample_n(sentences_grouped, 100, replace = TRUE)
  
  readability_results <- textstat_readability(
      bootstrap_sample$sentence, measure = "Flesch")

    
  #store results  
  readability_grouped <- group_by(
      readability_results, bootstrap_sample$year)
  readability_means <- summarize(readability_grouped, mean(Flesch))
  
  year_FRE[i, ] <- t(readability_means[, 2])
  
}

# Define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

year_ses <- apply(year_FRE, 2, std)

year_means <- apply(year_FRE, 2, mean)

# Plot results
coefs <- year_means
ses <- year_ses

#95% confidence intervals of the mean
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(year_FRE)
adjust <- 0

plot(var.names, coefs, type = "p", pch = 19, cex = .8, 
     ylim=c(min,max),xlim = c(1940,2010), main = "", axes = F, 
     xlab="Year", ylab="FRE")
rect(1940,min,1945,max, col = c("grey97"), border="grey90", lty = 2)
rect(1945,min,1950,max, col = c("grey95"), border="grey90", lty = 2)
rect(1950,min,1955,max, col = c("grey97"), border="grey90", lty = 2)
rect(1955,min,1960,max, col = c("grey95"), border="grey90", lty = 2)
rect(1960,min,1965,max, col = c("grey97"), border="grey90", lty = 2)
rect(1965,min,1970,max, col = c("grey95"), border="grey90", lty = 2)
rect(1970,min,1975,max, col = c("grey97"), border="grey90", lty = 2)
rect(1975,min,1980,max, col = c("grey95"), border="grey90", lty = 2)
rect(1980,min,1985,max, col = c("grey97"), border="grey90", lty = 2)
rect(1985,min,1990,max, col = c("grey95"), border="grey90", lty = 2)
rect(1990,min,1995,max, col = c("grey97"), border="grey90", lty = 2)
rect(1995,min,2000,max, col = c("grey95"), border="grey90", lty = 2)
rect(2000,min,2005,max, col = c("grey97"), border="grey90", lty = 2)
rect(2005,min,2010,max, col = c("grey95"), border="grey90", lty = 2)

axis(1, at=seq(1940,2010,5),
    labels=c(1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010),
    tick=T, cex.axis=.75, mgp=c(2,.7,0))
axis(2, at=seq(min,max,(max-min)/10),
    labels = c(round(min+0*((max-min)/10),0),
                round(min+1*((max-min)/10),0),
                round(min+2*((max-min)/10),0),
                round(min+3*((max-min)/10),0),
                round(min+4*((max-min)/10),0),
                round(min+5*((max-min)/10),0),
                round(min+6*((max-min)/10),0),
                round(min+7*((max-min)/10),0),
                round(min+8*((max-min)/10),0),
                round(min+9*((max-min)/10),0),
                round(max,0)),tick = T,cex.axis = .75, mgp = c(2,.7,0))

segments(as.numeric(var.names)+2*adjust, coefs-qnorm(.975)*ses,  
         as.numeric(var.names)+2*adjust, coefs+qnorm(.975)*ses, lwd=1)
segments(as.numeric(var.names)+2*adjust-.3, coefs-qnorm(.95)*ses, 
         as.numeric(var.names)+2*adjust+.3, coefs-qnorm(.95)*ses, lwd=.9)
segments(as.numeric(var.names)+2*adjust-.3, coefs+qnorm(.95)*ses, 
         as.numeric(var.names)+2*adjust+.3, coefs+qnorm(.95)*ses, lwd=.9)
points(var.names, coefs, pch=21, cex=.8, bg="white")
title("FRE Score by Year of UK Labour Party (bootstrapped)")

plot(summarize(group_by(
    textstat_readability(df$sentence, measure = "Flesch"), df$year), 
          mean(Flesch)), 
     col="red", xlab="Year", ylab="FRE")
points(var.names, coefs, col="blue")
legend("topright", legend=c("observed", "bootstrapped results"),
       col=c("red", "blue"), cex=0.8, inset=.02, bty="n", pch = 21:21)
title("Contrast of means between observed and bootstrapped results")

summarize(group_by(
    textstat_readability(df$sentence, measure = "Flesch"), df$year), 
          mean(Flesch))

print(coefs)

readability_means_f <- df$sentence %>% textstat_readability(
    "Flesch") %>% group_by(df$year) %>% summarize(mean(Flesch))
readability_means_d <- df$sentence %>% textstat_readability(
    "Dale.Chall.old") %>% group_by(df$year) %>% summarize(mean(Dale.Chall.old))

readability_matrix <- cbind(readability_means_f[['mean(Flesch)']], 
                            readability_means_d[['mean(Dale.Chall.old)']])

cor(readability_matrix)

readability_measures <- textstat_readability(df$sentence, 
                                             c("Flesch", "Dale.Chall.old"))

readability_matrix <- cbind(readability_measures$Flesch, 
                            readability_measures$Dale.Chall.old)

cor(readability_matrix)
