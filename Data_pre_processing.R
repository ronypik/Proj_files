#############################################################################
#################DATA PREPROCESSING##########################################
#############################################################################

library(tm)
library(stringi)
library(rJava)
library(RWeka)
library(qdap)

# setwd("C:/Users/ronip/Desktop/WORKS/R/DataScience/Project/final/en_US") # Set Working Directory
# setwd("C:/Users/home/Desktop/Rony/Proj/Coursera-SwiftKey/final/en_US")

# Read files
con <- file("en_US.twitter.txt", "r")
twitter<-readLines(con , 10000)
close(con) 

con <- file("en_US.news.txt", "r")
news<-readLines(con, 10000)
close(con) 

# Combine files
all_data <- paste(twitter,news,sep = " ")

# Cleaning data
corpus <- Corpus(VectorSource(all_data))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))

#Remove junk characters
junk_rm <- content_transformer(function(x) stri_replace_all_regex(x,"[^\\p{L}\\s[']]+",""))
corpus <- tm_map(corpus, junk_rm)
num_rm <- content_transformer(function(x) stri_replace_all_regex (x, "[^A-Za-z0-9 ']+", " "))
corpus <- tm_map(corpus, num_rm)

corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, stripWhitespace) # Removing whitespaces
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# corpus <- tm_map(corpus, stemDocument) 


# Remove misspeled words
adtm <-DocumentTermMatrix(corpus)
nam <- paste(c(adtm$dimnames[[2]]),collapse = " ")
a<- unique(matrix( which_misspelled(nam, suggest=FALSE), ncol = 1, byrow=T))
corpus_a <- tm_map(corpus, removeWords,  a[1:2000,1])
corpus_b <- tm_map(corpus_a, removeWords,  a[2001:4000,1])
corpus_c <- tm_map(corpus_b, removeWords,  a[4001:6000,1])


# N_GRAM preporation
options(mc.cores=1) 
control<-Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!")
ngram<-lapply(corpus_a, NGramTokenizer, control) 
ngram_v<-rle(unlist(ngram))$values
ngram_tb <- as.data.frame(table(ngram_v))

# Divide the N-GRAM on the words W1 : W4
aaa <- unlist(strsplit(as.character(ngram_tb$ngram), '\\s'))  # [[i]][1]
ngram_tb$w1 <- data.frame(matrix(unlist(aaa), ncol = 4, byrow=T))[,1]
ngram_tb$w2 <- data.frame(matrix(unlist(aaa), ncol = 4, byrow=T))[,2]
ngram_tb$w3 <- data.frame(matrix(unlist(aaa), ncol = 4, byrow=T))[,3]
ngram_tb$w4 <- data.frame(matrix(unlist(aaa), ncol = 4, byrow=T))[,4]

ngram_tb <- subset(ngram_tb, select = -c(ngram_v) )

# Save as N-GRAM as file 
# setwd("C:/Users/ronip/Desktop/WORKS/R/DataScience/Project/my_app")
write.csv(ngram_tb, file = "ngram_tb.csv", row.names = FALSE) ## write without row names


#############################################################################
#################END OF THE PREPROCESSING ###################################
#############################################################################