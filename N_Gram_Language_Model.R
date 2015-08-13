#############################################################################
################# Main algorithm - prediction of the next word ##############
#############################################################################


library(tm)

my_search <- function(query)
{
  # Pre-processing query
  query <- removeNumbers(removePunctuation(removeWords(query ,stopwords("english"))))
  query_norm <- rev(unlist(strsplit(tolower(as.character(query)), '\\s'))) 
  query_norm <- query_norm[query_norm != ""] #Remove empty words 

  # Init tables
  res_3w<-data.frame(Group.1=NULL,x=NULL)
  res_2w<-data.frame(Group.1=NULL,x=NULL)
  res_1w<-data.frame(Group.1=NULL,x=NULL)
  res_3w_or<-data.frame(Group.1=NULL,x=NULL)
  
  if (nchar(query) > 0) 
  {
    # Read 3 last words
    ngram_tb_3n <- ngram_tb[(ngram_tb$w1) == query_norm[3] & 
                              ngram_tb$w2  == query_norm[2] &
                              ngram_tb$w3  == query_norm[1] ,]
    
    if (nrow(ngram_tb_3n) >0) {  
      aggdata <-aggregate(ngram_tb_3n$Freq, by=list(ngram_tb_3n$w4),FUN=length)
      if (nrow(aggdata)>0 ) {
        res_3w <- head(aggdata[order(-aggdata$x),],5)
      }
    }
    
    # Read 2 last words
    ngram_tb_2n <- ngram_tb[ngram_tb$w2  == query_norm[2] &
                              ngram_tb$w3  == query_norm[1]   ,]
    
    if (nrow(ngram_tb_2n) >0) {  
      aggdata <-aggregate(ngram_tb_2n$Freq, by=list(ngram_tb_2n$w4),FUN=length)
      if (nrow(aggdata)>0 ) {
        res_2w <- head(aggdata[order(-aggdata$x),],5)
      }
    }
    
    # Read 1 last words
    ngram_tb_1n <- ngram_tb[ngram_tb$w3  == query_norm[1]   ,]
    
    if (nrow(ngram_tb_1n) >0) {  
      aggdata <-aggregate(ngram_tb_1n$Freq, by=list(ngram_tb_1n$w4),FUN=length)
      if (nrow(aggdata)>0 ) {
        res_1w <- head(aggdata[order(-aggdata$x),],5)
      }
    }
    
    strtrim(ngram_tb$w3, nchar(query_norm[1]))
    
    # Predicts the next word based on a partial (not completed) word
    ngram_tb_3n_or <- ngram_tb[ ngram_tb$w2  == query_norm[2] |
                                  strtrim(ngram_tb$w3, nchar(query_norm[1])) == query_norm[1] ,]
    
    if (nrow(ngram_tb_3n_or) >0) {  
      aggdata <-aggregate(ngram_tb_3n_or$Freq, by=list(ngram_tb_3n_or$w4),FUN=length)
      if (nrow(aggdata)>0 ) {
        res_3w_or <- head(aggdata[order(-aggdata$x),],5)
      }
    }
  }
  
  res <- rbind(res_3w, res_2w, res_1w, res_3w_or)
  
  # Predifined Next words
  if (nrow(res) == 0) { 
    res <-data.frame(Group.1=c("the", "to", "from", "with", "me"), x=c(1,1,1,1,1)) 
  }
  
  res <- subset(res, select = -c(x) )
  res <- head(unique(res) ,5)
  res <- as.data.frame(matrix(res$Group.1, ncol = 1, byrow=T))   
  names(res) <- "Next words"
  
  return(res)
}
