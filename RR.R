library(tm)
library(quanteda)
library(qdap)
library(dplyr)
library(tidyr)
library(data.table)

    setwd("C:/Users/avtarsingh/Downloads/jhu/c10")
    
    blogstxt <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8")
    
    newstxt <- readLines("./final/en_US/en_US.news.txt", skipNul = TRUE, encoding = "UTF-8" )
    
    twittxt<- readLines("./final/en_US/en_US.twitter.txt", skipNul = TRUE, encoding = "UTF8")

  set.seed(6703)  
    ##  train on half the data, and store the other half for testing    
        train_data <- function(x,ratio=50){
                set.seed(397)
                index <- 1:length(x)
                tr_index<- index[sample(c(T,F), length(x), replace = T, prob = c((ratio)/100,(100-ratio)/100))]
                
            return(list(train = x[tr_index], test = x[-tr_index]))
                
        }
        
        
        blogs_train <- train_data(blogstxt)
        blogs_train <- blogs_train[[1]]
        blogs_test <- blogs_train[[2]]    
        
        news_train <- train_data(newstxt)
        news_train <- news_train[[1]]
        news_test <- news_train[[2]]
        
        twt_train <- train_data(twittxt)
        twt_train <- twt_train[[1]]
        twt_test <- twt_train[[2]]
        

                
##
    smp_list <- list(blogs_train, news_train, twt_train)
    lg_names <- c("blog", "news", "twitter")
    names(smp_list) <- lg_names


    
    
    smp_list <- lapply(smp_list, iconv, from = "utf-8", to = "ascii", sub ="")

    
        corpus <- VCorpus(VectorSource(smp_list))
        
        
        # tidy
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, PlainTextDocument)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, content_transformer(tolower)) 
           
        # remove urls
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(f|ht)tp(s?)(:?)(s?)(.){0,4}(www.).* |(f|ht)tp(s?):|(wwws?\\..*\\.com)|[aA-zZ]{0,}.com|http//", replacement = "" )
        
        
        # change contaction to two words.  Example i'm to I am...
        contractions <-readRDS("contractions.Rda") # a matix with contractions and its replacements. 1 column has contraction 2nd replacements
        corpus <- tm_map(corpus, content_transformer(mgsub), pattern = contractions[,1] , replacement = contractions[,2], fixed = T)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(?<=[aA-zZ])'(?=[aA-zZ])", replacement = "", perl = TRUE)
        
        corpus <- tm_map(corpus, removePunctuation)
        
        
       
    
    
    smtxt <- data.frame(text=unlist(sapply(corpus, `[`)), stringsAsFactors=F)   # train data is in 'smtxt'
    
    
    #   Unigram
    
    ##  This has a relatively small memory footprint and was done using quanteda package:
    
    token_1 <- tokens(smtxt$text, what = "fastestword", ngrams = 1, verbose = TRUE)
    
    dfm_1 <- dfm(token_1)
   
    dfm_1 <- colSums(dfm_1)
    # dfm_1 is the frequency of all the Unigrams
    saveRDS(token_1, file="token_1.Rda")
    
    dfm_1 <- tbl_df(dfm_1) %>% add_rownames()
    
    saveRDS(dfm_1, file="dfm_1.Rda")
    

    
    ##
    ##  From Unigram I will determine which term are misspelled and repalce them from 'smtxt' below.
    ##  I also choose to replace the badwords
    ##  * Replace them with 3 character '<', 'UNK', '>' and thus they are not creating false n-grams,
    ##  And importantly I later remove these, yet they are not distorting the prediction model:
    ##  Since as in "We are < unk > going", if they placeholder are removed after N-gram process, are not be in predictor terms for going as long as we are using up to 4 - term N grams.
    
    #################################
    #################################
    rare <- dfm_1 %>% filter(value<4) # I determined to label
                                    #  words that occus 1-3 times
                                    #   as unknowns.
    rare <- rare$rowname
    rare <- sort(rare)
    
        b<-matrix(nrow=3843,ncol=1)
    
    for (i in 0:3842){b[i+1,1] <- paste0("\\b(",paste(rare[((100*i)+1):(100*(i+1))],collapse = "|"),")\\b")}
    
    for(i in 1:3843){
    ptc <- proc.time()
    smtxt$text<-gsub(b[i,1],"<UNK>",smtxt$text, perl=TRUE);
    print(i)
    print(proc.time()-ptc)
    }
# Note length(rare)-(100*(3842+1))=92, thus last 92 words in `rare`.
# There are the other words, badwords, that I have scraped from the web and I would like to save will take them out of the text as well
    setwd("./..")    # They are saved in different dir.
    badwords <- readRDS("badwords.Rda")
    badwords <- badwords[[1]]
    length(badwords)
    bad2<- c(badwords,tail(rare,92))
    bad2 <- sort(bad2)
    
    bad2 <- paste0("\\b(",paste(bad2,collapse = "|"),")\\b")
    smtxt$text <- gsub(bad2,"<UNK>",smtxt$text, perl =TRUE)

    
# Looking at some more rare, frequency 4, even 5 to see if we should replace other unknown or rare words with "<UNK>" tag. 
    testmore <- dfm_1 %>% filter(value==5|value==4|value==6)
    View(testmore)
# There are still misspelled words and the threshold seems to be with the words that have frequency 4,5 and 6 I can further remove some words, those with frequency 4 and 5
    
    # Here I will take out the words with frequency 4
    rare2 <- dfm_1 %>% filter(value==4) 
    rare2 <- rare2$rowname
    rare2 <- sort(rare2)
    length(rare2) #13900
    length(rare2)/100 #139
    
        b<-matrix(nrow=139,ncol=1)
    
    for (i in 0:138){b[i+1,1] <- paste0("\\b(",paste(rare2[((100*i)+1):(100*(i+1))],collapse = "|"),")\\b")}
    
    for(i in 1:139){
    ptc <- proc.time()
    smtxt$text<-gsub(b[i,1],"<UNK>",smtxt$text, perl=TRUE);
    print(i)
    print(proc.time()-ptc)
    }

    
    #################################
    #################################
    
    
 setwd("./modtej/fifty/Reduce/hl")  
 #      The latest model is saved in `hl`
    
    
    # 2- grams and above are more memory intense to compute these I will do so in batches.
    
    
    #   N-grams
     
    # split the tokenization into smaller batches and only save the 'data frequency matrix'.  
    
    # First we extract the 'dfm' and then we combine them.
    # 2 steps and 2 loops.
    
    #   Note in each of the loops I am deleting everything and only interested in the 'data frequency matrix', thus keep what is needed and dump other variable at the end of each loop.

    
    # Bigram
    for (i in 0:9){
ptc <- proc.time()                    
        text_tbl <- fread("smtxt", skip = 166932*i, nrows = 166932, header = FALSE, col.names = "text")
        
                            
        
        bi_tkn_one <- tokens(text_tbl$text, what = "word", ngrams = 2, concatenator =" ", verbose = TRUE)
        
        dfm_1 <- dfm(bi_tkn_one)
        
        ## Using colSums(), gives a numeric vector`final_dfm_1`
        ## tib is the desired oject I will save with new name each time.
        
        final_dfm_1 <- colSums(dfm_1)
        print(setNames(length(final_dfm_1), "no. N-grams in this batch"))
            # no. N-grams
        
        
        tib <- tbl_df(final_dfm_1) %>% add_rownames()  
        tib <- tib[!grepl("<|UNK|>", tib$rowname, perl=TRUE, ignore.case=TRUE),]
        # This is what I wanted to extract 'the freq of each token'
        
        
             # B Here I change the name `tib`` is saved uneder each time.
        iplus = i+1
        saveRDS(tib, file = paste0("tiu",iplus,".Rda"))
        print(proc.time()-ptc)
        #rm(list=ls())
        Sys.sleep(5)
        gc()
        Sys.sleep(8)
     }
    #
    #   
    #       Compline term frequency table
    naute <- readRDS("tiu1.Rda")
    naute_2 <- readRDS("tiu2.Rda")

Out_y0 <- rbindlist(list(naute, naute_2))[,sum(value), rowname]
#"naute"   "naute_2" "Out_y0" 
for(i in 3:10) {
    
    in_x <- readRDS(paste0("tiu",i,".Rda"))
    names(in_x)[2] <- "V1"
    Out_y0 <- rbindlist(list(Out_y0, in_x))[, sum(V1), rowname]
    rm(list=setdiff(ls(), "Out_y0"))
    Sys.sleep(5)
    gc()
    print(nrow(Out_y0)) # verbose, no. rows in df complied
}
    
    saveRDS(Out_y0, file="dfm_2.Rda")   # This is what I was after.

    

#-------------------------------------------------------------    
    # Trigram
    for (i in 0:9){
ptc <- proc.time()                    
        text_tbl <- fread("smtxt", skip = 166932*i, nrows = 166932, header = FALSE, col.names = "text")
        
                            
        
        bi_tkn_one <- tokens(text_tbl$text, what = "word", ngrams = 3, concatenator =" ", verbose = TRUE)
        
        dfm_1 <- dfm(bi_tkn_one)
        
        ## Using colSums(), gives a numeric vector`final_dfm_1`
        ## tib is the desired oject I will save with new name each time.
        
        final_dfm_1 <- colSums(dfm_1)
        print(setNames(length(final_dfm_1), "no. N-grams in this batch"))
            # no. N-grams
        
        
        tib <- tbl_df(final_dfm_1) %>% add_rownames()
        tib <- tib[!grepl("<|UNK|>", tib$rowname, perl=TRUE, ignore.case=TRUE),]
        # This is what I wanted to extract 'the freq of each token'
        
        
             # B Here I change the name `tib`` is saved uneder each time.
        iplus = i+1
        saveRDS(tib, file = paste0("tri",iplus,".Rda"))
print(proc.time()-ptc)        
        #rm(list=ls())
        Sys.sleep(5)
        gc()
        Sys.sleep(8)
     }
    #
    #   
    #       Compline term frequency table
    naute <- readRDS("tri1.Rda")
    naute_2 <- readRDS("tri2.Rda")

Out_y0 <- rbindlist(list(naute, naute_2))[,sum(value), rowname]
#"naute"   "naute_2" "Out_y0" 
for(i in 3:10) {
    
    in_x <- readRDS(paste0("tri",i,".Rda"))
    names(in_x)[2] <- "V1"
    Out_y0 <- rbindlist(list(Out_y0, in_x))[, sum(V1), rowname]
    rm(list=setdiff(ls(), "Out_y0"))
    Sys.sleep(5)
    gc()
    print(nrow(Out_y0)) # verbose, no. rows in df complied
}
    
    saveRDS(Out_y0, file="dfm_3.Rda")   # This is what I was after.
    
    
   proc.time()-ptct 
#-------------------------------------------------------------        
    
    # Quadgram
    for (i in 0:9){
ptc <- proc.time()                    
        text_tbl <- fread("smtxt", skip = 166932*i, nrows = 166932, header = FALSE, col.names = "text")
        
                            
        
        bi_tkn_one <- tokens(text_tbl$text, what = "word", ngrams = 4, concatenator =" ", verbose = TRUE)
        
        dfm_1 <- dfm(bi_tkn_one)
        
        ## Using colSums(), gives a numeric vector`final_dfm_1`
        ## tib is the desired oject I will save with new name each time.
        
        final_dfm_1 <- colSums(dfm_1)
        print(setNames(length(final_dfm_1), "no. N-grams in this batch"))
            # no. N-grams
        
        
        tib <- tbl_df(final_dfm_1) %>% add_rownames()  
        tib <- tib[!grepl("<|UNK|>", tib$rowname, perl=TRUE, ignore.case=TRUE),]
        # This is what I wanted to extract 'the freq of each token'
        
        
             # B Here I change the name `tib`` is saved uneder each time.
        iplus = i+1
        saveRDS(tib, file = paste0("tf",iplus,".Rda"))
print(proc.time()-ptc)        
        #rm(list=ls())
        Sys.sleep(5)
        gc()
        Sys.sleep(8)
     }
    #
    #   
    #       Compline term frequency table
    naute <- readRDS("tf1.Rda")
    naute_2 <- readRDS("tf2.Rda")

Out_y0 <- rbindlist(list(naute, naute_2))[,sum(value), rowname]
#"naute"   "naute_2" "Out_y0" 
for(i in 3:10) {
    
    in_x <- readRDS(paste0("tf",i,".Rda"))
    names(in_x)[2] <- "V1"
    Out_y0 <- rbindlist(list(Out_y0, in_x))[, sum(V1), rowname]
    rm(list=setdiff(ls(), "Out_y0"))
    Sys.sleep(5)
    gc()
    print(nrow(Out_y0)) # verbose, no. rows in df complied
}
    
    saveRDS(Out_y0, file="dfm_4.Rda")   # This is what I was after.
    
#----------------------------------------------------------------------
#######################################################################
    
    # Build an N-gram model where, to reduce the size of the `dfm` files used the 2 facts: 
    # 1) Don't want to predict with or give as prediction any misspelled term, thus they are removed in the 1st loops above.
    # 2) Group words by the predictor terms and we are only interseted in the predicted word, i.e. last term, with the most frequency, thus we can keep minimum number of top chioces. We are predicting under the N-grams, model with markov assumption.
    
    dfm_1 <- readRDS("dfm_1.RDa")
    rare <- dfm_1 %>% filter(value<4) 
    rare <- rare$rowname
    rare <- sort(rare)
    
    # These are all the terms that occured 3 times of less and by inspecting them I determine that these are either misspelled are they are not proper words.
    
    dfm_4 <- readRDS("dfm_4.Rda")
    ptc = proc.time()
    dfm_4[, c("w1","w2","w3","w4") := tstrsplit(rowname, " ") ]
    proc.tim()- ptc

    
    ###################################################
    #########################
        #####
    
    #   tidy up ngram frequencies.
    
    setwd("C:/Users/avtarsingh/Downloads/jhu/c10/modtej/fifty/Reduce/hl")
    
        library(data.table)
        library(dplyr)
        library(tidyr)    
        library(splitstackshape)
    
    
    ####    dfm_4 is a very large table I will shrink it my only keeping the rows that have only one count, i.e. there is a good chance that I didn't get that row out of the 1/2 that I choose from each source but first I will try to read the data in chunks and split quadgram into words, if that works I could then group by words and choose the 6 highest frequencies.
    

    ##  4-gram frequency table
    
    
    ##  Too big a file I have to split it into chunks to split 4-grams into single words.
    
    y0 <- data.table()
    for(i in 0:22) {
        ptc <- proc.time()
            
            a <- fread("bNckdfm4", skip=1000000*i, nrows =1000000 ,  header = F, drop=1)
    
            a <- cSplit(a , "V2", " ")
            
            y0 <- rbindlist(list(y0, a))
            
            rm(list=setdiff(ls(), c("y0","i","ptc")))
            
            
            gc()
            
            print(i)
            print(nrow(y0))
            
            
            print(proc.time()-ptc)
        
    }

    y0<- y0[,c(2,3,4,5,1)]
    
    names(y0) <- c("w1","w2","w3","w4","freq")
    saveRDS(y0, file="dfm_4.Rda")

    
        
    ##   3-gram frequency table
    
    dfm_3 <- dfm_3 %>% separate(rowname, c("w1","w2","w3"), " ") 
    names(dfm_3) <- c("w1", "w2", "w3","freq")
    saveRDS(dfm_3, file="dfm_3.Rda")

    ##   2-gram frequency table
    dfm_2 <- readRDS("dfm_2.Rda")
    
    dfm_2 <- dfm_2 %>% separate(rowname, c("w1", "w2")," ")
    names(dfm_2) <- c("w1","w2","freq")
    saveRDS(dfm_2, file = "dfm_2.Rda")
    
    ##  1-gram frequncy table
        dfm_1 <- readRDS("dfm_1.Rda")
    names(dfm_1) <- "w1"
    
    
    
    
    
#####################################################################
setwd("C:/Users/avtarsingh/Downloads/jhu/c10/modtej/fifty/Reduce/hl")

library(quanteda)
library(tm)
library(qdap)
library(dplyr)
library(tidyr)
library(data.table)
    
dfm_4 <- readRDS("dfm_4.Rda")    
dfm_3 <- readRDS("dfm_3.Rda")
dfm_2 <- readRDS("dfm_2.Rda")
dfm_1 <- readRDS("dfm_1.Rda")

        Q_gram <- function(words, pred) {
            
            tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            ww <- tok[len-3]
            
            prd <- tokens(pred, removePunct = TRUE)[[1]]
            
a <- dfm_4 %>% filter(w1==wx, w2==wy, w3==wz, w4 %in% prd) %>% arrange(desc(freq))
 
b <- dfm_3 %>% filter(w1==wy, w2==wz, w3 %in% prd) %>% arrange(desc(freq))

c <- dfm_2 %>% filter(w1==wz, w2 %in% prd) %>% arrange(desc(freq))

d <- dfm_1 %>% filter(w1 == wz)  # useful to look at w/bi-gram probs.


            output <- list(a,b,c,d)
            return(output)
         
        }
        
    
#########################################################
        
        # Only keep the most frequent occuring substructures.
        # Here an important part is how to resolve ties.
        
        dfm_4 <- dfm_4 %>% group_by(w1,w2,w3) %>% top_n(n = 1, wt = freq)
        
        saveRDS(dfm_4, file ="Qngram.Rda")
        
        #
        
        dfm_3 <- dfm_3 %>% group_by(w1,w2) %>% top_n(n= 1, wt = freq)
        
        saveRDS(dfm_3, file="Tngrams.Rda")
        
        #
        
        dfm_2 <- dfm_2 %>% group_by(w1) %>% top_n(n = 1, wt = freq)
        saveRDS(dfm_2, file = "Bngrams.Rda")
        
        # These dfm's have many ties.
        # Starting with Bi-grams and Tri-grams, combine the columns with frequencies.  And reslove the ties by choosing the 'last' words that has higher frequencies in lower order N-grams.   ---  To be completed.
        
#############################################################
 
#   Developing a prediction function
        
library(microbenchmark)
library(compiler)

               
setwd("C:/Users/avtarsingh/Downloads/jhu/c10/modtej/fifty/Reduce/hl")
    dfm_2 <- readRDS("Bngrams.Rda")
    dfm_3 <- readRDS("Tngrams.Rda")
    dfm_4 <- readRDS("Qngrams.Rda")
    
    twt_test <- readRDS("twt_test.Rda")
    rs200 <- twt_test[sample(1:length(twt_test), 200)]     
        
        Q_gram <- function(words) {
            
            tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            ww <- tok[len-3]
           
a <- dfm_4 %>% filter(w1 == wx, w2 == wy, w3 == wz) %>% select(w4)
 
#b <- dfm_3 %>% filter(w1 == wy, w2==wz)

#c <- dfm_2 %>% filter(w1 == wz)

#d <- dfm_1 %>% filter(w1 == wz)  

#output <- list(a,b,c,d)
            return(a)
                 }
     
        
        #############
#   SPEED  
        #############
   # Try the compiler and check results
     Q_gram_c <- compiler::cmpfun(Q_gram)    
     
        i = 20
    microbenchmark(
        
        (Q_gram(rs200[i])),
            
        (Q_gram_c(rs200[i])),
        times=1
    )
    
    
    

            #############       ############
    ##  Note: skipping resoliving ties, for now, as my machine is not able to work with memory intense jobs at this time.  Upon resolving this setback, I will fix this section by first combining the 'freq` columns from all the N-grams and then order based on freqencies from W_n-2,..,W_n+1-N frequency of the last terms.  i.e. if "you ought to" gives a tie with multiple 'last' word, use the Trigram to find the frequency of the highest 'last' word following the term "ought to" that is also present in Quadgram, if that don't resolve, I would choose the frequency from the Bigram to complete the "to .." Bigram, for the last word that is also in the tie for Quadgram, If still not resolved, go to the Unigram to choose the word from those that tie in the Quadgram model.
    ## This require binding together the columns that correspond to the frequency of the last 1,2 or 3 terms in case of 4-gram models, and if resolving 3 gram ties this require corresponding 1 and 2 grams frequencies for the last 1 and 2 words.  All this is not difficult and in worst case could be resolved by splitting the task into chunks.
    ## However note that from the quiz the results were mixed when we rely on lower ngram models for prediction.
    
    ## Some more testing is in order to optimize the App.
###############     Comeback to above section ###################
    

        ## data.table
    # Futher speed up by keeping only one answer # More accuracy should be sought here and will come back to how should I keep one term to predict from many with same frequency.

    #
    dfm_4 <- readRDS("dfm_4.Rda")
    class(dfm_4)
    ungroup(dfm_4)
    
    dfm_4 <- setDT(dfm_4)[order(w1,w2,w3,-freq), .SD[1L], by=.(w1,w2,w3)]   
    
    dfm_4 <- as.data.table(dfm_4, key=c("w1","w2","w3"))
    dfm_4$w4 <- as.character(dfm_4$w4) 
    # setting the character will ensure we match 'no' result.
    saveRDS(dfm_4, file = "aph4g.Rda")

    #
    dfm_3 <- readRDS("dfm_3.Rda")
    
    dfm_3 <- setDT(dfm_3)[order(w1,w2,-freq), .SD[1L], by=.(w1,w2)]   
    
    dfm_3 <- as.data.table(dfm_3, key=c("w1","w2"))
    dfm_3$w3 <- as.character(dfm_3$w3)
    saveRDS(dfm_3, file="aph3g.Rda")

    #
    dfm_2 <- readRDS("dfm_2.Rda")
    
    dfm_2 <- setDT(dfm_2)[order(w1,-freq), .SD[1L], by=.(w1)]   
    
    dfm_2 <- as.data.table(dfm_2, key=c("w1"))
    dfm_2$w2 <- as.character(dfm_2$w2)
    saveRDS(dfm_2, file = "aph2g.Rda")
       
    
    #
#    > identical((Q_gram(rs200[i]))[[1]],character(0))
#[1] TRUE
    
    ####
    
    T_gram <- function(words) {
            
            tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            ww <- tok[len-3]
           
#a <- dfm_4 %>% filter(w1 == wx, w2 == wy, w3 == wz) %>% select(w4)
 
b <- dfm_3 %>% filter(w1 == wy, w2==wz) %>% select(w3)

#c <- dfm_2 %>% filter(w1 == wz)

#d <- dfm_1 %>% filter(w1 == wz)  

#output <- list(a,b,c,d)
            return(b)
    }
######
    
    B_gram <- function(words) {
            
            tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            ww <- tok[len-3]
           
#a <- dfm_4 %>% filter(w1 == wx, w2 == wy, w3 == wz) %>% select(w4)
 
#b <- dfm_3 %>% filter(w1 == wy, w2==wz) %>% select(w3)

c <- dfm_2 %>% filter(w1 == wz) %>% select(w2)

#d <- dfm_1 %>% filter(w1 == wz)  

#output <- list(a,b,c,d)
            return(c)
    }
    
    ########
    
    # Test vs Q_gram
    
    Q2_gram <- function(words) {
  
    tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            ww <- tok[len-3]
 a <- dfm_4[w1 == wx & w2 == wy & w3 == wz][, w4]          
#a <- dfm_4 %>% filter(w1 == wx, w2 == wy, w3 == wz) %>% select(w4)
 
#b <- dfm_3 %>% filter(w1 == wy, w2==wz)

#c <- dfm_2 %>% filter(w1 == wz)

#d <- dfm_1 %>% filter(w1 == wz)  

#output <- list(a,b,c,d)
            return(a)
    }
##
    
system.time(Q2_gram("what in the world"))
system.time(Q_gram("this is getting confusing"))

# No advantage detected.


#####     predict last word (plw)
# use::character(0); to skip to lower ngram

    plw <- function(words) {
  
    tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]
            
            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
            
a <- dfm_4 %>% filter(w1 == wx, w2 == wy, w3 == wz) %>% select(w4) 

b <- dfm_3 %>% filter(w1 == wy, w2 == wz) %>% select(w3)

c <- dfm_2 %>% filter(w1 == wz) %>% select(w2)    

    if(len >=3 & (!identical(a[[1]], character(0)))){
        return(a)
    }else if(len >=2 & (!identical(b[[1]], character(0)))){
        return(b)
    }else if(len >= 1 & (!identical(c[[1]], character(0)))){
        return(c)
    }else{
        print("No match found")
    }
    }
    

   
