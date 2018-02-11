# Text-Mining Capstone for Data Science Specialization from John Hopkin via Coursera
N-grams model with markov assumptions.  A model is trained on the given data after cleaning the corpus, a frequency matrix is created and N-gram model with markov assumptions is used to predict last work in my app. 

All the code for the project is in the [RR.R](https://github.com/blsingh/Text-prediction-Application/blob/master/RR.R) file, below is brief explanation for the fuction that generates the last word in Shiny app. 

[Here you will find the Shiny app](https://bhael.shinyapps.io/Ngrams/) that runs on the trained model.

[And here are couple slides in Rpubs](https://bhael.shinyapps.io/Ngrams).




```R
plw <- function(words) {
  
    tok <- tokens(removePunctuation(tolower(removeNumbers(replace_contraction(words)))))[[1]]

#strip the given sequence of words to most basic words and use the last 3 words/token

            len <- length(tok)
            wz <- tok[len]
            wy <- tok[len-1]
            wx <- tok[len-2]
       
    ifelse(len >= 3 & (!identical(dfm_4[w1 == wx & w2 == wy & w3 == wz][, w4], character(0))),
        return(dfm_4[w1 == wx & w2 == wy & w3 == wz][, w4]),
        
        ifelse(len >=2 & (!identical(dfm_3[w1 == wy & w2 == wz][, w3], character(0))),
               return(dfm_3[w1 == wy & w2 == wz][, w3]),
               
               ifelse(len >= 1 & (!identical( dfm_2[w1 == wz][, w2], character(0) )),
                      return(dfm_2[w1 == wz][, w2]), print("No Match Found"))))
                      
#choose the final words after matching last 3, 2, or 1 word, if no match return "No Match Found".
}
```
