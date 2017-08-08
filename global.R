library(tm)
library(SnowballC)
library(wordcloud)
library(memoise)

source(file.path("R", "minIndicatorsPanel.R"), local = TRUE)

## https://www.r-bloggers.com/r-and-foreign-characters/

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(text) {
  ## Careful not to let just any name slip in here; a
  ## malicious user could manipulate this value.
  text <- iconv(enc2utf8(text),sub="byte")
  vsource <- VectorSource(text)
  myCorpus <- Corpus(vsource, readerControl = list(reader = reader(vsource), language = "fr"))
   myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords,
                     c(stopwords("SMART"), "le", "la", "de", "du", "des", "dans"))

  #myCorpus <- tm_map(myCorpus, stem, lazy = TRUE)

  myDTM <- TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m <- as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

## https://shiny.rstudio.com/articles/bookmarking-state.html
## https://shiny.rstudio.com/articles/advanced-bookmarking.html
enableBookmarking(store = "url")
