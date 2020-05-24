install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

filePath <- "C:/Users/jared_000/Desktop/post (1).txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "\t")
docs <- tm_map(docs, toSpace, "\\[")
docs <- tm_map(docs, toSpace, "]")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs2 <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("quote","ssfs")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

library(quanteda)
myCorpus <- corpus(text)
system.time(theDFM <- dfm(myCorpus,tolower=TRUE,
                          remove=c(stopwords(),",",".","-","\"","'","(",")",";",":","[","]","=")))
#system.time(theDFM <- dfm(myCorpus,tolower=TRUE,
#                          remove=c(stopwords(),",",".","&","!","?","|",">","???","1","+","2","*","-","\"","'","(",")",";",":","=")))

system.time(textFreq <- textstat_frequency(theDFM))

head(textFreq,200)
#freqs <- textFreq[-c(1,2,4,5,6,7,9,10,11,12,13,14,21,23,24,25,27,28,36,41,43,44,45,47,50,55,60,61,66,68,69,71,79,85,86,94,97,99,113,120,126,130,137,139,151,153,154,156,192,199),]
freqs <- textFreq[-c(1,2,3,4,5,6,7,8,9,10,12,15,19,20,22,23,24,25,26,27,29),]
freqs <- freqs[(which(nchar(freqs$feature) > 1)),]
head(freqs,200)
freqs2 <- freqs[order(-freqs$docfreq),]
head(freqs2,200)

set.seed(1234)
wordcloud(words = freqs$feature, freq = freqs$frequency, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = freqs2$feature, freq = freqs2$docfreq, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
