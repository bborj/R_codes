#PREDICTING THE REVENUE USING LINEAR REGRESSION
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#1000 observations of 16 variables
apple <- import("https://github.com/finnstats/finnstats/raw/main/Data1.csv")
head(apple)

#We are just interested on text
install.packages("tm")
#NLP library
library(tm)

corpus <- iconv(apple$text)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

###CLEANING DATA
#Converting to minus
corpus <- tm_map(corpus, tolower)
#Removing special characters
corpus <- tm_map(corpus, removePunctuation)
#Removing numbers
corpus <- tm_map(corpus, removeNumbers)
#Now we need to remove stop words, (p.e. "the" "is" "at" "on")
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
#Removing links
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
#Text stemming
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)

###INCIDENCE OF EVERY WORD
#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

#Plotting in a barchart
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

###Word cloud
install.packages("wordcloud")
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)
###Word cloud 2
install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)
