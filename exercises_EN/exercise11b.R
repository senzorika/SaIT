# ============================================================
# Exercise 11b: Text mining and sentiment analysis
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise11b.html
# ============================================================

#------------------------------------------------------------
# Sentiment analysis + text mining and word cloud, Vietoris, 2020
#------------------------------------------------------------
# Install the required packages
# install.packages("tm")  # text mining
# install.packages("SnowballC") # text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # colour palettes
# install.packages("syuzhet") # sentiment analysis
# install.packages("ggplot2") # plotting

# load the packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# read a txt file from the local computer
# sample file with made-up chocolate reviews (the NRC lexicon is English, so the text must be in English):
# https://raw.githubusercontent.com/senzorika/SaIT/master/datasety/recenzie_en.txt
text <- readLines(file.choose())
text
# data cleaning and parsing
TextDoc <- Corpus(VectorSource(text))

# preparing and parsing the document
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)

# the 5 most frequent words
head(dtm_d, 5)

# chart of the 5 most frequent words
barplot(dtm_d[1:5, ]$freq, las = 2, names.arg = dtm_d[1:5, ]$word, col = "lightgreen", main = "Top 5 most used words", ylab = "Word frequencies")

# word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5, max.words = 100, random.order = FALSE, rot.per = 0.40, colors = brewer.pal(8, "Dark2"))

# run the sentiment analysis
d <- get_nrc_sentiment(text)
# head(d,10) - first 10 rows of the binary emotion coding
head(d, 10)

# transpose the matrix and a few operations :)
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2 <- td_new[1:8, ] # the variable also contains the negative/positive summary, which is not plotted :)

# bar chart of all emotions
quickplot(sentiment, data = td_new2, weight = count, geom = "bar", fill = sentiment, ylab = "count") + ggtitle("Sentiment analysis (selected product)")
