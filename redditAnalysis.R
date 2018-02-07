library(tm)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

comments <-
  readLines("Text/commentTextReddit.txt", encoding = "utf-8")
summary(comments)

commentCorpus <- Corpus(VectorSource(comments))
summary(commentCorpus)

commentCorpus <- tm_map(commentCorpus, tolower)
commentCorpus <-
  tm_map(commentCorpus, removeWords, stopwords("english"))
commentCorpus <- tm_map(commentCorpus, removePunctuation)
commentCorpus <- tm_map(commentCorpus, removeNumbers)
commentCorpus <- tm_map(commentCorpus, stripWhitespace)
commentCorpus <- tm_map(commentCorpus, stemDocument)

dtm <- TermDocumentMatrix(commentCorpus)
#summary(dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d)
par(bg = "black")
wordcloud(
  d$word,
  d$freq,
  random.order = FALSE,
  min.freq = 100,
  colors = brewer.pal(9, "PuBuGn"),
  rot.per = 0.1
)

commentDF <- data_frame(text = comments)
tidyCommentDF <-
  commentDF %>% unnest_tokens(word, text) %>% anti_join(stop_words)

library(reshape2)

par(bg="black")
tidyCommentDF %>% inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~
                                                                                                         sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = brewer.pal(9, "PuRd"), max.words = 100, scale = c(5, 0.1))
