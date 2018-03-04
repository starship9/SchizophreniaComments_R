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
commentCorpus <- tm_map(commentCorpus,removeWords,stop_words$word)
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

set.seed(100)
wordcloud(
  d$word,
  d$freq,
  random.order = FALSE,
  min.freq = 100,
  colors = brewer.pal(9, "PuBuGn"),
  rot.per = 0.1
)
title(main = "Most frequently occurring words on the submission URL",col.main = "white",line = 3,cex.main = 2)

commentDF <- data_frame(text = comments)
tidyCommentDF <-
  commentDF %>% unnest_tokens(word, text) %>% anti_join(stop_words)

library(reshape2)

par(bg="black")
set.seed(100)

tidyCommentDF %>% inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~
                                                                                                         sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = brewer.pal(9, "PuRd"), max.words = 100, scale = c(5, 0.5))
title(main = "Most frequently occurring words, classified by sentiment",col.main = "white",line = 3,cex.main = 2)

#totalWords <- tidyCommentDF %>% group_by(word) %>%  summarize(total = sum(n))

tidyCommentDFTrigrams <- commentDF %>% unnest_tokens(trigram,text,token = "ngrams", n = 2)
tidyCommentDFTrigrams

tidyCommentDFBigrams %>% count(trigram, sort = TRUE)

trigramsSeparated <- tidyCommentDFTrigrams %>% separate(trigram,c("word1","word2"),sep = " ")
trigramsFiltered <-  trigramsSeparated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
trigramCounts <- trigramsFiltered %>% count(word1,word2,sort = TRUE)
trigramCounts

library(igraph)
library(ggraph)

trigramCounts

trigramGraph <- trigramCounts %>% graph_from_data_frame()
trigramGraph

set.seed(100)
ggraph(trigramGraph,layout = "fr") + geom_edge_link() + geom_node_point() + geom_node_text(aes(label = name),vjust = 1, hjust = 1)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

commentBigrams <- commentDF %>% count_bigrams()
commentBigrams %>% filter(n>30) %>% visualize_bigrams()
