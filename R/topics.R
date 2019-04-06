setwd("projects/enron")
library(broom)
library(GGally)
library(here)
library(pdftools)
library(slam)
library(stringr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
load("g_enron.Rda")
load("wolak_clean.Rda")



# NLP analysis of [Wolak report] on the California energy market

# Note on supplemental data cleaning operations
# uri <- here("sources/wolak.pdf")
# engine <- "pdftools"
# rseader <- readPDF(engine)
# wolak_text <- reader(elem = list(uri = uri), language = "en", id = "id1")$content # discard metadata
# See supplemental script wolak_scrub.R for operations
# to clean wolak_text for topic analysis as a single document
# rather than as a collection of documents (one per page).
# saved as wolak_clean.Rda

# Bring the [Wolak report] plaintext into a `tm` package object
wolak    <- VectorSource(wolak_clean)
w_corpus <- VCorpus(wolak)
w_corpus <- tm_map(w_corpus, removeWords, stopwords("english"))
w_corpus <- tm_map(w_corpus, removeNumbers)
#save(w_corpus, file = "w_corpus.Rda")

# create a document term matrix and remove seldom occurring word pairs
w_dtm <- DocumentTermMatrix(w_corpus)
removeSparseTerms(w_dtm, 0.2)
#save(w_dtm, file = "w_dtm.Rda")

# Inspect the frequent terms for the document

findFreqTerms(w_dtm, 5)

# This code follows the example in the [tidytext] book.

# Create a model for six topics (selected after review of
# more and fewer topics)

w_lda <- LDA(w_dtm, control = list(seed = 2203), k = 6)

# extract topics and their respective per topic per word frequencies

w_topics <- tidy(w_lda, matrix = "beta")

w_top_terms <- w_topics %>%
  group_by(topic) 		%>%
  top_n(25, beta) 		%>%
  ungroup() 			%>%
  arrange(topic, -beta)

w_top_terms 							%>%
  mutate(term = reorder(term, beta)) 	%>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) 				+
  facet_wrap(~ topic, scales = "free") 			+
  coord_flip()

# Compare log differences of topics to highlight differences


beta_spread2_1

beta_spread2_1 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

#===============================================================================
beta_spread2_3 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))

beta_spread2_3

beta_spread2_3 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 3") +
  coord_flip()

#===============================================================================
beta_spread2_4 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic4 > .001) %>%
  mutate(log_ratio = log2(topic4 / topic1))

beta_spread2_4

beta_spread2_4 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 4") +
  coord_flip()

#===============================================================================
beta_spread2_5 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic5 > .001) %>%
  mutate(log_ratio = log2(topic5 / topic1))

beta_spread2_5

beta_spread2_5 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 5") +
  coord_flip()
#===============================================================================
beta_spread2_6 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic6 > .001) %>%
  mutate(log_ratio = log2(topic6 / topic1))

beta_spread2_6

beta_spread2_6 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 6") +
  coord_flip()

#===============================================================================
beta_spread3_1 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))

beta_spread3_1

beta_spread3_1 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 3 / topic 1") +
  coord_flip()

#===============================================================================
beta_spread4_1 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic4 > .001) %>%
  mutate(log_ratio = log2(topic4 / topic1))

beta_spread4_1

beta_spread4_1 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 4 / topic 1") +
  coord_flip()


#===============================================================================
beta_spread4_3 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic3 > .001 | topic4 > .001) %>%
  mutate(log_ratio = log2(topic4 / topic3))

beta_spread4_3

beta_spread4_3 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 4 / topic 3") +
  coord_flip()


#===============================================================================
beta_spread4_5 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic5 > .001 | topic4 > .001) %>%
  mutate(log_ratio = log2(topic4 / topic5))

beta_spread4_5

beta_spread4_5 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 4 / topic 5") +
  coord_flip()


#===============================================================================
beta_spread4_6 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic6 > .001 | topic4 > .001) %>%
  mutate(log_ratio = log2(topic6 / topic4))

beta_spread4_6

beta_spread4_6 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 6 / topic 4") +
  coord_flip()


#===============================================================================
beta_spread5_6 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic6 > .001 | topic6 > .001) %>%
  mutate(log_ratio = log2(topic6 / topic5))

beta_spread5_6

beta_spread5_6 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 6 / topic 4") +
  coord_flip()

#===============================================================================

# begin analysis of Enron corpus with original content off all emails

cluster0_raw <- n_enron$payload
#save(cluster0_raw, file = "cluster0_raw.Rda")

# preprocessing "textified" newlines, returns, tabs and Notes hypenation

pattern1  <- "\\\n"
pattern2  <- "\\\t"
pattern3  <- "\\\r"
pattern4  <- "="

milled <- str_replace_all(cluster0_raw, pattern1, " ")
milled <- str_replace_all(milled, pattern2, " ")
milled <- str_replace_all(milled, pattern3, " ")
milled <- str_replace_all(milled, pattern4, " ")

# join list into a single character string

cluster0_cleaned <- paste(milled, sep = '', collapse = '')
#save(cluster0_cleaned, file = "cluster0_cleaned.Rda")

# Bring the unclustered Enron plaintext into a `tm` package object
c0_corpus <- VCorpus(c0_corpus)
c0_corpus <- tm_map(c0_corpus, stripWhitespace)
c0_corpus <- tm_map(c0_corpus, content_transformer(tolower))
c0_corpus <- tm_map(c0_corpus, removeWords, stopwords("english"))
c0_corpus <- tm_map(c0_corpus, removeNumbers)
c0_corpus <- tm_map(c0_corpus, removePunctuation)

#save(w_corpus, file = "c0_corpus.Rda")

# create a document term matrix and remove seldom occurring word pairs
c0_dtm <- DocumentTermMatrix(c0_corpus)
removeSparseTerms(c0_dtm, 0.2)
#save(c0_dtm, file = "c0_dtm.Rda")

# Inspect the frequent terms for the document

findFreqTerms(c0_dtm, 5)

# This code follows the example in the [tidytext] book.

# Create a model for four topics

c0_lda <- LDA(c0_dtm, control = list(seed = 2203), k = 4)

# extract topics and their respective per topic per word frequencies

c0_topics <- tidy(c0_lda, matrix = "beta")

c0_top_terms <- c0_topics %>%
  group_by(topic)    	  	%>%
  top_n(25, beta) 	    	%>%
  ungroup() 			        %>%
  arrange(topic, -beta)

c0_top_terms 			            				%>%
  mutate(term = reorder(term, beta)) 	%>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE)          				+
  facet_wrap(~ topic, scales = "free") 	    		+
  coord_flip()

# Replicate on a cluster-by-cluster basis

cluster1_raw <- n_enron %>% filter(f_cluster == 1) %>% select(payload)
save(cluster1_raw, file = "cluster1_raw.Rda")

# preprocessing "textified" newlines, returns, tabs and Notes hypenation

pattern1  <- "\\\\n"
pattern2  <- "\\\\t"
pattern3  <- "\\\\r"
pattern4  <- "="

milled <- str_replace_all(cluster1_raw, pattern1, " ")
milled <- str_replace_all(milled, pattern2, " ")
milled <- str_replace_all(milled, pattern3, " ")
milled <- str_replace_all(milled, pattern4, " ")

# join list into a single character string

cluster1_cleaned <- paste(milled, sep = '', collapse = '')
save(cluster1_cleaned, file = "cluster1_cleaned.Rda")

# Bring the unclustered Enron plaintext into a `tm` package object
c1_corpus <- VectorSource(cluster1_cleaned)
c1_corpus <- VCorpus(c1_corpus)
c1_corpus <- tm_map(c1_corpus, stripWhitespace)
c1_corpus <- tm_map(c1_corpus, content_transformer(tolower))
c1_corpus <- tm_map(c1_corpus, removeWords, stopwords("english"))
c1_corpus <- tm_map(c1_corpus, removeNumbers)
c1_corpus <- tm_map(c1_corpus, removePunctuation)

save(c1_corpus, file = "c1_corpus.Rda")

# create a document term matrix and remove seldom occurring word pairs
c1_dtm <- DocumentTermMatrix(c1_corpus)
removeSparseTerms(c1_dtm, 1.2)
save(c1_dtm, file = "c1_dtm.Rda")

# Inspect the frequent terms for the document

#findFreqTerms(c1_dtm, 5)

# This code follows the example in the [tidytext] book.

# Create a model for three topics

c1_lda <- LDA(c1_dtm, control = list(seed = 2213), k = 3)

# extract topics and their respective per topic per word frequencies

c1_topics <- tidy(c1_lda, matrix = "beta")

c1_top_terms <- c1_topics 	%>%
  group_by(topic)    	  	%>%
  top_n(25, beta) 	    	%>%
  ungroup() 			    %>%
  arrange(topic, -beta)
