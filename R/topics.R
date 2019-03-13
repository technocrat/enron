setwd("projects/enron")
library(broom)
library(GGally)
library(here)
library(pdftools)
library(slam)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
load("g_enron.Rda")

# Mon Mar 11 21:55:19 2019 ------------------------------
# uri <- here("sources/wolak.pdf")
# engine <- "pdftools"
# rseader <- readPDF(engine)
# wolak_text <- reader(elem = list(uri = uri), language = "en", id = "id1")$content # discard metadata
# See supplemental script wolak_scrub.R for operations
# to clean wolak_text for topic analysis as a single document
# rather than as a collection of documents (one per page).
# saved as wolak_clean.Rda

load("wolak_clean.Rda")

wolak <- VectorSource(wolak_clean)
w_corpus <- VCorpus(wolak)
w_corpus <- tm_map(w_corpus, removeWords, stopwords("english"))
w_corpus <- tm_map(w_corpus, removeNumbers)
#save(w_corpus, file = "w_corpus.Rda")
w_dtm <- DocumentTermMatrix(w_corpus)
removeSparseTerms(w_dtm, 0.2)
#save(w_dtm, file = "w_dtm.Rda")
findFreqTerms(w_dtm, 5)
# ff ch 6 of tt
w_lda <- LDA(w_dtm, control = list(seed = 2203), k = 6)
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

#===============================================================================
beta_spread2_1 <- w_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

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





