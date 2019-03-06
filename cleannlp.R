options(java.parameters = "-Xmx8000m")
setwd("projects/enron")
library(coda)
library(cleanNLP)
library(tidyverse)
library(GGally)
library(tm)
library(tidyr)
library(tidytext)
'%out%' <- Negate ('%in%')
load("n_enron.Rda")
data(stop_words)
cnlp_init_corenlp("en", anno_level = 2, mem = "12g")
cnlp_init_tokenizers()
n1_text <- n_enron %>% filter(f_cluster == 1) %>% select(payload)
n2_text <- n_enron %>% filter(f_cluster == 2) %>% select(payload)
n3_text <- n_enron %>% filter(f_cluster == 3) %>% select(payload)

n1_ann <- cnlp_annotate(n1_text, text_var = "payload")
n2_ann <- cnlp_annotate(n2_text, text_var = "payload")
n3_ann <- cnlp_annotate(n3_text, text_var = "payload")

n1_sent_length <- cnlp_get_token(n1_ann) %>%
  group_by(id, sid) %>%
  summarize(sentence_length = max(tid)) %>%
  summarize(avg_sentence_length = mean(sentence_length)) %>% arrange(desc(avg_sentence_length)) %>% ungroup()

n2_sent_length <-cnlp_get_token(n2_ann) %>%
  group_by(id, sid) %>%
  summarize(sentence_length = max(tid)) %>%
  summarize(avg_sentence_length = mean(sentence_length)) %>% arrange(desc(avg_sentence_length)) %>% ungroup()

n3_sent_length <-cnlp_get_token(n3_ann) %>%
  group_by(id, sid) %>%
  summarize(sentence_length = max(tid)) %>%
  summarize(avg_sentence_length = mean(sentence_length)) %>% arrange(desc(avg_sentence_length)) %>% ungroup()
