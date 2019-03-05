setwd("projects/enron")
library(broom)
library(GGally)
library(here)
library(kernlab)
library(statnet)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
uri <- here("wolak.pdf")
engine <- "pdftools"
reader <- readPDF(engine)
wolak <- reader(elem = list(uri = uri), language = "en", id = "id1")
save(wolak, file = "wolak.Rda")
wolak <- VectorSource(wolak)
w_corpus <- VCorpus(wolak)
w_corpus <- tm_map(w_corpus, stripWhitespace)
w_corpus <- tm_map(w_corpus, removeWords, stopwords("english"))
w_corpus <- tm_map(w_corpus, removeNumbers)
w_corpus <- tm_map(w_corpus, removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
w_corpus <- tm_map(w_corpus, stemDocument)
save(w_corpus, file = "w_corpus.Rda")
w_dtm <- DocumentTermMatrix(w_corpus)
removeSparseTerms(w_dtm, 0.2)
save(w_dtm, file = "w_dtm.Rda")
#findFreqTerms(w_dtm, 5)
#findAssocs(w_dtm, "day-ahead", 0.8)
w_ctm <- CTM(w_dtm, k = 5) # how to unpack
w_lda <- LDA(w_dtm, control = list(alpha = 0.1), k = 5)
w_lda_inf <- posterior(w_lda, w_dtm)

term_tfidf <-
  + tapply(w_dtm$v/row_sums(w_dtm)[w_dtm$i], w_dtm$j, mean) *
  + log2(nDocs(w_dtm)/col_sums(w_dtm > 0))

w_dtm <- w_dtm[,term_tfidf >= 0.1]
w_dtm <- w_dtm[row_sums(w_dtm) > 0,]
summary(col_sums(w_dtm))

library("topicmodels")
k <- 3
SEED <- 2010
w_TM <- list(VEM = LDA(w_dtm, k = k, control = list(seed = SEED)),VEM_fixed = LDA(w_dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)), Gibbs = LDA(w_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)), CTM = CTM(w_dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(w_TM[1:2], slot, "alpha")

w_Topic <- topics(w_TM[["VEM"]], 1)
w_terms <- terms(w_TM[["VEM"]], 250)
Terms[,1:5]

w_terms <- tidy(w_dtm)
w_terms <- w_terms %>% mutate(term = str_replace(term, "^[:punct:]", ""))

# Sat Feb 23 17:05:05 2019 ------------------------------
# can be no blank rows in dtm, but nrow must be kept in sync
# Must also re run networks -----------------------------
#g_enron <- g_enron %>% filter(nchar(payload) <10) # filters 692 blanks
g_bag <- g_enron %>% select(payload)
g <- VectorSource(g_bag)
g_corpus <- tm_map(g_corpus, stripWhitespace)
g_corpus <- tm_map(g_corpus, removeWords, stopwords("english"))
g_corpus <- tm_map(g_corpus, removeNumbers)
g_corpus <- tm_map(g_corpus, removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
g_corpus <- tm_map(g_corpus, stemDocument)
#save(g_corpus, file = "g_corpus.Rda")
g_dtm <- DocumentTermMatrix(g_corpus)
# Sat Feb 23 15:12:14 2019 ------------------------------
# Remove all zero rows
rowTotals <- apply(g_dtm , 1, sum)
g_dtm <- g_dtm[rowTotals> 0, ]

removeSparseTerms(g_dtm, 0.2)
save(g_dtm, file = "g_dtm.Rda")

library("topicmodels")



k <- 3
SEED <- 2010
g_TM <- list(VEM = LDA(g_dtm, k = k, control = list(seed = SEED)),VEM_fixed = LDA(g_dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)), Gibbs = LDA(g_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)), CTM = CTM(g_dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(g_TM[1:2], slot, "alpha")


g_terms<- terms(g_TM[["VEM"]], 250)
