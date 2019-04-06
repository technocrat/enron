suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(pander))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(slam))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(topicmodels))

# convenience function to exclude items from in another list

'%out%' <- Negate ('%in%')

# load full Enron dataset

load("enron.Rda")

# extract original content (part added by an email)

enron <- enron %>% rename(payload = lastword)

# load tidytext package of parts of speech to ignore
data(stop_words)

# identify possibly interesting words to bypass stop word filter

exempt_stopwords <- enframe(c("against", "all", "allow", "allows", "always",
                              "awfully", "beforehand", "behind", "below",
                              "better", "big", "can't", "cannot", "cant",
                              "case", "cases", "downwards", "except", "gives",
                              "good", "great", "highest", "hopefully",
                              "immediate", "might", "necessary", "not",
                              "nowhere", "numbers", "otherwise", "point",
                              "serious", "seriously", "state", "states",
                              "unfortunately", "value", "worked", "working",
                              "zero"))

# create custom stopword list

colnames(exempt_stopwords) <- c("line", "word")
stop_words <- anti_join(stop_words, exempt_stopwords)
add_stops <- c("ect", "hou", "enronxgate")
lexicon <- c("user", "user", "user")
new_stops <- cbind(add_stops, lexicon)
colnames(new_stops) <- c("word", "lexicon")
new_stops <- as_tibble(new_stops)
stop_words <- rbind(stop_words, new_stops)
stop_words <- stop_words$word

enron <- enron$payload

# patterns for "textified" newlines, returns, tabs and Notes hypenation

pattern1  <- "\\\n"
pattern2  <- "\\\t"
pattern3  <- "\\\r"
pattern4  <- "="

# Convert tibble column to vector of strings

e_text <- enron

# remove patterns1-4

e_text <- str_replace_all(e_text, pattern1, " ")
e_text <- str_replace_all(e_text, pattern2, " ")
e_text <- str_replace_all(e_text, pattern3, " ")
e_text <- str_replace_all(e_text, pattern4, " ")

# join list into a single character string

e_text <- paste(e_text, sep = '', collapse = '')

# Bring the unreduced Enron plaintext into a `tm` package object

e_corpus <- VectorSource(e_text)
e_corpus <- tm_map(e_corpus, removeWords, stop_words)

# convert to VCorpus object

e_corpus <- VCorpus(e_corpus)

# tokenize, make lowercase, remove punctuation,
# remove numerals, remove stop words, remove whitespace

e_corpus <- tm_map(e_corpus, content_transformer(tolower))
e_corpus <- tm_map(e_corpus, removeNumbers)
e_corpus <- tm_map(e_corpus, removePunctuation)
e_corpus <- tm_map(e_corpus, removeWords, stop_words)
e_corpus <- tm_map(e_corpus, stripWhitespace)

save(e_corpus, file = "e_corpus.Rda")

# create a document term matrix (dtm)

e_dtm <- DocumentTermMatrix(e_corpus)
e_dtm
# removeSparseTerms(e_dtm, 0.2) # no diff


# frequent terms
findFreqTerms(e_dtm, 50)

e_lda <- LDA(e_dtm, control = list(seed = 2203), k = 6)

e_topics <- tidy(e_lda, matrix = "beta")

e_top_terms <- e_topics %>%
  group_by(topic) 		%>%
  top_n(25, beta) 		%>%
  ungroup() 			%>%
  arrange(topic, -beta)


# #tf-idf c_ = cluster words
# c_words <- n_enron                                            %>%
#   unnest_tokens(word,payload)                     %>%
#   anti_join(stop_words)                           %>%
#   filter(word != str_match_all(word,"\\d+"))      %>%
#   filter(word != str_match_all(word,"\\d+.\\d+")) %>%
#   count(f_cluster, word, sort = TRUE)             %>%
#   ungroup()
#
# total_words <- c_words                              %>%
#   group_by(f_cluster)                  %>%
#   summarize(total = sum(n))
#
# c_words <- left_join(c_words, total_words)
#
# freq_by_rank <- c_words               %>%
#   group_by(f_cluster)                  %>%
#   mutate(rank = row_number(),
#          `term frequency` = n/total)
#
# rank_subset <- freq_by_rank %>%
#   filter(rank < 500,
#          rank > 10)
#
# c_words <- c_words  %>%
#   bind_tf_idf(word, f_cluster, n)
#
# fit <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

