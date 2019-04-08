suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(pander))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(slam))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(topicmodels))

# convenience function to exclude items from in another list

'%out%' <- Negate ('%in%')

# load full Enron dataset
# not run
# con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
# load(con)
# close(con)
# enron %>% as_tibble(enron)
# save(enron, file = "data/enron.Rda")

# extract original content (part added by an email)
load("data/enron.Rda")
e_text <- enron %>% rename(text = lastword) %>% select(text)

# patterns for "textified" newlines, returns, tabs and Notes hypenation
# plus tokens tidytext::unnest_tokens fails to remove

pattern1  <- "\\\n"
pattern2  <- "\\\t"
pattern3  <- "\\\r"
pattern4  <- "="

# pre-process for tokenization

e_text <- e_text %>%  mutate(text = str_replace_all(text, pattern1, " "))    %>%
                      mutate(text = str_replace_all(text, pattern2, " "))    %>%
                      mutate(text = str_replace_all(text, pattern3, " "))    %>%
                      mutate(text = str_replace_all(text, pattern4, " "))    %>%
                      mutate(text = str_replace_all(text, "[:punct:]", " ")) %>%
                      mutate(text = str_replace_all(text, "[:blank:]", " ")) %>%
                      mutate(text = str_replace_all(text, "[:digit:]", " "))

# remove numbers, urls, punctuation and lowercase

e_text <- e_text %>% unnest_tokens(word, text, to_lower = TRUE)

# load tidytext package of parts of speech to ignore
data(stop_words)

# add stops

add_stops <- c("ect", "hou", "enronxgate", "enron", "corp", "company",
               "enron.com", "houston", "subject", "love")
lexicon <- c("user", "user", "user", "user", "user", "user", "user", "user",
             "user","user", "user")
new_stops <- cbind(add_stops, lexicon)
colnames(new_stops) <- c("word", "lexicon")
new_stops <- as_tibble(new_stops)
stop_words <- bind_rows(stop_words, new_stops)

# remove stop_words from e_text

e_text <- e_text %>% anti_join(stop_words)

# for Rmd
e_top_100_words <- e_text %>% count(word, sort = TRUE) %>% print(n = 100)

# checkpoint
#save(e_text, file = "data/e_text.Rda")

# consider for Rmd

e_top_words <- e_text %>% count(word, sort = TRUE)  %>%
            filter(n > 1500)                       %>%
            mutate(word = reorder(word,n))         %>%
            ggplot(aes(word,n))       +
            geom_col(fill = "grey50")  +
            xlab(NULL)                +
            coord_flip()              +
            labs(title="Most frequent words in un-reduced Enron corpus",
                 subtitle= "Occuring 1,500 times or more",
            caption="Source: Richard Careaga")	+
            theme_ipsum_rc()

# Zipf for Rmd

e_zipf <- e_text %>%  count(word, sort = TRUE)  %>%
            mutate(word = reorder(word, n))   %>%
            mutate(rank = row_number())       %>%
            mutate(freq = n/nrow(e_text))     %>%
            ggplot(aes(rank, freq))                 +
            xlab("Rank")                            +
            ylab("Frequency")                       +
            geom_line()                             +
            scale_x_log10()                         +
            scale_y_log10()                         +
            labs(title="Word frequency of un-reduced Enron corpus",
                subtitle= "Zipf distribution of approximately 570K words",
                caption="Source: Richard Careaga")	+
                theme_ipsum_rc()

# Enron wide vocabulary

e_vocab <- e_text %>% distinct(word)
e_vocab_size <- nrow(e_vocab)

# process the graph reduced words

#load("data/g_enron.Rda")

g_text <- g_enron %>% rename(text = payload) %>% select(text)

# pre-process for tokenization

g_text <- g_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
  mutate(text = str_replace_all(text, pattern2, " "))                     %>%
  mutate(text = str_replace_all(text, pattern3, " "))                     %>%
  mutate(text = str_replace_all(text, pattern4, " "))                     %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))                  %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))                  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))

# remove numbers, urls, punctuation and lowercase

g_text <- g_text %>% unnest_tokens(word, text, to_lower = TRUE)

# tokenize

g_text <- g_text %>% unnest_tokens(word, text, to_lower = TRUE)

# remove stop_words from g_text

g_text <- g_text %>% anti_join(stop_words)

# for Rmd
g_top_100_words <- g_text %>% count(word, sort = TRUE) %>% filter(n > 100)

# checkpoint
# save(g_text, file = "data/g_text.Rda")

# # consider for Rmd

g_top_words <- g_text %>% count(word, sort = TRUE)  %>%
  filter(n > 250)                                   %>%
  mutate(word = reorder(word,n))                    %>%
  ggplot(aes(word,n))         +
  geom_col(fill = "grey50")   +
  xlab(NULL)                  +
  coord_flip()                +
  labs(title="Most frequent words in un-reduced Enron corpus",
       subtitle= "Occuring 250 times or more",
       caption="Source: Richard Careaga")	+
  theme_ipsum_rc()

# Zipf for Rmd

g_zipf <- g_text %>%  count(word, sort = TRUE)  %>%
  mutate(word = reorder(word, n))   %>%
  mutate(rank = row_number())       %>%
  mutate(freq = n/nrow(g_text))     %>%
  ggplot(aes(rank, freq))                 +
  xlab("Rank")                            +
  ylab("Frequency")                       +
  geom_line()                             +
  scale_x_log10()                         +
  scale_y_log10()                         +
  labs(title="Word frequency of core Enron corpus",
       subtitle= "Zipf distribution of approximately 13K words",
       caption="Source: Richard Careaga")	+
  theme_ipsum_rc()

# Core graph vocabulary
# for Rmd note reduction

g_vocab <- g_text %>% distinct(word)

g_vocab_size <- nrow(g_vocab)

# for Rmd

g_vocab_proportion <- g_vocab_size/e_vocab_size

g_excluded <- setdiff(e_vocab,g_vocab)

d_top_100_words <- g_excluded %>% count(word, sort = TRUE) %>% print(n = 100)

d_singlets <- g_excluded %>% count(word)

max_singlets <- max(d_singlets$n)

# checkpoint save vocabulary of core graph; the vocabulary of the entire
# corpus consists of singletons
# save(g_vocab, file = "data/g_vocab.Rda")


# swtich to tm to create document term matrix

g_vs  <- VectorSource(g_text$word)
g_corpus <- VCorpus(g_vs)
g_dtm <- DocumentTermMatrix(g_corpus) # 100% sparse


# remove empty rows
# note: 3 4 6 9 12 tried
ui = unique(g_dtm$i)
g_dtm = g_dtm[ui,]
g_lda <- LDA(g_dtm, k = 3, control =  list(seed = 2203))
g_topics <- tidy(g_lda, metrix = "beta")

g_top_terms <- g_topics %>% group_by(topic) %>% top_n(50, beta) %>% ungroup() %>%
  arrange(topic, -beta)

# Use Rmd
g_topic_plot <- g_top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))   +
  geom_col(show.legend = FALSE)                   +
  facet_wrap(~ topic, scales = "free")            +
  coord_flip()

# load the three latent sna graphs
# load("data/glc1.Rda")
# load("data/glc2.Rda")
# load("data/glc3.Rda")

