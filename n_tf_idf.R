# patterned on TidyText ch 3
setwd("projects/enron")
library(tidyverse)
library(tidytext)
load("n_enron.Rda")
n_enron <- as_tibble(n_enron)
book_words <- n_enron %>% unnest_tokens(word,payload) %>% count(f_cluster, word, sort = TRUE) %>% ungroup()
total_words <- book_words %>% group_by(f_cluster) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words
total_words <- book_words %>% group_by(f_cluster) %>% summarise(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words
ggplot(book_words, aes(n/total, fill = f_cluster)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~f_cluster, ncol = 2, scales = "free_y")
ggplot(book_words, aes(n/total, fill = f_cluster)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~f_cluster, ncol = 2, scales = "free_y")
freq_by_rank <- book_words %>%
  group_by(f_fluster) %>%
  mutate(rank = row_number(),
         ggplot(book_words, aes(n/total, fill = f_cluster)) +
           geom_histogram(show.legend = FALSE) +
           xlim(NA, 0.0009) +
           facet_wrap(~f_cluster, ncol = 2, scales = "free_y")
         freq_by_rank <- book_words %>%
           group_by(f_cluster) %>%
           mutate(rank = row_number(),
                  `term frequency` = n/total)
         freq_by_rank
         freq_by_rank %>%
           ggplot(aes(rank, `term frequency`, color = f_cluster)) +
           geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
           scale_x_log10() +
           scale_y_log10()
         rank_subset <- freq_by_rank %>%
           filter(rank < 500,
                  rank > 10)
         lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
         book_words
         book_words <- book_words %>%
           bind_tf_idf(word, f_cluster, n)
         book_words
         book_words %>%
           select(-total) %>%
           arrange(desc(tf_idf))
         book_words <- book_words %>%
           bind_tf_idf(word, f_cluster, n)
         book_words
         book_words %>%
           select(-total) %>%
           arrange(desc(tf_idf))
         book_words %>%
           arrange(desc(tf_idf)) %>%
           mutate(word = factor(word, levels = rev(unique(word)))) %>%
           group_by(f_cluster) %>%
           top_n(15) %>%
           ungroup %>%
           ggplot(aes(word, tf_idf, fill = f_cluster)) +
           geom_col(show.legend = FALSE) +
           labs(x = NULL, y = "tf-idf") +
           facet_wrap(~f_cluster, ncol = 2, scales = "free") +
           coord_flip()

