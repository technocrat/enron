# patterned on TidyText ch 3
setwd("projects/enron")
library(tidyverse)
library(tidytext)
load("n_enron.Rda")
n_enron <- as_tibble(n_enron)

c_words <- n_enron                                  %>%
              unnest_tokens(word,payload)           %>%
              count(f_cluster, word, sort = TRUE)   %>%
              ungroup()

total_words <- c_words                              %>%
               group_by(f_cluster)                  %>%
               summarize(total = sum(n))

c_words <- left_join(c_words, total_words)


ggplot(c_words, aes(n/total, fill = f_cluster))  +
  geom_histogram(show.legend = FALSE)               +
  xlim(NA, 0.0009)                                  +
  facet_wrap(~f_cluster, ncol = 2, scales = "free_y")

freq_by_rank <- c_words               %>%
 group_by(f_cluster)                  %>%
 mutate(rank = row_number(),
        `term frequency` = n/total)

freq_by_rank %>%
 ggplot(aes(rank, `term frequency`, color = f_cluster))   +
 geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE)  +
 scale_x_log10()                                          +
 scale_y_log10()

rank_subset <- freq_by_rank %>%
 filter(rank < 500,
        rank > 10)

fit <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
summary(fit)
#par = (mfrow = c(2,2))
#plot(fit)

c_words <- c_words  %>%
  bind_tf_idf(word, f_cluster, n)

c_words                           %>%
  select(-total)                  %>%
  arrange(desc(tf_idf))           %>%
  mutate(word =
    factor(word, levels =
             rev(unique(word))))  %>%
  group_by(f_cluster)             %>%
  top_n(15)                       %>%
  ungroup                         %>%
  ggplot(aes(word, tf_idf, fill = f_cluster))       +
  geom_col(show.legend = FALSE)                     +
  labs(x = NULL, y = "tf-idf")                      +
  facet_wrap(~f_cluster, ncol = 2, scales = "free") +
  coord_flip()

