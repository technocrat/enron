setwd("projects/enron")
library(dplyr)
library(ggplot2)
library(magrittr)
library(scales)
library(stringr)
library(tibble)
library(tidyr)
library(tidytext)


'%out%' <- Negate ('%in%')
load("n_enron.Rda")
data(stop_words)

exempt_stopwords <- enframe(c("against", "all", "allow", "allows", "always", "awfully", "beforehand", "behind", "below", "better", "big", "can't", "cannot", "cant", "case", "cases", "downwards", "except", "gives", "good", "great", "highest", "hopefully", "immediate", "might", "necessary", "not", "nowhere", "numbers", "otherwise", "point", "serious", "seriously", "state", "states", "unfortunately", "value", "worked", "working", "zero"))
colnames(exempt_stopwords) <- c("line", "word")
stop_words <- anti_join(stop_words, exempt_stopwords)
add_stops <- c("ect", "hou", "enronxgate")
lexicon <- c("user", "user", "user")
new_stops <- cbind(add_stops, lexicon)
colnames(new_stops) <- c("word", "lexicon")
new_stops <- as_tibble(new_stops)
stop_words <- rbind(stop_words, new_stops)

n_text  <- n_enron %>% select(payload)
n1_text <- n_enron %>% filter(f_cluster == 1) %>% select(payload)
n2_text <- n_enron %>% filter(f_cluster == 2) %>% select(payload)
n3_text <- n_enron %>% filter(f_cluster == 3) %>% select(payload)

n_words <- n_text                             %>%
  unnest_tokens(word, payload)                %>%
  anti_join(stop_words)                       %>%
  filter(word != str_match_all(word,"\\d+"))  %>%
  filter(word != str_match_all(word,"\\d+.\\d+"))

n1_words <- n1_text                           %>%
  unnest_tokens(word, payload)                %>%
  anti_join(stop_words)                       %>%
  filter(word != str_match_all(word,"\\d+"))  %>%
  filter(word != str_match_all(word,"\\d+.\\d+"))

n2_words <- n2_text                           %>%
  unnest_tokens(word, payload)                %>%
  anti_join(stop_words)                       %>%
  filter(word != str_match_all(word,"\\d+"))  %>%
  filter(word != str_match_all(word,"\\d+.\\d+"))

n3_words <- n3_text                           %>%
  unnest_tokens(word, payload)                %>%
  anti_join(stop_words)                       %>%
  filter(word != str_match_all(word,"\\d+"))  %>%
  filter(word != str_match_all(word,"\\d+.\\d+"))

n_dist  <- n_words  %>% distinct()
n1_dist <- n1_words %>% distinct()
n2_dist <- n2_words %>% distinct()
n3_dist <- n3_words %>% distinct()

n1_vocab_n2 <- setdiff(n1_dist, n2_dist)
n1_vocab    <- setdiff(n1_vocab_n2, n3_dist)
n2_vocab_n1 <- setdiff(n2_dist, n1_dist)
n2_vocab    <- setdiff(n2_vocab_n1, n3_dist)
n3_vocab    <- setdiff(n3_dist, union(n1_dist, n2_dist))
n_vocab     <- setdiff(n_dist, union(n1_vocab, n2_vocab, n3_vocab))

n_dist_bag  <- n_words  %>%
  mutate(Cluster0 = word %in% n_vocab$word)   %>%
  filter(Cluster0 == TRUE)

n1_dist_bag <- n1_words %>%
  mutate(Cluster1 = word %in% n1_vocab$word)  %>%
  filter(Cluster1 == TRUE)

n2_dist_bag <- n2_words %>%
  mutate(Cluster2 = word %in% n2_vocab$word)  %>%
  filter(Cluster2 == TRUE)

n3_dist_bag <- n3_words %>%
  mutate(Cluster3 = word %in% n3_vocab$word)  %>%
  filter(Cluster3 == TRUE)

n_words %>%
  count(word)                       %>%
  filter(n > 100)                   %>%
  mutate(word = reorder(word, n))   %>%
  arrange(desc(n))                  %>%
  ggplot(aes(word,n))                 	+
  geom_col()                          	+
  xlab("All Clusters Top Common Words")	+
  ylab("Occuring 100 times or more")   	+
  coord_flip()

n1_words %>%
  count(word)                       %>%
  filter(n > 75)                    %>%
  mutate(word = reorder(word, n))   %>%
  arrange(desc(n))                  %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("All Cluster 1 Top Words")       +
  ylab("Occuring 75 times or more") +
  coord_flip()

n1_dist_bag                         %>%
  count(word)                       %>%
  filter(n > 25)                    %>%
  mutate(word = reorder(word, n))   %>%
  arrange(desc(n))                  %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 1 Only Top Words")  +
  ylab("Occuring 25 times or more") +
  coord_flip()

n2_words %>%
  count(word, sort = TRUE)          %>%
  filter(n > 25)                    %>%
  mutate(word = reorder(word, n))   %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("All Cluster 2 Top Words")       +
  ylab("Occuring 25 times or more") +
  coord_flip()

n2_dist_bag                         %>%
  count(word)                       %>%
  filter(n > 5)                     %>%
  mutate(word = reorder(word, n))   %>%
  arrange(desc(n))                  %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 2 Only Top Words")  +
  ylab("Occuring 5 times or more") +
  coord_flip()

n3_words %>%
  count(word, sort = TRUE)          %>%
  filter(n > 50)                    %>%
  mutate(word = reorder(word, n))   %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("All Cluster 3 Top Words")   +
  ylab("Occuring 50 times or more") +
  coord_flip()

n3_dist_bag                         %>%
  count(word)                       %>%
  filter(n > 35)                    %>%
  mutate(word = reorder(word, n))   %>%
  arrange(desc(n))                  %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 3 Only Top Words")  +
  ylab("Occuring 25 times or more") +
  coord_flip()


n_clusters <- n_words %>%
  mutate(Cluster1 = word %in% n1_dist_bag$word) %>%
  mutate(Cluster2 = word %in% n2_dist_bag$word) %>%
  mutate(Cluster3 = word %in% n3_dist_bag$word)

n_freq <- n_clusters %>%
  group_by(word,Cluster1,Cluster2,Cluster3) %>%
  count(word)                               %>%
  mutate(frequency = n/nrow(.))             %>%
  ungroup()

n_freq <- n_freq %>%
  mutate(Cluster1 = Cluster1 * 1) %>%
  mutate(Cluster2 = Cluster2 * 1) %>%
  mutate(Cluster3 = Cluster3 * 1)

n_freq <- n_freq %>%
  mutate(Cluster1 = Cluster1*frequency) %>%
  mutate(Cluster2 = Cluster2*frequency) %>%
  mutate(Cluster3 = Cluster3*frequency)


n_freq <- n_freq %>%
  mutate(Cluster1 = Cluster1*frequency) %>%
  mutate(Cluster2 = Cluster2*frequency) %>%
  mutate(Cluster3 = Cluster3*frequency)


cor.test(n_freq$Cluster1,n_freq$Cluster2)
cor.test(n_freq$Cluster1,n_freq$Cluster3)
cor.test(n_freq$Cluster2,n_freq$Cluster3)
