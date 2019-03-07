setwd("projects/enron")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
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


n1_text <- n_enron %>% filter(f_cluster == 1) %>% select(payload)
n2_text <- n_enron %>% filter(f_cluster == 2) %>% select(payload)
n3_text <- n_enron %>% filter(f_cluster == 3) %>% select(payload)

n1_words <- n1_text %>%
  unnest_tokens(word, payload) %>%
  anti_join(stop_words) %>%
  filter(word != str_match_all(word,"^\\d+"))

n2_words <- n2_text %>%
  unnest_tokens(word, payload) %>%
  anti_join(stop_words) %>%
  filter(word != str_match_all(word,"^\\d+"))


n3_words <- n3_text %>%
  unnest_tokens(word, payload) %>%
  anti_join(stop_words) %>%
  filter(word != str_match_all(word,"^\\d+"))


n1_words %>% count(word, sort = TRUE) %>% print(n = 25)
n2_words %>% count(word, sort = TRUE) %>% print(n = 25)
n3_words %>% count(word, sort = TRUE) %>% print(n = 25)

n1_words %>%
  count(word, sort = TRUE)          %>%
  filter(n > 75)                    %>%
  mutate(word = reorder(word, n))   %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 1 Top Words")       +
  ylab("Occuring 75 times or more") +
  coord_flip()


n2_words %>%
  count(word, sort = TRUE)          %>%
  filter(n > 25)                    %>%
  mutate(word = reorder(word, n))   %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 2 Top Words")       +
  ylab("Occuring 25 times or more") +
  coord_flip()

n3_words %>%
  count(word, sort = TRUE)          %>%
  filter(n > 50)                    %>%
  mutate(word = reorder(word, n))   %>%
  ggplot(aes(word,n))               +
  geom_col()                        +
  xlab("Cluster 3 Top Words")       +
  ylab("Occuring 50 times or more") +
  coord_flip()

# Wed Mar  6 20:36:18 2019 ------------------------------
# This has to be refactored to create freq for all three clusters,
#send to matrix, convert NA to zero then do cor.test

frequency <- bind_rows(mutate(n1_words, cluster = "Cluster 1"),
                       mutate(n2_words, cluster = "Cluster 2"),
                       mutate(n3_words, cluster = "Cluster 3"))  %>%
                       mutate(word = str_extract(word,
                          "[a-z']+")) 	                        %>%
                       count(cluster, word)      					   		%>%
                       group_by(cluster)      						   		%>%
                       mutate(proportion = n /sum(n))			   		%>%
                       select(-n)				          				   		%>%
                       spread(cluster, proportion)				   		%>%
                       gather(cluster, proportion,
                              `Cluster 2`:`Cluster 3`)


cor.test(data = frequency[frequency$cluster == "Cluster 2",],
          ~ proportion + `Cluster 1`)

cor.test(data = frequency[frequency$cluster == "Cluster 2",],
         ~ proportion + `Cluster 3`)

cor.test(data = frequency[frequency$cluster == "Cluster 3",],
         ~ proportion + `Cluster 1`)

