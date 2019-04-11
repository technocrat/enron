suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(pander))
suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(slam))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupg_Messages(library(tm))
suppressPackageStartupMessages(library(topicmodels))

# convenience function to exclude items from in another list

'%out%' <- Negate ('%in%')


# extract original content
load("data/enron.Rda")

e_text <- enron %>% rename(text = lastword) %>% select(text)

# patterns for "textified" newlines, returns, tabs and Notes hypenation
# plus tokens tidytext::unnest_tokens fails to remove
# load tidytext package of parts of speech to ignore


# augment tm::stop_words

data(stop_words)

# first iteration identified words that occurred only once, no discriminatory
# power. To be added to tm:stopwords
load("data/singlets.Rda")

add_stops <- c("company", "corp", "ect", "enron", "enron.com",
               "enronxgate", "hou", "houston", "love", "subject")
add_stops <- append(add_stops, singlets)
stop_source <- rep("user", length(add_stops))
supp_stops <- as_tibble(cbind(add_stops, stop_source)) %>%
              rename(word = add_stops, lexicon = stop_source)
my_stopwords <- bind_rows(stop_words, supp_stops)
stop_words <- my_stopwords

#save(stop_words, file = "data/stop_words.Rda")

# stringr patters to remove textified control sequences
pattern1  <- "\\\n"
pattern2  <- "\\\t"
pattern3  <- "\\\r"
pattern4  <- "= " # Notes hypenator

# pre-process and tokenize

e_text <- e_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
            mutate(text = str_replace_all(text, pattern2, " "))           %>%
            mutate(text = str_replace_all(text, pattern3, " "))           %>%
            mutate(text = str_replace_all(text, pattern4, " "))           %>%
            # removes possessives and hypenated words
            # mutate(text = str_replace_all(text, "[:punct:]", " "))        %>%
            mutate(text = str_replace_all(text, "[:blank:]", " "))        %>%
            mutate(text = str_replace_all(text, "[:digit:]", " "))

# tokenize

e_text <- e_text %>% unnest_tokens(word, text, to_lower = TRUE)

# remove common words and those that occur only once
#load("data/stop_words.Rda")
e_text <- e_text %>% anti_join(stop_words)

# # for Rmd

e_top_100_words <- e_text %>% count(word, sort = TRUE) %>% print(n = 100)

# checkpoint
# save(e_text, file = "data/e_text.Rda")

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
                theme_void()                        +
                theme_ipsum_rc()

# # Enron wide vocabulary

e_vocab <- e_text %>% distinct(word)
e_vocab_size <- nrow(e_vocab)

# process the graph reduced words

load("data/g_enron.Rda")

g_text <- g_enron %>% rename(text = payload) %>% select(text)

# pre-process for tokenization

g_text <- g_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
  mutate(text = str_replace_all(text, pattern2, " "))                     %>%
  mutate(text = str_replace_all(text, pattern3, " "))                     %>%
  mutate(text = str_replace_all(text, pattern4, " "))                     %>%
  # removes possessives and hypenated words
  # mutate(text = str_replace_all(text, "[:punct:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))                  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))

# tokenize

g_text <- g_text %>% unnest_tokens(word, text, to_lower = TRUE)

# remove stop_words from g_text

g_text <- g_text %>% anti_join(stop_words)

# for Rmd

g_top_100_words <- g_text %>% count(word, sort = TRUE) %>% filter(n > 100)

# checkpoint
# save(g_text, file = "data/g_text.Rda")

# consider for Rmd

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

# source of additons to tm::stop_words
max_singlets <- max(d_singlets$n)

#checkpoint save vocabulary of core graph
# save(g_vocab, file = "data/g_vocab.Rda")

# end of NLP preprocessing
# swtich to quanteda package to create document term matrix
# and further modeling


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
load("data/glc1.Rda")
load("data/glc2.Rda")
load("data/glc3.Rda")

g1_text <- glc1 %>% rename(text = payload) %>% select(text)
g2_text <- glc2 %>% rename(text = payload) %>% select(text)
g3_text <- glc3 %>% rename(text = payload) %>% select(text)

g1_text <- g1_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
  mutate(text = str_replace_all(text, pattern2, " "))           %>%
  mutate(text = str_replace_all(text, pattern3, " "))           %>%
  mutate(text = str_replace_all(text, pattern4, " "))           %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))        %>%
  unnest_tokens(word, text, to_lower = TRUE)

g1_text <- g1_text %>% anti_join(stop_words)

g2_text <- g2_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
  mutate(text = str_replace_all(text, pattern2, " "))           %>%
  mutate(text = str_replace_all(text, pattern3, " "))           %>%
  mutate(text = str_replace_all(text, pattern4, " "))           %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))        %>%
  unnest_tokens(word, text, to_lower = TRUE)

g2_text <- g2_text %>% anti_join(stop_words)

g3_text <- g3_text %>%  mutate(text = str_replace_all(text, pattern1, " ")) %>%
  mutate(text = str_replace_all(text, pattern2, " "))           %>%
  mutate(text = str_replace_all(text, pattern3, " "))           %>%
  mutate(text = str_replace_all(text, pattern4, " "))           %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))        %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))        %>%
  unnest_tokens(word, text, to_lower = TRUE)

g3_text <- g3_text %>% anti_join(stop_words)

g1_vocab <- g1_text %>% distinct(word)
g1_vocab_size <- nrow(g1_vocab)

g2_vocab <- g2_text %>% distinct(word)
g2_vocab_size <- nrow(g2_vocab)

g3_vocab <- g3_text %>% distinct(word)
g3_vocab_size <- nrow(g3_vocab)

g1_vs  <- VectorSource(g1_text$word)
g1_corpus <- VCorpus(g1_vs)
g1_dtm <- DocumentTermMatrix(g1_corpus) # 100% sparse

# remove empty rows
# note: 3 4 6 9 12 tried
ui = unique(g1_dtm$i)
g1_dtm = g1_dtm[ui,]
g1_lda <- LDA(g1_dtm, k = 3, control =  list(seed = 2203))
g1_topics <- tidy(g1_lda, metrix = "beta")

g1_top_terms <- g1_topics %>% group_by(topic) %>% top_n(50, beta) %>%
                ungroup() %>% arrange(topic, -beta)

# Use Rmd
g1_topic_plot <- g1_top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))   +
  geom_col(show.legend = FALSE)                   +
  facet_wrap(~ topic, scales = "free")            +
  coord_flip()

g2_vs  <- VectorSource(g2_text$word)
g2_corpus <- VCorpus(g2_vs)
g2_dtm <- DocumentTermMatrix(g2_corpus) # 100% sparse

# remove empty rows
# note: 3 4 6 9 12 tried
ui = unique(g2_dtm$i)
g2_dtm = g2_dtm[ui,]
g2_lda <- LDA(g2_dtm, k = 3, control =  list(seed = 2203))
g2_topics <- tidy(g2_lda, metrix = "beta")

g2_top_terms <- g2_topics %>% group_by(topic) %>% top_n(50, beta) %>%
  ungroup() %>% arrange(topic, -beta)

# Use Rmd
g2_topic_plot <- g2_top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))   +
  geom_col(show.legend = FALSE)                   +
  facet_wrap(~ topic, scales = "free")            +
  coord_flip()

g3_vs  <- VectorSource(g3_text$word)
g3_corpus <- VCorpus(g3_vs)
g3_dtm <- DocumentTermMatrix(g3_corpus) # 100% sparse

# remove empty rows
# note: 3 4 6 9 12 tried
ui = unique(g3_dtm$i)
g3_dtm = g3_dtm[ui,]
g3_lda <- LDA(g3_dtm, k = 3, control =  list(seed = 2203))
g3_topics <- tidy(g3_lda, metrix = "beta")

g3_top_terms <- g3_topics %>% group_by(topic) %>% top_n(50, beta) %>%
                ungroup() %>% arrange(topic, -beta)

# Use Rmd

g3_topic_plot <- g3_top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))   +
  geom_col(show.legend = FALSE)                   +
  facet_wrap(~ topic, scales = "free")            +
  coord_flip()

# Use Rmd
# vocabularies are distinct

g1_2_vocab <- nrow(setdiff(g1_vocab, g2_vocab))/g1_vocab_size
g1_3_vocab <- nrow(setdiff(g1_vocab, g3_vocab))/g1_vocab_size
g2_3_vocab <- nrow(setdiff(g2_vocab, g3_vocab))/g2_vocab_size
g1_2_3_vocab <- nrow(setdiff(g1_vocab, union(g2_vocab,g3_vocab)))/g1_vocab_size
g2_1_3_vocab <- nrow(setdiff(g2_vocab, union(g1_vocab,g3_vocab)))/g2_vocab_size
g3_1_2_vocab <- nrow(setdiff(g3_vocab, union(g1_vocab,g2_vocab)))/g2_vocab_size

g1_text2 <- g1_text  %>% mutate(cluster = "g1")
g2_text2 <- g2_text  %>% mutate(cluster = "g2")
g3_text2 <- g3_text  %>% mutate(cluster = "g3")

c_texts <- bind_rows(g1_text2, g2_text2, g3_text2)

occurences <- c_texts       %>%
  group_by(cluster)         %>%
  count(word, sort = TRUE)  %>%
  left_join(c_texts         %>%
    group_by(cluster)       %>%
    summarise(total = n())) %>%
  mutate(freq = n/total)    %>%
  ungroup()

# 2,402 terms occur only once; no discriminaory power
# added to stop_words for second pass
singlets <- occurences %>% filter(n == 1) %>% select(word)
singlets <- singlets$word

#save(singlets, file = "data/singlets.Rda")

c_freq <- occurences                    %>%
            select(cluster, word, freq) %>%
            spread(cluster, freq)       %>%
            arrange(g1,g2,g3)

c1_2_plot <- ggplot(c_freq, aes(g1,g2)) +
             geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
             geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)   +
             xlab("Graph cluster 1")                                           +
             ylab("Graph cluster 2")                                           +
             scale_x_log10()                                                   +
             scale_y_log10()                                                   +
             geom_abline(color = "red")                                        +
             labs(title="Relative word frequency of graph clusters 1 and 2",
                  subtitle= "Scaled to log10",
                  caption="Source: Richard Careaga")		    	  				       +
            theme_void()  	      									                        	 +
            theme_ipsum_rc()

c1_3_plot <- ggplot(c_freq, aes(g1,g3)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)   +
  xlab("Graph cluster 1")                                           +
  ylab("Graph cluster 3")                                           +
  scale_x_log10()                                                   +
  scale_y_log10()                                                   +
  geom_abline(color = "red")                                        +
  labs(title="Relative word frequency of graph clusters 1 and 3",
       subtitle= "Scaled to log10",
       caption="Source: Richard Careaga")		    	  				        +
  theme_void()  	      									                        	+
  theme_ipsum_rc()

c2_3_plot <- ggplot(c_freq, aes(g1,g2)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)   +
  xlab("Graph cluster 2")                                           +
  ylab("Graph cluster 3")                                           +
  scale_x_log10()                                                   +
  scale_y_log10()                                                   +
  geom_abline(color = "red")                                        +
  labs(title="Relative word frequency of graph clusters 2 and 3",
       subtitle= "Scaled to log10",
       caption="Source: Richard Careaga")		    	  				        +
  theme_void()  	      									                        	+
  theme_ipsum_rc()

# topicmodels::CTM has zero documentation
# g_ctm <- CTM(g_dtm, k=3)


