suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggdendro))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(topicmodels))

# bring in core Enron corpus, tagged with clusters
load("data/c_enron.Rda")
load("data/stop_words.Rda")
# provide field names for corpus and order by date
c_enron <- c_enron %>%  rownames_to_column(var = "docid") %>%
                        rename(text = payload)            %>%
                        arrange(-desc(date))

c_corpus <- corpus(c_enron, docid_field = "docid", text_field = "text")

c_dfm <- dfm(c_corpus, remove_symbols = TRUE, remove_hyphens = FALSE,
             remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE,
             include_docvars = TRUE, remove = stopwords("en"))

# RMD

theme_set(theme_minimal())
textstat_frequency(c_dfm) %>%
  ggplot(aes(x = rank, y = log10(frequency)))       +
  geom_point()                                      +
  labs(title="Word frequency of core Enron corpus by frequency rank",
       subtitle= "Word frequency in log10",
       caption= "Source: Richard Careaga")           +
       theme_ipsum_rc()

sorted_c_rel_freqs_t <- topfeatures(c_dfm, n = nfeat(c_dfm))
sorted_c_rel_freqs_t[c("john", "arnold", "dutch", "dq")]

#textstat_frequency(c_dfm, n = 50)

#ntoken(c_dfm) just gives token by index

sorted_c_rel_freqs_t <- sorted_c_rel_freqs_t / sum(sorted_c_rel_freqs_t) * 100
sorted_c_rel_freqs_t["thanks"]
c_dfm_pct <-dfm_weight(c_dfm) %>% dfm_weight(scheme = "prop")

# freq off by 10 without using 1e3 as divisor but agrees with textstat_frequency
textstat_frequency(c_dfm_pct, n = 25) %>%
  ggplot(aes(x = reorder(feature, -rank), y = frequency/1000)) +
  geom_bar(stat = "identity")                             +
  geom_col(fill = "grey50")                               +
  xlab("Term")                                            +
  scale_y_continuous(labels = scales::percent)            +
  ylab("Percentage frequency")                            +
  coord_flip()                                            +
  labs(title="Term frequency of core Enron corpus as a percentage",
     subtitle= "Frequency as a percentage",
     caption= "Source: Richard Careaga")                  +
  theme_ipsum_rc()

# dispersion plot of a word
# doesn't order by index with either absolute or relative
# textplot_xray(kwic(c_corpus, pattern = "forward"),
#               kwic(c_corpus, pattern = "physical"))
#               scale = "absolute",  sort = TRUE)) +
# ggtitle("Lexical dispersion")

dfm_weight(c_dfm, scheme = "prop") %>%
  textstat_simil(selection = c("forward", "physical"), method = "correlation", margin = "features") %>%
  as.matrix() %>%
  head(2)

dfm_weight(c_dfm, scheme = "prop") %>%
  textstat_simil(selection = c("john", "arnold"), method = "correlation", margin = "features") %>%
  as.matrix() %>%
  head(2)


#start only John Arnold shows up well
cor_data_df <- dfm_weight(c_dfm, scheme = "prop") %>%
  dfm_keep(pattern = c("energy", "forward")) %>%
  convert(to = "data.frame")

# sample 1000 replicates and create data frame
n <- 1000
samples <- data.frame(
  cor_sample = replicate(n, cor(sample(cor_data_df$energy), cor_data_df$forward)),
  id_sample = 1:n
)

# plot distribution of resampled correlations
ggplot(data = samples, aes(x = cor_sample, y = ..density..)) +
  geom_histogram(colour = "black", binwidth = 0.01) +
  geom_density(colour = "red") +
  labs(x = "Correlation Coefficient", y = NULL,
       title = "Histogram of Random Correlation Coefficients with Normal Curve")


# scaled mean word frequency by document

(ntoken(c_corpus) / ntype(c_corpus)) %>%
  scale() %>%
  plot(type = "h", ylab = "Scaled mean word frequency")


mean_word_use_c <- (ntoken(c_corpus) / ntype(c_corpus))
sort(mean_word_use_c, decreasing = TRUE) %>% head()

#TTR lexical diversity

dfm(c_corpus) %>%
  textstat_lexdiv(measure = "TTR") %>%
  head(n = 100)

# hapaxes per document

rowSums(c_dfm == 1) %>% head()

# hapaxes as a proportion

hapax_proportion <- rowSums(c_dfm == 1) / ntoken(c_dfm)
head(hapax_proportion)

# plot of hapax

barplot(hapax_proportion, beside = TRUE, col = "grey", names.arg = seq_len(ndoc(c_dfm)))

# hierarchical clustering - get distances on normalized dfm a mess 7 groups?

c_h <- c_dfm %>% dfm_trim(min_termfreq = 25, min_docfreq = 5)
c_mat <- dfm_weight(c_h, scheme = "prop") %>%
  textstat_dist(method = "euclidean")

# hiarchical clustering the distance object
c_cluster <- hclust(c_mat)

# label with document names
c_cluster$labels <- docnames(c_h)

# plot as a dendrogram too clustered with labels
# plot(c_cluster, xlab = "", sub = "")
#      main = "Euclidean Distance on Normalized Token Frequency")

c_dg <- as.dendrogram(c_cluster)
# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(c_dg, ylab = "Height", nodePar = nodePar, leaflab = "none")
# topicmodel

c_lda <- c_dfm %>% dfm_trim(min_termfreq = 25, min_docfreq = 5)
LDA_fit_3 <- convert(c_lda, to = "topicmodels") %>%
  LDA(k = 3)

# get top five terms per topic
get_terms(LDA_fit_3, 50)

set.seed(2208)
textplot_wordcloud(c_dfm, min_size = 0.5, max_size = 4, min_count = 10,
                   max_words = 750, color = "darkblue",  adjust = 0,
                   random_order = TRUE, random_color = TRUE, rotation = 0.1,
                   labelcolor = "gray20", labelsize = 1.5,
                   labeloffset = 0, fixed_aspect = TRUE, comparison = FALSE)

c_tfidf <- dfm_tfidf(c_dfm, base = 2)
topfeatures(c_tfidf, n = 100)

c_msl <- (textstat_readability(c_corpus, "meanSentenceLength",
                     remove_hyphens = TRUE,
                     min_sentence_length = 1, max_sentence_length = 10000,
                     intermediate = FALSE))
summary(c_msl) # median 7.7 whether or not censor long sentences



# c_topics <- topics(LDA_fit_3) # topics by index
# length(c_topics) # 9938
# # c_enron is only 2349

set.seed(2203)
k <- 3
c_tfidf_1 <- dfm_subset(c_tfidf, s_gcl == 1)
km1_out <- stats::kmeans(c_tfidf_1, centers = k)
km1_out$iter
colnames(km1_out$centers) <- featnames(c_tfidf_1)
for (i in 1:k) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid
  print(head(sort(km1_out$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Graph Cluster 1 Documents Classified: \n") # extract essays classified
  print(docnames(c_tfidf_1)[km1_out$cluster == i])
  cat("\n")
}

set.seed(2203)
k <- 3
c_tfidf_2 <- dfm_subset(c_tfidf, s_gcl == 2)
km2_out <- stats::kmeans(c_tfidf_2, centers = k)
km2_out$iter
colnames(km2_out$centers) <- featnames(c_tfidf_2)
for (i in 1:k) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid
  print(head(sort(km2_out$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Graph Cluster 2 Documents Classif: \n") # extract essays classified
  print(docnames(c_tfidf_2)[km2_out$cluster == i])
  cat("\n")
}

set.seed(2203)
k <- 3
c_tfidf_3 <- dfm_subset(c_tfidf, s_gcl == 3)
km3_out <- stats::kmeans(c_tfidf_3, centers = k)
km3_out$iter
colnames(km3_out$centers) <- featnames(c_tfidf_3)
for (i in 1:k) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid
  print(head(sort(km3_out$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Graph Cluster 3 Documents: \n") # extract essays classified
  print(docnames(c_tfidf_3)[km3_out$cluster == i])
  cat("\n")
}


# no subsetting
set.seed(2203)
k <- 3
km0_out <- stats::kmeans(c_tfidf, centers = k)
km0_out$iter
colnames(km0_out$centers) <- featnames(c_tfidf)
for (i in 1:k) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid
  print(head(sort(km0_out$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Graph Cluster 3 Documents: \n") # extract essays classified
  print(docnames(c_tfidxcf)[km0_out$cluster == i])
  cat("\n")
}

ks_t <- enframe(ks) %>% arrange(-desc(name)) %>% rename(docid = name, k = value)

# kmean Pearson's
kmeans_cor <- cor.test(q_enron$s_gcl, q_enron$k)
q0_enron <- left_join(c_enron, ks_t, by = "docid" )

ks1_t <- enframe(ks) %>% arrange(-desc(name)) %>% rename(docid = name, k = value)
ks2_t <- enframe(ks) %>% arrange(-desc(name)) %>% rename(docid = name, k = value)
ks3_t <- enframe(ks) %>% arrange(-desc(name)) %>% rename(docid = name, k = value)

q1_enron <- c_enron %>% filter(docid %in% ks1_t) %>%
  left_join(c_enron, ks1_t, by = "docid" )
q2_enron <- c_enron %>% filter(docid %in% ks2_t) %>%
  left_join(c_enron, ks2_t, by = "docid")
q3_enron <- c_enron %>% filter(docid %in% ks3_t) %>%
  left_join(c_enron, ks3_t, by = "docid")

kmeans1_cor <- cor.test(q_enron1$s_gcl, q1_enron$k)
kmeans2_cor <- cor.test(q_enron2$s_gcl, q2_enron$k)
kmeans3_cor <- cor.test(q_enron3$s_gcl, q3_enron$k)



