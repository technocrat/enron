---
title: "Notes on LDA"
author: "Richard Careaga"
date: "April 7, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# # alternative with wordfreq >= 20
# # 3-topic overlaps
# g_vs  <- VectorSource(g_25$word)
# g25_corpus <- VCorpus(g_vs)
# g25_dtm <- DocumentTermMatrix(g25_corpus) # 99% sparse
# 
# # remove empty rows
# ui = unique(g25_dtm$i)
# g25_dtm = g25_dtm[ui,]
# g25_lda <- LDA(g25_dtm, k = 3, control =  list(seed = 2203))
# g25_topics <- tidy(g25_lda, metrix = "beta")

too little discrimination
