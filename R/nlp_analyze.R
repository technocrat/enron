library(tibble)
library(ggplot2)
library(quanteda)
library(topicmodels)

# bring in core Enron corpus, tagged with clusters
load("data/c_enron.Rda")
load("data/stop_words.Rda")
# provide field names for corpus and order by date
c_enron <- c_enron %>%  rownames_to_column(var = "docid") %>%
                        rename(text = payload)            %>%
                        arrange(-desc(date))

c_corpus <- corpus(c_enron, docid_field = "docid", text_field = "text")

c_dfm <- dfm(c_corpus, remove_symbols = TRUE, remove_hyphens = FALSE,
             remove_punct = TRUE, remove_url = TRUE,
             include_docvars = TRUE, remove = stopwords("en"))
