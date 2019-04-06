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
# not run
# con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
# load(con)
# close(con)
# enron %>% as_tibble(enron)
# save(enron, file = "enron.Rda")

# extract original content (part added by an email)
#load("enron.Rda")
e_text <- enron %>% rename(text = lastword) %>% select(text)

# patterns for "textified" newlines, returns, tabs and Notes hypenation

pattern1  <- "\\\n"
pattern2  <- "\\\t"
pattern3  <- "\\\r"
pattern4  <- "="

# remove patterns1-4,
e_text <- e_text %>% mutate(text = str_replace_all(text, pattern1, " ")) %>%
                     mutate(text = str_replace_all(text, pattern2, " ")) %>%
                     mutate(text = str_replace_all(text, pattern3, " ")) %>%
                     mutate(text = str_replace_all(text, pattern4, " "))

# remove numbers, urls, punctuation and lowercase

e_text <- e_text %>% unnest_tokens(word, text, to_lower = TRUE,
                      strip_punct = TRUE, strip_numeric = TRUE)

# load tidytext package of parts of speech to ignore
data(stop_words)

# reconsider
# # identify possibly interesting words to bypass stop word filter
#
# exempt_stopwords <- enframe(c("against", "all", "allow", "allows", "always",
#                               "awfully", "beforehand", "behind", "below",
#                               "better", "big", "can't", "cannot", "cant",
#                               "case", "cases", "downwards", "except", "gives",
#                               "good", "great", "highest", "hopefully",
#                               "immediate", "might", "necessary", "not",
#                               "nowhere", "numbers", "otherwise", "point",
#                               "serious", "seriously", "state", "states",
#                               "unfortunately", "value", "worked", "working",
#                               "zero"))
#
# # create custom stopword list
#
# colnames(exempt_stopwords) <- c("line", "word")
# stop_words <- anti_join(stop_words, exempt_stopwords)

add_stops <- c("ect", "hou", "enronxgate", "enron", "corp", "company",
               "enron.com")
lexicon <- c("user", "user", "user", "user", "user", "user", "user")
new_stops <- cbind(add_stops, lexicon)
colnames(new_stops) <- c("word", "lexicon")
new_stops <- as_tibble(new_stops)
stop_words <- bind_rows(stop_words, new_stops)

# remove stop_words from e_text

e_text <- e_text %>% anti_join(stop_words)

# for Rmd
top_100_words <- e_text %>% count(word, sort = TRUE) %>% print(n = 100)

# checkpoint
#save(e_text, file = "e_text.Rda")
