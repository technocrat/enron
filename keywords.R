# tok.R use tidytext to tokenize pdf pages

library(pdftools)
library(tidytext)
library(tidyverse)
library(magrittr)

data(stop_words)

exec_summary <- pdf_text("sources/ExecutiveSummaryAug2002Staff.pdf")

exec_df <- enframe(exec_summary)

exec_tok <- exec_df %>% unnest_tokens(word,value) %>% select(-name)

exec_tok <- exec_tok %>% anti_join(stop_words)

exec_tok <- exec_tok                  %>%
                filter(str_detect(word, "[:alpha:]"))

tok_count <- exec_tok %>% count(word) %>%
                filter (n > 4)        %>%
                arrange(desc(n))

ggplot(data = tok_count, aes(x = word, y = n)) +
geom_col() + xlab("NULL") + coord_flip()

exec_tok_cnt %>% print(n = Inf)

target_keywords <- c("california", "data", "delivery", "electric", "ferc", "gas", "market", "osec", "price", "receive", "refund", "report", "spot", "strategy", "trade")

