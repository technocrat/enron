# tok.R use tidytext to tokenize pdf pages

library(dplyr)
library(pdftools)
library(stringr)
library(tidytext)
library(ggplot2)

data(stop_words)

exec_summary <- pdf_text("/sources/ExecutiveSummaryAug2003Staff.pdf")

exec_df <- enframe(exec_summary)

exec_tok <- exec_df %>% unnest_tokens(word,exec_summary) # can also do n-grams

exec_tok <- exec_tok %>% anti_join(stop_words)

exec_tok_cnt <- exec_tok %>% count(word, sort = TRUE) %>%
                 filter (n > 5)

ggplot(data = exec_tok_cnt, aes(x = word, y = n)) +
geom_col() + xlab("NULL") + coord_flip()

exec_tok_cnt %>% print(n = Inf)

target_keywords <- c("price","data","gas","california","spot","refund","strategy","market","electric","delivery","receive","ferc","report","trade")
