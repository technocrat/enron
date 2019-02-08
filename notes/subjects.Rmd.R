# tok.R use tidytext to tokenize pdf pages

library(dplyr)
library(pdftools)
library(stringr)
library(tidytext)

data(stop_words)

exec_summary <- pdf_text("sources/ExecutiveSummaryAug2003Staff.pdf")

exec_df <- enframe(exec_summary)

exec_tok <- exec_df %>% unnest_tokens(word,exec_summary) # can also do n-grams

exec_tok <- exec_tok %>% anti_join(stop_words)

exec_tok_cnt <- exec_tok %>% count(word, sort = TRUE) %>%
                 filter (n > 5)

ggplot(data = exec_tok_cnt, aes(x = word, y = n)) +
geom_col() + xlab("NULL") + coord_flip()

exec_tok_cnt %>% print(n = Inf)

# A tibble: 39 x 2
word              n
<chr>         <int>
  1 staff            29
2 prices           26
3 enron            22
4 data             21
5 commission       17
6 gas              17
7 price            17
8 california       16
9 natural          16
10 spot             15
11 refund           12
12 strategies       12
13 trading          12
14 2                11
15 market           11
16 electric         10
17 information      10
18 2002              9
19 docket            9
20 including         9
21 000               8
22 investigation     8
23 pa02              8
24 proceeding        8
25 13                7
26 delivery          7
27 public            7
28 received          7
29 reported          7
30 specific          7
31 0468              6# A tibble: 39 x 2
word              n
<chr>         <int>

  target_keywords <- c("price","data","gas","california","spot","refund","strategy","market","electric","delivery","receive","ferc","report","trade")
