# Mon Mar 11 21:58:16 2019 ------------------------------
# wolak_text scrub
library(tidyverse)
library(stringr)
load("wolak_text.Rda")


# patterns identified by visual inspection of pdf conversion
pattern1  <- "MSC Report"
pattern2  <- "October 18, 1999"
pattern3  <- "Page \\d+ of 148"
pattern4  <- "[:punct:]"
pattern5  <- "\\\n"
pattern6  <- "[:blank:]+"
pattern7  <- "[:digit:]"
pattern8  <-  "S pike"
pattern9  <-  "S ystem"
pattern10 <-  "\\n"
pattern11 <-  "PG E"
pattern12 <-  "s ufficiency"
pattern13 <-  "M illio n"
pattern14 <-  "thank Keith Casey"
pattern15 <-  "expertise"
pattern16 <-  "quantitites"
pattern17 <-  "ngo"
pattern18 <-  "considerable"
pattern19 <-  "preparation"
pattern20 <-  "chairman"
pattern21 <-  "S ufficiency"

milled <- str_replace_all(wolak_text, pattern1, "")

milled <- str_replace_all(milled, pattern2, "")

milled <- str_replace_all(milled, pattern3, "")

milled <- str_replace_all(milled, pattern4, " ")

milled <- str_replace_all(milled, pattern5, " ")

milled <- str_replace_all(milled, pattern6, " ")

milled <- str_replace_all(milled, pattern7, "")

milled <- str_replace_all(milled, pattern8, "Spike")

milled <- str_replace_all(milled, pattern9, "System")

milled <- str_replace_all(milled, pattern10, " ")

milled <- str_replace_all(milled, pattern11, "PG&E")

milled <- str_replace_all(milled, pattern12, "sufficiency")

milled <- str_replace_all(milled, pattern13, "Million")

milled <- str_replace_all(milled, pattern14, "")

milled <- str_replace_all(milled, pattern15, "")

milled <- str_replace_all(milled, pattern16, "quantities")

milled <- str_replace_all(milled, pattern17, "")

milled <- str_replace_all(milled, pattern18, "")

milled <- str_replace_all(milled, pattern19, "")

milled <- str_replace_all(milled, pattern20, "")

milled <- str_replace_all(milled, pattern12, "Sufficiency")


# conversation was by page, reulting in vector of characters
# this concatenates them

paste(milled, sep = '', collapse = '')

wolak_clean <- milled

#save(wolak_clean, file = "wolak_clean.Rda")
