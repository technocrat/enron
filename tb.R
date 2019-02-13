# syntax to load Rda from S3
library(tidyverse)
library(tm)
library(rebus)
con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
load(con)
close(con)
enron <- as_tibble(enron)
# Tue Feb 12 22:26:02 2019 ------------------------------
# function to convert chr string of emails into list of emails
listifer <- function(x){
  x <- strsplit(x, comma)
  x <- x[[1]]
  x <- str_replace_all(x, sqmark,"")
}

# Tue Feb 12 22:26:51 2019 ------------------------------
# regex pattern
new_line  = "\\\n"
dquote <- escape_special('"')
opunct <- "^[:punct:]"
comma = ","
end_comma = ",$"
bad_start <- "^c.."
end_punct = '[:punct:]$'
sqmark = "'"
bslash = "\\\\"
email = "e-mail "
email_pat <- '([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))'

# Tue Feb 12 23:05:03 2019 ------------------------------
# cleanse fields with mutliple emails as character strings, add indexer
enron <- enron   %>%  mutate(ccs = gsub(new_line, "", ccs))      %>%
                      mutate(ccs = gsub(OPEN_BRACKET, "", ccs))  %>%
                      mutate(ccs = gsub(CLOSE_BRACKET, "", ccs)) %>%
                      mutate(ccs = str_trim(ccs))                %>%
                      mutate(tos = gsub(new_line, "", tos))      %>%
                      mutate(tos = gsub(OPEN_BRACKET, "", tos))  %>%
                      mutate(tos = gsub(CLOSE_BRACKET, "", tos)) %>%
                      mutate(tos = str_trim(tos))                %>%
                      rownames_to_column(., var = 'dex')


# Tue Feb 12 20:32:47 2019 -------------------------------------------
# Convert emailers fields to lists fails: all end up in the list field
# use dex as index? <==============================================

sender <- enron$sender
sender_rep <- map(sender, listifer)
enron <- enron %>% mutate(sender = sender_rep)


tos <- enron$tos
tos_rep <- map(tos, listifer)
enron <- enron %>% mutate(tos = tos_rep)
ccs <- enron$ccs
ccs_rep <- map(ccs, listifer)
enron <- enron %>% mutate(ccs = ccs_rep)

# Tue Feb 12 20:48:36 2019 ------------------------------
# prepare lists of mailers and combine and de-dupe
from_names <- enframe(enron$sender)
to_names <- enframe(enron$tos)
cc_names <- enframe(enron$ccs)

# Tue Feb 12 22:47:27 2019 ------------------------------
# 7,025
all_names <- rbind(from_names, to_names, cc_names) %>% select(-name) %>%
             rename(emailer = value)                                 %>%
             mutate(emailer = as.character(emailer))                 %>%
             unique()


# Tue Feb 12 22:47:50 2019 ------------------------------
# create unique user identifications, uids
uid_pool <- seq(1000,9999,1)
uids <- enframe(sample(uid_pool, nrow(all_names), replace = FALSE)) %>%
        select(-name)                                               %>%
        rename(uid = value)                                         %>%
        mutate(uid = as.integer(uid))


all_names <- all_names %>% mutate(uid = uids$uid)

# Tue Feb 12 23:07:22 2019 ------------------------------
# save(all_names, file = "all_names.Rda")


# Tue Feb 12 23:14:33 2019 ------------------------------
# not working, for one thing b/c more senders than unique emailers
# assign user ids to senders
# See `rlist` https://renkun-ken.github.io/rlist-tutorial/Features/Mapping.html
enron <- enron %>%
  mutate(s_uid = ifelse(sender %in% all_names$emailer, all_names[[1]][dex]), NA)

%>%
  mutate(s_uid = as.list(s_suid))



