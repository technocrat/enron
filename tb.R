# syntax to load Rda from S3
library(tidyverse)
library(tm)
library(rebus)
# con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
# load(con)
# close(con)
# enron <- as_tibble(enron)
# Tue Feb 12 22:26:02 2019 ------------------------------
# function to convert chr string of emails into list of emails
listifer <- function(x){
  x <- strsplit(x, comma)
  x <- x[[1]]
  x <- str_replace_all(x, sqmark,"")
  x <- str_replace_all(x, " ", "")
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
endemic <- "^[.*\\@enron.com]"

# Tue Feb 12 23:05:03 2019 ------------------------------
# cleanse fields with mutliple emails as character strings, add indexer
enron <- enron   %>%  mutate(ccs = gsub(new_line, "", ccs))      %>%
                      mutate(ccs = gsub(OPEN_BRACKET, "", ccs))  %>%
                      mutate(ccs = gsub(CLOSE_BRACKET, "", ccs)) %>%
                      mutate(ccs = str_trim(ccs))                %>%
                      mutate(tos = gsub(new_line, "", tos))      %>%
                      mutate(tos = gsub(OPEN_BRACKET, "", tos))  %>%
                      mutate(tos = gsub(CLOSE_BRACKET, "", tos)) %>%
                      mutate(tos = str_trim(tos))




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
# Tue Feb 12 23:07:22 2019 ------------------------------
# save(all_names, file = "all_names.Rda")

# Wed Feb 13 11:32:36 2019 ------------------------------
# unique user assembly not working, brining tos ccs in as lists
# exported all_names to csv; bash: grep'd ~enron, scrubed anomolous
# addresses such as houston   .ward@enron.com => .ward@enron.com
# eliminated trailing :uid, removed single and double quotes
# sorted | uniq
# reimported to one column tibble 'users'

# Wed Feb 13 11:39:22 2019 ------------------------------
# create unique user identifications, uids
# uid_pool <- seq(1000,nrow(users)+1000,1)
# uids <- enframe(sample(uid_pool, nrow(users), replace = FALSE)) %>%
#   select(-name)                                                 %>%
#   rename(uid = value)                                           %>%
#   mutate(uid = as.integer(uid))

# save(users, file = "users.Rda")

# Wed Feb 13 11:57:38 2019 ------------------------------
# created users
# A tibble: 2,332 x 3
# dex   user                                uid
# <int> <chr>                               <int>
# 1     1 40enron@enron.com                 1915
# 2     2 a..allen@enron.com                1159
# save(users, file = "users.Rda")
#
# Wed Feb 13 13:19:58 2019 ------------------------------
# joined users with enron by senders, one uid per sender
# Wed Feb 13 13:31:57 2019 ------------------------------
# lost 22 irregular mailers houston <.ward@enron.com>"
s_users <- users %>% rename(s_uid = uid, sender = user)
enron <-  left_join(enron, s_users, by = "sender") %>%
          filter(!is.na(s_uid))
# Wed Feb 13 13:51:48 2019 ------------------------------
# save(enron, file = "enron.Rda)


tos <- enron$tos
# Wed Feb 13 13:56:17 2019 ------------------------------
# censored non-enron, b/c no uid
# write_csv; vi

tos <- enron$tos
tos_rep <- map(tos, listifer)
ccs <- enron$ccs
ccs_rep <- map(ccs, listifer)

# Sat Feb 16 15:40:53 2019 ------------------------------
# How user_ids were generated
edge_pool <- seq(10000,20000,1)
edges <- enframe(sample(edge_pool, nrow(all_names), replace = FALSE))	%>%
  select(-name)                                                	    	%>%
  rename(edge = value)                                           		%>%
  mutate(edge = as.integer(edge))

user_pool <- seq(1562,3662,1)
userid <- sample(user_pool, 1561, replace = FALSE) %>%
  select(-name)                                    %>%
  rename(userid = value)                           %>%
  mutate(userid = as.integer(userid))

# Sat Feb 16 15:45:54 2019 ------------------------------
# code to create Vector source column
e <- e %>% mutate(edge_corp = map(payload, VectorSource)

# Sun Feb 17 19:40:37 2019 ------------------------------
library(sna)
pairs <- enron %>% select(t_userid, f_userid) %>% distinct()
m <- as.matrix(pairs, rownames.force = NA)
pals <- pairs %>% mutate(mutual = t_userid %in% f_userid & f_userid %in% t_userid)
penpals <- pals %>% filter(mutual == TRUE)
g_enron <- enron %>%
  mutate(mutual = t_userid %in% penpals$f_userid & f_userid %in% penpals$t_userid) %>%
  filter(mutual == TRUE) %>% select(-mutual)
