# syntax to load Rda from S3
setwd("projects/enron")
library(broom)
library(cody)
library(GGally)
library(here)
library(statnet)
library(latentnet)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
#library(rebus)
#library(ergm.ego)
#library(UserNetR)
# Tue Feb 12 22:26:02 2019 ------------------------------
# function to convert chr string of emails into list of emails
listifer <- function(x){
  x <- strsplit(x, comma)
  x <- x[[1]]
  x <- str_replace_all(x, sqmark,"")
  x <- str_replace_all(x, " ", "")
}

# Tue Feb 12 22:26:51 2019 ------------------------------
# regex patterns
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

# Sun Feb 24 15:03:20 2019 ------------------------------
userids <- g_enron %>% select(f_userid, t_userid)
f <- g_enron %>% distinct(f_userid)
t <- g_enron %>% distinct(t_userid)
userids <- bind_rows(f,t)
userids <- userids %>% distinct()


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
pairs <- g_enron %>% select(t_userid, f_userid) %>% distinct()
m <- as.matrix(pairs, rownames.force = NA)
pals <- pairs %>% mutate(mutual = t_userid %in% f_userid & f_userid %in% t_userid)
penpals <- pals %>% filter(mutual == TRUE)
g_enron <- g_enron %>%
  mutate(mutual = t_userid %in% penpals$f_userid & f_userid %in% penpals$t_userid) %>%
  filter(mutual == TRUE) %>% select(-mutual)
g_enron <- as_tibble(g_enron)
# remove memos to self
g_enron <- g_enron %>% filter(f_userid != t_userid)
# remove "empty" messages
g_enron <- g_enron %>% filter(nchar(payload) > 10) # filters 692 blanks
# save(g_enron, file = "g_enron")

library(statnet)
library(UserNetR)


netmat <- g_enron %>% select(t_userid, f_userid)
net <- network(netmat, matrix.type = "edgelist") # need to filter empty payloads
vertices <- network.vertex.names(net)

cd_weak <- component.dist(net, connected = "weak")
net_weak <- length(cd_weak$csize)
cd_strong <- component.dist(net ,connected="strong")
net_strong <- length(cd_strong$csize)

clus <- gtrans(net, mode = "graph")

delete.vertices(net, isolates(net))

df.prom <- data.frame(
  deg = degree(net),
  btw = betweenness(net),
  inf = infocent(net)
)

cor(df.prom)
# deg       btw       inf
# deg 1.0000000 0.6006736 0.3655786
# btw 0.6006736 1.0000000 0.4212161
# inf 0.3655786 0.4212161 1.0000000
#


df.prom <- data.frame(
  deg = degree(net),
  btw = betweenness(net),
  inf = infocent(net)
)


netmat <- g_enron %>% select(t_userid, f_userid)
net <- network(netmat, matrix.type = "edgelist")
vertices <- network.vertex.names(net)



netmat <- g_enron %>% select(t_userid, f_userid)
net <- network(netmat, matrix.type = "edgelist")
vertices <- network.vertex.names(net)
net_raw <- network(netmat, matrix.type = "edgelist")


delete.vertices(net, isolates(net))


df.prom <- data.frame(
  deg = degree(net),
  btw = betweenness(net),
  inf = infocent(net)
)
vertices <- net %v% 'vertex.names'
vertices <- enframe(vertices) %>% rename(node = value)
prominence <- as_tibble(df.prom)
prominence <- bind_cols(prominence, vertices)
prominence <- prominence %>% select(name, node, deg, btw, inf)
prominence <- prominence %>% mutate(pareto_deg = cume_dist(deg))	%>%
  mutate(pareto_btw = cume_dist(btw))	%>%
  mutate(pareto_inf = cume_dist(inf))
prominence <- prominence %>% filter(pareto_deg >= 0.9)				%>%
  filter(pareto_btw >= 0.9)				%>%
  filter(pareto_inf >= 0.9)
prominence %>% select(deg, btw, inf) %>% cor()
# three measures are not independent
# deg       btw       inf
# deg 1.0000000 0.4670800 0.4799712
# btw 0.4670800 1.0000000 0.5591332
# inf 0.4799712 0.5591332 1.0000000
#
#
centrals <- prominence$node
# save(centrals, file = "centrals.Rda")



netmat <- prominent %>% select(t_userid, f_userid)
net_p <- network(netmat, matrix.type = "edgelist")
vertices <- network.vertex.names(net_p)
ggnet(net_p, size = 0.1, alpha = 0.84)
delete.vertices(net_p, isolates(net_p))
ggnet(net_p, size = 0.1, alpha = 0.84)

# Sun Mar  3 15:58:10 2019 ------------------------------
# saved net_p_fit object in error
# net_p.fit <- ergmm(net_p ~euclidean(d = 2, G = 3), verbose = TRUE)
# summary(net_p.fit)
# plot(net_p.fit)
#
# plot(net_p.fit, pie = TRUE, vertex.cex = 2.5)
#
plot(net_p.fit, what = "pmean")
net_p.sim <- simulate(net_p.fit)
net_p.par <- attr(net_p.sim, "ergmm.par")
net_p.fit.gof <- gof(net_p.fit)
summary(net_p.par.gof)

plot(net_p.fit, pie=TRUE)
plot(net_p.fit, what="cloud", rand.eff="receiver", Z.ref=Z.ref, Z.K.ref=Z.K.ref)
plot(net_p.fit, what="density", rand.eff="receiver", Z.ref=Z.ref, Z.K.ref=Z.K.ref)

net_p.gof <- gof(net_p.fit)
net_p.gof
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(net_p.gof, plotlogodds=TRUE)

net_p_vertices <- network.vertex.names(net_p)
clusters_p <- net_p.fit$start$Z.K
cluster <- enframe(clusters_p) %>% rename(n_cluster = value)
net_p_vertices <- enframe(net_p_vertices) %>% rename(f_userid = value)
cluster_p <- left_join(net_p_vertices, cluster)
n_enron <- prominent %>% left_join(cluster_p, by = "f_userid") %>% select(-name) %>% rename(f_cluster = n_cluster)
cluster_p1 <- cluster_p %>% rename(t_userid = f_userid)
n_enron <- n_enron %>% left_join(cluster_p1, by = "t_userid") %>% select(-name) %>% rename(t_cluster = n_cluster)


