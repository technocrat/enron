suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(ggnetwork))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(ggnetwork))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(latentnet))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(pander))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(statnet))
suppressPackageStartupMessages(library(statnet.common))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tidyverse))

# make reproducible

set.seed(2203)

# functions

# verb to exclude items from another list

'%out%' <- Negate ('%in%')

# create graph object

netr <- function(x) {
  net <- network(x, matrix.type = "edgelist")
}

# remove isolates: remove disconnected users

neti <- function(x) {
  delete.vertices(x, isolates(x))
}

# format digits to zero places

comma0 <- function(x) format(x, digits = 0, big.mark = ",")

# format digits to two places

comma2 <- function(x) format(x, digits = 2, big.mark = ",")

# format digits to four places

comma4 <- function(x) format(x, digits = 4, big.mark = ",")

# number of vertices in a network object

census <- function(x) {
  length(network.vertex.names(x))
}

# vertices names in a network object

cohort <- function(x) {
  network.vertex.names(x)
}

# x is a graph object, y is (possibly empty) string

plot_graph <- function(x,y) {
  ggplot(x, aes(x, y, xend = xend, yend = yend))  +
    geom_edges(size = 0.25, color = "gray")       +
    geom_nodes(size = 0.75, color = "steelblue")  +
    labs(title="Graph of reduced Enron corpus",
         subtitle= y,
         caption="Source: Richard Careaga")			  +
    theme_ipsum_rc()                              +
    theme(legend.position = "none")
}

# x is a graph object, y is (possibly empty) string
# color is a vector of length x
plot_graph_w_nodes <- function(x,y) {
  ggplot(x, aes(x, y, xend = xend, yend = yend))  +
    geom_edges(size = 0.25, color = "gray")       +
    geom_nodes(size = 0.75, color = "steelblue")  +
    geom_nodetext(aes(label = vertex.names,
                      color = sts))               +
    scale_colour_gradient(low = "white",
                          high = "black")           +
    labs(title="Graph of reduced Enron corpus",
         subtitle= y,
         caption="Source: Richard Careaga")			  +
    theme_ipsum_rc()		                        	+
    guides(color = FALSE)                         +
    theme_void()
}

# end functions

# email recipient cleanser patterns

faux_nl   	<-	"\\\n"
left_brak	  <-	"\\["
right_brak	<-	"\\]"
quote_s     <-  "'"

# load source data
# con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
# load(con)
# close(con)
#save(enron, file = "data/enron.Rda")
load("data/enron.Rda")

# save for Rmd inline

size_of_enron <- nrow(enron)

# begin data cleansing

# remove spurious characters from recipients field, 'tos'
# g_ indicates that tibble will be primarily used for graph purposes

g_enron <- enron 								                		  %>%
  mutate(tos = str_replace_all(tos, faux_nl, ""))		  %>%
  mutate(tos = str_replace_all(tos, left_brak, "")) 	%>%
  mutate(tos = str_replace_all(tos, right_brak, ""))	%>%
  mutate(tos = str_replace_all(tos, quote_s, ""))

# restrict to single recipients
g_enron <- g_enron %>% filter(tosctn == 1 & ccsctn == 0)
singlets <- nrow(g_enron)
# censor non-enron addresses and reduce fields

g_enron <- g_enron                      %>%
  filter(str_detect(tos, "enron.com"))    %>%
  filter(str_detect(sender, "enron.com")) %>%
  select(sender, tos, date, lastword)     %>%
  rename(recipient = tos, payload = lastword)

# save for Rmd inline

size_of_g_enron_no_external <- nrow(g_enron)

# remove @enron.com from user names

g_enron  <- g_enron                                           %>%
  mutate(sender = str_replace_all(sender, ".enron.com", ""))  %>%
  mutate(recipient = str_replace_all(recipient, ".enron.com", ""))

# censor internal group addresses

excluded_users <- c(
  "all-hou.dl-bus",
  "all.america",
  "all.employees",
  "center.dl_portland",
  "chick.home",
  "clickathome",
  "clickathomepilot3",
  "dl-ga-all-ews",
  "dl-ga-all_enron_worldwide ",
  "dl-ga-all_enron_worldwide1",
  "dl-ga_all_enron_worldwide2",
  "dl-gal-all_enron_north_america2",
  "enron.chairman",
  "enron.gss",
  "executive.committee",
  "expense.report",
  "group.dl-ets",
  "helpdesk.mailman",
  "info",
  "outlook.team",
  "perfmgmt",
  "perfmgmt@ect",
  "portland",
  "portland.desk",
  "portland.shift",
  "sap_security",
  "the.globalist",
  "traders.eol",
  "trading .williams",
  "transportation.parking",
  "undisclosed-recipients"
)

g_enron <- g_enron %>% filter(sender %out% excluded_users)
g_enron <- g_enron %>% filter(recipient %out% excluded_users)

# save for Rmd inline

size_of_g_enron_no_broadcast <- nrow(g_enron)

# censor emails before 1999-12-31 and after 2001-12-02 and sort
# censor empty rows

g_enron <- g_enron            %>%
  filter(date > "1999-12-31") %>%
  filter(date < "2001-12-03") %>%
  filter(length(payload) > 0) %>%
  arrange(-desc(date))

size_of_g_enron_2000 <- nrow(g_enron)

# end data cleansing

# create unique identifiers for users to create graph object

# collect unique user names

sender    <- g_enron %>% select(sender)     %>% distinct()
recipient <- g_enron %>% select(recipient)  %>% distinct()
colnames(sender)    <- "user"
colnames(recipient) <- "user"
users <- bind_rows(sender,recipient)
users <- users %>% distinct()

# create pool of userids

set.seed(2203)
user_pool <- seq(1000,nrow(users)+1001,1)
userid <- enframe(sample(user_pool, nrow(users), replace = FALSE))  %>%
  select(-name)                                                     %>%
  rename(userid = value)                                            %>%
  mutate(userid = as.integer(userid))

userid <- bind_cols(users, userid)

#save(userid, file = "data/userid.Rda")


# for Rmd

unique_users <- nrow(userid)

# rename userid columns to join to g_enron as sender s_uid

colnames(userid) <- c("sender", "s_uid")
g_enron <- left_join(g_enron, userid)

# again for recipient r_uid

colnames(userid) <- c("recipient", "r_uid")
g_enron <- left_join(g_enron, userid)

gclean_enron <- g_enron

# "the reduced Enron corpus"

# remove unneed objects from namespace

rm(con, enron, g_enron, faux_nl, left_brak, quote_s, recipient, right_brak, sender,
   user_pool, userid, users)

# begin exploratory analysis

time_series <- gclean_enron %>% group_by(date) %>% count() %>% ungroup()

# for Rmd summary

distinct_dates <- nrow(time_series)
total_msg  <- sum(time_series$n)
ts_plot <- time_series %>% ggplot(., aes(x = date, y = log10(n))) +
  geom_point(size = 0.75) + geom_smooth()                         +
  xlab("Month")                                                   +
  ylab("Number (log10) of emails by month")                       +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")    +
  labs(title="Time series chart of reduced Enron corpus",
       subtitle= "January 1, 2000 - December 2, 2001",
       caption="Source: Richard Careaga")			  				          +
  theme_minimal()										                        		  +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# create initial graph

net0 <- gclean_enron %>% select(s_uid, r_uid) %>% netr(.)

# for Rmd

net0_vertex_number <- census(net0)
net0_members       <- cohort(net0)

# net0 plot, including isolates

net0_plot <- plot_graph(net0, "Graph of Enron corpus with isolates")

# net1, removing isolates

net1 <- net0 %>% neti(.)

# for Rmd

net1_vertex_number <- census(net1)
net1_members       <- cohort(net1)

# net1 plot, excluding isolates

net1_plot <- plot_graph(net1, "Graph of Enron corpus without isolates")

# for Rmd

# High transitivity
#gt.cg <- gtrans(net1, measure = "strong", use.adjacency = FALSE)
#save(gt.cg, file = "data/gt.cg.Rda")
load("data/gt.cg.Rda")

# Assess measures of centrality on de-isolated graph net1

# calculate measures of centrality
deg   <- degree(net1, rescale = TRUE)
ldctr <- loadcent(net1, rescale = TRUE)
sts   <- stresscent(net1, rescale = TRUE)

# rejected measures of centrality

# btw <- betweenness(net1) # redundant with ldctr
# inf <- infocent(net1) all 1.206801e-13
# cls <- closeness(net1) all 0
# evc <- evcent(net1, use.eigen = TRUE) net1 not symmetrical
# bon <- bonpow(net1) Lapack routine dgesv: system is exactly singular:
# flo <- flowbet(net1) ran 10 minutes without finishing
# flo <- flowbet(net1, cmode = "normflow") ditto
# harg <- graphcent(net1) all 0

vertices    <- net1 %v% 'vertex.names'
vertices    <- enframe(vertices) %>% rename(userid = value) %>% select(-name)
prominence  <- bind_cols(vertices = vertices, deg = deg,
                         ldctr = ldctr, sts = sts)

# prominence with centrality metrics for Rmd

prom_with_ctrs <- nrow(prominence)

# For Rmd

deg_ldctr <- cor.test(deg,ldctr)  # 0.6786184
deg_sts   <- cor.test(deg,sts)    # 0.8656096
ldctr_sts <- cor.test(ldctr,sts)  # 0.7176663


top25_d <- prominence %>% arrange(desc(deg)) %>% head(25) %>% select(userid)
top25_l <- prominence %>% arrange(desc(ldctr)) %>% head(25) %>% select(userid)
top25_s <- prominence %>% arrange(desc(sts)) %>% head(25) %>% select(userid)

top100_s <- prominence %>% arrange(desc(sts)) %>% head(100) %>% select(userid)

# union of the userids with top 25 scores on three measures of centrality 12 +

well_positioned <- union(union(top25_d,top25_l), top25_s)

# core Enron high-centrality exchanges

# degree centrality

d_enron <- gclean_enron %>% filter(s_uid %in% top25_d$userid &
                                   r_uid %in% top25_d$userid)
net_d <- d_enron %>%  select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net_d_plot <- plot_graph(net_d, "Graph of Enron corpus after degree filter")

# load centrality

l_enron <- gclean_enron %>% filter(s_uid %in% top25_l$userid &
                                     r_uid %in% top25_l$userid)
net_l <- l_enron %>%  select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net_l_plot <- plot_graph(net_l, "Graph of Enron corpus after loadcent filter")

# stress centrality

s_enron <- gclean_enron %>% filter(s_uid %in% top25_s$userid &
                                     r_uid %in% top25_s$userid)
net_s <- s_enron %>%  select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net_s_plot <- plot_graph(net_s, "Graph of Enron corpus after stresscent filter")

rm(d_enron, l_enron, s_enron, net_d, net_l, net_s)

# both sender and receiver central using stress centrality

c_enron <- gclean_enron %>% filter(s_uid %in% top100_s$userid &
                                     r_uid %in% top100_s$userid)
net_c <- c_enron %>% netr(.) %>% neti(.)
net_c_plot <- plot_graph(net_c, "Graph of Enron corpus, both sender and receiver")
#c.fit <- ergmm(net_c ~ euclidean(d=2, G=3)+rreceiver,
#               control=ergmm.control(store.burnin=TRUE), seed = 2203)
#save(c.fit, file = "data/c.fit.Rda")
load("data/c.fit.Rda")
plot(c.fit,pie=TRUE,rand.eff="receiver")
mcmc.diagnostics(c.fit)
summary(c.fit)

# Goodness of fit

c.gof <- gof(c.fit)
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(c.gof)
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(c.gof, plotlogodds=TRUE)

# for Rmd

size_of_core <- nrow(c_enron)

#Map userids to clusters

c.gc <-  enframe(c.fit$mkl$Z.K) %>%
  select(-name)         %>%
  rename(gcl = value)

c.gc <- bind_cols(top100_s, c.gc)

c.gc <- c.gc %>% rename(s_uid = userid, s_gcl = gcl)

c_enron <- left_join(c_enron, c.gc)

c.gc <- c.gc %>% rename(r_uid = s_uid, r_gcl = s_gcl)

c_enron <- left_join(c_enron, c.gc)

# n_enron for "network"
n_enron <- c_enron

#save(n_enron, file = "data/n_enron.Rda")
#load("data/n_enron.Rda")

# subset by _glc

g1 <- n_enron %>% filter(s_gcl == 1)
g2 <- n_enron %>% filter(s_gcl == 2)
g3 <- n_enron %>% filter(s_gcl == 3)

net1 <- g1 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net2 <- g2 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net3 <- g3 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)

# For Rmd

# To highlight only the vertices with high degrees of
# centrality using the stresscent measure, the zero-valued
# vertices must be positive; 2 is used due to the subsequent
# log transformation

m <-  as.matrix(stresscent(net1))
m <- m + 2
sts <- round(log(m),1)
net1%v%"sts" <- sts[,1]
clust1_plot <- plot_graph_w_nodes(net1, "Cluster 1")

m <-  as.matrix(stresscent(net2))
m <- m + 2
sts <- round(log(m),1)
net2%v%"sts" <- sts[,1]
clust2_plot <- plot_graph_w_nodes(net2, "Cluster 2")

m <-  as.matrix(stresscent(net3))
m <- m + 2
sts <- round(log(m),1)
net3%v%"sts" <- sts[,1]
clust3_plot <- plot_graph_w_nodes(net3, "Cluster 3")

# short NLP analysis

# Extract payloads and tokenize

t_all <- gclean_enron                                     %>%
  select(payload)                                         %>%
  rename(text = payload)                                  %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))  %>%
  unnest_tokens(word, text, to_lower = TRUE)              %>%
  anti_join(stop_words)                                   %>%
  distinct(word)

t1 <- g1                                                  %>%
  select(payload)                                         %>%
  rename(text = payload)                                  %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))  %>%
  unnest_tokens(word, text, to_lower = TRUE)              %>%
  anti_join(stop_words)                                   %>%
  distinct(word)

t2 <- g2                                                  %>%
  select(payload)                                         %>%
  rename(text = payload)                                  %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))  %>%
  unnest_tokens(word, text, to_lower = TRUE)              %>%
  anti_join(stop_words)                                   %>%
  distinct(word)

t3 <- g3                                                  %>%
  select(payload)                                         %>%
  mutate(text = str_replace_all(text, "[:punct:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:blank:]", " "))  %>%
  mutate(text = str_replace_all(text, "[:digit:]", " "))  %>%
  unnest_tokens(word, text, to_lower = TRUE)              %>%
  anti_join(stop_words)                                   %>%
  distinct(word)

# counts of total words
t1_words <- nrow(t1)
t2_words <- nrow(t2)
t3_words <- nrow(t3)

tot_words <- t1_words + t2_words + t3_words

# percentages in each cluster of total words
# and percentage of words unique to each cluster

t1_pct <- t1_words/tot_words
t2t3 <- union(t2,t3)
t2t3_pct <- nrow(t2t3)/tot_words
t1_unique <- setdiff(t1,union(t2,t3))
t1_unique_pct <- nrow(t1_unique)/tot_words
t2_unique <- setdiff(t2,union(t1,t3))
t2_unique_pct <- nrow(t2_unique)/tot_words
t3_unique <- setdiff(t1,union(t1,t2))
t3_unique_pct <- nrow(t3_unique)/tot_words

# End of program


