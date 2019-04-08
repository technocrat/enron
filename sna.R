suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(ggnetwork))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(ggnetwork))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(latentnet))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(statnet))
suppressPackageStartupMessages(library(statnet.common))
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
    theme_ipsum_rc()									            +
    theme_void()
}

# email recipient cleanser patterns

faux_nl   	<-	"\\\n"
left_brak	  <-	"\\["
right_brak	<-	"\\]"
quote_s     <-  "'"


# load source data
con <- url("https://s3-us-west-2.amazonaws.com/dslabs.nostromo/enron.Rda")
load(con)
close(con)
#save(enron, file = "enron.Rda")
load("enron.Rda")

# save for Rmd inline

size_of_enron <- nrow(enron)

# begin data cleansing

# remove spurious characters from recipients field, 'tos'
# g_ indicate that tibble will be primarily used for graph purposes

g_enron <- enron 								                		  %>%
  mutate(tos = str_replace_all(tos, faux_nl, ""))		  %>%
  mutate(tos = str_replace_all(tos, left_brak, "")) 	%>%
  mutate(tos = str_replace_all(tos, right_brak, ""))	%>%
  mutate(tos = str_replace_all(tos, quote_s, ""))


# restrict to single recipients
g_enron <- g_enron %>% filter(tosctn == 1 & ccsctn == 0)

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

excluded_users <- c("all-hou.dl-bus", "all.america", "all.employees",
                    "center.dl_portland", "chick.home", "clickathome",
                    "clickathomepilot3", "dl-ga-all-ews",
                    "dl-ga-all_enron_worldwide ", "dl-ga-all_enron_worldwide1",
                    "dl-ga_all_enron_worldwide2",
                    "dl-gal-all_enron_north_america2", "enron.chairman",
                    "enron.gss", "executive.committee", "expense.report",
                    "group.dl-ets", "helpdesk.mailman", "info", "outlook.team",
                    "perfmgmt", "perfmgmt@ect", "portland", "portland.desk",
                    "portland.shift", "sap_security", "the.globalist",
                    "traders.eol", "trading .williams",
                    "transportation.parking", "undisclosed-recipients")

g_enron <- g_enron %>% filter(sender %out% excluded_users)
g_enron <- g_enron %>% filter(recipient %out% excluded_users)

# save for Rmd inline

size_of_g_enron_no_broadcast <- nrow(g_enron)

# censor emails before 1999-12-31 and after 2001-12-02

g_enron <- g_enron            %>%
  filter(date > "1999-12-31") %>%
  filter(date < "2001-12-03")

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

# for Rmd

unique_users <- nrow(userid)

#save(userid, file = "userid.Rda")

# rename userid columns to join to g_enron as sender s_uid

colnames(userid) <- c("sender", "s_uid")
g_enron <- left_join(g_enron, userid)

# again for recipient r_uid

colnames(userid) <- c("recipient", "r_uid")
g_enron <- left_join(g_enron, userid)

# for inter-session convenience

# save(g_enron, file = "g_enron.Rda") # "the reduced Enron corpus"

# remove unneed objects from namespace

rm(con, enron, faux_nl, left_brak, quote_s, recipient, right_brak, sender,
   user_pool, userid, users)

# begin exploratory analysis
load("g_enron.Rda")
time_series <- g_enron %>% group_by(date) %>% count() %>% ungroup()

# for Rmd summary

distinct_dates <- nrow(time_series)
total_msg  <- sum(time_series$n)
ts_plot <- time_series %>% ggplot(., aes(x = date, y = log10(n)))      					    +
  geom_point(size = 0.75) + geom_smooth()                       +
  xlab("Month")                                                 +
  ylab("Number (log10) of email by month")                      +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")  +
  labs(title="Time series chart of reduced Enron corpus",
       subtitle= "January 1, 2000 - December 2, 2001",
       caption="Source: Richard Careaga")			  				        +
  theme_minimal()										                        		+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# create initial graph

net0 <- g_enron %>% select(s_uid, r_uid) %>% netr(.)

# for Rmd

net0_vertex_number <- census(net0)
net0_members       <- cohort(net0)

# net0 plot, including isolates

net0_plot <- plot_graph(net0, "Graph of Enron corpus before cleansing")

# net1, removing isolates

net1 <- net0 %>% neti(.)

# for Rmd

net1_vertex_number <- census(net1)
net1_members       <- cohort(net1)

# net0 plot, including isolates

net1_plot <- plot_graph(net1, "Graph of Enron corpus after cleansing")

# Assess measures of centrality on de-isolated graph net1

# High transitivity
gt.cg <- gtrans(net1, measure = "strong", use.adjacency = FALSE)

deg   <- degree(net1, rescale = TRUE)
ldctr <- loadcent(net1, rescale = TRUE)
sts   <- stresscent(net1, rescale = TRUE)

# btw <- betweenness(net1) # redundant with ldctr
# inf <- infocent(net1) all 1.206801e-13
# cls <- closeness(net1) all 0
# evc <- evcent(net1, use.eigen = TRUE) net1 not symmetrical
# bon <- bonpow(net1) Lapack routine dgesv: system is exactly singular:
# flo <- flowbet(net1) ran 10 minutes without finished
# flo <- flowbet(net1, cmode = "normflow") ditto
# harg <- graphcent(net1) all 0

vertices    <- net1 %v% 'vertex.names'
vertices    <- enframe(vertices) %>% rename(userid = value) %>% select(-name)
prominence  <- bind_cols(vertices = vertices, deg = deg,
                         ldctr = ldctr, sts = sts)

# size of prominence with centrality metrics for RMD

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

c_enron <- g_enron %>% filter(s_uid %in% top100_s$userid &
                              r_uid %in% top100_s$userid)

# for Rmd

size_of_core <- nrow(c_enron)

net2 <- c_enron %>% select(s_uid, r_uid) %>% netr(.)
net2_plot <- plot_graph(net2, "Graph of Enron corpus after centrality filter")
net2 <- neti(net2)
net2_plot <- plot_graph(net2, "Graph of Enron corpus centrality without isolates")

# 100 vertices and d=2 gives only one cluster

c0.fit <- ergmm(net2 ~ euclidean(d=2))
mcmc.diagnostics(c0.fit)
plot(c0.fit,labels=TRUE,rand.eff="receiver")

c1.fit <- ergmm(net2 ~ euclidean(d=2, G=3)+rreceiver,
                control=ergmm.control(store.burnin=TRUE), seed = 2203)
# save(c1.fit, file = "c1.fit")
# charts are interactively triggered
mcmc.diagnostics(c1.fit)
plot(c1.fit,pie=TRUE,rand.eff="receiver")
plot(c1.fit,what="pmean",rand.eff="receiver")
plot(c1.fit,what="cloud",rand.eff="receiver")
plot(c1.fit,what="density",rand.eff="receiver")
plot(c1.fit,what=5,rand.eff="receiver")
summary(c1.fit)

# Goodnet of fit

c1.gof <- gof(c1.fit)
c1.gof.plot <- plot(c1.gof, plotlogodds=TRUE)

#Map userids to clusters

net2.gc <-  enframe(c1.fit$mkl$Z.K)             %>%
            select(-name)                       %>%
            rename(gcl = value)

net2.gc <- bind_cols(top100_s, net2.gc)

net2.gc <- net2.gc %>% rename(s_uid = userid, s_gcl = gcl)


c_enron <- left_join(c_enron, net2.gc)

net2.gc <- net2.gc %>% rename(r_uid = s_uid, r_gcl = s_gcl)

c_enron <- left_join(c_enron, net2.gc)

#save(c_enron, file = "c_enron.Rda")

# subset by _glc

glc1 <- c_enron %>% filter(s_gcl == 1)
glc2 <- c_enron %>% filter(s_gcl == 2)
glc3 <- c_enron %>% filter(s_gcl == 3)

# save(glc1, file = "glc1.Rda")
# save(glc2, file = "glc2.Rda")
# save(glc3, file = "glc3.Rda")

net_glc1 <- glc1 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net_glc2 <- glc2 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)
net_glc3 <- glc3 %>% select(s_uid, r_uid) %>% netr(.) %>% neti(.)

# For Rmd

plot_graph(net_glc1, "Cluster 1")
plot_graph(net_glc2, "Cluster 2")
plot_graph(net_glc3, "Cluster 3")
