more recievers than senders and vv
intersect ~ 300 # interesting

# syntax to load Rda from S3
setwd("projects/enron")
library(broom)
library(GGally)
library(here)
library(sna)
library(statnet)
library(tidytext)
library(tidyverse)
library(latentnet)
library(coda)


netmat <- g_enron %>% select(s_uid, r_uid)
net <- network(netmat, matrix.type = "edgelist")
vertices <- network.vertex.names(net)
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

g_enron
load("net.Rda")
net

mcmc.diagnostics(net_p.fit, which.diags = c("cor", "acf", "rafferty"))
dir()
net_p.gof <- gof(net_p.fit)
net_p.gof
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(net_p.gof, plotlogodds=TRUE)
```
net_p.gof <- gof(net_p.fit)
dir()
load("net_p.Rda")
net_p.gof <- gof(net_p.fit)
net.p.gof
net_p.gof
net_p.gof <- gof(net_p.fit)
plot(net_p.gof, plotlogodds=TRUE)

plot(net_p.fit, pie=TRUE) GOLD
plot(net_p.fit, what="cloud", rand.eff="receiver", Z.ref=Z.ref, Z.K.ref=Z.K.ref) SILVER
plot(net_p.fit, what="density", rand.eff="receiver", Z.ref=Z.ref, Z.K.ref=Z.K.ref) BRONZE


> prominence
# A tibble: 16 x 8
    name  node   deg    btw   inf pareto_deg pareto_btw pareto_inf
   <int> <int> <dbl>  <dbl> <dbl>      <dbl>      <dbl>      <dbl>
 1    25  1648    65 14624.  1.32      0.946      0.969      0.996
 2    47  1717   130  3057.  1.11      0.969      0.912      0.962
 3    62  1790    76 19095.  1.21      0.951      0.980      0.978
 4   119  2055    52  8640.  1.20      0.930      0.951      0.975
 5   149  2168    46 14787.  1.09      0.924      0.971      0.960
 6   152  2191   170 15536.  1.29      0.978      0.973      0.987
 7   174  2276   480 19518.  1.32      0.989      0.982      0.993
 8   186  2310   216  9992.  1.31      0.982      0.957      0.989
 9   236  2575  1439  3538.  1.26      1.000      0.917      0.982
10   237  2576   859 77161.  1.31      0.993      0.998      0.991
11   300  2946    38  6258.  1.02      0.917      0.939      0.910
12   347  3146  1009 86520.  1.35      0.998      1.000      1.000
13   349  3152    84 10121.  1.07      0.953      0.962      0.946
14   374  3250    65 34146.  1.25      0.946      0.993      0.980
15   375  3255   429 54562.  1.34      0.987      0.996      0.998
16   404  3421   577 33634.  1.28      0.991      0.991      0.984
> 
> prominent
# A tibble: 4,765 x 9
   recid from     f_userid to      t_userid  edge date   payload       edge_corp
   <int> <chr>       <int> <chr>      <int> <int> <chr>  <chr>         <list>   
 1     5 mike.gr…     1791 john.a…     3146 13910 2001-… "My wife is … <S3: Vec…
 2     6 christo…     1750 john.a…     3146 11877 2001-… "John,\n\nIt… <S3: Vec…
 3     7 ravi.th…     3536 john.a…     3146 11004 2001-… "John, here … <S3: Vec…
 4     8 kimberl…     1693 john.a…     3146 18789 2001-… "I completel… <S3: Vec…
 5    10 john.gr…     3155 john.a…     3146 12952 2001-… "John,\n\nAs… <S3: Vec…
 6    12 christo…     1750 john.a…     3146 16116 2001-… "John,\n\nI … <S3: Vec…
 7    13 kimberl…     1693 john.a…     3146 17718 2001-… I think you … <S3: Vec…
 8    14 sarah.w…     2565 john.a…     3146 13881 2001-… John - you m… <S3: Vec…
 9    15 jeb.lig…     1771 john.a…     3146 19192 2001-… "John,\n\tI … <S3: Vec…
10    16 david.f…     3619 john.a…     3146 14465 2001-… "John,\n\nAt… <S3: Vec…
# 
