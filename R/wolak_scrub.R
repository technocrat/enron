# Mon Mar 11 21:58:16 2019 ------------------------------
# wolak_text scrub
setwd("projects/enron")
library(tidyverse)
load("wolak_text.Rda")


# paterns to write

# MSC Report
# October 18, 1999
# Page 1 of 148
# \n # as a literal
# runs of whitespace

# [:digit:]
# [:punct:]
# (A/S) = ancillary services
# S pike => Spike
# S ystem => System
# HA_RU_PRC (NP15)\n
# HA_RD_PRC (NP15)\n
# HA_SPN_PRC (NP15)\n
# HA_NSPN_PRC (NP15)
# [74]
