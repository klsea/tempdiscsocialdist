# Correlations for social dist variables in  tempdiscsocialdist data set
# 5.22.20 KLS

# load required packages
library(here)
library(tidyverse)
library(Hmisc)

# load source functions
source(here::here("scr", "CIcorr.R"))
source(here::here("scr", "corrTableCI.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

# add social/health difference score
dt$discountdiff <- dt$SC5 - dt$SC4

# correlation table for social distancing variables
# formula removes all people with NA values, so analyze Q16, Q20 & Q22 separately
                            
table1 <- corrTableCI(dt[c(6, 11:15, 24, 39:41, 186:192)])

table2 <- corrTableCI(dt[c(6, 17:22, 189:192)])

table3 <- corrTableCI(dt[c(6, 26:31, 189:192)])

table4 <- corrTableCI(dt[c(6, 33:37, 189:192)])

