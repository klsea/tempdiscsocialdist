# Correlations for social dist variables in  tempdiscsocialdist data set
# 5.22.20 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here::here("scr", "CIcorr.R"))
source(here::here("scr", "corrTableCI.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

# correlation table for social distancing variables

table <- corrTableCI(dt[c(6, 11:15, 17:22, 186)])
