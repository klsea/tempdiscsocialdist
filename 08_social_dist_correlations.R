# Correlations for social dist variables in  tempdiscsocialdist data set
# 5.22.20 KLS updated 6.27.20 with sample #

# load required packages
library(here)
library(tidyverse)
library(Hmisc)

# load source functions
source(here::here("scr", "CIcorr.R"))
source(here::here("scr", "corrTableCI.R"))

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'), stringsAsFactors=FALSE)
}

# add social/health discounting difference score
dt$discountdiff <- dt$SC5 - dt$SC4

# add social/health valuedifference score
dt$valuediff <- dt$Q169_2 - dt$Q169_3

# correlation table for social distancing variables
# formula removes all people with NA values, so analyze Q16, Q20 & Q22 separately

if (sample == 1) {
  table1 <- corrTableCI(dt[c(6, 11:15, 24, 39:41, 181:183, 186:191, 193:194)])
  table2 <- corrTableCI(dt[c(6, 17:22, 181:183, 189:191, 193:194)])
  table3 <- corrTableCI(dt[c(6, 26:31, 181:183, 189:191, 193:194)])
  table4 <- corrTableCI(dt[c(6, 33:37, 181:183, 189:191, 193:194)])
} else {
  table1 <- corrTableCI(dt[c(6, 11:15, 24, 39:41, 181:183, 187:192, 194:195)])
  table2 <- corrTableCI(dt[c(6, 17:22, 181:183, 190:192, 194:195)])
  table3 <- corrTableCI(dt[c(6, 26:31, 181:183, 190:192, 194:195)])
  table4 <- corrTableCI(dt[c(6, 33:37, 181:183, 190:192, 194:195)])
}

#rm(dd, dt, table1, table2, table3, table4)
