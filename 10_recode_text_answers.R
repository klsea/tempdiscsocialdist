# Recode text answer tempdiscsocialdist data set
# 5.22.20 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

# pull out question Q17
 d1 <- dt[c(1, 23)]

 d1 %>%
   mutate(Q17 = recode(Q17, 'No' = '0', 'NO' = '0', 'no' = '0', 'None' = '0', 'none' = '0', 'nothing' = '0', 'Nothing' = '0', 'None ' = '0', 
                       'N/A' = '0', 'Walking' = '1', 'Exercise' = '1'
                       ))
 