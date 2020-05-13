# Manipulation check on tempdiscsocialdist data set
# 5.11.20 KLS

# load required packages
library(here)
library(tidyverse)

# load sources function

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

# start and stop values
socialstart <- grep('Q35', colnames(dt))
socialend <- grep('Q76', colnames(dt))
moneystart <- grep('Q80', colnames(dt))
moneyend <- grep('Q121', colnames(dt))
healthstart <- grep('Q125', colnames(dt))
healthend <- grep('Q166', colnames(dt))

d1 <- dt %>%
  mutate(socialcheck = rowSums(.[socialstart:socialend]), 
         moneycheck = rowSums(.[moneystart:moneyend]), 
         healthcheck = rowSums(.[healthstart:healthend]), 
         socialcompare = socialcheck - SC5, 
         moneycompare = moneycheck - SC3, 
         healthcompare = healthcheck - SC4)

