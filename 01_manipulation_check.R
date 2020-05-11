# Manipulation check on tempdiscsocialdist data set
# 5.11.20 KLS

# load required packages
library(here)
library(tidyverse)

# load sources function

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tempdiscsocialdist_Study1_data.csv"))

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

# double check the rowsums function
d2 <- dt[c(1, moneystart:moneyend)]
money <- rowSums(d2[2:43])

d3 <- dt[c(1, healthstart:healthend)]
health <- rowSums(d3[2:43])

# start from raw data
raw <- read.csv(here::here('data', 'Time+discounting+across+domains_May+7,+2020_09.54.csv'))
moneystart <- grep('Q80', colnames(raw))
moneyend <- grep('Q121', colnames(raw))
d2 <- raw[c(moneystart:moneyend)]
d2 <- d2[-c(1:2),]

d2[] <- lapply(d2, function(x) as.numeric(as.character(x)))
money <- rowSums(d2[1:42])


healthstart <- grep('Q125', colnames(raw))
healthend <- grep('Q166', colnames(raw))
d3 <- raw[c(healthstart:healthend)]
d3 <- d3[-c(1:2),]

d3[] <- lapply(d3, function(x) as.numeric(as.character(x)))
health <- rowSums(d3[1:42])
