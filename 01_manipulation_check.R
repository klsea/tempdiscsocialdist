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

d2 <- dt[c(1, 16)] %>% separate(Q15, as.character(1:12))
colnames(d2) <- c('ID', paste0('C', colnames(d2[2:13])))

d2 %>% 
  mutate_at(vars(paste0("C", 1:12)), 
            funs(recode(., '1' = '-3', '2' = '0', '3' = '1',  '4' = '1', '5' = '1', 
                        '6' = '1', '7' = '1', '8' = '1', '9' = '1', '10' = '1', '11' = '1', '12' = '0'))) %>%
  mutate_at(vars(paste0("C", 1:12)), as.numeric) %>%
  mutate(sum = rowSums(.[2:13], na.rm = TRUE))




