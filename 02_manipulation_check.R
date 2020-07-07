# Manipulation check on tempdiscsocialdist data set
# 5.11.20 KLS and 5.22.20 updated 6.26.20 with sample #

# load required packages
library(here)
library(tidyverse)

# load sources function

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
} else{
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
}

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

d2 <- d2 %>% 
  mutate_at(vars(paste0("C", 1:12)), 
            funs(recode(., '1' = '-3', '2' = '0', '3' = '1',  '4' = '1', '5' = '1', 
                        '6' = '1', '7' = '1', '8' = '1', '9' = '1', '10' = '1', '11' = '1', '12' = '0'))) %>%
  mutate_at(vars(paste0("C", 1:12)), as.numeric) %>%
  mutate(socialbehcheck = rowSums(.[2:13], na.rm = TRUE))

d3 <- dt[c(1, 25)]  %>% separate(Q19, as.character(1:12)) 
colnames(d3) <- c('ID', paste0('C', colnames(d3[2:13])))

d3 <- d3 %>% 
  mutate_at(vars(paste0("C", 1:12)), 
            funs(recode(., '1' = '-3', '2' = '0', '3' = '1',  '4' = '1', '5' = '1', 
                        '6' = '1', '7' = '1', '8' = '1', '9' = '1', '10' = '1', '11' = '1', '12' = '0'))) %>%
  mutate_at(vars(paste0("C", 1:12)), as.numeric) %>%
  mutate(visitorbehcheck = rowSums(.[2:13], na.rm = TRUE))

d4 <- dt[c(1, 42)]  %>% separate(Q26, as.character(1:20)) 
colnames(d4) <- c('ID', paste0('C', colnames(d4[2:21])))

d4 <- d4 %>% 
  mutate_at(vars(paste0("C", 1:20)), 
            funs(recode(., '1' = '1', '2' = '1', '3' = '1',  '4' = '1', '5' = '1', '6' = '1', '7' = '1', '8' = '1', 
                        '9' = '1', '10' = '1', '11' = '1', '12' = '1', '13' = '1', '14' = '1', '15' = '1', '16' = '1', 
                        '17' = '1', '18' = '1', '19' = '1', '20' = '0'))) %>% 
  mutate_at(vars(paste0("C", 1:20)), as.numeric) %>%
  mutate(mentalhealthcheck = rowSums(.[2:21], na.rm = TRUE))

d5 <- merge(d2[c(1, 14)], d3[c(1,14)])
d5 <- merge(d5, d4[c(1, 22)])
d5 <- merge(d1, d5)

# check all calculated variables

compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# social behavior
sum(compareNA(d5$SC0, d5$socialbehcheck))

# mental health
sum(compareNA(d5$SC1, d5$mentalhealthcheck))

# visitor behavior
sum(compareNA(d5$SC2, d5$visitorbehcheck))

# clean up 
#rm(d1, d2, d3, d4, d5, dt, compareNA, healthend, healthstart, moneyend, moneystart, socialend, socialstart)
