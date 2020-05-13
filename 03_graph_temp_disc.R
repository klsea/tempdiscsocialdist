# Graph temp discount data for tempdiscsocialdist data set
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load sources function

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

# convert from wide to long format
d1 <- gather(dt[c(1, 6, grep('SC3', colnames(dt)):grep('SC5', colnames(dt)))], domain, propChoice, SC3:SC5)
d1$Age <- d1$Q5; d1$Q5 <- NULL

d1$domain[which(d1$domain == 'SC3')] <- 'Money'
d1$domain[which(d1$domain == 'SC4')] <- 'Health'
d1$domain[which(d1$domain == 'SC5')] <- 'Social'

d1$domain <- factor(d1$domain, levels = c('Money', 'Health', 'Social'))

# graph
ggplot(d1, aes(Age, propChoice, color = domain, fill = domain)) + geom_point() + geom_smooth(method=lm) + 
  facet_grid(.~ domain) +theme_minimal()
