# Compare to P&A 2016 data 
# 7.7.20 updated 4.5.21 with matching data

#Comment out rows 53-54 if running all data 

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(rstatix)
library(ggplot2)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))

# set hard-coded variables

# load data
s1 <- read.csv(here::here("data", "tdsd_s1_data.csv"))
s2 <- read.csv(here::here("data", "tdsd_s2_data.csv"))
pag <- read.csv(here::here("data", "all_sublevel_data.csv"))
match <- read.csv(here::here("output", "matching_data_table.csv"))
  
# calculate propImmediate for new samples
s1 <- td_wide_to_long(s1)
s1$propImmediate <- s1$propChoice / 42
s1$sample <- 'Primary'
s1$ID <- 1000 + s1$ID
s1 <- s1[c(1, 7, 4, 2, 6)]

s2 <- td_wide_to_long(s2)
s2$propImmediate <- s2$propChoice / 42
s2$sample <- 'Replication'
s2$ID <- 2000 + s2$ID
s2 <- s2[c(1, 7, 4, 2, 6)]

# pull out relevant columns for PAG sample
pag$sample <- 'Original'
pag$domain <- pag$domainReward
pag$ID <- pag$partNum
pag <- pag[which(pag$domainTask == 'Time'),]
pag <- pag[c(25, 23, 19, 24, 5)]
pag$domain[which(pag$domain == 'money')] <- 'Money'
pag$domain[which(pag$domain == 'health')] <- 'Health'
pag$domain[which(pag$domain == 'social')] <- 'Social'

# concatenate all data sets and save ####
dt <- rbind(s1, s2)
dt <- rbind(dt, pag)
rm(s1, s2, pag)
write.csv(dt, here::here('output', 'all_three_samples.csv'))

# alternate code for matching analysis ####
#dt <- dt[which(dt$ID %in% match$partNum),]
#dt$sample <- mapvalues(dt$sample, from = c("Primary", "Replication"), to = c("Matching", "Matching"))
#write.csv(dt, here::here('output', 'matching_samples.csv'))

# 3 Sample (Original, Sample 1, Sample 2) x 3 Reward Domain (Money, Health, Social) ####
# repeated-measures ANCOVA
m1 <- anova_test(data = dt, dv = propImmediate, wid = ID, between = sample, within = domain, covariate = Age, effect.size = "pes")
m1

# follow-up tests
fu <- dt %>%
  pairwise_t_test(
    propImmediate ~ sample, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
fu

fu_money <- dt[which(dt$domain == 'Money'),] %>%
  pairwise_t_test(
    propImmediate ~ sample, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
fu_money

fu_health <- dt[which(dt$domain == 'Health'),] %>%
  pairwise_t_test(
    propImmediate ~ sample, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
fu_health

fu_social <- dt[which(dt$domain == 'Social'),] %>%
  pairwise_t_test(
    propImmediate ~ sample, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
fu_social




  