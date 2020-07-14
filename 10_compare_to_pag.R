# Compare to P&A 2016 data 
# 7.7.20

# load required packages
library(here)
library(tidyverse)
library(rstatix)
library(ggplot2)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "SummarySE.R"))

# set hard-coded variables

# load data
s1 <- read.csv(here::here("data", "tdsd_s1_data.csv"))
s2 <- read.csv(here::here("data", "tdsd_s2_data.csv"))
pag <- read.csv(here::here("data", "all_sublevel_data.csv"))

# calculate propImmediate for new samples
s1 <- td_wide_to_long(s1)
s1$propImmediate <- s1$propChoice / 42
s1$sample <- 'Primary'
s1$ID <- 1000 + s1$ID
s1 <- s1[c(1, 6, 4, 2, 5)]

s2 <- td_wide_to_long(s2)
s2$propImmediate <- s2$propChoice / 42
s2$sample <- 'Replication'
s2$ID <- 2000 + s2$ID
s2 <- s2[c(1, 6, 4, 2, 5)]

# pull out relevant columns for PAG sample
pag$sample <- 'Original'
pag$domain <- pag$domainReward
pag$ID <- pag$partNum
pag <- pag[which(pag$domainTask == 'Time'),]
pag <- pag[c(25, 23, 19, 24, 5)]
pag$domain[which(pag$domain == 'money')] <- 'Money'
pag$domain[which(pag$domain == 'health')] <- 'Health'
pag$domain[which(pag$domain == 'social')] <- 'Social'

# concatenate all data sets
dt <- rbind(s1, s2)
dt <- rbind(dt, pag)
rm(s1, s2, pag)

# 3 Sample (Original, Sample 1, Sample 2) x 3 Reward Domain (Money, Health, Social) 
# repeated-measures ANCOVA
m1 <- anova_test(data = dt, dv = propImmediate, wid = ID, between = sample, within = domain, covariate = Age)
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

# graph means
d1 <- summarySE(dt, 'propImmediate', groupvars = c('sample', 'domain'))

td_x_sample <- ggplot(d1, aes(x = domain, y = propImmediate, colour = sample, fill = sample)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = propImmediate - se, ymax = propImmediate + se), width = .2, position = position_dodge(.9)) + 
  scale_fill_brewer(palette="Dark2") + scale_colour_brewer(palette="Dark2") + 
  theme_minimal() + theme(legend.position = 'top', plot.title = element_text(face="bold", size = 20), 
      axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), 
      axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
      strip.text.x = element_text(size=16), legend.title = element_text(size = 20), 
      legend.text = element_text(size = 16)) + xlab('Reward domain') + 
  ylab('Proportion of Smaller, Sooner Choices')

png(file = here::here('figs', 'td_x_sample.png'), width = 500, height = 500)
td_x_sample
dev.off()
       