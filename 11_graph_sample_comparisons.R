# Compare to P&A 2016 data 
# 7.13.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here("scr", "SummarySE.R"))
source(here::here('scr', 'R_rainclouds.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('output', 'all_three_samples.csv'))

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

# raincloud plot
dt$domain <- factor(dt$domain, levels = c("Money", "Social", "Health"))
ggplot(dt, aes(x = domain, y = propImmediate, colour = sample, fill = sample)) + 
  geom_flat_violin(alpha=.3, position = position_nudge(x = .2, y = 0), adjust =2) + 
  geom_point(position = position_jitter(width = .15), size = .25) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  theme_minimal() + theme(legend.position = 'top') + xlab('Reward domain') + 
  ylab('Proportion of Smaller, Sooner Choices') + coord_flip() + facet_wrap(. ~ sample)

 
