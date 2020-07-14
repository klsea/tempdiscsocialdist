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
dt$domain <- factor(dt$domain, levels = c("Money", "Health", "Social"))
dt$sample <- factor(dt$sample, levels = c('Replication', 'Primary', 'Original'))
td_x_sample_rain <- ggplot(dt, aes(x = sample, y = propImmediate, colour = sample, fill = sample)) + 
  geom_flat_violin(alpha=.75, position = position_nudge(x = .2, y = 0), adjust =2) + 
  geom_point(position = position_jitter(width = .15), size = .25) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  theme_minimal() + xlab('Sample') + ggtitle('Reward Domain') +
  scale_fill_manual(values=c("#CC0066", "#990066", "gray")) + 
  scale_colour_manual(values=c("#CC0066", "#990066", "gray")) +
  ylab('Proportion of Smaller, \nSooner Choices') + coord_flip() + 
  theme(legend.position = 'none', plot.title = element_text(size = 20, hjust = 0.5), 
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
        strip.text.x = element_text(size=16), legend.title = element_text(size = 20), 
        legend.text = element_text(size = 16)) + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + facet_wrap(. ~ domain)

png(file = here::here('figs', 'td_x_sample_rain.png'), width = 1000, height = 750)
td_x_sample_rain
dev.off()
 
