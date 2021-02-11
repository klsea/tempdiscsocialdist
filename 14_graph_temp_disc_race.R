# Graph temp discount data for tempdiscsocialdist data set
# 5.13.20 KLS updated 6.26.20 with sample #

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(plyr)
library(Hmisc)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "SummarySE.R"))
source(here::here("scr", "CIcorr.R"))
source(here::here("scr", "CImean.R"))

# set hard-coded variables

# load data
if( sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)
  plottitle <- 'Primary Sample'
  plotcolor <- '#990066'
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'), stringsAsFactors=FALSE)
  plottitle <- 'Replication Sample'
  plotcolor <- '#CC0066'
}

d1 <- td_wide_to_long(dt)
d1$Race <- factor(d1$Race, levels = c('Black/African American', 'Hispanic/Latinx', 'White/Caucasian'))

# graph td x age
td_x_age <- ggplot(d1, aes(Age, propChoice, colour = Race, fill = Race)) + geom_point(alpha=.75) + geom_smooth(method=lm) + 
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), alpha=.3) +
  facet_grid(.~ domain) + theme_minimal() + theme(legend.position="top") + 
  ylab('Proportion of Smaller, \nSooner Choices') + ggtitle(plottitle) + 
  theme(plot.title = element_text(face="bold", size = 20), 
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
        strip.text.x = element_text(size=16))

td_x_age
plotname <- paste0('td_x_age_race_study_', sample, '.png')
png(file = here::here('figs', plotname), width = 1000, height = 425)
td_x_age
dev.off()

# svg file for presentations
#plotname2 <- paste0('td_x_age_study_', sample, '.svg')
#ggsave(file = here::here('figs', plotname2), plot = td_x_age, width = 8, height = 3.4)

# graph td means


