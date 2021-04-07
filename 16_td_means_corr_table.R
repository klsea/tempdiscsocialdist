# Create table of temp discount data for tempdiscsocialdist data set
# 4.7.21 KLS

# load required packages
library(here)
library(tidyverse)
library(rstatix)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
}

# convert to long format
d1 <- td_wide_to_long(dt)


# make a bit wide for correlations
d2 <- spread(d1, domain, propChoice)

mean <- t(d2 %>% summarize_at(c('Age', 'Money', 'Health', 'Social'), mean))
sd <- t(d2 %>% summarize_at(c('Age', 'Money', 'Health', 'Social'), sd))
corr <- cor(d2[c(2, 4:6)])
corr <- round(corr, 3)
corr[upper.tri(corr)] <- ""

t1 <- data.frame(round(mean,2), round(sd, 2), corr)
t1$meansd <- paste0(as.character(t1$round.mean..2.), ' (', as.character(t1$round.sd..2.), ')')
t1 <- t1[c(7, 3:6)]

write.csv(t1, here::here('figs', 'temp_disc_means_corr.csv'))
