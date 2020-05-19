# Graph temp discount data for tempdiscsocialdist data set
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "SummarySE.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

d1 <- td_wide_to_long(dt)

# graph td x age
td_x_age <- ggplot(d1, aes(Age, propChoice)) + geom_point(aes(color = domain)) + geom_smooth(method=lm, color = "black") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = domain, fill = domain )) +
  facet_grid(.~ domain) +theme_minimal() + theme(legend.position="none") + 
  ylab('Proportion of Smaller, Sooner Choices')
td_x_age

# graph td means
td_means <- ggplot(d1, aes(domain, propChoice)) + geom_violin(trim = FALSE, aes(fill = domain)) + 
 geom_boxplot(width=0.1, fill = "white") + theme_minimal() + theme(legend.position = "none")
td_means
