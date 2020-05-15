# Graph temp discount data for tempdiscsocialdist data set
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

d1 <- td_wide_to_long(dt)

# graph
ggplot(d1, aes(Age, propChoice, color = domain, fill = domain)) + geom_point() + geom_smooth(method=lm) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "black", fill = "black") +
  facet_grid(.~ domain) +theme_minimal()


