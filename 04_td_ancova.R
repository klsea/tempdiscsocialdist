# Analyze temp discount data for tempdiscsocialdist data set
# 5.15.20 KLS

# load required packages
library(here)
library(tidyverse)
library(rstatix)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

# convert to long format
d1 <- td_wide_to_long(dt)

# repeated-measures ANCOVA
m1 <- anova_test(data = d1, dv = propChoice, wid = ID, within = domain, covariate = Age)
m1

# follow-up tests
fu <- d1 %>%
  pairwise_t_test(
    propChoice ~ domain, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
fu
