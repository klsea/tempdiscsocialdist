# Analyze temp discount data for tempdiscsocialdist data set
# 5.15.20 KLS updated 6.26.20 with sample #

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

#rm(dt, d1, fu, m1, td_wide_to_long)

# repeated-measures ANCOVA with race
d1$Race <- factor(dt$Q6)
d1$Q6 <- NULL
levels(d1$Race)[levels(d1$Race) == 1] <- 'White/Caucasian'
levels(d1$Race)[levels(d1$Race) == 2] <- 'Black/African American'
levels(d1$Race)[levels(d1$Race) == 4] <- 'Hapanic/Latinx'

m2 <- anova_test(data = d1, dv = propChoice, wid = ID, within = domain, between = Race, covariate = Age)
m2
