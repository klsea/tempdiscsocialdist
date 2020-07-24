# Power analysis for tempdiscsocialdist study

# load required packages
library(here)
library(tidyverse)
library(hausekeep)
library(WebPower)

# load source functions

# set hard-coded variables

# load data
pag <- read.csv(here::here("data", "all_sublevel_data.csv"))
pag <- pag[which(pag$domainReward == 'social' & pag$domainTask == 'Time'),]

# correlation between age and discounting
r = cor(pag$Age, pag$propImmediate)
f = es(r=r)$f
f2 = f^2

# power analysis - two predictors (age + age2), linear regression
n <- wp.regression(p1 = 2, f2 = f2, power = 0.8)$n
