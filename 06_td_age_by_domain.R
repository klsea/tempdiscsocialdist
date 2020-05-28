# Analyze temp discount data for tempdiscsocialdist data set
# 5.15.20 KLS

# load required packages
library(here)
library(tidyverse)
library(sjPlot)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

# convert to long format
d1 <- td_wide_to_long(dt)

# Scale Age
d1$Age <- scale(d1$Age)
d1$Age2 <- d1$Age^2
d1$propChoice <- scale(d1$propChoice) # scale so standardized coefficients in output

# regressions
models <- function(data) {
  # this fits age and age2 regression models to subset of td data
  m1 <- lm(propChoice ~ Age, data = data)
  m2 <- lm(propChoice ~ Age2, data = data)
  m3 <- lm(propChoice ~ Age + Age2, data = data)
  return(list(m1, m2, m3))
}

# Money
d2 <- d1[which(d1$domain == "Money"),] # money data only
money_models <- models(d2)
summary(money_models[[1]])
summary(money_models[[2]])
summary(money_models[[3]])
# no effect of age

# Health
d4 <- d1[which(d1$domain == "Health"),] # health data only
health_models <- models(d4)
summary(health_models[[1]])
summary(health_models[[2]])
summary(health_models[[3]])
anova(health_models[[1]], health_models[[3]])
#M1 wins!

# Social
d3 <- d1[which(d1$domain == "Social"),] # social data only
social_models <- models(d3)
summary(social_models[[1]])
summary(social_models[[2]])
summary(social_models[[3]])
anova(social_models[[1]], social_models[[3]])
anova(health_models[[2]], health_models[[3]])
# M3 wins

tab_model(money_models[[1]], money_models[[3]], health_models[[1]], health_models[[3]], social_models[[1]], social_models[[3]], 
          dv.labels = c("Money 1", "Money 2", "Health 1", "Health 2", "Social 1", "Social 2"))
