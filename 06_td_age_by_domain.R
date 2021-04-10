# Analyze temp discount data for tempdiscsocialdist data set
# 5.15.20 KLS updated 6.27.20 with sample #

# load required packages
library(here)
library(tidyverse)
library(sjPlot)
library(boot)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "residuals.R"))

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
}

# convert to long format
d1 <- td_wide_to_long(dt)

# Scale Age
d1$Age <- scale(d1$Age)
d1$Age2 <- d1$Age^2
d1$propChoice <- scale(d1$propChoice) # scale so standardized coefficients in output

# regressions ####
models <- function(data) {
  # this fits age and age2 regression models to subset of td data
  m1 <- lm(propChoice ~ Age, data = data)
  m2 <- lm(propChoice ~ Age2, data = data)
  m3 <- lm(propChoice ~ Age + Age2, data = data)
  m4 <- lm(propChoice ~ Age + Race, data = data)
  m5 <- lm(propChoice ~ Age * Race, data = data)
  #m5 <- lm(propChoice ~ Age + Age2 + Race, data = data)
  return(list(m1, m2, m3, m4, m5))
}

# Money
d2 <- d1[which(d1$domain == "Money"),] # money data only
money_models <- models(d2)
summary(money_models[[1]])
summary(money_models[[2]])
summary(money_models[[3]])
summary(money_models[[4]])
summary(money_models[[5]])
anova(money_models[[1]], money_models[[4]])  # add main effect of race 
anova(money_models[[1]], money_models[[5]])  # add main effect of race + interaction with age 
# no effect of age or race

# Health
d4 <- d1[which(d1$domain == "Health"),] # health data only
health_models <- models(d4)
summary(health_models[[1]])
summary(health_models[[2]])
summary(health_models[[3]])
summary(health_models[[4]])
summary(health_models[[5]])
anova(health_models[[1]], health_models[[3]]) 
anova(health_models[[1]], health_models[[4]])  # add main effect of race
anova(health_models[[1]], health_models[[5]]) # add main effect of race + interaction with age 
#M1 wins!

# Social
d3 <- d1[which(d1$domain == "Social"),] # social data only
social_models <- models(d3)
summary(social_models[[1]])
summary(social_models[[2]])
summary(social_models[[3]])
summary(social_models[[4]])
summary(social_models[[5]])
anova(social_models[[1]], social_models[[3]])
anova(social_models[[2]], social_models[[3]])
anova(social_models[[1]], social_models[[4]]) # add main effect of race
anova(social_models[[1]], social_models[[5]]) # add main effect of race + interaction with age 
# M3 wins

# make table ####
tablename <- paste0('regressions_study_', sample, '.html')
tab_model(money_models[[1]], money_models[[3]], health_models[[1]], health_models[[3]], social_models[[1]], social_models[[3]], 
          dv.labels = c("Money 1", "Money 2", "Health 1", "Health 2", "Social 1", "Social 2"), 
          file = here::here('figs', tablename))

#supplement ####
tablename <- paste0('regressions_race_study_', sample, '.html')
tab_model(money_models[[4]], money_models[[5]], health_models[[4]], health_models[[5]], social_models[[4]], social_models[[5]], 
          dv.labels = c("Money 3", "Money 4", "Health 3", "Health 4", "Social 3", "Social 4"), 
          file = here::here('figs', tablename))

#rm(d1, d2, d3, d4, dt, health_models, money_models, social_models, models, td_wide_to_long)

# test assumptions about residuals ####
plot(money_models[[1]])
plot(health_models[[1]])
plot(social_models[[3]])

# bootstrap regressions
bootreg <- function(formula, data, indices) {
  d <- data [indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootMoney <- boot(statistic = bootreg, formula = propChoice ~ Age, data = d2, R = 2000)
bootMoney
boot.ci(bootMoney, type = 'bca', index = 1) # intercept
boot.ci(bootMoney, type = 'bca', index = 2) # age

bootHealth <- boot(statistic = bootreg, formula = propChoice ~ Age, data = d4, R = 2000)
bootHealth
boot.ci(bootHealth, type = 'bca', index = 1) # intercept
boot.ci(bootHealth, type = 'bca', index = 2) # age

bootSocial <- boot(statistic = bootreg, formula = propChoice ~ Age + Age2, data = d3, R = 2000)
bootSocial
boot.ci(bootSocial, type = 'bca', index = 1) # intercept
boot.ci(bootSocial, type = 'bca', index = 2) # age
boot.ci(bootSocial, type = 'bca', index = 3) # age^2
