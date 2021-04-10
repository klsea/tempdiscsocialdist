# Assumptions Testing
# 4.9.21 KLS

# load required packages
library(here)
library(tidyverse)
library(pastecs)
library(car)

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

# convert to prop score
d1$propChoice <- d1$propChoice / 42

# Assumption of Normality ####
ggplot(d1, aes(propChoice)) + geom_histogram(aes(y=..density..))
stat.desc(d1[3:4], norm = TRUE)

# try square transformation
d1$tpropChoice <- d1$propChoice^2
ggplot(d1, aes(tpropChoice)) + geom_histogram(aes(y=..density..))
stat.desc(d1$tpropChoice, norm = TRUE)

# try cube root transformation
#d1$tpropChoice <- d1$propChoice^(1/3)
# ggplot(d1, aes(tpropChoice)) + geom_histogram(aes(y=..density..))
#stat.desc(d1$tpropChoice, norm = TRUE)

# try log transformation
# d1$tpropChoice <- log(d1$propChoice)
# ggplot(d1, aes(tpropChoice)) + geom_histogram(aes(y=..density..))
# stat.desc(d1$tpropChoice, norm = TRUE)


