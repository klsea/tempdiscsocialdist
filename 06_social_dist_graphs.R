# Graph covid data for tempdiscsocialdist data set
# 5.19.20 KLS

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

# histogram
ggplot(dt, aes(Q10)) + geom_histogram() + ggtitle("Covid Positive")
ggplot(dt, aes(Q11)) + geom_histogram() + ggtitle("Covid previously")
ggplot(dt, aes(Q12)) + geom_histogram() + ggtitle("Covid symptoms")

# age graphs
ggplot(dt, aes(Q5, Q10)) + geom_point() + geom_smooth(method = lm) + xlab("Covid Positive")
ggplot(dt, aes(Q5, Q11)) + geom_point() + geom_smooth(method = lm)
ggplot(dt, aes(Q5, Q12)) + geom_point() + geom_smooth(method = lm)


