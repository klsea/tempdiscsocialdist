# Who isn't social distancing?
# 7.16.20 

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))

# pull out question Q328
x <- strsplit(dt$Q328, ',')
y <- unlist(x)
nodistance <- as.numeric(gsub("([0-9]+).*$", "\\1", y))

# histogram
hist(nodistance)
