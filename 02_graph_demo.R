# Graph demo data for tempdiscsocialdist data set
# 5.11.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(plyr)

# load sources function

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tempdiscsocialdist_Study1_data.csv"))

# histogram of age
dt$Age <- dt$Q5
ageplot <- ggplot(dt, aes(Age)) + geom_histogram(stat='count') + theme_minimal()

# pie chart of sex
dt$Sex <- dt$Q3
dt$Sex <- mapvalues(dt$Sex, from = c(0,1), to = c('Male','Female'))
sex_frequency <- count(dt$Sex)
sex_frequency$Sex <- sex_frequency$x
sexplot <- ggplot(sex_frequency, aes(x='', y=freq, fill = Sex)) + geom_bar(stat = 'identity', width = 1) + 
  coord_polar("y", start = 0) + theme(axis.text.x = element_blank()) + theme_void()
