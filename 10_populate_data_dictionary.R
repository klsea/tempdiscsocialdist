# Populate data dictionary
# 5.12.20 KLS

# load required packages
library(here)
library(tidyverse)

# load sources function

# set hard-coded variables

# load data
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'))

# add task name to question field
dd$Question <- as.character(dd$Question)

socialstart <- grep('Q35', dd$Variable)
socialend <- grep('Q76', dd$Variable)
dd$Question[socialstart:socialend] <- paste0('Social: ', dd$Question[socialstart:socialend])

moneystart <- grep('Q80', dd$Variable)
moneyend <- grep('Q121', dd$Variable)
dd$Question[moneystart:moneyend] <- paste0('Money: ', dd$Question[moneystart:moneyend])

healthstart <- grep('Q125', dd$Variable)
healthend <- grep('Q166', dd$Variable)
dd$Question[healthstart:healthend] <- paste0('Health: ', dd$Question[healthstart:healthend])

catch <- c(grep('Q77', dd$Variable), grep('Q78', dd$Variable), grep('Q122', dd$Variable), 
           grep('123', dd$Variable), grep('Q167', dd$Variable), grep('168', dd$Variable))
dd$Question[catch] <- paste0('Catch Trial: ', dd$Question[catch])

# Create and populate allowed_values in data dictionary
dd[,'allowed_values'] <- NA # Create allowed values dd$allowed_values

## demo data
dd$allowed_values[grep("ID", dd$Variable)] <- '001-233'
dd$allowed_values[grep('Q3', dd$Variable)] <- '0 = Male, 1 = Female'
dd$allowed_values[grep('Q4', dd$Variable)] <- '5 = Middle School, 12 = High School Diploma, 14 = Some College, 
16 = Bachelors Degree, 18 = Masters Degree, 21 = Doctoral Degree'
dd$allowed_values[age] <- '18-86'
dd$allowed_values[grep('Q6', dd$Variable)] <- '1 = White/Caucasian, 2 = Black/African American,  4 = Hispanic/Latino' 
dd$allowed_values[grep('Q7', dd$Variable)] <- '5 = less than $10,000, 15 = $10,000-$19,999, 
25 = $20,000-$29,999, 35 = $30,000-$39,999, 45 = $40,000-$49,999, 55 = $50,000-$59,999, 65 = $60,000-$69,999, 
75 = $70,000-$79,999, 85 = $80,000-$89,999, 95 = $90,000-$99,999, 105 = $100,000-$109,999, 115 = $110,000-$119,999, 
125 = $120,000-$129,999, 135 = $130,000-$139,999, 145 = $140,000-$149,999, 155 = $150,000 or more'

dd$allowed_values[grep('Q8', dd$Variable)] <- 'TBD' ### fix this line!

## covid data

## catch trials
dd$allowed_values[grep('Q77', dd$Variable)] <- 0
dd$allowed_values[grep('Q78', dd$Variable)] <- 1
dd$allowed_values[grep('Q122', dd$Variable)] <- 0
dd$allowed_values[grep('Q123', dd$Variable)] <- 1
dd$allowed_values[grep('Q167', dd$Variable)] <- 0
dd$allowed_values[grep('Q168', dd$Variable)] <- 1

# td data
dd$allowed_values[c(socialstart:socialend, moneystart:moneyend, healthstart:healthend)] <- '1 = smaller sooner, 0 = larger later'
dd$allowed_values[c(grep('SC3', dd$Variable), grep('SC4', dd$Variable), grep('SC5', dd$Variable))] <- '0-42'

