# Populate data dictionary
# 5.13.20 KLS

# load required packages
library(here)
library(stringr)
library(tidyverse)
library(stringr)

# load sources function

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", 'tdsd_s1_data.csv'))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'))

#create and populate variable name column
dd[,'variable_name'] <- NA

dd$variable_name[1:9] <- c("ID", "NeuroHealth", "PsychHealth", "Sex", "Education", "Age", "Race", "Income", "Employment")
dd$variable_name[11:13] <- c("covid_pos", "covid_prev", "covid_symp")


#add names to td
socialstart <- grep('Q35', dd$Variable)
socialend <- grep('Q76', dd$Variable)
moneystart <- grep('Q80', dd$Variable)
moneyend <- grep('Q121', dd$Variable)
healthstart <- grep('Q125', dd$Variable)
healthend <- grep('Q166', dd$Variable)
catch <- c(grep('Q77', dd$Variable), grep('Q78', dd$Variable), grep('Q122', dd$Variable), 
           grep('123', dd$Variable), grep('Q167', dd$Variable), grep('168', dd$Variable))

dd$variable_name[socialstart:socialend] <- paste0(rep('Social_', 42), str_pad(seq(1,42,1), 2, pad="0"))
dd$variable_name[moneystart:moneyend] <- paste0(rep('Money_', 42), str_pad(seq(1,42,1), 2, pad="0"))
dd$variable_name[healthstart:healthend] <- paste0(rep('health_', 42), str_pad(seq(1,42,1), 2, pad="0"))
dd$variable_name[catch] <- paste0(rep('catch_',6), seq(1,6,1))
dd$variable_name[grep('SC3', dd$Variable)] <- 'propSSmoney'
dd$variable_name[grep('SC4', dd$Variable)] <- 'propSShealth'
dd$variable_name[grep('SC5', dd$Variable)] <- 'propSSsocial'

# add data type to data dictionary
dd$type <- sapply(dt, class)

# Create and populate allowed_values in data dictionary
dd[,'allowed_values'] <- NA 

# demo data
dd$allowed_values[grep('ID'), dd$Variable] <- '001-233'
dd$allowed_values[grep('Q3', dd$Variable)] <- '0 = Male, 1 = Female'

# finish this later

# covid question
dd$allowed_values[11:12] <- '1 = No, 2 = Yes have symptoms, but not confirmed with test, 3 = Yes, confirmed with test'
dd$allowed_values[13] <- '1 = No, 2 = Some symptoms, 3 = All symptoms'

# catch trials
dd$allowed_values[grep('Q77', dd$Variable)] <- 0
dd$allowed_values[grep('Q78', dd$Variable)] <- 1
dd$allowed_values[grep('Q122', dd$Variable)] <- 0
dd$allowed_values[grep('Q123', dd$Variable)] <- 1
dd$allowed_values[grep('Q167', dd$Variable)] <- 0
dd$allowed_values[grep('Q168', dd$Variable)] <- 1

# td data
dd$allowed_values[c(socialstart:socialend, moneystart:moneyend, healthstart:healthend)] <- '0 = larger later, 1 = smaller sooner'


