# Clean tempdiscsocialdist data set
# 5.8.20 KLS updated 6.26.20 with sample #
# Written with R version 3.6.1 (2019-07-05)

# load required packages
library(here)
library(stringr)

# load sources function

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here('data','Time+discounting+across+domains_May+7,+2020_09.54.csv'))  
} else { 
  dt <- read.csv(here::here('data','Time+discounting+across+domains-Replication_June 26, 2020_11.43.csv'))
}

# add id column to data
dt$ID <- c(0,0,seq(1,nrow(dt)-2, 1))
dt$ID <- str_pad(dt$ID, 3, pad="0")

# clean out qualtrics columns
dt <- dt[-c(1:grep('UserLanguage', colnames(dt)))] # get rid of garbage at the front
dt <- dt[c(ncol(dt), 1:(ncol(dt)-1))] # moves ID column to the front
dt <- dt[-c(grep('opp', colnames(dt)):ncol(dt))] # get rid of garbage at the back

# create data dictionary
dd <- t(dt[1,])
dd <- cbind(rownames(dd), data.frame(dd,row.names = NULL))
colnames(dd) <- c('Variable', 'Question')

# finish cleaning data file
dt <- dt[-c(1:2),]

# recode reverse-coded items (2 items were reverse-coded in Qualtrics)
dt$Q101 <- as.numeric(as.character(dt$Q101))
dt$Q101 <- (dt$Q101 - 1) * - 1 # reverse - 0 become 1 and 1 becomes 0

dt$Q146 <- as.numeric(as.character(dt$Q146))
dt$Q146 <- (dt$Q146 - 1) * - 1 # reverse - 0 become 1 and 1 becomes 0

# Save 
if (sample == 1) {
  write.csv(dd, here::here("data", "tdsd_s1_data_dictionary.csv"), row.names = FALSE)
  write.csv(dt, here::here("data", "tdsd_s1_data.csv"), row.names = FALSE)
} else {
  write.csv(dd, here::here("data", "tdsd_s2_data_dictionary.csv"), row.names = FALSE)
  write.csv(dt, here::here("data", "tdsd_s2_data.csv"), row.names = FALSE)
}