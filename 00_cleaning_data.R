# Clean tempdiscsocialdist data set
# 5.8.20 KLS

# load required packages
library(here)
library(stringr)

# load sources function

# set hard-coded variables
  
# load data
dt <- read.csv(here::here('data','Time+discounting+across+domains_May+7,+2020_09.54.csv'))

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

# refactor all variables
factor_cols <- vapply(dt, is.factor, logical(1))
dt[factor_cols] <- lapply(dt[factor_cols], factor)
str(dt)

# change variable types for demo questions
age <- grep('Q5', colnames(dt))
dt[age] <- sapply(dt[age], as.character)
dt[age] <- sapply(dt[age], as.integer)

# change variable types for covid questions

# change variable types for td questions
start_td <- grep('Q35', colnames(dt)) 
end_td <- grep('Q168', colnames(dt)) 
dt[start_td:end_td] <- sapply(dt[start_td:end_td], as.character)
dt[start_td:end_td] <- sapply(dt[start_td:end_td], as.integer)

# add data type to data dictionary
dd$type <- sapply(dt, class)

# Save 
write.csv(dd, here::here("data", "tdsd_s1_data_dictionary.csv"), row.names = FALSE)
write.csv(dt, here::here("data", "tdsd_s1_data.csv"), row.names = FALSE)
