# Clean tempdiscsocialdist data set
# 5.8.20 KLS

# load required packages
library(here)
library(stringr)

# load sources function

# set hard-coded variables
  
# load data
dt <- read.csv(here::here('data','Time+discounting+across+domains_May+7,+2020_09.54.csv'))

# add id column
dt$ID <- c(0,0,seq(1,nrow(dt)-2, 1))
dt$ID <- str_pad(dt$ID, 3, pad="0")

# clean out qualtrics columns
dt <- dt[-c(1:17)]
dt <- dt[c(ncol(dt), 1:(ncol(dt)-1))]

# create data dictionary
dd <- t(dt[1,])
dd <- cbind(rownames(dd), data.frame(dd,row.names = NULL))
colnames(dd) <- c('Variable', 'Question')

# finish cleaning data file
dt <- dt[-c(1:2),]

# Save 
write.csv(dd, here::here("data", "tempdiscsocildist_Study1_data_dictionary.csv"), row.names = FALSE)
write.csv(dt, here::here("data", "tempdiscsocialdist_Study1_data.csv"), row.names = FALSE)
