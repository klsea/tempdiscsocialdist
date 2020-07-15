# Race group differences in  social dist variables in  tempdiscsocialdist data set
# 7.14.20 

# load required packages
library(here)
library(tidyverse)
library(rstatix)

# load source functions

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)
  filename <- 'r1.csv'
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'), stringsAsFactors=FALSE)
  filename <- 'r2.csv'
}

# add social/health discounting difference score
dt$discountdiff <- dt$SC5 - dt$SC4

# add social/health valuedifference score
dt$valuediff <- dt$Q169_2 - dt$Q169_3

# group differences by race
dt$Race <- factor(dt$Q6)
levels(dt$Race)[levels(dt$Race) == 1] <- 'White/Caucasian'
levels(dt$Race)[levels(dt$Race) == 2] <- 'Black/African American'
levels(dt$Race)[levels(dt$Race) == 4] <- 'Hispanic/Latinx'
dt$Race <- factor(dt$Race, levels = c('White/Caucasian', 'Black/African American', 'Hispanic/Latinx'))

race_diff <- function(data, y) {
  model <- anova_test(data, dv = y, between = Race)
  return(model)
}

if (sample == 1){
  d1 <- dt[c(11:15, 24, 39:41, 181:183, 186:191, 193:194)]
} else {
  d1 <- dt[c(11:15, 24, 39:41, 181:183, 187:192, 194:195)]
}

race_sig <- matrix(nrow = ncol(d1), ncol = 3)
for (x in colnames(d1)) {
  index <- grep(x, colnames(d1))
  race_sig[index, 1] <- x
  race_sig[index, 2] <- race_diff(dt, x)$p
  race_sig[index, 3] <- race_diff(dt, x)$`p<.05`
}
colnames(race_sig) <- c("Variable", "pvalue", "significance")

write.csv(race_sig, here::here('output', filename))

# race_models <- vector(mode = 'list')
# for (x in colnames(d1)) {
#   index <- grep(x, colnames(d1))
#   race_models[[index]] <- race_diff(dt, x)
# }


