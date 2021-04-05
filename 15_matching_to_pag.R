# Matching participants in PAG 2016 
# 4.2.21 KLS

# load required packages
library(here)
library(tidyverse)
library(StatMatch)

# load source functions

# set hard-coded variables

# load data
s1 <- read.csv(here::here("data", "tdsd_s1_data.csv"))
s2 <- read.csv(here::here("data", "tdsd_s2_data.csv"))
pag <- read.csv(here::here("data", "all_sublevel_data.csv"))

# prep pag data frame ####
# pull relevant variables from PAG 2016
pag <- pag[match(unique(pag$partNum), pag$partNum), c(1, 20, 21, 19, 8, 9)]
vars <- colnames(pag)
pag$Ethnicity <- mapvalues(pag$Ethnicity, from = c("Non-Hispanic White", "African-American", "East Asian", "Hispanic ", "Indian", ""), 
                           to = c("White/Caucasian", "Black/African American", "Asian", "Hispanic/Latino", "Asian", ""))

pag$Sex <- factor(pag$Sex)
pag$Ethnicity <- factor(pag$Ethnicity)

# remove Asians and no answer ( 4 participants)
pag <- pag[!pag$Ethnicity == "Asian",]
pag <- pag[!pag$Ethnicity == "",]
pag$Ethnicity <- factor(pag$Ethnicity)

# bin highest two incomes
pag$Income[which(pag$Income == 11)] <- 10

# prep new study data frames ####
# pull out relevant variables from s1 & s2, and combine 
# sex = Q3, ethnicity = Q6, age = Q5, education = Q4, income = Q7
s1 <- s1[c(1, 4, 7, 6, 5, 8)]
s2 <- s2[c(1, 4, 7, 6, 5, 8)]
 
# make unique part number
s1$ID <- s1$ID + 1000
s2$ID <- s2$ID + 2000

# give columns meaningful names
colnames(s1) <- vars
colnames(s2) <- vars

# combine
dt <- rbind(s1,s2)
rm(s1,s2)

# remap some values
dt$Sex <- mapvalues(dt$Sex, from = c(0,1), to = c('M','F'))
dt$Ethnicity <- mapvalues(dt$Ethnicity, from = c(1, 2, 3, 4, 5, 6, 7, 8), 
                     to = c("White/Caucasian", "Black/African American", "Asian", "Hispanic/Latino", 
                            "American Indian/Alaska Native", "Pacific Islander", "Multiracial", "Other"))
dt$Sex <- factor(dt$Sex)
dt$Ethnicity <- factor(dt$Ethnicity)

# convert income to same scale
dt$Income <- mapvalues(dt$Income, from = seq(5,155,10), to = c('3', '3', '4','4', '5', '5', '6','6', '7','7', '8', '8', '9', '9', '10','10'))
dt$Income <- as.numeric(dt$Income)

# begin matching ####
pt = pag[1,]
find_match <- function(pt, dt) {
  #pt = one participant's data
  #dt = data fram with pool of potential matches
  d1 <- dt[which(dt$Sex == pt$Sex),]
  d2 <- d1[which(d1$Ethnicity == pt$Ethnicity),]
  md <- mahalanobis.dist(pt[4:6], d2[4:6])
  min_dist <- colnames(md)[which.min(md)]
  match <- d2[which(rownames(d2) == min_dist), ]
  return(match)
}

cdt <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(cdt) <- vars
for (row in 1:nrow(pag)) {
  pt = pag[row,]
  match <- find_match(pt, dt)
  cdt <- rbind(cdt, match)  # add match to comparison data table
  dt <- dt[which(dt$partNum != match$partNum),] # remove match from possibilities
}

fdt <- rbind(cdt, pag)

write.csv(fdt, here::here('output', 'matching_data_table.csv'))
