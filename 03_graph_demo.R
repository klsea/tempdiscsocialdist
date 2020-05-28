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
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))

# histogram of age
dt$Age <- dt$Q5
ageplot <- ggplot(dt, aes(Age)) + geom_histogram(binwidth= 5, colour = 'black', fill = 'gray') + 
  scale_x_continuous(breaks=seq(18,88, by=5)) +
  theme_minimal()

# pie chart of sex
dt$Sex <- dt$Q3
dt$Sex <- mapvalues(dt$Sex, from = c(0,1), to = c('Male','Female'))
sex_freq <- count(dt$Sex) # count the number in each category in new table
sex_freq$Sex <- sex_freq$x 
sexplot <- ggplot(sex_freq, aes(x='', y=freq, fill = Sex)) + geom_bar(stat = 'identity', width = 1) + 
  coord_polar("y", start = 0) + theme_void()
rm(sex_freq)

# pie chart education
dt$Education <- dt$Q4
dt$Education <- mapvalues(dt$Education, from = c(5, 12, 14, 16, 18, 21), 
                          to = c("Middle School", "High School Diploma", "Some College", 
                                 "Bachelors Degree", "Masters Degree", "Doctoral Degree"))
dt$Education <- ordered(dt$Education, levels =  c("Middle School", "High School Diploma", "Some College", 
                                                  "Bachelors Degree", "Masters Degree", "Doctoral Degree"))
ed_freq <- count(dt$Education) # count education levels in new table
ed_freq$Education <- ed_freq$x
edplot <- ggplot(ed_freq, aes(x='', y=freq, fill = Education)) + geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) + theme_void()
rm(ed_freq)

# pie chart race/ethnicity
dt$Race <- dt$Q6
dt$Race <- mapvalues(dt$Race, from = c(1, 2, 3, 4, 5, 6, 7, 8), 
                     to = c("White/Caucasian", "Black/African American", "Asian", "Hispanic/Latino", 
                            "American Indian/Alaska Native", "Pacific Islander", "Multiracial", "Other"))
race_freq <- count(dt$Race) # count racial distribution in new table
race_freq$Race <- race_freq$x
raceplot <- ggplot(race_freq, aes(x='', y=freq, fill = Race)) + geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) + theme_void()
rm(race_freq)

# histogram income
dt$Income <- dt$Q7
dt$Income <- mapvalues(dt$Income, from = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 
                                           135, 145, 155), 
                       to = c('< $10,000', '$10,000-$19,999', '$20,000-$29,999', 
                              '30,000-$39,999', '$40,000-$49,999','$50,000-$59,999', 
                              '$60,000-$69,999', '$70,000-$79,999', '$80,000-$89,999', 
                              '$90,000-$99,999', '$100,000-$109,999', '$110,000-$119,999',
                              '$120,000-$129,999', '$130,000-$139,999', '$140,000-$149,999',
                              '>= $150,000'))
dt$Income <- ordered(dt$Income, levels = c('< $10,000', '$10,000-$19,999', '$20,000-$29,999', 
                                           '30,000-$39,999', '$40,000-$49,999','$50,000-$59,999', 
                                           '$60,000-$69,999', '$70,000-$79,999', '$80,000-$89,999', 
                                           '$90,000-$99,999', '$100,000-$109,999', '$110,000-$119,999',
                                           '$120,000-$129,999', '$130,000-$139,999', '$140,000-$149,999',
                                           '>= $150,000'))
incomeplot <- ggplot(dt, aes(Income)) + geom_histogram(stat = 'count', colour = 'black', fill = 'gray') + 
  theme_minimal() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))

# pie chart employment
dt$Employment <- dt$Q8
dt$Employment <- mapvalues(dt$Employment, from = seq(1,8.1), to = c( 'Yes, has paid employment: Full time employee (30 hours a week or more)', 
'Yes, has paid employment: Part time employee (less than 30 hours a week)', 'Self-employeed', 'No, no paid employment: Retired/pension', 
'No, no paid employment: Homemaker not otherwise employed', 'No, no paid employment: Student', 'No, no paid employment: Unemployed', 
'No paid employment for now: Unemployed now but will have job after virus'))
employ_freq <- count(dt$Employment) # count racial distribution in new table
employ_freq$Employment <- employ_freq$x
employplot <- ggplot(employ_freq, aes(x='', y=freq, fill = Employment)) + geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) + theme_void()
rm(race_freq)

# histogram class
dt$Class <- dt$Q9
dt$Class <- mapvalues(dt$Class, from = seq(1,6,1), to = c('Lower class','Working class', 'Lower middle class', 'Middle class', 
                                                          'Upper middle class', 'Upper class'))
dt$Class <- ordered(dt$Class, levels = c('Lower class','Working class', 'Lower middle class', 'Middle class', 
                                         'Upper middle class', 'Upper class'))
classplot <- ggplot(dt, aes(Class)) + geom_histogram(stat = 'count', colour = 'black', fill = 'gray') + 
  theme_minimal() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
classplot
