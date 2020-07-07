# Populate data dictionary
# 5.13.20 KLS updated 6.26.20 with sample #

# load required packages
library(here)
library(stringr)
library(tidyverse)
library(stringr)

# load sources function

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", 'tdsd_s1_data.csv'))
  dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'))
} else {
  dt <- read.csv(here::here("data", 'tdsd_s2_data.csv'))
  dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'))
}

#create and populate variable name column
dd[,'variable_name'] <- NA

dd$variable_name[1:10] <- c("ID", "NeuroHealth", "PsychHealth", "Sex", "Education", "Age", "Race", 
                           "Income", "Employment", "Class")
dd$variable_name[11:15] <- c("covid_pos", "covid_prev", "covid_symp", "work", "leave_home")
dd$variable_name[16:22] <- c('leavehome','havework', 'getessentials', 'socialcontact', 
                             'changescenery', 'community', 'caresomeone' )
dd$variable_name[23:25] <- c('otherreasonsleave','visitors', 'whenvisitors')
dd$variable_name[26:32] <- c('dowork', 'deliveressentials', 'socialcontact', 'changescenery', 
                             'community', 'providecare', 'otherreasonsvisit')
dd$variable_name[33:38] <-c('selfhealth', 'familyhealth', 'limitspread', '4someoneelse', 
                            'otherdistance', 'otherdisttext')
dd$variable_name[39:42] <-c('catch_covid', 'worried_covid', 'covid_outcome', 'mental_health')
dd$variable_name[43:45] <- c('imagine_weeks', 'imagine_months', 'imagine_years')
dd$variable_name[46:48] <- c('comprehension_check', 'social_partner', 'social_motivation')
dd$variable_name[181:183] <- c('import_money', 'import_social', 'import_health')
dd$variable_name[184:185] <- c('spend_money', 'spend_money_other')
dd$variable_name[192] <- 'duration'

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
dd$variable_name[grep('SC0', dd$Variable)] <- 'socialDistBehavior' # Q15
dd$variable_name[grep('SC1', dd$Variable)] <- 'mentalHealthSymptoms' # Q26
dd$variable_name[grep('SC2', dd$Variable)] <- 'cautiosBehVisitors' # Q19
dd$variable_name[grep('SC3', dd$Variable)] <- 'propSSmoney'
dd$variable_name[grep('SC4', dd$Variable)] <- 'propSShealth'
dd$variable_name[grep('SC5', dd$Variable)] <- 'propSSsocial'

if (sample == 2) { dd$variable_name[grep('Q328', dd$Variable)] <- 'notSocialDistancing' }

# add data type to data dictionary
dd$type <- sapply(dt, class)

# Create and populate allowed_values in data dictionary
dd[,'allowed_values'] <- NA 

# demo data
dd$allowed_values[grep('ID', dd$Variable)] <- '001-233'
dd$allowed_values[c(2:3, 14)] <- '0 = No, 1 = Yes'
dd$allowed_values[grep('Q3', dd$Variable)] <- '0 = Male, 1 = Female'
dd$allowed_values[grep('Q4', dd$Variable)] <- "5 = Middle School, 12 = High School Diploma, 14 = Some College, 16 = Bachelor's Degree, 
18 = Master's Degree, 21 = Doctoral Degree"
dd$allowed_values[grep('Q5', dd$Variable)] <- '18-86'
dd$allowed_values[grep('Q6', dd$Variable)] <- '1 = White/Caucasian, 2 = Black/African American, 4 = Hispanic/Latino'
dd$allowed_values[grep('Q7', dd$Variable)] <- '5 = less than $10,000, 
15 = $10,000-$19,999, 
25 = $20,000-$29,999, 
35 = $30,000-$39,999, 
45 = $40,000-$49,999, 
55 = $50,000-$59,999, 
65= $60,000-$69,999, 
75 = $70,000-$79,999, 
85= $80,000-$89,999, 
95= $90,000-$99,999, 
105 = $100,000-$109,999, 
115 = $110,000-$119,999, 
125 = $120,000-$129,999, 
135 = $130,000-$139,999, 
145 = $140,000-$149,999, 
155 = $150,000 or more'
dd$allowed_values[grep('Q8', dd$Variable)] <- '1 = Yes, has paid employment: Full time employee (30 hours a week or more), 
2 = Yes, has paid employment: Part time employee (less than 30 hours a week), 
3 = Self-employeed, 
4 = No, no paid employment: Retired/pension, 
5 = No, no paid employment: Homemaker not otherwise employed, 
6 = No, no paid employment: Student, 
7 = No, no paid employment: Unemployed, 
8 = No paid employment for now: Unemployed now but will have job after virus'
dd$allowed_values[grep('Q9', dd$Variable)] <- '1 = Lower class, 2 = Working class, 3 = Lower middle class, 4 = Middle class, 
5 = Upper middle class, 6 = Upper class'

# covid questions
dd$allowed_values[11:12] <- '0 = No, 1 = Yes have symptoms, but not confirmed with test, 2 = Yes, confirmed with test'
dd$allowed_values[13] <- '1 = No, 2 = Some symptoms, 3 = All symptoms'
dd$allowed_values[15] <- "0 - No one visited my home in the past week (0 days), 
1 = 1 day, 2 = 2 days, 3 = 3 days, 4 = 4 days, 5 = 5 days, 6 = 6 days, 7 = Every day (7 days)"
dd$allowed_values[16] <- "1 = Make physical contact with other people (handshake, hug, etc),
2 = Try to stay at least away from other people but sometimes end up closer than 6 feet away, 
3 = Stay at least 6 feet (2 meters) from other people, 4 = Wear gloves, 5 = Wear a face covering or mask, 
6 = Make an effort to not touch your face, eyes, nose, or mouth, 
7 = Wash hands with soap and water for at least 20 seconds when returning home, 
8 = Try to minimize touching anything when away from home, 
9 = Wash all clothes worn outside the home, 
10 = Clean and disinfect everything you've touched. Examples: phone, car steering wheel, doorknobs, handles, sink, etc, 
11 = Shower when returning home, 12 = None of the above"
dd$allowed_values[17:22] <- '1-6'
dd$allowed_values[c(23, 32, 38)] <- 'text'
dd$allowed_values[24] <- "0 - Never left house or only went somewhere I didn't encounter other people (0 days), 
1 = 1 day, 2 = 2 days, 3 = 3 days, 4 = 4 days, 5 = 5 days, 6 = 6 days, 7 = Every day (7 days)"
dd$allowed_values[25] <- "1 = Make physical contact with the visitor(s) (handshake, hug, etc),
2 = Try to stay at least away from other people but sometimes end up closer than 6 feet away, 
3 = Stay at least 6 feet (2 meters) from other people, 4 = Wear gloves, 5 = Wear a face covering or mask, 
6 = Make an effort to not touch your face, eyes, nose, or mouth, 
7 = Wash hands with soap and water for at least 20 seconds when the visitor leaves, 
8 = Try to minimize touching anything the visitor brought, 
9 = Wash all clothes worn when a visitor was in your house, 
10 = Clean and disinfect everything you've touched or your visitor touched. Examples: phone, car steering wheel, doorknobs, handles, sink, etc, 
11 = Shower after your visitor leaves, 12 = None of the above"
dd$allowed_values[26:31] <- '1-6'
dd$allowed_values[33:37] <- '1-5'
dd$allowed_values[39] <- "0 = Extremely unlikely, 17 = Moderately unlikely, 34 = Slightly unlikely, 
50 = Neither likely nor unlikely, 67 = Slightly likely, 84 = Moderately likely, 100 = Extremely likely"
dd$allowed_values[40] <- "0 = Not worried at all, 25 = A little worried, 50 = Moderately worried, 75 = Quite worried, 100 = Exteremely woried"
dd$allowed_values[41] <- "1 = Recover without serious symptoms or complications (like a typical flu), 
2 = Have serious symptoms (more than a typical flu) but recover at home, 3 = Have to be hospitalized, 
4 = Would not survive"
dd$allowed_values[42] <- "1 = Anxiety/worry, 2 = Sadness/depression, 3 = Loss of sleep, 4 = Loss of appetite, 5 = Lack of motivation, 
6 = Loss of interest in things you normally enjoy, 7 = Difficulty concentrating, 8 = Stress, 9 = Communicate less often with family/friends, 
10 = Obsessive cleaning, 11 = Can't stop consuming media (news, social media, etc) related to the virus, 12 = Obsessively checking in on family and friends, 
13 = Obsessively checking your own temperature or physical symptoms, 14 = Loneliness, 15 = Low self worth, 16 = Irritability, 17 = Anger, 
18 = Aggressive behavior, 19 = Head aches, stomach aches, or muscle tension, 20 = None of the above"
dd$allowed_values[43:45] <- '0-100; 0 = Not well at all, 25 = Slightly well, 50 = Moderately well, 75 = Very well, 100 = Extremely well' 
dd$allowed_values[46] <- '2 = Health improvement, Social contact, Money'
dd$allowed_values[47] <- '1 = My spouse or significant other, 2 = My child, 3 = My parent, 4 = My grandparent or other relative, 7 = My close friend, 
8 = A new friend, 9 = My co-worker'
dd$allowed_values[48] <- '0-100; 0 = Not at all, 25 = Somewhat, 50 = Very, 75 = Moderately, 100 = Extremely'
dd$allowed_values[181:183] <- '0-100; 0 = Not important, 25 = Slightly important, 50 = Moderately important, 75 = Very important, 100 = Extremely important'
dd$allowed_values[184] <- '1 = Paying bills / buying essential personal items (groceries, gas, essential household items, 
2 = Buying something non-essential or spending it on an experience for myself, 
3 = Buying something for someone else, 
4 = Giving the money to someone else, 
5 = Spending it on an experience I would share with someone else, 
6 = Other'
dd$allowed_values[185] <- 'text'

if (sample == 1) {
  dd$allowed_values[c(186, 188)] <- '-3 to 10'
  dd$allowed_values[187] <- '0-19'
  dd$allowed_values[189:191] <- '0-42'
  dd$allowed_values[192] <- 'positive integer'
} else {
  dd$allowed_values[186] <- 'Text'
  dd$allowed_values[c(187, 189)] <- '-3 to 10'
  dd$allowed_values[188] <- '0-19'
  dd$allowed_values[190:192] <- '0-42'
  dd$allowed_values[193] <- 'positive integer'
}

# catch trials
dd$allowed_values[grep('Q77', dd$Variable)] <- 0
dd$allowed_values[grep('Q78', dd$Variable)] <- 1
dd$allowed_values[grep('Q122', dd$Variable)] <- 0
dd$allowed_values[grep('Q123', dd$Variable)] <- 1
dd$allowed_values[grep('Q167', dd$Variable)] <- 0
dd$allowed_values[grep('Q168', dd$Variable)] <- 1

# td data
dd$allowed_values[c(socialstart:socialend, moneystart:moneyend, healthstart:healthend)] <- '0 = larger later, 1 = smaller sooner'

# save file
if (sample == 1) {
  write.csv(dd, here::here('data', 'tdsd_s1_data_dictionary.csv'), row.names = FALSE)
} else {
  write.csv(dd, here::here('data', 'tdsd_s2_data_dictionary.csv'), row.names = FALSE)
}

# clean up 
#rm(dd, dt, catch, healthend, healthstart, moneyend, moneystart, socialend, socialstart)
