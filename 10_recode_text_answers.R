# Recode text answer tempdiscsocialdist data set
# 5.22.20 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

# pull out question Q17
 d1 <- dt[c(1, 23)]

 d1 %>%
   mutate(Q17 = recode(Q17, 'No' = '0', "No " = "0", 'NO' = '0', 'no' = '0', 'None' = '0', 'none' = '0', 'nothing' = '0', 'Nothing' = '0', 
                       'None ' = '0', "No, just staying home as much as I can." = '0', 'No.' = '0', 'No other reason ' = '0',
                       'N/A' = '0', 'No other reason' = '0', 'No other reason. ' = '0', '<NA>' = '0', 'n' = '0', 'No comment' = '0',
                       "No there are no other reasons" = "0", 'no other reason' = '0', 'na' = '0', 'none-0' = '0',
                       'in fact I only leave my house for the first two reasons, not the others' = '0',
                       'Walking' = '1', 'Exercise' = '1', "Walk for an hour to exercise" = "1", '2 walk for exercise' = '1', 
                       "Going for walks in fresh air and sunshine." = "1", "walk my dog  #1" = "1", "Exercise " = '1',
                       "exercise walking" = "1", "Go for a walk" = '1', "Exercise, running" = '1', "walking outside every 3 or 4 days" = '1', 
                       "walk" = "1", "Walk for an hour to exercise	" = "1" , 'exercising' = '1', 'walk for exercise' = '1', 
                       'Docters' = '2', 'I had to get fungus treatment ' = '2', 'Dialysis ' = '2', 'For doctor  appointments ' = '2', 
                       'medical appointment' = '2', 'I ONLY went to my eye surgeon. Necessary follow-up to cataract surgery.' = '2', 
                       'Medical appointments and will be #1' = '2', 'Pick up essential items ' = '3', 'Food pick up from restaurant ' = '3', 
                       'Pick up school lunches ' = '3', 'To go get food for my house' = '3', 'pick up money from atm' = '3', 'Gasoline ' = '3', 
                       "I leave my home only to get groceries and pick up prescriptions. " = "3", 'HAD TO BUY GAS' = "3", 
                       'Just to get my 5 gallon jug filled with filtered water at Publix.' = '3', 'post office' = '3', 'Family emergency' = '4', 
                       'check in on parent' = '4', 'Pick kid from daycare ' = '4', 'Dinner with my daughter' = '4', ' to go to work' = '5',
                       'i leave on sundays to minister thru facebook' = '6', 'Groceries, doc appointment and restaurants' = '3', 
                       'Fresh Air Mind Clearing Moment. ' = '7', 'mental health' = '7', "to take essiential step" = '10', 
                       'To ensurexa well timr management' = '10', 'asdfn' = '10'
                       ))
 # 0 = none, 1 = exercise, 2 = medical, 3 = essential item, 4 = family, 5 = work, 6 = community, 7 = mental health, 10 = unclear
 
 
# # pull out question Q21
 d2 <- dt[c(1, 32)]
 
 d2 %>%
   mutate(Q21 = recode(Q21, 'no' = '0', 'No' = '0', 'NO' = '0', 'No.' = '0', 'none' = '0', 'None' = '0', 'None ' = '0', 'No other reason' = '0', 
                       'na' = '0', 'No other reasons' = '0', 'My daughrt grocery shops for me' = '1', 'Fix sink' = '1', 'to check on me at home' = '1', 
                       'to drop off milk' = '1', 'my son-in-law came to mow my lawn but there was no direct contact' = '1', 
                       'did a repair job in the yard' = '1', 'See If We Were Alright ' = '1', 'Fix internet ' = '1', 'To get something to eat' = '2',
                       'Only my significant other  visits  me no one else' = '3', 'social visit' = '3', '1.  Family visit' = '3', 'Exercise ' = '4', 
                       'Be careful' = '10', 'xcvbnm' = '10'
                       ))

 # 0 = none, 1 = get aid/assistance, 2 = to provide aid/assistance, 3 = family/significant other/social visit, 4 =  exercise, 10 = unclear
 
 # pull out question Q22_5
 d3 <- dt[c(1, 38)]
 
 d3 %>%
   mutate(Q22_5_TEXT = recode(Q22_5_TEXT,'No other reason' = '0', 'None' = '0', 'None ' = '0', 'none' = '0', 'nothing' = '0', 'NONE' = '0', 
                              'no other reason' = '0',
                              'Ns' = '10', 'Stay clean' = '1', 'Be able to test negative when given the opportunity to be tested.' = '1', 'Keep alive' = '1', 
                              'Gov. urges stay at home' = '2', 'Following the government order' = '2', 'requested by retirement officers' = '2',
                              "think it's the right thing to do" = '3', 'I am NOT A FLORIDA MORON!' = '3', 
                              'Already home...retired ' = '4', 
                              'Exercise ' = '10'
                              ))
 
 # 0 = none, 1 = health/cleanliness, 2 = authority, 3 = moral, 4 = no change
 # 0 = none, 1 = health/cleanliness