# Recode text answer tempdiscsocialdist data set
# 5.22.20 KLS  updated 7.15.20 with sample #

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# load data
if (sample == 1) {
   dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
   dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)
} else {
   dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
   dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'), stringsAsFactors=FALSE)
}

# pull out question Q17
d1 <- dt[c(1, 23)]

d1 <- d1 %>% mutate(Q17 = recode(Q17, 
                       # none
                       'No' = '0', "No " = "0", 'NO' = '0', 'no' = '0', 'None' = '0', 'none' = '0', 'nothing' = '0', 'Nothing' = '0', 
                       'None ' = '0', "No, just staying home as much as I can." = '0', 'No.' = '0', 'No other reason ' = '0',
                       'N/A' = '0', 'No other reason' = '0', 'No other reason. ' = '0', '<NA>' = '0', 'n' = '0', 'No comment' = '0',
                       "No there are no other reasons" = "0", 'no other reason' = '0', 'na' = '0', 'none-0' = '0', 'Nome' = '0', 
                       'in fact I only leave my house for the first two reasons, not the others' = '0', "#s 5 & 6 are N/A" = '0', 
                       "Nothing that was not listed already" = "0", "Not applicable " = "0", "There's no other reason I leave home." = '0', 
                       "no other reasons" = "0", "None." = "0", 'n/a' = '0', 'NONE' = '0', "Not Applicable " = '0', 'No. ' = '0', 
                       "I am not going out " = "0", "not applicable" = "0", "No other reasons " = "0", 'Ony iif i have to leave ' = '0',
                       "I do not have a work or job to go to so that part of this question is irrelevant for me." = "0", 
                       # exercise
                       "I go out twice a week just to play single tennis with the same person who practice safe cleaning habits and little exposure with other people" = "1", 
                       "Go for walk each day" = '1', "to run" = "1", "Running. Walk my dog." = '1', 'walking for exercise' = '1', 
                       'Walking' = '1', 'Exercise' = '1', "Walk for an hour to exercise" = "1", '2 walk for exercise' = '1', 
                       "Going for walks in fresh air and sunshine." = "1", "walk my dog  #1" = "1", "Exercise " = '1',
                       "exercise walking" = "1", "Go for a walk" = '1', "Exercise, running" = '1', "walking outside every 3 or 4 days" = '1', 
                       "walk" = "1", "Walk for an hour to exercise	" = "1" , 'exercising' = '1', 'walk for exercise' = '1', 'walk in the park' = '1', 
                       # doctor 
                       "7	Go to see the Dr and take test for them" = '2', "Medical" = "2", "Doctor Appointment" = "2", "Doctors" = "2", 
                       "Doctor visit" = '2',  "medical appointment " = "2", "#1.  To have needed medical test done." = "2", 
                       'Docters' = '2', 'I had to get fungus treatment ' = '2', 'Dialysis ' = '2', 'For doctor  appointments ' = '2', 
                       'medical appointment' = '2', 'I ONLY went to my eye surgeon. Necessary follow-up to cataract surgery.' = '2', 
                       'Number one. I go to dialysis 3 times a week.' = '2', '#2 Routine medical care (lab tests, dental work)' = '2', 
                       'Medical appointments and will be #1' = '2', 'Go to see the Dr and take test for them' = '2', 
                       # essential supplies
                       'Pick up essential items ' = '3', 'Food pick up from restaurant ' = '3', 
                       'Pick up school lunches ' = '3', 'To go get food for my house' = '3', 'pick up money from atm' = '3', 'Gasoline ' = '3', 
                       "I leave my home only to get groceries and pick up prescriptions. " = "3", 'HAD TO BUY GAS' = "3", 
                       'Just to get my 5 gallon jug filled with filtered water at Publix.' = '3', 'post office' = '3', 
                       'To buy food suppliments' = '3', "Sure to go get drugs" = "3", "Groceries" = "3", "Grocery shopping" = "3", 
                       "To pick up food from a restaurant. I would rank this second on the list " = "3", "gas" = '3', "to buy food." = '3', 
                       'Groceries, doc appointment and restaurants' = '3', 
                       "Shopping, number 2" = '3', 
                       # family
                       'Family emergency' = '4', 'family wise' = '4', '6 Go out with my partner.' = '4', 
                       'Attending funeral of important members of my household and also travelling to do charity work.' = '4',
                       'check in on parent' = '4', 'Pick kid from daycare ' = '4', 'Dinner with my daughter' = '4', 
                       # work
                       ' to go to work' = '5',
                       # community/religion
                       'i leave on sundays to minister thru facebook' = '6', "Drive in church service " = '6', "Church" = '6', 
                       # mental health
                       'Fresh Air Mind Clearing Moment. ' = '7', 'mental health' = '7', 'Go fishing' = '7', 
                       'To go some place where there are no people just get air for 30 minutes and return home. I would rank it #2' = '7',
                       # other errands
                       'Go to post office to send something I’ve sold online on eBay. This would be ranked #2.' = '8', 
                       'car repair, product delivery of personal business.' = '8', 'Go to credit union' = '8', 'TO GET ALCOHOL' = '8', 
                       'To pick up my mail.' = '8', 'Maintain my yard.' = '8', 
                       # entertainment 
                       "For entertainment" = '9',
                       # unclear
                       "to take essiential step" = '10', 
                       'To ensurexa well timr management' = '10', 'asdfn' = '10'
                       ))
 
# # pull out question Q21
d2 <- dt[c(1, 32)]
 
d2 <- d2 %>% mutate(Q21 = recode(Q21, 
                       # none 
                       'no' = '0', 'No' = '0', 'NO' = '0', 'No.' = '0', 'none' = '0', 'None' = '0', 'None ' = '0', 'No other reason' = '0', 
                       'na' = '0', 'No other reasons' = '0', 'n/a' = '0', 'NA' = '0', 'nno' = '0', 'No other visitors.' = '0', 'NONE OTHER' = '0', 
                       'no other reason' = '0', 
                       # get aid/assistance
                       'My daughrt grocery shops for me' = '1', 'Fix sink' = '1', 'to check on me at home' = '1', 
                       'to drop off milk' = '1', 'my son-in-law came to mow my lawn but there was no direct contact' = '1', 
                       'did a repair job in the yard' = '1', 'See If We Were Alright ' = '1', 'Fix internet ' = '1', 'To get something to eat' = '1',
                       '1   Replace a broken window' = '1', 'I needed a plumber for a repair.' = '1', 'Helped move in clothes dryer and fix sink.' = '1', 
                       'To do some plumbing works' = '1', 'to take us to the store' = '1', 'repair sprinklers' = '1', 'fix internet service' = '1', 
                       "Bring essentials from Mexico since they're out of stock in Texas" = '1', 'drop off needed items, rank 1' = '1',
                       # give aid/assistance
                       "Had to look after their kids while they went to the dentist." = '2', 'Pick up some items #2' = '2', 
                       # family/sig other/social visit
                       'Only my significant other  visits  me no one else' = '3', 'social visit' = '3', '1.  Family visit' = '3', 
                       'To bond' = '3', 'My roommate girlfriend come by to visit him which would be ranked number one dince she is our only visitor' = '3', 
                       'To pick up their child ' = '3', 'JUST TO SEE HOW WE DOING' = '3', 'Relationship ' = '3', ' To hangout and that would rank #7' = '3', 
                       'My boyfriend and I live about an hour apart and we usually spend our weekends together.' = '3', 
                       '1 To become intimate with my partner.' = '3', 'Family, not living here.' = '3', 'To eat takeout together' = '3', 
                       # exercise
                       'Exercise ' = '4', 'to run for workout' = '4',
                       # medical
                       'GO AND SEE THE DOCTOR AND TAKE TEST AND GO TO DINNER' = '5',                        
                       # unclear
                       'Be careful' = '10', 'xcvbnm' = '10', 'great' = '10'
                       ))
 
# pull out question Q22_5
d3 <- dt[c(1, 38)]
 
d3 <- d3 %>% mutate(Q22_5_TEXT = recode(Q22_5_TEXT,
                              # none
                              'No other reason' = '0', 'None' = '0', 'None ' = '0', 'none' = '0', 'nothing' = '0', 'NONE' = '0', 
                              'no other reason' = '0', 'No' = '0', 
                              # health/safety/cleanliness
                              'Stay clean' = '1', 'Be able to test negative when given the opportunity to be tested.' = '1', 'Keep alive' = '1', 
                              'Have underlying health issues ' = '1', 'Flatten the curve so that we may recover from this pandemic' = '1', 
                              'safety' = '1', 'Diminishing the curve' = '1', 'My mother died from Covid-19' = '1', 'too stay healthy' = '1', 
                              'FEEL THE URGENCY TO KEEP WELL & STAY WELL!' = '1', 
                              # authority
                              'Gov. urges stay at home' = '2', 'Following the government order' = '2', 'requested by retirement officers' = '2',
                              'Government put it into action' = '2', 'ordered to' = '2', 'Follow governors orders' = '2', 'Government rules ' = '2',
                              'Obey authorities recommendations ' = '2', 'It’s the local law' = '2', 'Following state guidelines to stay home' = '2', 
                              # moral
                              "think it's the right thing to do" = '3', 'I am NOT A FLORIDA MORON!' = '3', 'Keep the world safe' = '3', 
                              'Right thing to do' = '3', 'do what i think is right' = '3', 'trying to end this agony sooner than soon' = '3', 
                              # no change
                              'Already home...retired ' = '4', 'No need to go out' = '4', 
                              # avoid consumption
                              'Not to spend every day. ' = '5', 'Avoid the frenzy of hoarders.' = '5', 

                              # social norms
                              'It is the strongest of recommendations from people I trust ' = '6', 
                              # unclear
                              'Exercise ' = '10',  'Ns' = '10', "TO SEE DOCTORS" = '10', 'no' = '10', 
                              'To buy groceries and house hold supplies' = '10', 'HOW WE DOING' = '10', 
                              'Because truth be told I believe the government is partially if not completely to blame for or covid19' = '10'
                              
                              ))
 
# 0 = none, 1 = health/cleanliness, 2 = authority, 3 = moral, 4 = no change

d5 <- merge(d1,d2, by = 'ID')
d5 <- merge(d5,d3, by = 'ID')

 