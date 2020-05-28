# Graph temp discount data for tempdiscsocialdist data set
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(plyr)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "SummarySE.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

d1 <- td_wide_to_long(dt)

# graph td x age
td_x_age <- ggplot(d1, aes(Age, propChoice)) + geom_point(aes(color = domain)) + geom_smooth(method=lm, color = "black") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = domain, fill = domain )) +
  facet_grid(.~ domain) +theme_minimal() + theme(legend.position="none") + 
  ylab('Proportion of Smaller, Sooner Choices')
td_x_age

# graph td means
td_means <- ggplot(d1, aes(domain, propChoice)) + geom_violin(trim = FALSE, aes(fill = domain)) + 
 geom_boxplot(width=0.1, fill = "white") + theme_minimal() + theme(legend.position = "none")
td_means

# graph who is social partner
dt$socialpartner <- dt$Q32
dt$socialpartner <- mapvalues(dt$socialpartner, from = c(1,2,3,4,7,8,9), to = c('My spouse or significant other', 'My child', 'My parent', 
'My grandparent or other relative', 'My close friend', 'A new friend', 'My co-worker'))
part_freq <- count(dt$socialpartner) # count education levels in new table
part_freq$socialpartner <- part_freq$x
partplot <- ggplot(part_freq, aes(x='', y=freq, fill = socialpartner)) + geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) + theme_void()
partplot
rm(part_freq)

# graph where money is spent
d2 <- dt[c(1, 184)]  %>% separate(Q170, as.character(1:6)) 
colnames(d2) <- c('ID', paste0('C', colnames(d2[2:7])))

d2 <- d2 %>%
  mutate(M1 = ifelse(C1 == 1, 1, 0), 
         M2 = ifelse(C1 == 2 | C2 == 2, 1, 0),
         M3 = ifelse(C1 == 3 | C2 == 3 | C3 == 3, 1, 0), 
         M4 = ifelse(C1 == 4 | C2 == 4 | C3 == 4 | C4 == 4, 1, 0), 
         M5 = ifelse(C1 == 5 | C2 == 5 | C3 == 5 | C4 == 5 | C5 == 5, 1, 0), 
         M6 = ifelse(C1 == 6 | C2 == 6 | C3 == 6 | C4 == 6 | C5 == 6 | C6 == 6, 1, 0)) %>%
  mutate_at(vars(paste0("M", 1:6)), as.numeric) %>%
  mutate_at(vars(paste0("M", 1:6)), replace_na, 0)

d3 <- as.data.frame(colSums(d2[8:13]))
colnames(d3) <- 'count'
d3$expense <- c('Bills / essential items', 
                  'Non-essential items', 'For someone else', 
                  'Giving to someone else', 'Shared experience', 'Other')
d3$expense <- ordered(d3$expense, levels = c('Bills / essential items', 
                                             'Non-essential items', 'For someone else', 
                                             'Giving to someone else', 'Shared experience', 'Other'))
moneyuseplot <- ggplot(d3, aes(expense, count, fill = expense)) + geom_bar(stat = 'identity') + theme_minimal() + 
  theme(legend.position='none', axis.text.x  = element_text(angle=90, vjust=0.5, size = 10))
moneyuseplot

# importance of money, social, health
dt$moneyimport <- dt$Q169_1
dt$socialimport <- dt$Q169_2
dt$healthimport <- dt$Q169_3
dt$Age <- dt$Q5

library(scales)
show_col(hue_pal()(3))

moneyimportplot <- ggplot(dt, aes(Age, moneyimport)) + geom_point(color = "#F8766D") + 
  geom_smooth(method = lm, color = "#F8766D", fill = "#F8766D") + theme_minimal() + theme(legend.position = 'none') + ylab('') + 
  annotate(geom = 'text', x = 80 , y = 0, label = "Not important") +  annotate(geom = 'text', x = 75 , y = 100, label = "Extremely important") 
moneyimportplot

socialimportplot <- ggplot(dt, aes(Age, socialimport)) + geom_point(color = "#619CFF") + 
  geom_smooth(method = lm, fill = "#619CFF", color = "#619CFF") + theme_minimal() + theme(legend.position = 'none') + ylab('') + 
  annotate(geom = 'text', x = 80 , y = 0, label = "Not important") +  annotate(geom = 'text', x = 75 , y = 100, label = "Extremely important") 
socialimportplot

healthimportplot <- ggplot(dt, aes(Age, healthimport)) + geom_point(color = "#00BA38") + 
  geom_smooth(method = lm, color = "#00BA38", fill = "#00BA38") + theme_minimal() + theme(legend.position = 'none') + ylab('') + 
  annotate(geom = 'text', x = 80 , y = 0, label = "Not important") +  annotate(geom = 'text', x = 75 , y = 100, label = "Extremely important")  
healthimportplot
