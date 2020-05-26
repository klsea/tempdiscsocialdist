# Graph covid data for tempdiscsocialdist data set
# 5.19.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(plyr)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "multiplot.R"))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)

# create a function to make fancy graphs with histogram & age lined up

fancy_graph <- function(plot1, plot2, ylabel, color_var) {
  g1 <- plot1 + geom_jitter(aes(color = color_var)) + geom_smooth(method=lm, color = "black") +
    theme_minimal() + theme(legend.position = 'none') +
    ylab(ylabel) + xlab("Age")
  g2 <- plot2 + geom_histogram(stat='count') + coord_flip() + 
    theme_minimal() + theme(legend.position = 'none', axis.title.y = element_blank(), 
                            axis.text.y = element_blank()) + xlab('Count') 
  multiplot(g1,g2, cols = 2)
}

# Covid questions

color <- factor(dt$Q10)
p1 <- ggplot(dt, aes(Q5, Q10))
p2 <- ggplot(dt, aes(Q10, fill = color))
covid1 <- fancy_graph(p1, p2, dd$Question[grep('Q10', dd$Variable)[1]], color)

color <- factor(dt$Q11)
p1 <- ggplot(dt, aes(Q5, Q11))
p2 <- ggplot(dt, aes(Q11, fill = color))
covid2 <- fancy_graph(p1, p2, dd$Question[grep('Q11', dd$Variable)[1]], color)

color <- factor(dt$Q12)
p1 <- ggplot(dt, aes(Q5, Q12))
p2 <- ggplot(dt, aes(Q12, fill = color))
covid3 <- fancy_graph(p1, p2, dd$Question[grep('Q12', dd$Variable)[1]], color)

# Work

color <- factor(dt$Q13)
p1 <- ggplot(dt, aes(Q5, Q13))
p2 <- ggplot(dt, aes(Q13, fill = color))
work <- fancy_graph(p1, p2, dd$Question[grep('Q13', dd$Variable)[1]], color)

# Leave the house

color <- factor(dt$Q14)
p1 <- ggplot(dt, aes(Q5, Q14)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q14, fill = color))
leave1 <- fancy_graph(p1, p2, "How many days did you leave your home during the past week?", color)

color <- factor(dt$SC0)
p1 <- ggplot(dt, aes(Q5, SC0)) 
p2 <- ggplot(dt, aes(SC0, fill = color))
leave2 <- fancy_graph(p1, p2, dd$variable_name[grep('SC0', dd$Variable)], color)

color <- factor(dt$Q16_1)
p1 <- ggplot(dt, aes(Q5, Q16_1)) 
p2 <- ggplot(dt, aes(Q16_1, fill = color))
leave3 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_1', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_2)
p1 <- ggplot(dt, aes(Q5, Q16_2)) 
p2 <- ggplot(dt, aes(Q16_2, fill = color))
leave4 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_3)
p1 <- ggplot(dt, aes(Q5, Q16_3)) 
p2 <- ggplot(dt, aes(Q16_3, fill = color))
leave5 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_4)
p1 <- ggplot(dt, aes(Q5, Q16_4)) 
p2 <- ggplot(dt, aes(Q16_4, fill = color))
leave6 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_4', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_5)
p1 <- ggplot(dt, aes(Q5, Q16_5)) 
p2 <- ggplot(dt, aes(Q16_5, fill = color))
leave7 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_5', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_6)
p1 <- ggplot(dt, aes(Q5, Q16_6)) 
p2 <- ggplot(dt, aes(Q16_6, fill = color))
leave8 <- fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_6', dd$Variable)[[1]]]), color)

color <- factor(dt$Q18)
p1 <- ggplot(dt, aes(Q5, Q18)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q18, fill = color))
fancy_graph(p1, p2, "How many days did you have a visitor in your home during the past week?", color)

# Visitors

color <- factor(dt$SC2)
p1 <- ggplot(dt, aes(Q5, SC2)) 
p2 <- ggplot(dt, aes(SC2, fill = color))
visitor1 <- fancy_graph(p1, p2, dd$variable_name[grep('SC2', dd$Variable)], color)

color <- factor(dt$Q20_1)
p1 <- ggplot(dt, aes(Q5, Q20_1)) 
p2 <- ggplot(dt, aes(Q20_1, fill = color))
visitor2 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_1', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_2)
p1 <- ggplot(dt, aes(Q5, Q20_2)) 
p2 <- ggplot(dt, aes(Q20_2, fill = color))
visitor3 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_2)
p1 <- ggplot(dt, aes(Q5, Q20_2)) 
p2 <- ggplot(dt, aes(Q20_2, fill = color))
visitor4 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_3)
p1 <- ggplot(dt, aes(Q5, Q20_3)) 
p2 <- ggplot(dt, aes(Q20_3, fill = color))
visitor5 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_4)
p1 <- ggplot(dt, aes(Q5, Q20_4)) 
p2 <- ggplot(dt, aes(Q20_4, fill = color))
visitor6 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_4', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_5)
p1 <- ggplot(dt, aes(Q5, Q20_5)) 
p2 <- ggplot(dt, aes(Q20_5, fill = color))
visitor7 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_5', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_6)
p1 <- ggplot(dt, aes(Q5, Q20_6)) 
p2 <- ggplot(dt, aes(Q20_6, fill = color))
visitor8 <- fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_6', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_1)
p1 <- ggplot(dt, aes(Q5, Q22_1)) 
p2 <- ggplot(dt, aes(Q22_1, fill = color))
socialdist1 <- fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_1', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_2)
p1 <- ggplot(dt, aes(Q5, Q22_2)) 
p2 <- ggplot(dt, aes(Q22_2, fill = color))
socialdist2 <- fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_3)
p1 <- ggplot(dt, aes(Q5, Q22_3)) 
p2 <- ggplot(dt, aes(Q22_3, fill = color))
socialdist3 <- fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_4)
p1 <- ggplot(dt, aes(Q5, Q22_4)) 
p2 <- ggplot(dt, aes(Q22_4, fill = color))
socialdist4 <- fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_4', dd$Variable)[[1]]]), color)

# More covid

color <- factor(dt$Q23_1)
p1 <- ggplot(dt, aes(Q5, Q23_1))
p2 <- ggplot(dt, aes(Q23_1, fill = color))
covid4 <- fancy_graph(p1, p2, "How likely do you think you are to catch Covid19?", color)
saveRDS(p1, here::here('output', 'cc1.RDS'))
saveRDS(p2, here::here('output', 'cc2.RDS'))


color <- factor(dt$Q24_1)
p1 <- ggplot(dt, aes(Q5, Q24_1))
p2 <- ggplot(dt, aes(Q24_1, fill = color))
covid5 <- fancy_graph(p1, p2, "How worried are you about catching Covid19?", color)

color <- factor(dt$Q25)
p1 <- ggplot(dt, aes(Q5, Q25))
p2 <- ggplot(dt, aes(Q25, fill = color))
covid6 <- fancy_graph(p1, p2, "What is the most likely outcome if you caught Covid-19", color)
saveRDS(p1, here::here('output', 'co1.RDS'))
saveRDS(p2, here::here('output', 'co2.RDS'))

# mental health

color <- factor(dt$SC1)
p1 <- ggplot(dt, aes(Q5, SC1)) 
p2 <- ggplot(dt, aes(SC1, fill = color))
mentalhealth <- fancy_graph(p1, p2, dd$variable_name[grep('SC1', dd$Variable)], color)
saveRDS(p1, here::here('output', 'mh1.RDS'))
saveRDS(p2, here::here('output', 'mh2.RDS'))

rm(p1, p2)