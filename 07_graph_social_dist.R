# Graph covid data for tempdiscsocialdist data set
# 5.19.20 KLS updated 6.27.20 with sample #

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(plyr)
library(gridExtra)

# load source functions
source(here::here("scr", "td_wide_to_long.R"))
source(here::here("scr", "multiplot.R"))

# set hard-coded variables

# load data
if (sample == 1) {
  dt <- read.csv(here::here("data", "tdsd_s1_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'), stringsAsFactors=FALSE)
  plottitle <- 'Primary Sample'
} else {
  dt <- read.csv(here::here("data", "tdsd_s2_data.csv"))
  dd <- read.csv(here::here("data", 'tdsd_s2_data_dictionary.csv'), stringsAsFactors=FALSE)
  plottitle <- 'Replication Sample'
}

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

fancy_graph_2 <- function(plot1, plot2, ylabel, color_var, width, plottitle) {
  g1 <- plot1 + geom_jitter(aes(color = color_var)) + geom_smooth(method=lm, color = "black") +
    theme_minimal() + ylab(ylabel) + xlab("Age") + ylim(-1,20) + 
    theme(legend.position = 'none', axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16))
  g2 <- plot2 + geom_histogram(stat='count') + coord_flip() + theme_minimal() + ylab('Count') +
    theme(legend.position = 'none', axis.title.y = element_blank(), axis.text.y = element_blank(),  
    axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 20)) +
    xlim(-1,20) + scale_y_continuous(breaks = c(0,25,50))
  grid.arrange(g1,g2, nrow = 1, widths = c(width,1), 
               top = grid::textGrob(plottitle, gp = grid::gpar(fontsize = 20, fontface="bold"), x = 0, hjust = 0))
}

# create name variable
if (sample == 1) {
  name <- 's1_'
} else {
  name <- 's2_'
}

# Covid questions

color <- factor(dt$Q10)
p1 <- ggplot(dt, aes(Q5, Q10))
p2 <- ggplot(dt, aes(Q10, fill = color))
fancy_graph(p1, p2, dd$Question[grep('Q10', dd$Variable)[1]], color)

color <- factor(dt$Q11)
p1 <- ggplot(dt, aes(Q5, Q11))
p2 <- ggplot(dt, aes(Q11, fill = color))
fancy_graph(p1, p2, dd$Question[grep('Q11', dd$Variable)[1]], color)

color <- factor(dt$Q12)
p1 <- ggplot(dt, aes(Q5, Q12))
p2 <- ggplot(dt, aes(Q12, fill = color))
fancy_graph(p1, p2, dd$Question[grep('Q12', dd$Variable)[1]], color)

# Work

color <- factor(dt$Q13)
p1 <- ggplot(dt, aes(Q5, Q13))
p2 <- ggplot(dt, aes(Q13, fill = color))
fancy_graph(p1, p2, dd$Question[grep('Q13', dd$Variable)[1]], color)

# Leave the house

color <- factor(dt$Q14)
p1 <- ggplot(dt, aes(Q5, Q14)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q14, fill = color))
fancy_graph(p1, p2, "How many days did you leave your home during the past week?", color)

color <- factor(dt$SC0)
p1 <- ggplot(dt, aes(Q5, SC0)) 
p2 <- ggplot(dt, aes(SC0, fill = color))
fancy_graph(p1, p2, dd$variable_name[grep('SC0', dd$Variable)], color)

color <- factor(dt$Q16_1)
p1 <- ggplot(dt, aes(Q5, Q16_1)) 
p2 <- ggplot(dt, aes(Q16_1, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_1', dd$Variable)[[1]]]), color)
saveRDS(p1, here::here('output', paste0(name, 'work1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'work2.RDS')))

color <- factor(dt$Q16_2)
p1 <- ggplot(dt, aes(Q5, Q16_2)) 
p2 <- ggplot(dt, aes(Q16_2, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_3)
p1 <- ggplot(dt, aes(Q5, Q16_3)) 
p2 <- ggplot(dt, aes(Q16_3, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_4)
p1 <- ggplot(dt, aes(Q5, Q16_4)) 
p2 <- ggplot(dt, aes(Q16_4, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_4', dd$Variable)[[1]]]), color)

color <- factor(dt$Q16_5)
p1 <- ggplot(dt, aes(Q5, Q16_5)) 
p2 <- ggplot(dt, aes(Q16_5, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_5', dd$Variable)[[1]]]), color)
saveRDS(p1, here::here('output', paste0(name, 'com1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'com2.RDS')))


color <- factor(dt$Q16_6)
p1 <- ggplot(dt, aes(Q5, Q16_6)) 
p2 <- ggplot(dt, aes(Q16_6, fill = color))
fancy_graph(p1, p2, paste0('Why leave? ', 
                           dd$variable_name[grep('Q16_6', dd$Variable)[[1]]]), color)

color <- factor(dt$Q18)
p1 <- ggplot(dt, aes(Q5, Q18)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q18, fill = color))
fancy_graph(p1, p2, "How many days did you have a visitor in your home during the past week?", color)

# Visitors

color <- factor(dt$SC2)
p1 <- ggplot(dt, aes(Q5, SC2)) 
p2 <- ggplot(dt, aes(SC2, fill = color))
fancy_graph(p1, p2, dd$variable_name[grep('SC2', dd$Variable)], color)

color <- factor(dt$Q20_1)
p1 <- ggplot(dt, aes(Q5, Q20_1)) 
p2 <- ggplot(dt, aes(Q20_1, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_1', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_2)
p1 <- ggplot(dt, aes(Q5, Q20_2)) 
p2 <- ggplot(dt, aes(Q20_2, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_2)
p1 <- ggplot(dt, aes(Q5, Q20_2)) 
p2 <- ggplot(dt, aes(Q20_2, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_2', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_3)
p1 <- ggplot(dt, aes(Q5, Q20_3)) 
p2 <- ggplot(dt, aes(Q20_3, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_4)
p1 <- ggplot(dt, aes(Q5, Q20_4)) 
p2 <- ggplot(dt, aes(Q20_4, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_4', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_5)
p1 <- ggplot(dt, aes(Q5, Q20_5)) 
p2 <- ggplot(dt, aes(Q20_5, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_5', dd$Variable)[[1]]]), color)

color <- factor(dt$Q20_6)
p1 <- ggplot(dt, aes(Q5, Q20_6)) 
p2 <- ggplot(dt, aes(Q20_6, fill = color))
fancy_graph(p1, p2, paste0('Why have visitors? ', 
                           dd$variable_name[grep('Q20_6', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_1)
p1 <- ggplot(dt, aes(Q5, Q22_1)) 
p2 <- ggplot(dt, aes(Q22_1, fill = color))
fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_1', dd$Variable)[[1]]]), color)
saveRDS(p1, here::here('output', paste0(name, 'sh1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'sh2.RDS')))

color <- factor(dt$Q22_2)
p1 <- ggplot(dt, aes(Q5, Q22_2)) 
p2 <- ggplot(dt, aes(Q22_2, fill = color))
fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_2', dd$Variable)[[1]]]), color)
saveRDS(p1, here::here('output', paste0(name, 'fh1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'fh2.RDS')))

color <- factor(dt$Q22_3)
p1 <- ggplot(dt, aes(Q5, Q22_3)) 
p2 <- ggplot(dt, aes(Q22_3, fill = color))
fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_3', dd$Variable)[[1]]]), color)

color <- factor(dt$Q22_4)
p1 <- ggplot(dt, aes(Q5, Q22_4)) 
p2 <- ggplot(dt, aes(Q22_4, fill = color))
fancy_graph(p1, p2, paste0('Why social distance? ', 
                           dd$variable_name[grep('Q22_4', dd$Variable)[[1]]]), color)

# More covid

color <- factor(dt$Q23_1)
p1 <- ggplot(dt, aes(Q5, Q23_1))
p2 <- ggplot(dt, aes(Q23_1, fill = color))
fancy_graph(p1, p2, "How likely do you think you are to catch Covid19?", color)
saveRDS(p1, here::here('output', paste0(name, 'cc1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'cc2.RDS')))

color <- factor(dt$Q24_1)
p1 <- ggplot(dt, aes(Q5, Q24_1))
p2 <- ggplot(dt, aes(Q24_1, fill = color))
fancy_graph(p1, p2, "How worried are you about catching Covid19?", color)

color <- factor(dt$Q25)
p1 <- ggplot(dt, aes(Q5, Q25))
p2 <- ggplot(dt, aes(Q25, fill = color))
fancy_graph(p1, p2, "What is the most likely outcome if you caught Covid-19", color)
saveRDS(p1, here::here('output', paste0(name, 'co1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'co2.RDS')))

# mental health

color <- factor(dt$SC1)
p1 <- ggplot(dt, aes(Q5, SC1)) 
p2 <- ggplot(dt, aes(SC1, fill = color))
fancy_graph(p1, p2, dd$variable_name[grep('SC1', dd$Variable)], color)
saveRDS(p1, here::here('output', paste0(name, 'mh1.RDS')))
saveRDS(p2, here::here('output', paste0(name, 'mh2.RDS')))

#rm(p1, p2, color, fancy_graph, multiplot, td_wide_to_long, dd, dt, name)

#mental health from pub
color <- factor(dt$SC1)
p1 <- ggplot(dt, aes(Q5, SC1)) 
p2 <- ggplot(dt, aes(SC1, fill = color))
plotname <- paste0('mental_health_study_', sample, '.png')
png(file = here::here('figs', plotname), width = 450, height = 500)
fancy_graph_2(p1, p2, 'Number of Mental Health \nSymptoms', color, 3, plottitle)
dev.off()
