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
dd <- read.csv(here::here("data", 'tdsd_s1_data_dictionary.csv'))

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

color <- factor(dt$Q10)
p1 <- ggplot(dt, aes(Q5, Q10))
p2 <- ggplot(dt, aes(Q10, fill = color))
fancy_graph(p1, p2, as.character(dd$Question[grep('Q10', dd$Variable)[1]]), color)

color <- factor(dt$Q11)
p1 <- ggplot(dt, aes(Q5, Q11))
p2 <- ggplot(dt, aes(Q11, fill = color))
fancy_graph(p1, p2, as.character(dd$Question[grep('Q11', dd$Variable)[1]]), color)

color <- factor(dt$Q12)
p1 <- ggplot(dt, aes(Q5, Q12))
p2 <- ggplot(dt, aes(Q12, fill = color))
fancy_graph(p1, p2, as.character(dd$Question[grep('Q12', dd$Variable)[1]]), color)

color <- factor(dt$Q13)
p1 <- ggplot(dt, aes(Q5, Q13))
p2 <- ggplot(dt, aes(Q13, fill = color))
fancy_graph(p1, p2, as.character(dd$Question[grep('Q13', dd$Variable)[1]]), color)

color <- factor(dt$Q14)
p1 <- ggplot(dt, aes(Q5, Q14)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q14, fill = color))
fancy_graph(p1, p2, "How many days did you leave your home during the past week?", color)

color <- factor(dt$Q18)
p1 <- ggplot(dt, aes(Q5, Q18)) + scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
p2 <- ggplot(dt, aes(Q18, fill = color))
fancy_graph(p1, p2, "How many days did you have a visitor in your home during the past week?", color)

color <- factor(dt$Q23_1)
p1 <- ggplot(dt, aes(Q5, Q23_1))
p2 <- ggplot(dt, aes(Q23_1, fill = color))
fancy_graph(p1, p2, "How likely do you think you are to catch Covid19?", color)

color <- factor(dt$Q24_1)
p1 <- ggplot(dt, aes(Q5, Q24_1))
p2 <- ggplot(dt, aes(Q24_1, fill = color))
fancy_graph(p1, p2, "How worried are you about catching Covid19?", color)

color <- factor(dt$Q25)
p1 <- ggplot(dt, aes(Q5, Q25))
p2 <- ggplot(dt, aes(Q25, fill = color))
fancy_graph(p1, p2, "What is the most likely outcome if you caught Covid-19", color)

