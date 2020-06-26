# Run all scripts
# 6.26.20 KLS

#libraries
library(tidyverse)
library(here)

# remove generated folders and add empty folders
#unlink(here::here('figs'), recursive = TRUE)
#unlink(here::here('output'), recursive = TRUE)
#dir.create(here::here('figs'))
#dir.create(here::here('output'))

# get data set from user
getsamplenumber <- function()
{ sample <- 0
  `%notin%` <- Negate(`%in%`)
  while (sample %notin% c(1,2)){
    sample <- readline(prompt="Which sample would you like to analyze? Please enter 1 or 2: ")
  }
  return(as.integer(sample))
}

sample <- getsamplenumber()

5# run all scripts in order
list.files(here::here(), full.names = TRUE) %>% walk(source)

