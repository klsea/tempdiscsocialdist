# Check for problem data 
# Using criteria from Gillian et al 2016 and Decker et al 2016
# 8.3.20 KLS

# load required packages
library(here)

# load source functions

# set hard-coded variables
path_to_data <- "./task/data/"

# read data in
files <- list.files(path_to_data, pattern = ".csv")