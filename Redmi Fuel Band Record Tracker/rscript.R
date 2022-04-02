# Clean the environment
rm(list = ls())
graphics.off()

# Load the packages
library(tidyverse)
library(lubridate)

# Set the working directory
setwd('C:/Users/System Administrator/Desktop/Redmi Fuel Band Record Tracker/')

# Load the data
fitbit <- read.csv('./Activity_Dataset_V1.csv')
fitbit %>% head()

# Remove the first unnecessory column
fitbit <- fitbit[,-1]

# Variable structures
fitbit %>% glimpse()

# Format activity_day column as date-time 
fitbit <- within(fitbit, ymd(activity_day))
