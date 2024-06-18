# Script to get everyone started 


### FIRST: CHANGE THE DIRECTORY 

setwd("path/to/your/files")



#############

library(tidyverse)

flowering_time <- read.table("flowering_data.txt",header = TRUE)
flowering_time_clean <- na.omit(flowering_time)

germination <- read.csv("germination_data.csv")
germination_clean <- germination[!(germination$Name==""),]


