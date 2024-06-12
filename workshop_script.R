# R workshop -- This is the script I actually typed out while you were watching. 

library(tidyverse)

total <- 5+10 #calculated total 

subtotal <- "5"
subtotal2 <- 10 
subtotal+subtotal2
typeof(subtotal)

subtotal <- as.numeric(subtotal)

a_variable <- "guttatus"
a_vector <- c("nusutus","decorus")
a_vector <- c(a_vector,a_variable)

length(a_vector)
class(a_vector)

another_vector <- 1:3
another_vector <- seq(3)
another_vector <- seq(from=2, to=30, by=2)


a_dataframe <- as.data.frame(cbind(a_vector,another_vector))

head(a_dataframe)
str(a_dataframe)

colnames(a_dataframe) <- c("species","index")

setwd("~/1b_postdoc/summer_2024_Rworkshop/")


#Data 

flowering_time <- read.table("flowering_data.txt",header = TRUE)

unique(flowering_time$Population)
nrow(flowering_time)
is.na(flowering_time)
flowering_time_clean <- na.omit(flowering_time)

a <- flowering_time$Date_of_first_flower
b <- na.omit(a)
c <- mean(b)

germination <- read.csv("germination_data.csv")
germination_clean <- germination[!(germination$Name==""),]


# Question 1: Of all pots planted, how many germinated?

a <- germination_clean[(germination_clean$Germinated=="Yes"),]
nrow(germination_clean[(germination_clean$Germinated=="Yes"),])

b <- nrow(germination_clean[(germination_clean$Germinated=="Yes"),]) / nrow(germination_clean) * 100

# Question 2 : Which population had the highest germination rate?

germination_props <- germination_clean %>% 
  group_by(Population,Germinated) %>% tally() %>% 
  mutate(prop = n/sum(n))

germination_props %>% filter(Germinated=="Yes") %>% arrange(desc(prop))

