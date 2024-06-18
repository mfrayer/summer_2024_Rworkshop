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

setwd("~/summer_2024_Rworkshop/")


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



###############################################################################
### Day 2 
###############################################################################

setwd("~/summer_2024_Rworkshop/")

library(tidyverse)

flowering_time <- read.table("flowering_data.txt",header = TRUE)
flowering_time_clean <- na.omit(flowering_time)

germination <- read.csv("germination_data.csv")
germination_clean <- germination[!(germination$Name==""),]

# Question 3 : What was the average number of days to first flower for all of the plants? 

flowering_time_clean$Date_of_first_flower_2 <- mdy(flowering_time_clean$Date_of_first_flower)
flowering_time_clean$Date_of_planting <- mdy("March 21, 2023")
flowering_time_clean$flowering_time <- flowering_time_clean$Date_of_first_flower_2 - 
  flowering_time_clean$Date_of_planting 

mean(flowering_time_clean$flowering_time)

flowering_time_clean$Date_of_planting[flowering_time_clean$Population == "OD" | flowering_time_clean$Population == "ODG" ] <- 
  mdy("April 26, 2023")

flowering_time_clean <- flowering_time_clean %>% mutate(Date_of_planting = if_else(Population == "OD" | Population == "ODG", mdy("April 26, 2023"), Date_of_planting))

flowering_time_clean$flowering_time <- flowering_time_clean$Date_of_first_flower_2 - 
  flowering_time_clean$Date_of_planting 

mean(flowering_time_clean$flowering_time)

## GGPLOT

plot <- ggplot(flowering_time_clean, aes(Population,as.numeric(flowering_time)))

plot + geom_boxplot() +
  geom_hline(yintercept = mean(flowering_time_clean$flowering_time)) +
  labs(y="Flowering Time (Days)",title="Flowering Time by Population") +
  theme_bw() + theme(axis.text.x=element_text(angle=90))


plot + geom_boxplot() +
  geom_point(data=flowering_time_clean, aes(x=Population,y=flowering_time,color=Population),shape=6, alpha=0.5)


plot + geom_boxplot() + annotate("text", x="ODG", y=65, label="Total Sample Size = 69")

big_label <- paste("Total Sample Size = ",nrow(flowering_time_clean),sep="")
plot + geom_boxplot() + annotate("text", x="ODG", y=65, label=big_label)

plot + geom_boxplot() + geom_point(data=flowering_time_clean, aes(x=Population,y=flowering_time)) + annotate("text", x=c("CM","CMG","OD","ODG","ODW","ODWD","ODWG"), y=65, label=c(21,4,6,10,17,9,2))

plot + geom_boxplot() + 
  geom_text(data=flowering_time_clean %>% group_by(Population) %>% summarise(top=max(flowering_time), n=n()), aes(x=Population, y=top+2, label= paste0("n = ", n)), nudge_y=1)



germination_summary <- germination_clean %>% group_by(Population,Germinated,GA_treated) %>% tally()

ggplot(germination_summary, aes(Germinated,n,fill=Population)) + geom_col()

ggplot(germination_summary, aes(Germinated,n,fill=GA_treated)) + geom_col() + labs(y="Count",fill="GA Treated") + facet_wrap(~Population)

ggplot(germination_summary, aes(GA_treated,n,fill=Germinated)) + geom_col() + labs(y="Count") + facet_wrap(~Population)

