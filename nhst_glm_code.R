# R script 
# GLM lecture

# load required packages 
library(ggplot2)
library(jtools)

# read in the data
injuries <- read.table(file = "~/SURV400-SP25/Data/injuries.csv", sep = ",", header = TRUE)

# summarize the data 
summary(injuries)

# create correlation table for descriptives
cor(injuries)

# visualize the relationship between exhaustion and injury frequency
ggplot(data = injuries) + 
  geom_point(aes(x = exhaust, y = injury)) + 
  jtools::theme_apa() + labs(x = "Exhaustion", y = "Injury Frequency (6 months later)")

# fit the model to the data
injure_model <- glm(injury ~ exhaust, data = injuries)
summary(injure_model) # extract results

(1-pt(0.13001/0.05626,298))*2


### End of script