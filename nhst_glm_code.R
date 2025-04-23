# Hypothesis Testing and Generalized Linear Model Lecture
# SURV400

# load required packages 
library(ggplot2) # create nice graphics
library(jtools) # make figures APA format

# read in the data
injuries <- read.csv(file = "~/SURV400-SP25/Data/injuries.csv") # read .csv file

# summarize the data 
summary(injuries) # obtain means and other descriptives of variables in data 
str(injuries) # see data types and variable heads (also visible in global envrionment)

# create correlation table for all variables in the model
cor(injuries)

# visualize the relationship between exhaustion and injury frequency
ggplot(data = injuries, aes(x = exhaust, y = injury)) + # x and y variable go here
  geom_point() + # create scatterplot
  jtools::theme_apa() + # make APA format
  labs(x = "Exhaustion", y = "Injury Frequency (6 months later)") + # change x- and y-axis labels
  geom_smooth(method = lm, color = "blue") # add a line of best fit 

# fit a simple regression model to the data
simp_reg <- glm(injury ~ exhaust, data = injuries) # simple regression with injuries as outcome and exhaustion as predictor 
summary(simp_reg) # extract results 
# Y = b0 + b1X
# Y = 1.53 + 0.13X 
# the effect of exhaustion is statistically significant
# Intercept: when exhaustion is at 0, someone is expected to have 1.53 injuries. Exhaustion ranges from 1-7 so this isn't a meaningful interpretation for the y-intercept
# Slope/effect of X: as exhaustion increases by 1 unit, injuries increases by 0.130 units

### OPTIONAL: if you wanted to do the hypothesis test manually
# test statistic calculation 
test_stat = 0.13001/0.05626; test_stat

# obtain p-value
(1-pt(test_stat,298))*2
### end optional content

# fit a multiple regression model to the data
mult_reg = lm(injury ~ exhaust + safety, data = injuries)
summary(mult_reg) # generate model results

# Y = b0 + b1X1 + b2X2
# the effect of exhaustion is no longer significant
# the effect of safety shortcuts is statistically significant
# Intercept: When exhaustion AND safety shortcuts are both at 0, someone is expected to have 0.77734 injuries. The scaling of both of these variables is 1-7, so once again, this is meaningless. 
# Slope/effect of exhaustion: As exhaustion increases by 1 unit, injuries are expected to increase by 0.055 units CONTROLLING for the effet of safety shortcuts
# Slope/effect of safety: As safety shortcuts increases by 1 unit, 0.246 more injuries are expected to happen. 

# these models can get crazy fast, and it's easy to add more of them in R. Just separate the variable names by addition symbols! 
big_mult_reg = lm(injury ~ exhaust + safety + injuryb + tenure + sex, data = injuries)
summary(big_mult_reg) # obtain model results
# only the effects of safety shortcuts and injury baseline are significant
# the interpretations are similar to above, the intercept is just when every variable in the model is 0 and the effects of specific variables are a one-unit increase in that variable holding everything else constant. 



### End of script