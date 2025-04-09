# Reliability of scales 
# R script created by Jacob J. Coutts
# SURV400

# load required packages
#install.packages("MBESS")
library(MBESS)

# read in the data
surveys <- read.table(file = "~/SURV400-AU24/scales.csv", sep = ",", header = TRUE)
colnames(surveys)[1] <- "rse_1" # rename first column

# summarize data and remove missing values
summary(surveys)
surveys <- na.omit(surveys)

# estimate Cronbach's alpha of RSE
MBESS::ci.reliability(surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")], type = "alpha")$est

# estimate McDonald's omega of RSE
MBESS::ci.reliability(surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")], type = "omega")$est

# create rse data frame to make computations easier
rse <- surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")]

# example of recoding reverse scored items for a 4 point scale
# always take the number of caterogies minus 1
rse$rse_1 <- 5-rse$rse_1

# leave one out approach to see why reliability is bad 
# create empty results vector
alpha_loo <- rep(NA, ncol(rse))

# for loop going over number of columns
for(i in 1:ncol(rse)){
  # remove each column in each step of the loop
  alpha_loo[i] <- MBESS::ci.reliability(rse[,-i], type = "omega")$est
}
# print results
alpha_loo # item one is the issue

# create total score
rse$rse_TOT <- rowSums(rse[, 2:6])
# alternative way to create total score 
rse$rse_TOT2 <- rse$rse_2 + rse$rse_3 + rse$rse_4 + rse$rse_5 + rse$rse_6



### end of script