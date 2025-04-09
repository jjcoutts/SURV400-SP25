# Monty Hall Problem Proof in R
# R script created by Dr. Jacob Coutts
# SURV400

## cretae a function with 3 arguments: one's choice of door, decision to switch or not, and the number of games played
mh_proof <- function(door_choice, switch,games_played = 5000){
  # create doors vector so we can establish winning condition
  doors = c(1,2,3)
  # start win counter at zero
  win = 0 
  
  # loop through many games played
  for(i in 1:games_played){
    # choose a random winning door for each game
    winning_door = sample(doors, size = 1)
    
    # program the two winning conditions -- choosing the wrong door and switching and choosing the right door and not switching
    if((winning_door == door_choice) & (switch == "No")){
      win = win + 1 # add one to win counter
    } else if((winning_door != door_choice) & (switch == "Yes")){
      win = win + 1 # add one to win counter
    }
  }
  # return object -- the probability of winning
  return(win/games_played)
}

# validate that the simulation worked. Should be ~ 2/3 for "Yes"
mh_proof(1, "Yes")

# should be ~ 1/3 for "No"
mh_proof(2, "No")

# Play around with the permutations. It's invariant to door choice and dependent on switching decision



### end of script