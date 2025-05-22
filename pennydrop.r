# penny drop simulator

roll <- function(){
  roller <- sample(1:6,1,replace=F)
  return(roller)
}

strategy <- function(gamestatus){
  # nplayers can be inferred from the length of gamestatus
  # length = 1 (pturn) + nplayers (pennies) + 5 (holes) + 1 (bank) + nplayers (strategies)
  # length = 2 * nplayers + 7
  # So, nplayers = (length(gamestatus) - 7) / 2
  nplayers <- (length(gamestatus) - 7) / 2

  # Accessing Hole Statuses (h1 to h5)
  # Holes are from index (nplayers + 2) to (nplayers + 6)
  hole_start_idx <- nplayers + 2
  hole_end_idx <- nplayers + 6
  up.pennies <- sum(gamestatus[hole_start_idx:hole_end_idx])

  # Accessing Current Player's Strategy
  pturn <- gamestatus[1] # Current player number (1-indexed)
  # Strategies start after pturn, nplayer pennies, 5 holes, 1 bank
  # Strategy block starts at index: 1 (pturn) + nplayers (pennies) + 5 (holes) + 1 (bank) + 1 = nplayers + 8
  # Player pturn's strategy is at index: (nplayers + 8) + pturn - 1
  player_strategy_idx <- nplayers + 7 + pturn
  strat <- gamestatus[player_strategy_idx]
  
  decision <- ifelse(up.pennies < strat, 1, 0)  
  return(decision)
}

pennydrop <- function(nplayers=length(strategies), npennies=10, strategies=c(3,3), printgame=FALSE){
  # gamestatus is a vector describing the status of the game
  # New structure: c(pturn, p1_penny, ..., pn_penny, h1, ..., h5, bank, s1_strat, ..., sn_strat)
  # Length: 1 (pturn) + nplayers (pennies) + 5 (holes) + 1 (bank) + nplayers (strategies) = 2 * nplayers + 7
  gamestatus <- c(
    1, # pturn
    rep(npennies, nplayers), # pennies for each player
    rep(0, 5), # h1-h5
    0, # bank
    strategies # strategies for each player
  )
  turns <- 1
  playermove <- 1

  # Loop while more than one player has pennies.
  player_pennies_indices <- 2:(1 + nplayers)
  while(sum(gamestatus[player_pennies_indices] > 0) > 1){
  
    pturn <- gamestatus[1]
    # If the current player is already out (has 0 pennies), skip to the next player.
    # This is necessary if the game continues until only one player has pennies.
    if (gamestatus[1 + pturn] == 0) {
      # Cycle through players: 1 -> 2 -> ... -> nplayers -> 1
      gamestatus[1] <- (pturn %% nplayers) + 1 
      turns <- turns +1 # A turn is consumed by skipping
      playermove <- 1 # Ensure the next player starts their move
      next # Skip the rest of the loop for this player
    }
    player_penny_idx <- 1 + pturn
    
    hole_start_idx <- 1 + nplayers + 1
    hole_end_idx <- 1 + nplayers + 5
    bank_idx <- 1 + nplayers + 5 + 1 # This is nplayers + 7
    
  #if(playermove ==1){
    rollresult <- roll()
    
    # Hole indices are 1-based for rollresult 1-5
    current_hole_idx <- 1 + nplayers + rollresult 

    if(rollresult < 6 && gamestatus[current_hole_idx] == 0 ){
      gamestatus[current_hole_idx] <- 1
    
      #deduct a penny from active player
      gamestatus[player_penny_idx] <- gamestatus[player_penny_idx] - 1
      playermove <- strategy(gamestatus)  # strategy function will need update too
      
      
    }else if(rollresult==6){
      gamestatus[bank_idx] <- gamestatus[bank_idx] + 1
      
      #deduct a penny from active player
      gamestatus[player_penny_idx] <- gamestatus[player_penny_idx] - 1
      playermove <- strategy(gamestatus)  # strategy function will need update too
      
        
    }else{ # Bad roll (rolled occupied slot or rolled > 5 and it wasn't 6 - though roll() only gives 1-6)
           # Assuming a roll that hits an occupied slot (1-5) is a "bad roll"
      num_pennies_in_holes <- sum(gamestatus[hole_start_idx:hole_end_idx])
      gamestatus[hole_start_idx:hole_end_idx] <- 0 # Clear all holes
      
      #add pennies to active player
      gamestatus[player_penny_idx] <- gamestatus[player_penny_idx] + num_pennies_in_holes
      playermove = 0 
      
    }
  #}
    #if they don't choose to roll, it defaults to pass, and they switch turns
    
  if(printgame==TRUE){
    print(gamestatus)
  }
    
    #switch turns
    if(playermove==0){
      # Cycle through players: 1 -> 2 -> ... -> nplayers -> 1
      gamestatus[1] <- (pturn %% nplayers) + 1 
      turns <- turns +1
      playermove <-1
    }
     
      }

  # Determine winner: the player who has 0 pennies.
  # This assumes the game stops as soon as one player reaches 0.
  # If multiple players could reach 0 simultaneously, this returns all of them.
  player_pennies <- gamestatus[player_pennies_indices]
  winner <- which(player_pennies == 0)
  
  # If, due to rules, only one player should be declared winner (e.g., the first to reach 0),
  # and 'winner' could contain multiple, one might take winner[1].
  # For Penny Drop, the first player to 0 causes the game to end.
  if (length(winner) > 1) {
    # This case should ideally not happen if game stops after first player is out.
    # If it can, specific tie-breaking rules would be needed.
    # For now, take the first one listed.
    winner <- winner[1]
  }

  return(c(winner,turns))
}  

# --- Old Examples (Commented Out due to changed function signature) ---
# These examples used stratp1 and stratp2 arguments which are no longer supported directly.
# The function now takes a 'strategies' vector and 'nplayers' is often inferred from its length.

# # games <- as.data.frame(t(replicate(20000,pennydrop(npennies=50,stratp1=3, stratp2=3),simplify = T)))
# # games <- as.data.frame(t(replicate(20000,pennydrop(npennies=10,stratp1=3, stratp2=3),simplify = T)))
# # table(games$V1)
# # pennydrop(stratp1=2, stratp2=3, printgame=TRUE)


# --- New Examples for Multi-Player Penny Drop ---

# Example 1: Run a single game with 3 players and print game status at each step.
# Player 1 strategy = 2, Player 2 strategy = 3, Player 3 strategy = 4
# Note: nplayers is automatically determined by the length of the strategies vector.
cat("\n--- Example 1: Single 3-Player Game (Printing Game Steps) ---\n")
pennydrop(npennies=10, strategies=c(2,3,4), printgame=TRUE)
# To explicitly set nplayers (though usually inferred):
# pennydrop(nplayers=3, npennies=10, strategies=c(2,3,4), printgame=TRUE)


# Example 2: Replicate a 4-player game 1000 times and tabulate the "winners"
# "Winner" in this context refers to the player(s) who reached 0 pennies.
# If the game is played until only one player has pennies, these are the players who were eliminated.
cat("\n--- Example 2: 1000 Replications of a 4-Player Game ---\n")
games_multi <- as.data.frame(t(replicate(1000, pennydrop(npennies=15, strategies=c(2,3,2,4)), simplify=T)))
# V1 contains the player number who ran out of pennies (or one of them, if simultaneous - though rare).
cat("Distribution of players running out of pennies (V1):\n")
print(table(games_multi$V1))
cat("Summary of turns (V2):\n")
print(summary(games_multi$V2))


# Example 3: A 2-player game using the new structure
cat("\n--- Example 3: Single 2-Player Game (New Structure) ---\n")
pennydrop(npennies=10, strategies=c(3,3), printgame=TRUE)




# --- Questions to Ask (remain relevant) ---
# game length vs # starting pennies and # players - both individually and together
# p1 advantage for pennies and players and strategies (p1 refers to player index 1)
# do different player indices show different game lengths if strategies are the same?

# average number of pennies up when rolling - vary by strategy
# average outcome of successful rolls - vary by strategy

# create a game summary that runs when a game is completed talking about win percents, game length, etc.
# For example, to see how often each player is the one to run out of pennies:
# if (exists("games_multi")) {
#   table(games_multi$V1)
# }
