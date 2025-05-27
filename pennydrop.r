# penny drop simulator

roll <- function(){
  roller <- sample(1:6,1,replace=F)
  return(roller)
}

strategy <- function(gamestatus){
  # Get pturn (current player index)
  pturn <- gamestatus$game_info$current_player_idx

  # Calculate up.pennies (sum of pennies in slots 1-5)
  # The board_slots are: slot1, slot2, slot3, slot4, slot5, bank
  up.pennies <- sum(gamestatus$board_slots[1:5])

  # Get current player's strategy
  strat <- gamestatus$players[[pturn]]$strategy
  
  decision <- ifelse(up.pennies < strat, 1, 0)  
  return(decision)
}

pennydrop <- function(nplayers=length(strategies), npennies=10, strategies=c(3,3), printgame=FALSE){
  # gamestatus is now a list with board_slots, players, and game_info

  gamestatus <- list(
    board_slots = c(slot1=0, slot2=0, slot3=0, slot4=0, slot5=0, bank=0),
    players = lapply(1:nplayers, function(i) { # nplayers is directly from function arguments
      list(pennies = npennies, strategy = strategies[i])
    }),
    game_info = list(current_player_idx = 1, turns_taken = 1)
  )
  
  # The rest of the game logic will need to be updated to use this new gamestatus structure.
  # For now, the subtask is only to change the initialization.
  # The existing logic below will likely break.

  
  playermove <- 1 # Retained: 1 for roll, 0 for pass
  nplayers_val <- length(gamestatus$players) # nplayers for convenience

  # Loop while all players have pennies.
  while(all(sapply(gamestatus$players, function(p) p$pennies > 0))){
  
    pturn <- gamestatus$game_info$current_player_idx
    
    # Skip player if they are already out of pennies (should not happen with the while condition, but good for robustness)
    if (gamestatus$players[[pturn]]$pennies == 0) {
      gamestatus$game_info$current_player_idx <- (pturn %% nplayers_val) + 1 
      gamestatus$game_info$turns_taken <- gamestatus$game_info$turns_taken + 1
      playermove <- 1 # Ensure the next player starts their move
      next 
    }
    
    if(playermove == 1){ # Player chose to roll (or it's the start of their turn)
      rollresult <- roll()
    
      if(rollresult < 6){ # Rolled a slot 1-5
        if(gamestatus$board_slots[[rollresult]] == 0 ){ # Slot is empty
          gamestatus$board_slots[[rollresult]] <- 1
          gamestatus$players[[pturn]]$pennies <- gamestatus$players[[pturn]]$pennies - 1
          # Player decides to roll again or pass
          playermove <- strategy(gamestatus) 
        } else { # Slot is occupied - bad roll
          num_pennies_in_holes <- sum(gamestatus$board_slots[1:5])
          gamestatus$board_slots[1:5] <- 0 # Clear all slots 1-5
          gamestatus$players[[pturn]]$pennies <- gamestatus$players[[pturn]]$pennies + num_pennies_in_holes
          playermove <- 0 # Player loses turn
        }
      } else if(rollresult == 6){ # Rolled a 6 - penny to the bank
        gamestatus$board_slots[["bank"]] <- gamestatus$board_slots[["bank"]] + 1
        gamestatus$players[[pturn]]$pennies <- gamestatus$players[[pturn]]$pennies - 1
        # Player decides to roll again or pass
        playermove <- strategy(gamestatus)
      }
      # No 'else' needed here as roll() only returns 1-6
    }
    
    if(printgame==TRUE){
      print(gamestatus)
    }
    
    # Switch turns if player chose to pass (playermove == 0) or after a bad roll
    if(playermove == 0){
      gamestatus$game_info$current_player_idx <- (pturn %% nplayers_val) + 1 
      gamestatus$game_info$turns_taken <- gamestatus$game_info$turns_taken + 1
      playermove <- 1 # Reset for next player's turn
    }
     
  } # End of while loop

  # Determine winner: (This section will be updated in the next subtask)
  # The game ends as soon as any player reaches 0 pennies.
  # The loop condition `all(sapply(gamestatus$players, function(p) p$pennies > 0))` ensures this.
  # The player who caused the condition to be false is the one whose pennies just became <= 0.
  # However, the subtask asks to leave winner determination for later.
  # The old logic below will likely not work correctly without updates.
  
  player_pennies <- sapply(gamestatus$players, function(p) p$pennies) # Renamed for clarity from problem desc.
  winner <- which(player_pennies == 0) 
  
  # If multiple players reach 0 pennies simultaneously (rare), take the first one listed.
  if (length(winner) > 1) {
    winner <- winner[1]
  }
  # Note: If length(winner) == 0, it means no player has 0 pennies.
  # This should ideally not be reached if the main game loop `while(all(sapply(gamestatus$players, function(p) p$pennies > 0)))`
  # correctly terminates only when at least one player has 0 pennies.
  # If it's possible for the loop to terminate under other conditions in the future,
  # this section might need to handle `length(winner) == 0` (e.g., by returning NA or error).
  # For now, assuming the loop condition implies a winner will be found.

  return(c(winner, gamestatus$game_info$turns_taken)) # Use updated turns_taken
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
