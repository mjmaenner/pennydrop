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

  # Extract player strategies
  player_strategies <- sapply(gamestatus$players, function(p) p$strategy)
  
  # Final penny counts
  final_penny_counts <- sapply(gamestatus$players, function(p) p$pennies)

  return(list(
    winner_player_idx = winner,
    turns_taken = gamestatus$game_info$turns_taken,
    player_strategies = player_strategies,
    final_penny_counts = final_penny_counts
  ))
}

# --- Old Examples (Commented Out due to changed function signature & new return type) ---
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
game_result_ex1 <- pennydrop(npennies=10, strategies=c(2,3,4), printgame=TRUE)
cat("\n--- Result of Example 1: ---\n")
print(game_result_ex1)
# To explicitly set nplayers (though usually inferred):
# game_result_ex1_b <- pennydrop(nplayers=3, npennies=10, strategies=c(2,3,4), printgame=TRUE)
# print(game_result_ex1_b)


# Example 2: Replicate a 4-player game 1000 times and tabulate the "winners"
# "Winner" in this context refers to the player(s) who reached 0 pennies.
# The output of pennydrop is now a list, so direct replication into a data.frame needs adjustment.
cat("\n--- Example 2: 1000 Replications of a 4-Player Game (Illustrative - results not directly comparable to old table) ---\n")
# This example shows how to collect list outputs. For table summaries, you'd process the list of lists.
results_ex2 <- replicate(100, pennydrop(npennies=15, strategies=c(2,3,2,4)), simplify=FALSE) # Reduced from 1000 for brevity
# To see winner distribution:
# winner_dist_ex2 <- table(sapply(results_ex2, function(res) res$winner_player_idx))
# cat("Distribution of players running out of pennies (winner_player_idx):\n")
# print(winner_dist_ex2)
# To see turns summary:
# turns_summary_ex2 <- summary(sapply(results_ex2, function(res) res$turns_taken))
# cat("Summary of turns (turns_taken):\n")
# print(turns_summary_ex2)
cat("First few results from Example 2 replication:\n")
print(head(results_ex2, 2))


# Example 3: A 2-player game using the new structure
cat("\n--- Example 3: Single 2-Player Game (New Structure) ---\n")
game_result_ex3 <- pennydrop(npennies=10, strategies=c(3,3), printgame=TRUE)
cat("\n--- Result of Example 3: ---\n")
print(game_result_ex3)


# --- New Function: run_simulations ---
run_simulations <- function(n_simulations, npennies, strategies_list) {
  all_results_list <- list()
  
  max_players_overall <- 0
  if (length(strategies_list) > 0) {
    max_players_overall <- max(sapply(strategies_list, length))
  }

  for (strategy_combo_idx in seq_along(strategies_list)) {
    current_strategies <- strategies_list[[strategy_combo_idx]]
    current_n_players <- length(current_strategies)
    
    if (current_n_players == 0) {
      warning(paste("Strategy set", strategy_combo_idx, "is empty. Skipping."))
      next
    }
    
    cat(paste("\nRunning simulations for strategy set", strategy_combo_idx, ":", paste(current_strategies, collapse=", "), "\n"))
    
    for (sim_id in 1:n_simulations) {
      if (sim_id %% (n_simulations/10) == 0 || sim_id == 1 || sim_id == n_simulations) { # Print progress
          cat(paste("  Simulating game", sim_id, "of", n_simulations, "for strategy set", strategy_combo_idx, "\n"))
      }
      
      game_res <- pennydrop(nplayers = current_n_players, npennies = npennies, strategies = current_strategies, printgame = FALSE)
      
      # Prepare a flat list for this game's results
      # Player strategies and final pennies need to be padded to max_players_overall if necessary
      # for consistent data frame structure, though this adds complexity if player counts vary widely.
      # For now, let's store them as is and handle potential raggedness later or by ensuring
      # strategies_list implies a consistent number of players for typical use cases.
      # A simpler approach for the data frame is to make strategy_vector and final_pennies_vector columns
      # that store the vectors directly, or handle padding when creating the final DF.

      # For creating a wide data frame, we need to ensure all rows have the same number of columns.
      # We'll pad with NA for strategy sets with fewer players than max_players_overall.
      
      res_row <- list(
        simulation_run_id = sim_id, # simulation_id within this strategy set
        strategy_combination_id = strategy_combo_idx,
        winner_player_idx = game_res$winner_player_idx,
        turns_taken = game_res$turns_taken
      )
      
      # Add player strategies
      for (p_idx in 1:max_players_overall) {
        if (p_idx <= current_n_players) {
          res_row[[paste0("player", p_idx, "_strategy")]] <- game_res$player_strategies[p_idx]
        } else {
          res_row[[paste0("player", p_idx, "_strategy")]] <- NA # Pad with NA
        }
      }
      
      # Add final penny counts
      for (p_idx in 1:max_players_overall) {
        if (p_idx <= current_n_players) {
          res_row[[paste0("player", p_idx, "_final_pennies")]] <- game_res$final_penny_counts[p_idx]
        } else {
          res_row[[paste0("player", p_idx, "_final_pennies")]] <- NA # Pad with NA
        }
      }
      
      all_results_list[[length(all_results_list) + 1]] <- res_row
    }
  }
  
  if (length(all_results_list) == 0) {
    # Return an empty data frame with expected column names if no simulations were run
    # This helps prevent errors downstream if strategies_list was empty.
    # Define column names based on max_players_overall, even if it's 0 (no player columns then)
    col_names <- c("simulation_run_id", "strategy_combination_id", "winner_player_idx", "turns_taken")
    if (max_players_overall > 0) {
        for (p_idx in 1:max_players_overall) {
            col_names <- c(col_names, paste0("player", p_idx, "_strategy"))
        }
        for (p_idx in 1:max_players_overall) {
            col_names <- c(col_names, paste0("player", p_idx, "_final_pennies"))
        }
    }
    return(data.frame(matrix(ncol = length(col_names), nrow = 0, dimnames = list(NULL, col_names))))
  }
  
  # Convert list of lists to data frame
  # do.call(rbind, lapply(all_results_list, data.frame)) might be slow for large lists / many columns
  # Using dplyr::bind_rows or data.table::rbindlist is often more efficient but adds dependencies.
  # For base R, constructing the data frame column by column or using a more complex rbind can be better.
  # Given the environment, a simpler `do.call(rbind.data.frame, all_results_list)` is used.
  # Note: `rbind.data.frame` can be very slow for many rows.
  # A more robust way for base R:
  results_df <- do.call(rbind, lapply(all_results_list, function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
  
  return(results_df)
}

# --- Example for run_simulations ---
cat("\n--- Example 4: Running Simulations for Multiple Strategy Sets ---\n")
# Define strategy sets for 2 players
strategies_to_test_2p <- list(
  c(3, 3), # Both players use strategy 3
  c(2, 4)  # Player 1 uses strategy 2, Player 2 uses strategy 4
)

# Run 100 simulations for each 2-player strategy set with 10 pennies each
# Reduced n_simulations to 20 for faster example execution.
sim_results_2p <- run_simulations(n_simulations = 20, npennies = 10, strategies_list = strategies_to_test_2p)
cat("\n--- Simulation Results (2 Players): ---\n")
print(head(sim_results_2p))
summary(sim_results_2p)

# Define strategy sets for 3 players
strategies_to_test_3p <- list(
  c(2, 3, 4), # Strategies 2, 3, 4
  c(5, 5, 5)  # All players use strategy 5
)
# Run 100 simulations for each 3-player strategy set with 12 pennies each
# Reduced n_simulations to 20 for faster example execution.
sim_results_3p <- run_simulations(n_simulations = 20, npennies = 12, strategies_list = strategies_to_test_3p)
cat("\n--- Simulation Results (3 Players): ---\n")
print(head(sim_results_3p))
summary(sim_results_3p)

# Example with an empty strategy list
cat("\n--- Example 5: Running Simulations with Empty Strategy List ---\n")
empty_sim_results <- run_simulations(n_simulations = 10, npennies = 10, strategies_list = list())
cat("\n--- Simulation Results (Empty List): ---\n")
print(empty_sim_results)

# Example with mixed number of players (e.g. one 2-player strategy, one 3-player strategy)
# This will create columns up to player3_strategy and player3_final_pennies,
# with NAs for the 2-player games in the player3 columns.
cat("\n--- Example 6: Running Simulations for Mixed Player Counts ---\n")
strategies_to_test_mixed <- list(
  c(3,3),    # 2 players
  c(2,3,4)   # 3 players
)
sim_results_mixed <- run_simulations(n_simulations = 10, npennies = 10, strategies_list = strategies_to_test_mixed)
cat("\n--- Simulation Results (Mixed Players): ---\n")
print(sim_results_mixed)


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
