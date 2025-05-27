# strategy_analysis.r
#
# This script analyzes penny drop game strategies by running simulations
# and calculating win rates for different strategy combinations.

# Source the penny drop game functions
source("pennydrop.r")

# --- Simulation Parameters ---
num_sims <- 1000 # Number of simulations per strategy combination (reduced for faster testing, can be increased)
pennies_per_player <- 10 # Number of pennies each player starts with
strategy_values_2p <- 1:5 # Range of strategy values to test for 2-player games (e.g., 1 to 5)

# --- Strategy Analysis for 2-Player Games ---
cat("--- Starting 2-Player Strategy Analysis ---\n")

# Create a list of strategy combinations to test
strategies_to_test_2p <- list()
for (s1 in strategy_values_2p) {
  for (s2 in strategy_values_2p) {
    strategies_to_test_2p <- append(strategies_to_test_2p, list(c(s1, s2)))
  }
}

cat(paste("Number of 2-player strategy combinations to test:", length(strategies_to_test_2p), "\n"))
cat(paste("Simulations per combination:", num_sims, "\n"))
cat(paste("Total simulations to run for 2-player analysis:", length(strategies_to_test_2p) * num_sims, "\n\n"))

# Run simulations
# Note: The run_simulations function prints progress for each strategy set and simulation batch.
results_2p <- run_simulations(
  n_simulations = num_sims,
  npennies = pennies_per_player,
  strategies_list = strategies_to_test_2p
)

cat("\n--- Processing 2-Player Simulation Results ---\n")

if (nrow(results_2p) > 0) {
  # Calculate win rates
  # We need to map strategy_combination_id back to the actual strategies
  # Or, more directly, use player1_strategy and player2_strategy columns.
  
  # Aggregate results by strategy combination
  # Columns of interest: player1_strategy, player2_strategy, winner_player_idx
  
  # Create a summary data frame
  # For each unique strategy combination, count wins for P1 and P2
  
  # Using dplyr would be cleaner here, but sticking to base R as per previous work.
  # results_2p$p1_strat <- sapply(results_2p$strategy_combination_id, function(id) strategies_to_test_2p[[id]][1])
  # results_2p$p2_strat <- sapply(results_2p$strategy_combination_id, function(id) strategies_to_test_2p[[id]][2])
  # The above is not needed because playerX_strategy is already in results_2p

  # Let's ensure player strategies are correctly represented for aggregation
  # The run_simulations function should already provide player1_strategy and player2_strategy
  
  # Calculate win counts for each strategy combination
  win_counts <- aggregate(
    winner_player_idx ~ player1_strategy + player2_strategy,
    data = results_2p,
    FUN = function(x) {
      c(
        p1_wins = sum(x == 1, na.rm = TRUE),
        p2_wins = sum(x == 2, na.rm = TRUE),
        total_games = length(x)
      )
    }
  )
  
  # The result of aggregate with a matrix function is a bit complex. Let's simplify.
  # Alternative: Summarize by creating factors for strategies
  results_2p$strat_key <- paste(results_2p$player1_strategy, results_2p$player2_strategy, sep = "-")
  
  summary_stats_list <- lapply(split(results_2p, results_2p$strat_key), function(combo_data) {
    s1 <- combo_data$player1_strategy[1]
    s2 <- combo_data$player2_strategy[1]
    
    p1_wins <- sum(combo_data$winner_player_idx == 1, na.rm = TRUE)
    p2_wins <- sum(combo_data$winner_player_idx == 2, na.rm = TRUE)
    # total_games should be num_sims for each combo if all went well
    total_games_in_combo <- nrow(combo_data) 
    
    data.frame(
      s1 = s1,
      s2 = s2,
      p1_win_rate = p1_wins / total_games_in_combo,
      p2_win_rate = p2_wins / total_games_in_combo,
      n_games = total_games_in_combo
    )
  })
  
  analysis_summary_2p <- do.call(rbind, summary_stats_list)
  rownames(analysis_summary_2p) <- NULL # Clean row names
  
  cat("\n--- 2-Player Strategy Win Rates Table ---\n")
  print(analysis_summary_2p)
  
  # Identify optimal strategies
  if (nrow(analysis_summary_2p) > 0) {
    # Optimal for Player 1
    opt_p1 <- analysis_summary_2p[which.max(analysis_summary_2p$p1_win_rate), ]
    cat(paste0("\nOptimal strategy for Player 1 (vs tested range for P2):\n"))
    # If multiple strategies have the same max win rate, this picks the first.
    print(opt_p1)
    
    # Optimal for Player 2
    opt_p2 <- analysis_summary_2p[which.max(analysis_summary_2p$p2_win_rate), ]
    cat(paste0("\nOptimal strategy for Player 2 (vs tested range for P1):\n"))
    print(opt_p2)
  } else {
    cat("\nNo summary data generated for optimal strategy identification.\n")
  }
  
} else {
  cat("No simulation results to analyze for 2-player games.\n")
}

cat("\n--- 2-Player Strategy Analysis Complete ---\n")

# --- Strategy Analysis for 3-Player Games ---
cat("\n\n--- Starting 3-Player Strategy Analysis ---\n")

strategy_values_3p <- 1:3 # Range of strategy values for 3-player games (reduced to keep combinations manageable)
num_sims_3p <- 500      # Number of simulations for 3-player games (can be adjusted)

# Create a list of strategy combinations to test
strategies_to_test_3p <- list()
for (s1 in strategy_values_3p) {
  for (s2 in strategy_values_3p) {
    for (s3 in strategy_values_3p) {
      strategies_to_test_3p <- append(strategies_to_test_3p, list(c(s1, s2, s3)))
    }
  }
}

cat(paste("Number of 3-player strategy combinations to test:", length(strategies_to_test_3p), "\n"))
cat(paste("Simulations per combination:", num_sims_3p, "\n"))
cat(paste("Total simulations to run for 3-player analysis:", length(strategies_to_test_3p) * num_sims_3p, "\n\n"))

# Run simulations for 3-player games
results_3p <- run_simulations(
  n_simulations = num_sims_3p,
  npennies = pennies_per_player, # Using the same pennies_per_player as 2-player
  strategies_list = strategies_to_test_3p
)

cat("\n--- Processing 3-Player Simulation Results ---\n")

if (nrow(results_3p) > 0) {
  # Calculate win rates
  results_3p$strat_key <- paste(results_3p$player1_strategy, results_3p$player2_strategy, results_3p$player3_strategy, sep = "-")
  
  summary_stats_list_3p <- lapply(split(results_3p, results_3p$strat_key), function(combo_data) {
    s1 <- combo_data$player1_strategy[1]
    s2 <- combo_data$player2_strategy[1]
    s3 <- combo_data$player3_strategy[1]
    
    p1_wins <- sum(combo_data$winner_player_idx == 1, na.rm = TRUE)
    p2_wins <- sum(combo_data$winner_player_idx == 2, na.rm = TRUE)
    p3_wins <- sum(combo_data$winner_player_idx == 3, na.rm = TRUE)
    total_games_in_combo <- nrow(combo_data)
    
    data.frame(
      s1 = s1,
      s2 = s2,
      s3 = s3,
      p1_win_rate = p1_wins / total_games_in_combo,
      p2_win_rate = p2_wins / total_games_in_combo,
      p3_win_rate = p3_wins / total_games_in_combo,
      n_games = total_games_in_combo
    )
  })
  
  analysis_summary_3p <- do.call(rbind, summary_stats_list_3p)
  rownames(analysis_summary_3p) <- NULL # Clean row names
  
  cat("\n--- 3-Player Strategy Win Rates Table ---\n")
  print(analysis_summary_3p)
  
  # Identify optimal strategies
  if (nrow(analysis_summary_3p) > 0) {
    opt_p1_3p <- analysis_summary_3p[which.max(analysis_summary_3p$p1_win_rate), ]
    cat(paste0("\nOptimal strategy for Player 1 in 3-player games (vs tested range for P2, P3):\n"))
    print(opt_p1_3p)
    
    opt_p2_3p <- analysis_summary_3p[which.max(analysis_summary_3p$p2_win_rate), ]
    cat(paste0("\nOptimal strategy for Player 2 in 3-player games (vs tested range for P1, P3):\n"))
    print(opt_p2_3p)
    
    opt_p3_3p <- analysis_summary_3p[which.max(analysis_summary_3p$p3_win_rate), ]
    cat(paste0("\nOptimal strategy for Player 3 in 3-player games (vs tested range for P1, P2):\n"))
    print(opt_p3_3p)
  } else {
    cat("\nNo summary data generated for optimal strategy identification in 3-player games.\n")
  }
  
} else {
  cat("No simulation results to analyze for 3-player games.\n")
}

cat("\n--- 3-Player Strategy Analysis Complete ---\n")

cat("\n--- Script End ---\n")
