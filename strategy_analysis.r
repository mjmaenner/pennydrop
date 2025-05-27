# strategy_analysis.r
#
# This script performs player-centric analysis of penny drop game strategies.
# It identifies optimal individual strategies for 2-player games and
# analyzes first-player advantage across varying starting penny counts.

# Source the penny drop game functions
source("pennydrop.r")

# --- Simulation Parameters for New Analysis ---
num_sims_per_opponent_strategy <- 500 # Sims for P1 strategy X vs. P2 strategy Y
pennies_values_for_advantage_analysis <- c(5, 10, 15, 20)
strategy_values_range <- 1:5 # Range of strategies for individual optimality and opponents
default_npennies_for_indiv_optimality <- 10 # Pennies for individual optimality analysis

cat("--- Starting Player-Centric Strategy Analysis ---\n")

# --- Individual Optimal Strategy (2-Player Games) ---
cat("\n--- Analyzing Individual Optimal Strategy for 2-Player Games ---\n")
cat(paste("Strategy values to test for each player:", paste(strategy_values_range, collapse=", "), "\n"))
cat(paste("Simulations per specific P1 vs P2 strategy matchup:", num_sims_per_opponent_strategy, "\n"))
cat(paste("Pennies per player for this analysis:", default_npennies_for_indiv_optimality, "\n\n"))

# --- For Player 1 ---
cat("--- Finding Optimal Individual Strategy for Player 1 ---\n")
p1_avg_win_rates <- data.frame(
  s1_strategy = integer(),
  avg_win_rate_vs_all_s2 = numeric()
)

for (s1 in strategy_values_range) {
  cat(paste("Analyzing P1 strategy:", s1, "\n"))
  p1_win_rates_for_current_s1 <- c()
  
  for (s2 in strategy_values_range) {
    cat(paste("  P1 (", s1, ") vs P2 (", s2, "): Running simulations...\n"))
    current_strategies_list <- list(c(s1, s2))
    sim_results <- run_simulations(
      n_simulations = num_sims_per_opponent_strategy,
      npennies = default_npennies_for_indiv_optimality,
      strategies_list = current_strategies_list
    )
    
    if (nrow(sim_results) > 0) {
      p1_wins <- sum(sim_results$winner_player_idx == 1, na.rm = TRUE)
      total_games <- nrow(sim_results)
      p1_win_rate_for_pair <- ifelse(total_games > 0, p1_wins / total_games, 0)
      p1_win_rates_for_current_s1 <- c(p1_win_rates_for_current_s1, p1_win_rate_for_pair)
      cat(paste("    P1 win rate (", s1, " vs ", s2, "): ", round(p1_win_rate_for_pair, 3), "\n"))
    } else {
      cat(paste("    No results for P1(", s1, ") vs P2(", s2, "). Assuming 0 win rate.\n"))
      p1_win_rates_for_current_s1 <- c(p1_win_rates_for_current_s1, 0)
    }
  }
  
  avg_s1_win_rate <- mean(p1_win_rates_for_current_s1, na.rm = TRUE)
  p1_avg_win_rates <- rbind(p1_avg_win_rates, data.frame(s1_strategy = s1, avg_win_rate_vs_all_s2 = avg_s1_win_rate))
  cat(paste("  Average win rate for P1 strategy", s1, "across all P2 strategies:", round(avg_s1_win_rate, 3), "\n"))
}

if (nrow(p1_avg_win_rates) > 0) {
  best_p1_strategy_row <- p1_avg_win_rates[which.max(p1_avg_win_rates$avg_win_rate_vs_all_s2), ]
  cat("\nOptimal Individual Strategy for Player 1 (averaged across P2 strategies 1-5):\n")
  print(best_p1_strategy_row)
} else {
  cat("\nCould not determine Player 1's optimal individual strategy.\n")
}

# --- For Player 2 ---
cat("\n--- Finding Optimal Individual Strategy for Player 2 ---\n")
p2_avg_win_rates <- data.frame(
  s2_strategy = integer(),
  avg_win_rate_vs_all_s1 = numeric()
)

for (s2 in strategy_values_range) {
  cat(paste("Analyzing P2 strategy:", s2, "\n"))
  p2_win_rates_for_current_s2 <- c()
  
  for (s1 in strategy_values_range) {
    cat(paste("  P2 (", s2, ") vs P1 (", s1, "): Running simulations...\n"))
    current_strategies_list <- list(c(s1, s2)) # P1 is s1, P2 is s2
    sim_results <- run_simulations(
      n_simulations = num_sims_per_opponent_strategy,
      npennies = default_npennies_for_indiv_optimality,
      strategies_list = current_strategies_list
    )
    
    if (nrow(sim_results) > 0) {
      p2_wins <- sum(sim_results$winner_player_idx == 2, na.rm = TRUE)
      total_games <- nrow(sim_results)
      p2_win_rate_for_pair <- ifelse(total_games > 0, p2_wins / total_games, 0)
      p2_win_rates_for_current_s2 <- c(p2_win_rates_for_current_s2, p2_win_rate_for_pair)
      cat(paste("    P2 win rate (", s2, " vs P1 ", s1, "): ", round(p2_win_rate_for_pair, 3), "\n"))
    } else {
      cat(paste("    No results for P2(", s2, ") vs P1(", s1, "). Assuming 0 win rate.\n"))
      p2_win_rates_for_current_s2 <- c(p2_win_rates_for_current_s2, 0)
    }
  }
  
  avg_s2_win_rate <- mean(p2_win_rates_for_current_s2, na.rm = TRUE)
  p2_avg_win_rates <- rbind(p2_avg_win_rates, data.frame(s2_strategy = s2, avg_win_rate_vs_all_s1 = avg_s2_win_rate))
  cat(paste("  Average win rate for P2 strategy", s2, "across all P1 strategies:", round(avg_s2_win_rate, 3), "\n"))
}

if (nrow(p2_avg_win_rates) > 0) {
  best_p2_strategy_row <- p2_avg_win_rates[which.max(p2_avg_win_rates$avg_win_rate_vs_all_s1), ]
  cat("\nOptimal Individual Strategy for Player 2 (averaged across P1 strategies 1-5):\n")
  print(best_p2_strategy_row)
} else {
  cat("\nCould not determine Player 2's optimal individual strategy.\n")
}


# --- First-Player Advantage Analysis (2-Player Games) ---
cat("\n\n--- Analyzing First-Player Advantage ---\n")
cat(paste("Penny values for analysis:", paste(pennies_values_for_advantage_analysis, collapse=", "), "\n"))
num_sims_advantage <- 1000 # As per instruction for this section
cat(paste("Simulations per matchup and penny count:", num_sims_advantage, "\n"))

matchups <- list(
  "S3_vs_S3" = c(3, 3),
  "S4_vs_S4" = c(4, 4)
  # Can add more matchups, e.g., using optimal strategies found above
  # For example, if P1_best_strat = 2 and P2_best_strat = 2:
  # "P1_Opt_vs_P2_Opt" = c(best_p1_strategy_row$s1_strategy[1], best_p2_strategy_row$s2_strategy[1])
  # This would require best_p1_strategy_row and best_p2_strategy_row to be available here.
  # For simplicity now, using fixed predefined matchups.
)
# Example of how to add the optimal matchup IF the variables are defined and rows exist:
# if(exists("best_p1_strategy_row") && nrow(best_p1_strategy_row) > 0 && 
#    exists("best_p2_strategy_row") && nrow(best_p2_strategy_row) > 0) {
#    matchups[["OptP1_vs_OptP2"]] <- c(best_p1_strategy_row$s1_strategy[1], best_p2_strategy_row$s2_strategy[1])
# }


advantage_analysis_results <- data.frame(
  npennies = integer(),
  matchup_name = character(),
  p1_strategy = integer(),
  p2_strategy = integer(),
  p1_win_rate = numeric(),
  stringsAsFactors = FALSE
)

for (np in pennies_values_for_advantage_analysis) {
  cat(paste("\nTesting with", np, "pennies per player:\n"))
  for (m_name in names(matchups)) {
    current_matchup_strategies <- matchups[[m_name]]
    cat(paste("  Matchup:", m_name, "- Strategies P1:", current_matchup_strategies[1], "P2:", current_matchup_strategies[2], "\n"))
    
    sim_results_adv <- run_simulations(
      n_simulations = num_sims_advantage,
      npennies = np,
      strategies_list = list(current_matchup_strategies) # run_simulations expects a list of strategy vectors
    )
    
    if (nrow(sim_results_adv) > 0) {
      p1_wins_adv <- sum(sim_results_adv$winner_player_idx == 1, na.rm = TRUE)
      total_games_adv <- nrow(sim_results_adv)
      p1_win_rate_adv <- ifelse(total_games_adv > 0, p1_wins_adv / total_games_adv, 0)
      
      advantage_analysis_results <- rbind(advantage_analysis_results, data.frame(
        npennies = np,
        matchup_name = m_name,
        p1_strategy = current_matchup_strategies[1],
        p2_strategy = current_matchup_strategies[2],
        p1_win_rate = p1_win_rate_adv
      ))
      cat(paste("    P1 win rate:", round(p1_win_rate_adv, 3), "\n"))
    } else {
      cat(paste("    No results for matchup", m_name, "with", np, "pennies. Skipping.\n"))
    }
  }
}

cat("\n--- First-Player Advantage Results Table ---\n")
print(advantage_analysis_results)

cat("\nDiscussion of First-Player Advantage:\n")
# Basic check: if P1 win rate is consistently > 0.5 for symmetric matchups (e.g. 3v3, 4v4)
# this suggests a first player advantage.
symmetric_matchups_results <- advantage_analysis_results[
    advantage_analysis_results$p1_strategy == advantage_analysis_results$p2_strategy,
]
if(nrow(symmetric_matchups_results) > 0) {
    avg_p1_win_rate_symmetric <- mean(symmetric_matchups_results$p1_win_rate, na.rm=TRUE)
    cat(paste("Average P1 win rate across symmetric matchups (e.g., 3v3, 4v4) and all tested penny counts:", round(avg_p1_win_rate_symmetric,3), "\n"))
    if(avg_p1_win_rate_symmetric > 0.52) { # Using 0.52 as a threshold to indicate some advantage
        cat("A consistent P1 advantage is suggested as the average P1 win rate in symmetric matchups is above 50%.\n")
    } else if (avg_p1_win_rate_symmetric < 0.48) {
        cat("A consistent P2 advantage is suggested in symmetric matchups (unexpected).\n")
    } else {
        cat("No strong consistent first-player advantage is immediately obvious from average P1 win rates in symmetric matchups.\n")
    }
    cat("Individual results per penny count and matchup provide more detail:\n")
    for(np_val in unique(symmetric_matchups_results$npennies)){
        cat(paste("  For", np_val, "pennies:\n"))
        np_data <- symmetric_matchups_results[symmetric_matchups_results$npennies == np_val, ]
        for(r_idx in 1:nrow(np_data)){
            cat(paste0("    Matchup ", np_data[r_idx, "matchup_name"], 
                      " (P1:", np_data[r_idx, "p1_strategy"], ", P2:", np_data[r_idx, "p2_strategy"], "): ",
                      "P1 Win Rate = ", round(np_data[r_idx, "p1_win_rate"], 3), "\n"))
        }
    }
} else {
    cat("No symmetric matchups were analyzed to directly comment on first-player advantage.\n")
}


# --- Old Analysis Sections (Commented Out) ---

# # --- Strategy Analysis for 2-Player Games (Old Optimal Pair Analysis) ---
# cat("--- Starting 2-Player Strategy Analysis ---\n")
# ... (rest of the old 2-player code) ...
# cat("\n--- 2-Player Strategy Analysis Complete ---\n")

# # --- Strategy Analysis for 3-Player Games ---
# cat("\n\n--- Starting 3-Player Strategy Analysis ---\n")
# ... (rest of the 3-player code) ...
# cat("\n--- 3-Player Strategy Analysis Complete ---\n")

cat("\n--- Script End ---\n")
