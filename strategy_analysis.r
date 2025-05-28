# strategy_analysis.r
#
# This script performs player-centric analysis of penny drop game strategies.
# It identifies optimal individual strategies for 2-player games and
# analyzes first-player advantage across varying starting penny counts.

# --- Package Management ---
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr", repos = "http://cran.us.r-project.org")
}
library(knitr)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}
library(ggplot2)

# dplyr is used later, ensure it's loaded (already added in previous step, but good to consolidate)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  cat("Installing dplyr...\n") # Should already be installed by now
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
}
library(dplyr)


# --- Directory Setup ---
if (!dir.exists("graphs")) {
  dir.create("graphs")
  cat("Created 'graphs' directory.\n")
}

# Source the penny drop game functions
source("pennydrop.r")

# --- Simulation Parameters for New Analysis (REDUCED FOR TIMEOUT AVOIDANCE) ---
num_sims_per_opponent_strategy <- 100 # Sims for P1 strategy X vs. P2 strategy Y (Reduced from 500)
pennies_values_for_advantage_analysis <- c(5, 10, 15, 20)
strategy_values_range <- 1:5 # Range of strategies for individual optimality and opponents
default_npennies_for_indiv_optimality <- 10 # Pennies for individual optimality analysis

cat("--- Starting Player-Centric Strategy Analysis ---\n")
cat("--- NOTE: SIMULATION NUMBERS REDUCED TO PREVENT TIMEOUTS ---\n")

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
  cat("\n--- Player 1 Average Win Rates by Strategy ---\n")
  print(kable(p1_avg_win_rates, format = "pipe", caption = "Player 1: Average Win Rate by Strategy (vs All P2 Strategies)"))
  
  plot_p1_optimal_strategy <- ggplot(p1_avg_win_rates, aes(x = factor(s1_strategy), y = avg_win_rate_vs_all_s2)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Player 1: Average Win Rate by Strategy (vs All P2 Strategies)",
         x = "Player 1 Strategy",
         y = "Average Win Rate") +
    theme_minimal()
  ggsave("graphs/p1_optimal_strategy.png", plot = plot_p1_optimal_strategy, width = 8, height = 6)
  cat("\nSaved Player 1 optimal strategy graph to graphs/p1_optimal_strategy.png\n")

  best_p1_strategy_row <- p1_avg_win_rates[which.max(p1_avg_win_rates$avg_win_rate_vs_all_s2), ]
  cat("\n--- Optimal Individual Strategy for Player 1 ---\n")
  print(kable(best_p1_strategy_row, format = "pipe", caption = "Optimal Individual Strategy for Player 1 (Averaged Across P2 Strategies)"))
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
  cat("\n--- Player 2 Average Win Rates by Strategy ---\n")
  print(kable(p2_avg_win_rates, format = "pipe", caption = "Player 2: Average Win Rate by Strategy (vs All P1 Strategies)"))

  plot_p2_optimal_strategy <- ggplot(p2_avg_win_rates, aes(x = factor(s2_strategy), y = avg_win_rate_vs_all_s1)) +
    geom_bar(stat = "identity", fill = "coral") +
    labs(title = "Player 2: Average Win Rate by Strategy (vs All P1 Strategies)",
         x = "Player 2 Strategy",
         y = "Average Win Rate") +
    theme_minimal()
  ggsave("graphs/p2_optimal_strategy.png", plot = plot_p2_optimal_strategy, width = 8, height = 6)
  cat("\nSaved Player 2 optimal strategy graph to graphs/p2_optimal_strategy.png\n")

  best_p2_strategy_row <- p2_avg_win_rates[which.max(p2_avg_win_rates$avg_win_rate_vs_all_s1), ]
  cat("\n--- Optimal Individual Strategy for Player 2 ---\n")
  print(kable(best_p2_strategy_row, format = "pipe", caption = "Optimal Individual Strategy for Player 2 (Averaged Across P1 Strategies)"))
} else {
  cat("\nCould not determine Player 2's optimal individual strategy.\n")
}


# --- First-Player Advantage Analysis (2-Player Games) ---
cat("\n\n--- Analyzing First-Player Advantage ---\n")
cat(paste("Penny values for analysis:", paste(pennies_values_for_advantage_analysis, collapse=", "), "\n"))
num_sims_advantage <- 200 # Reduced from 1000
cat(paste("Simulations per matchup and penny count:", num_sims_advantage, "\n"))

# Ensure matchups are defined (copied from original context, ensure it's run)
matchups <- list(
  "S3_vs_S3" = c(3, 3),
  "S4_vs_S4" = c(4, 4)
)
# Add optimal matchup if variables exist
if(exists("best_p1_strategy_row") && nrow(best_p1_strategy_row) > 0 && 
   exists("best_p2_strategy_row") && nrow(best_p2_strategy_row) > 0) {
   matchups[["OptP1_vs_OptP2"]] <- c(best_p1_strategy_row$s1_strategy[1], best_p2_strategy_row$s2_strategy[1])
   cat("Added OptP1_vs_OptP2 matchup to advantage analysis.\n")
}


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
      strategies_list = list(current_matchup_strategies) 
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
print(kable(advantage_analysis_results, format="pipe", caption="First Player (P1) Win Rate by Starting Pennies and Matchup"))

if (nrow(advantage_analysis_results) > 0) {
  plot_first_player_advantage <- ggplot(advantage_analysis_results, 
                                      aes(x = npennies, y = p1_win_rate, color = matchup_name, group = matchup_name)) +
    geom_line() +
    geom_point() +
    labs(title = "First Player (P1) Win Rate by Starting Pennies and Matchup",
         x = "Number of Starting Pennies",
         y = "P1 Win Rate",
         color = "Matchup") +
    theme_minimal()
  ggsave("graphs/first_player_advantage_vs_pennies.png", plot = plot_first_player_advantage, width = 10, height = 6)
  cat("\nSaved First-Player Advantage graph to graphs/first_player_advantage_vs_pennies.png\n")
}


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

# --- Strategy Analysis for 3-Player Games ---
cat("\n\n--- Starting 3-Player Strategy Analysis ---\n")

# --- Simulation Parameters for 3-Player Analysis ---
num_sims_per_3p_combo <- 50 # Drastically reduced from 200 to avoid timeout
strategy_values_3p_range <- 1:4 # Reduced range to avoid timeout, was 1:5
default_npennies_3p <- 10
cat(paste("Strategies for each player (P1, P2, P3):", paste(strategy_values_3p_range, collapse=", "), "\n"))
cat(paste("Simulations per 3-player strategy combination:", num_sims_per_3p_combo, "\n"))
cat(paste("Pennies per player for this analysis:", default_npennies_3p, "\n\n"))

# Initialize a data frame to store detailed results for preceding player analysis
# This will be aggregated later.
raw_3p_results <- data.frame(
  s1 = integer(),
  s2 = integer(),
  s3 = integer(),
  winner_player_idx = integer()
)

cat("--- Running 3-Player Game Simulations ---\n")
# Loop through all strategy combinations for P1, P2, and P3
for (s1_val in strategy_values_3p_range) { # Renamed loop variables to avoid conflict with column names
  for (s2_val in strategy_values_3p_range) {
    for (s3_val in strategy_values_3p_range) {
      cat(paste("Simulating P1:", s1_val, ", P2:", s2_val, ", P3:", s3_val, "...\n"))
      current_3p_strategies <- list(c(s1_val, s2_val, s3_val))
      
      sim_results_3p_combo <- run_simulations(
        n_simulations = num_sims_per_3p_combo,
        npennies = default_npennies_3p,
        strategies_list = current_3p_strategies
      )
      
      if (nrow(sim_results_3p_combo) > 0) {
        # Append only relevant columns to raw_3p_results
        raw_3p_results <- rbind(raw_3p_results, data.frame(
            s1 = rep(s1_val, nrow(sim_results_3p_combo)),
            s2 = rep(s2_val, nrow(sim_results_3p_combo)),
            s3 = rep(s3_val, nrow(sim_results_3p_combo)),
            winner_player_idx = sim_results_3p_combo$winner_player_idx
        ))
      } else {
        cat(paste("    No simulation results for P1:", s1_val, ", P2:", s2_val, ", P3:", s3_val, ". Skipping.\n"))
      }
    }
  }
}

cat("\n--- Aggregating Results for Preceding Player Analysis ---\n")

# Transform raw results for preceding player analysis
# P1 is preceded by P3
# P2 is preceded by P1
# P3 is preceded by P2

# Analysis for Player 1 (preceded by Player 3)
p1_win_rates <- raw_3p_results %>%
  mutate(p_self_strategy = s1, p_preceding_strategy = s3, player_analysed_id = 1) %>%
  group_by(player_analysed_id, p_self_strategy, p_preceding_strategy) %>%
  summarise(
    wins = sum(winner_player_idx == 1, na.rm = TRUE),
    total_games = n(),
    .groups = 'drop'
  ) %>%
  mutate(win_rate = ifelse(total_games > 0, wins / total_games, 0),
         player_analysed = "P1 (preceded by P3)") %>%
  select(player_analysed, p_self_strategy, p_preceding_strategy, win_rate, wins, total_games)

cat("\n--- Player 1 Win Rates based on Player 3's Strategy ---\n")
print(kable(p1_win_rates, format="pipe", caption="Player 1 Win Rates by Own Strategy, Influenced by Player 3's Strategy"))
if(nrow(p1_win_rates) > 0) {
  plot_p1_vs_p3_strat <- ggplot(p1_win_rates, aes(x = factor(p_preceding_strategy), y = win_rate, fill = factor(p_self_strategy))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "P1 Win Rate by Own Strategy, Influenced by P3's Strategy",
         x = "Player 3's Strategy (Preceding Player)",
         y = "Player 1 Win Rate",
         fill = "Player 1's Strategy") +
    theme_minimal()
  ggsave("graphs/p1_win_rates_vs_p3_strategy.png", plot = plot_p1_vs_p3_strat, width = 10, height = 7)
  cat("\nSaved P1 vs P3 strategy graph to graphs/p1_win_rates_vs_p3_strategy.png\n")
}

# Analysis for Player 2 (preceded by Player 1)
p2_win_rates <- raw_3p_results %>%
  mutate(p_self_strategy = s2, p_preceding_strategy = s1, player_analysed_id = 2) %>%
  group_by(player_analysed_id, p_self_strategy, p_preceding_strategy) %>%
  summarise(
    wins = sum(winner_player_idx == 2, na.rm = TRUE),
    total_games = n(),
    .groups = 'drop'
  ) %>%
  mutate(win_rate = ifelse(total_games > 0, wins / total_games, 0),
         player_analysed = "P2 (preceded by P1)") %>%
  select(player_analysed, p_self_strategy, p_preceding_strategy, win_rate, wins, total_games)

cat("\n--- Player 2 Win Rates based on Player 1's Strategy ---\n")
print(kable(p2_win_rates, format="pipe", caption="Player 2 Win Rates by Own Strategy, Influenced by Player 1's Strategy"))
if(nrow(p2_win_rates) > 0) {
  plot_p2_vs_p1_strat <- ggplot(p2_win_rates, aes(x = factor(p_preceding_strategy), y = win_rate, fill = factor(p_self_strategy))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "P2 Win Rate by Own Strategy, Influenced by P1's Strategy",
         x = "Player 1's Strategy (Preceding Player)",
         y = "Player 2 Win Rate",
         fill = "Player 2's Strategy") +
    theme_minimal()
  ggsave("graphs/p2_win_rates_vs_p1_strategy.png", plot = plot_p2_vs_p1_strat, width = 10, height = 7)
  cat("\nSaved P2 vs P1 strategy graph to graphs/p2_win_rates_vs_p1_strategy.png\n")
}

# Analysis for Player 3 (preceded by Player 2)
p3_win_rates <- raw_3p_results %>%
  mutate(p_self_strategy = s3, p_preceding_strategy = s2, player_analysed_id = 3) %>%
  group_by(player_analysed_id, p_self_strategy, p_preceding_strategy) %>%
  summarise(
    wins = sum(winner_player_idx == 3, na.rm = TRUE),
    total_games = n(),
    .groups = 'drop'
  ) %>%
  mutate(win_rate = ifelse(total_games > 0, wins / total_games, 0),
         player_analysed = "P3 (preceded by P2)") %>%
  select(player_analysed, p_self_strategy, p_preceding_strategy, win_rate, wins, total_games)

cat("\n--- Player 3 Win Rates based on Player 2's Strategy ---\n")
print(kable(p3_win_rates, format="pipe", caption="Player 3 Win Rates by Own Strategy, Influenced by Player 2's Strategy"))
if(nrow(p3_win_rates) > 0) {
  plot_p3_vs_p2_strat <- ggplot(p3_win_rates, aes(x = factor(p_preceding_strategy), y = win_rate, fill = factor(p_self_strategy))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "P3 Win Rate by Own Strategy, Influenced by P2's Strategy",
         x = "Player 2's Strategy (Preceding Player)",
         y = "Player 3 Win Rate",
         fill = "Player 3's Strategy") +
    theme_minimal()
  ggsave("graphs/p3_win_rates_vs_p2_strategy.png", plot = plot_p3_vs_p2_strat, width = 10, height = 7)
  cat("\nSaved P3 vs P2 strategy graph to graphs/p3_win_rates_vs_p2_strategy.png\n")
}

# Further Discussion/Interpretation (Example)
cat("\n\n--- Discussion of 3-Player Preceding Strategy Analysis ---\n")
cat("The tables above show the win rate for a given player (e.g., P1) based on their own strategy AND the strategy of the player who immediately preceded them in the turn order (e.g., P3 for P1).\n")
cat("For example, to see how P1's strategy 'X' performs when P3 uses strategy 'Y', find the corresponding row in the 'Player 1 Win Rates' table.\n")
cat("This analysis helps understand if certain preceding player strategies create advantages or disadvantages for the current player.\n")
cat("A high win rate when the preceding player uses a specific strategy might indicate that the preceding player's strategy is 'setting up' the current player for a win, or that the current player's strategy is particularly effective against it.\n")
cat("Conversely, a low win rate might suggest the preceding player's strategy is disruptive or that the current player's strategy is vulnerable to it.\n")


cat("\n--- 3-Player Strategy Analysis Complete ---\n")

cat("\n--- Script End ---\n")
