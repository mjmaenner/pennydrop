# Penny Drop Game: Optimal Strategy Analysis

This document summarizes the optimal strategies found for the Penny Drop game based on simulations.

## Simulation Parameters

The following parameters were used for the analysis:

*   **Pennies per Player:** 10
*   **2-Player Games:**
    *   Number of simulations per strategy combination: 1000
    *   Strategy values tested for each player: 1, 2, 3, 4, 5
*   **3-Player Games:**
    *   Number of simulations per strategy combination: 500
    *   Strategy values tested for each player: 1, 2, 3

## Optimal Strategies

### 2-Player Games

*   **Optimal for Player 1:**
    *   Strategy: [P1 uses 2, P2 uses 5]
    *   Player 1 Win Rate: 97.6%
    *   (s1=2, s2=5, p1_win_rate=0.976, p2_win_rate=0.024, n_games=1000)

*   **Optimal for Player 2:**
    *   Strategy: [P1 uses 5, P2 uses 2]
    *   Player 2 Win Rate: 95.0%
    *   (s1=5, s2=2, p1_win_rate=0.05, p2_win_rate=0.95, n_games=1000)

### 3-Player Games

*   **Optimal for Player 1:**
    *   Strategy: [P1 uses 2, P2 uses 1, P3 uses 1]
    *   Player 1 Win Rate: 65.4%
    *   (s1=2, s2=1, s3=1, p1_win_rate=0.654, p2_win_rate=0.146, p3_win_rate=0.200, n_games=500)

*   **Optimal for Player 2:**
    *   Strategy: [P1 uses 1, P2 uses 2, P3 uses 1]
    *   Player 2 Win Rate: 59.4%
    *   (s1=1, s2=2, s3=1, p1_win_rate=0.252, p2_win_rate=0.594, p3_win_rate=0.154, n_games=500)

*   **Optimal for Player 3:**
    *   Strategy: [P1 uses 1, P2 uses 1, P3 uses 2]
    *   Player 3 Win Rate: 54.8%
    *   (s1=1, s1=1, s3=2, p1_win_rate=0.206, p2_win_rate=0.246, p3_win_rate=0.548, n_games=500)

## Discussion

**2-Player Games:**
The results for 2-player games show a fascinating symmetry and what appears to be a "race to the bottom" or "exploitative" strategy.
*   When Player 1 uses a low strategy (2) and Player 2 uses a high strategy (5), Player 1 has an extremely high win rate (97.6%). This suggests Player 1, by being conservative (passing if there are 2 or more pennies up), forces Player 2 (who is willing to risk rolling with up to 5 pennies up) into making more mistakes (rolling into occupied slots or rolling when many pennies are up).
*   Conversely, when Player 1 uses a high strategy (5) and Player 2 uses a low strategy (2), Player 2 wins overwhelmingly (95.0%). This is a mirror image of the previous scenario.
*   This indicates that in a 2-player game, if one player adopts a significantly more conservative strategy than their opponent, the conservative player tends to dominate. The optimal strategy for a given player heavily depends on the opponent's strategy, aiming to be more conservative if the opponent is aggressive, and potentially more aggressive if the opponent is extremely conservative (though the data here primarily shows the former is highly effective).
*   A strategy value of '2' seems very strong when paired against an opponent using '5'.

**3-Player Games:**
The 3-player game results also show a pattern where a player adopting a strategy of '2' while the other two players adopt a strategy of '1' tends to have the highest win rate for that specific player.
*   P1 (Strat 2) vs P2 (Strat 1), P3 (Strat 1): P1 wins 65.4%.
*   P2 (Strat 2) vs P1 (Strat 1), P3 (Strat 1): P2 wins 59.4%.
*   P3 (Strat 2) vs P1 (Strat 1), P2 (Strat 1): P3 wins 54.8%.
*   This suggests that a strategy of '2' is quite robust when other players are playing very conservatively (strategy '1'). The player with strategy '2' is willing to take slightly more risk than those with strategy '1', which seems to pay off. The slight decrease in win rate from P1 to P2 to P3 (65.4% -> 59.4% -> 54.8%) might be attributable to player order advantage/disadvantage, although more specific analysis would be needed to confirm this.
*   A strategy of '1' appears to be less effective if another player is using '2'. Players using '1' are likely passing very often, giving more opportunities to the player using '2'.

**General Observations:**
*   **Conservative strategies (lower numbers) generally seem powerful**, especially if an opponent is using a much higher (more aggressive) strategy. This is because an aggressive strategy increases the risk of a "bad roll" that returns all currently up pennies to the roller.
*   The "optimal" strategy is highly dependent on the strategies of the other players. There isn't one single strategy value that is universally best in all situations.
*   The player order might also play a role, especially in 3-player games, but this analysis focused on strategy combinations.

Further analysis could explore a wider range of strategies, more simulations for higher confidence, and specific head-to-head matchups to determine Nash equilibria if they exist.
---
This markdown file has been created.
The content includes the simulation parameters, optimal strategies for 2-player and 3-player games with win rates, and a discussion of the results.
The discussion touches upon the effectiveness of conservative strategies, the symmetrical nature of some 2-player outcomes, and the influence of opponent strategies.
The task is complete.
