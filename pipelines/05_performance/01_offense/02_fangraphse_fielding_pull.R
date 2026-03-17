# ============================================================
# PLAYER DEFENSE — FANGRAPHS FIELDING PULL
# ============================================================
# Name:
# 02_fangraphs_fielding_pull.R
#
# Purpose:
# Pull season-level player defensive statistics from the
# FanGraphs fielding leaderboard.
#
# These statistics provide the advanced defensive evaluation
# layer of the defensive performance pipeline and include
# metrics such as Defensive Runs Saved (DRS), FanGraphs
# defensive runs (Def), arm runs, and additional defensive
# components.
#
# This script retrieves the raw FanGraphs defensive leaderboard
# and standardizes the column names to match the canonical
# schema used throughout the scouting pipeline.
#
# Data Source:
#   FanGraphs Fielding Leaderboards (via baseballr)
#
# Season Logic:
#   Uses the most recently completed MLB season
#   season_complete = target_season - 1
#
# Grain:
#   fg_player_id | team_id | season | position
#
# Output Table:
#   fg_player_fielding_raw
#
# Downstream Usage:
#   99_join_player_defense.R
# ============================================================

message("Pulling FanGraphs fielding statistics...")

season_complete <- target_season - 1

fg_player_fielding_raw <- baseballr::fg_fielder_leaders(
  startseason = season_complete,
  endseason   = season_complete,
  pos         = "all",
  qual        = "0"
)

# ------------------------------------------------------------
# Standardize Column Names
# ------------------------------------------------------------

fg_player_fielding_raw <- fg_player_fielding_raw %>%
  dplyr::rename(
    
    fg_player_id = playerid,
    mlbam_id     = xMLBAMID,
    
    player_name  = PlayerName,
    team_id      = team_name_abb,
    season       = Season,
    position     = Pos,
    
    games_fielding = G,
    games_started  = GS,
    innings_def    = Inn,
    
    putouts        = PO,
    assists        = A,
    errors         = E,
    double_plays   = DP,
    fielding_pct   = FP,
    
    fg_rsb         = rSB,
    fg_rgdp        = rGDP,
    
    fg_drs         = DRS,
    fg_def         = Defense,
    
    fg_rzr         = RZR,
    fg_ooz         = OOZ,
    
    fg_oaa         = OAA,
    fg_arm         = rARM
  )

message("FanGraphs fielding pull complete.")