# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 00_schema.R (defense)
# ============================================================
# PURPOSE:
#   Documents the canonical grain and expected columns for all
#   player defense tables. Mirrors the grain used by offense
#   and pitching throughout this pipeline.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
#   primary_position = position with most games played.
#   It is a descriptive column, NOT a grain key.
#   This allows validate_performance_table() to pass.
#
# SOURCES:
#   01_mlb_fielding_pull.R      → player_season_mlb_defense
#   02_fangraphs_fielding_pull.R → player_season_fg_defense
#   03_statcast_fielding_pull.R  → player_season_statcast_defense
#   05_lahman_fielding_pull.R    → player_season_lahman_defense
#
# FINAL OUTPUT:
#   defense_master_season          (all players)
#   defense_master_position_players
#   defense_master_pitchers
#   defense_master_catchers
#
# NOTE:
#   validate_performance_table() is defined in:
#   pipelines/05_performance/00_schema/00_grain_definition.R
#   and is sourced by each pull script individually.
# ============================================================

defense_grain_description <- "
Primary Grain: One row per player (mlbam_id) per season per team_abbr.
primary_position: the position with the most games played (informational, not a key).
Statcast and Lahman join on mlbam_id + season only (season totals, team_abbr = TOT).
"

# Expected metric columns by source (informational — not enforced at this stage)
defense_metric_columns <- list(

  mlb = c(
    "primary_position",
    "mlb_games_fielding", "mlb_games_started", "mlb_innings_fielding",
    "mlb_putouts", "mlb_assists", "mlb_errors",
    "mlb_double_plays", "mlb_fielding_pct"
  ),

  fangraphs = c(
    "fg_id",
    "fg_G", "fg_GS", "fg_Inn",
    "fg_DRS", "fg_Defense", "fg_OAA",
    "fg_RZR", "fg_OOZ", "fg_rARM",
    "fg_rSB", "fg_rGDP"
  ),

  statcast = c(
    "sc_oaa", "sc_runs_prevented",
    "sc_oaa_infront", "sc_oaa_3b_side", "sc_oaa_1b_side",
    "sc_oaa_back", "sc_oaa_vs_rhh", "sc_oaa_vs_lhh"
  ),

  lahman = c(
    "lahman_id",
    "lahman_G", "lahman_GS", "lahman_Inn",
    "lahman_PO", "lahman_A", "lahman_E",
    "lahman_DP", "lahman_fielding_pct"
  )
)
