# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 00_schema.R (value)
# ============================================================
# PURPOSE:
#   Documents the canonical grain and expected columns for all
#   player value tables.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#   Same grain as offense, pitching, defense, and baserunning.
#
# PLAYER TYPE:
#   player_type = "batter"  → batter WAR / value
#   player_type = "pitcher" → pitcher WAR / value
#   (kept in separate intermediate tables; combined in join)
#
# SOURCES:
#   01_fangraphs_batting_value_season.R  → player_season_fg_batting_value
#   02_fangraphs_pitching_value_season.R → player_season_fg_pitching_value
#
# FINAL OUTPUT:
#   value_master_season
#   Columns: mlbam_id, player_name, season, team_abbr,
#            player_type, fg_war, fg_dollars,
#            fg_off (batters), fg_def (batters),
#            fg_rar (pitchers)
#
# KEY METRICS:
#   fg_war     — FanGraphs WAR (fWAR)
#   fg_dollars — Dollar value estimate based on $/WAR market rate
#   fg_off     — Offensive runs above average (batters)
#   fg_def     — Defensive runs above average (batters)
#   fg_rar     — Runs above replacement (pitchers)
# ============================================================

value_grain_description <- "
Primary Grain: One row per player (mlbam_id) per season per team_abbr.
Batters and pitchers are combined; distinguished by player_type column.
"

value_metric_columns <- list(

  shared = c(
    "fg_war",
    "fg_dollars"
  ),

  batters_only = c(
    "fg_off",
    "fg_def"
  ),

  pitchers_only = c(
    "fg_rar"
  )
)
