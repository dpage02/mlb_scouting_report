# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 00_schema.R (baserunning)
# ============================================================
# PURPOSE:
#   Documents the canonical grain and expected columns for all
#   player baserunning tables.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#   Same grain as offense, pitching, and defense modules.
#
# NOTE ON DUPLICATION:
#   The offense module already captures SB, CS, SB% from MLB
#   and SB/CS from Lahman. This module focuses on what the
#   offense module does NOT capture:
#     - MLB: extra_bases_taken_pct, runs_scored_pct, pickoffs
#     - FanGraphs: BsR, wSB, UBR, wGDP, Spd
#     - Statcast: sprint_speed, hp_to_1b, running splits
#
# SOURCES:
#   01_mlb_baserunning_season.R      → player_season_mlb_baserunning
#   02_fangraphs_baserunning_season.R → player_season_fg_baserunning
#   03_statcast_baserunning_season.R  → player_season_statcast_baserunning
#   05_lahman_baserunning_season.R    → player_season_lahman_baserunning
#
# FINAL OUTPUT:
#   baserunning_master_season
# ============================================================

baserunning_grain_description <- "
Primary Grain: One row per player (mlbam_id) per season per team_abbr.
Statcast and Lahman join on mlbam_id + season only (season totals, team_abbr = TOT).
"

# Expected metric columns by source (informational)
baserunning_metric_columns <- list(

  mlb = c(
    "mlb_sb", "mlb_cs", "mlb_sb_pct",
    "mlb_pickoffs",
    "mlb_extra_bases_taken_pct",
    "mlb_runs_scored_pct",
    "mlb_times_on_base"
  ),

  fangraphs = c(
    "fg_BsR",   # total baserunning runs above average
    "fg_wSB",   # weighted stolen base runs
    "fg_UBR",   # ultimate base running (non-SB baserunning)
    "fg_wGDP",  # grounded into double play runs
    "fg_Spd"    # speed score
  ),

  statcast = c(
    "sc_sprint_speed",      # ft/sec — primary speed metric
    "sc_hp_to_1b",          # median home-to-first time (sec)
    "sc_competitive_runs",  # qualifying sprint speed runs
    "sc_sprint_percentile"  # league percentile
  ),

  lahman = c(
    "lahman_SB",
    "lahman_CS"
  )
)
