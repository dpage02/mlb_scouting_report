# ============================================================
# 05 PERFORMANCE — GRAIN DEFINITION
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 00_grain_definition.R
# ============================================================
# Purpose:
# Defines the canonical grain and validation rules
# for all 05 performance tables.
#
# Primary Grain:
# One row per mlbam_id per season per team_abbr
#
# This file should be sourced by all 05 scripts.
# ============================================================

# -----------------------------
# Canonical Keys
# -----------------------------

performance_required_keys <- c(
  "mlbam_id",
  "season",
  "team_abbr"
)

# -----------------------------
# Grain Definition
# -----------------------------

performance_grain_description <- "
Primary Grain:
One row per player (mlbam_id)
per season
per team_abbr
"

# -----------------------------
# Integrity Check Functions
# -----------------------------

assert_required_columns <- function(df) {
  missing_cols <- setdiff(performance_required_keys, colnames(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

assert_unique_player_season_team <- function(df) {
  duplicate_check <- df %>%
    dplyr::count(mlbam_id, season, team_abbr) %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_check) > 0) {
    stop("Duplicate player-season-team rows detected.")
  }
}

validate_performance_table <- function(df) {
  assert_required_columns(df)
  assert_unique_player_season_team(df)
}
