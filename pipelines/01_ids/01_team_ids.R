# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 01_team_ids.R
# ============================================================
# PURPOSE:
#   Build the canonical team_ids table
#   - One row per MLB franchise identity
#   - Stable across pipeline runs
#   - No game- or date-specific logic
#
# ASSUMPTIONS (validated interactively before writing this script):
#   Source: baseballr::mlb_teams()
#   Expected columns used:
#     team_id
#     team_abbreviation
#     team_name
#     location_name
#     franchise_name
#     league_name
#     division_name
#     first_year_of_play
#     active
#
# INPUTS:
#   - MLB team metadata from baseballr / MLB Stats API
#
# OUTPUTS:
#   - team_ids (data frame)
#   - Written later by 99_write_id_tables.R
# ============================================================

# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 01_team_ids.R
# ============================================================

library(dplyr)
library(baseballr)

message("Running 01_team_ids.R")

# ------------------------------------------------------------
# 1. Pull all teams
# ------------------------------------------------------------

raw_teams <- baseballr::mlb_teams()

message("Raw teams pulled: ", nrow(raw_teams))

# ------------------------------------------------------------
# 2. Filter to MLB only (sport_id == 1)
# ------------------------------------------------------------

mlb_teams_only <- raw_teams %>%
  filter(
    sport_id == 1,
    active == TRUE
  )

message("After MLB + active filter: ", nrow(mlb_teams_only))

# ------------------------------------------------------------
# 3. Deduplicate by team_id
# ------------------------------------------------------------

team_ids <- mlb_teams_only %>%
  distinct(team_id, .keep_all = TRUE) %>%
  transmute(
    mlbam_team_id = team_id,
    team_name     = team_full_name,
    team_abbr     = team_abbreviation,
    league_id,
    division_id
  )

# ------------------------------------------------------------
# 4. Integrity check
# ------------------------------------------------------------

if (nrow(team_ids) != 30) {
  stop("Expected 30 MLB teams — found ", nrow(team_ids))
}

message("team_ids built: ", nrow(team_ids), " MLB teams")
