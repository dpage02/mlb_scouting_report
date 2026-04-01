# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 02_game_meta.R
# ============================================================
# PURPOSE:
#   Enrich schedule_context into structured game_meta.
#
# WHAT THIS SCRIPT DOES:
#   - Joins team_ids for league/division context
#   - Adds matchup classification flags
#   - Adds structural scheduling indicators
#
# INPUT:
#   - schedule_context
#   - team_ids
#
# OUTPUT:
#   - game_meta (one row per game_pk)
#
# DESIGN:
#   Pure event-level enrichment.
#   No performance metrics.
# ============================================================

library(dplyr)

message("Running 02_game_meta.R")

# ------------------------------------------------------------
# 1. Safety Checks
# ------------------------------------------------------------

if (!exists("schedule_context")) {
  stop("schedule_context not found. Run 01_schedule.R first.")
}

if (!exists("team_ids")) {
  stop("team_ids not found. Run Phase 01 ID layer first.")
}

# ------------------------------------------------------------
# 2. Join Team Context
# ------------------------------------------------------------

home_context <- team_ids %>%
  select(
    mlbam_team_id,
    home_team_abbr = team_abbr,
    home_league_id = league_id,
    home_division_id = division_id
  )

away_context <- team_ids %>%
  select(
    mlbam_team_id,
    away_team_abbr = team_abbr,
    away_league_id = league_id,
    away_division_id = division_id
  )

game_meta <- schedule_context %>%
  left_join(
    home_context,
    by = c("home_team_id" = "mlbam_team_id")
  ) %>%
  left_join(
    away_context,
    by = c("away_team_id" = "mlbam_team_id")
  )

# ------------------------------------------------------------
# 3. Derived Matchup Flags
# ------------------------------------------------------------

# A true doubleheader = two games between the same teams on the same date.
# (series_game_num > 1 incorrectly fires for game 2 of any regular series.)
dh_game_pks <- schedule_context %>%
  dplyr::group_by(game_date, home_team_id, away_team_id) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::pull(game_pk)

game_meta <- game_meta %>%
  mutate(
    is_interleague  = home_league_id != away_league_id,
    is_division_game = home_division_id == away_division_id,
    is_doubleheader = game_pk %in% dh_game_pks
  )

# ------------------------------------------------------------
# 4. Integrity Check
# ------------------------------------------------------------

stopifnot(
  nrow(game_meta) ==
    n_distinct(game_meta$game_pk)
)

message("game_meta built: ", nrow(game_meta), " game(s)")
message("02_game_meta.R complete")
