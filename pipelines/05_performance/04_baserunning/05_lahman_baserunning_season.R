# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 05_lahman_baserunning_season.R
# ============================================================
# PURPOSE:
#   Pull season-level baserunning counts from Lahman::Batting.
#   Aggregated across all teams (same pattern as offense module).
#   Lahman provides SB/CS for historical context and crosswalk
#   validation. Advanced metrics are not available in Lahman.
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT"
#
# OUTPUT:
#   player_season_lahman_baserunning
# ============================================================

library(Lahman)

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete      <- target_season - 1
latest_lahman_season <- max(Lahman::Batting$yearID, na.rm = TRUE)

season_to_pull <- if (season_complete > latest_lahman_season) {
  message("Lahman not available for ", season_complete,
          ". Falling back to ", latest_lahman_season)
  latest_lahman_season
} else {
  season_complete
}

# ------------------------------------------------------------
# Pull & Aggregate across teams
# ------------------------------------------------------------

lahman_agg <- Lahman::Batting %>%
  dplyr::filter(yearID == season_to_pull) %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    lahman_SB = sum(SB, na.rm = TRUE),
    lahman_CS = sum(CS, na.rm = TRUE),
    .groups   = "drop"
  )

# ------------------------------------------------------------
# Join crosswalk (lahman_id → mlbam_id)
# ------------------------------------------------------------

player_season_lahman_baserunning <- lahman_agg %>%
  dplyr::rename(lahman_id = playerID) %>%
  dplyr::left_join(
    player_master_ids %>% dplyr::select(lahman_id, mlbam_id),
    by = "lahman_id"
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::mutate(
    mlbam_id  = as.integer(mlbam_id),
    season    = as.integer(season_to_pull),
    team_abbr = "TOT"
  ) %>%
  dplyr::select(mlbam_id, season, team_abbr, lahman_id,
                lahman_SB, lahman_CS)

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_lahman_baserunning)

message("05_lahman_baserunning_season complete: ",
        nrow(player_season_lahman_baserunning),
        " player-season rows for season ", season_to_pull)
