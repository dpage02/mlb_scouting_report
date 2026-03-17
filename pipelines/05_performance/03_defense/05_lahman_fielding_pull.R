# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 05_lahman_fielding_pull.R
# ============================================================
# PURPOSE:
#   Construct Lahman fielding fact table (season totals).
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT" — aggregated across all positions and teams.
#   Mirrors the Lahman offense pattern.
#
# DATA SOURCE:
#   Lahman::Fielding
#
# SEASON LOGIC:
#   season_complete = target_season - 1
#   Falls back to latest available Lahman season if not yet updated.
#
# OUTPUT:
#   player_season_lahman_defense
# ============================================================

library(Lahman)

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete      <- target_season - 1
latest_lahman_season <- max(Lahman::Fielding$yearID, na.rm = TRUE)

season_to_pull <- if (season_complete > latest_lahman_season) {
  message("Lahman fielding not available for ", season_complete,
          ". Falling back to ", latest_lahman_season)
  latest_lahman_season
} else {
  season_complete
}

# ------------------------------------------------------------
# Pull & Aggregate
# Sum across all positions and teams to get one row per player-season
# ------------------------------------------------------------

lahman_agg <- Lahman::Fielding %>%
  dplyr::filter(yearID == season_to_pull) %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    lahman_G   = sum(G,       na.rm = TRUE),
    lahman_GS  = sum(GS,      na.rm = TRUE),
    lahman_Inn = sum(InnOuts, na.rm = TRUE) / 3,  # outs → innings
    lahman_PO  = sum(PO,      na.rm = TRUE),
    lahman_A   = sum(A,       na.rm = TRUE),
    lahman_E   = sum(E,       na.rm = TRUE),
    lahman_DP  = sum(DP,      na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  dplyr::mutate(
    lahman_fielding_pct = dplyr::if_else(
      (lahman_PO + lahman_A + lahman_E) > 0,
      (lahman_PO + lahman_A) / (lahman_PO + lahman_A + lahman_E),
      NA_real_
    )
  )

# ------------------------------------------------------------
# Join crosswalk (lahman_id → mlbam_id)
# ------------------------------------------------------------

player_season_lahman_defense <- lahman_agg %>%
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
  dplyr::select(
    mlbam_id, season, team_abbr, lahman_id,
    dplyr::starts_with("lahman_")
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_lahman_defense)

message("05_lahman_fielding_pull complete: ",
        nrow(player_season_lahman_defense),
        " player-season rows for season ", season_to_pull)
