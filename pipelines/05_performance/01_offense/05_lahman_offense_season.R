# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 05_lahman_offense_season.R
# ============================================================
# PURPOSE:
#   Construct league-wide season-level Lahman offense fact table.
#
# GRAIN:
#   One row per mlbam_id per season
#
# SOURCE:
#   Lahman::Batting
#
# DESIGN:
#   - League-wide
#   - Aggregated across teams (TOT behavior)
#   - Automatic fallback if season not available
#   - Source-prefixed metrics (lahman_)
# ============================================================


# ------------------------------------------------------------
# Load Package
# ------------------------------------------------------------

library(Lahman)


# ------------------------------------------------------------
# Determine Lahman Season To Pull
# ------------------------------------------------------------

latest_lahman_season <- max(Lahman::Batting$yearID, na.rm = TRUE)

season_to_pull <- if (target_season > latest_lahman_season) {
  message("Lahman does not contain ", target_season,
          ". Falling back to ", latest_lahman_season)
  latest_lahman_season
} else {
  target_season
}


# ------------------------------------------------------------
# Filter To Season
# ------------------------------------------------------------

lahman_raw <- Batting %>%
  dplyr::filter(yearID == season_to_pull)


# ------------------------------------------------------------
# Aggregate Across Teams (TOT Behavior)
# ------------------------------------------------------------

lahman_agg <- lahman_raw %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    G     = sum(G, na.rm = TRUE),
    AB    = sum(AB, na.rm = TRUE),
    R     = sum(R, na.rm = TRUE),
    H     = sum(H, na.rm = TRUE),
    X2B   = sum(X2B, na.rm = TRUE),
    X3B   = sum(X3B, na.rm = TRUE),
    HR    = sum(HR, na.rm = TRUE),
    RBI   = sum(RBI, na.rm = TRUE),
    SB    = sum(SB, na.rm = TRUE),
    CS    = sum(CS, na.rm = TRUE),
    BB    = sum(BB, na.rm = TRUE),
    SO    = sum(SO, na.rm = TRUE),
    HBP   = sum(HBP, na.rm = TRUE),
    SH    = sum(SH, na.rm = TRUE),
    SF    = sum(SF, na.rm = TRUE),
    GIDP  = sum(GIDP, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Join Crosswalk (lahman_id → mlbam_id)
# ------------------------------------------------------------

player_season_lahman_offense <- lahman_agg %>%
  
  dplyr::rename(lahman_id = playerID) %>%
  
  dplyr::left_join(player_master_ids, by = "lahman_id") %>%
  
  # enforce canonical key integrity
  dplyr::filter(!is.na(mlbam_id)) %>%
  
  dplyr::mutate(
    season = season_to_pull,
    team_abbr = "TOT"
  ) %>%
  
  dplyr::select(
    mlbam_id,
    season,
    team_abbr,
    lahman_id,
    dplyr::everything()
  ) %>%
  
  dplyr::rename_with(
    ~ paste0("lahman_", .x),
    -c(mlbam_id, season, team_abbr, lahman_id)
  )


# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_lahman_offense)


# ------------------------------------------------------------
# Completion Message
# ------------------------------------------------------------

message("05_lahman_offense_season complete: ",
        nrow(player_season_lahman_offense),
        " league-wide rows created for season ",
        season_to_pull, ".")