# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_baserunning_master_join.R
# ============================================================
# PURPOSE:
#   Join all season-level baserunning sources into one master
#   table. Follows the same pattern as offense, pitching, and
#   defense master joins.
#
# JOIN LOGIC:
#   MLB is the spine (team-level)
#   FanGraphs  → join on mlbam_id + season + team_abbr
#   Statcast   → join on mlbam_id + season only (TOT)
#   Lahman     → join on mlbam_id + season only (TOT)
#
# OUTPUT:
#   baserunning_master_season
# ============================================================

# ------------------------------------------------------------
# Required Objects
# ------------------------------------------------------------

required_objects <- c(
  "player_season_mlb_baserunning",
  "player_season_fg_baserunning",
  "player_season_statcast_baserunning",
  "player_season_lahman_baserunning"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Join (MLB Spine)
# ------------------------------------------------------------

baserunning_master_season <- player_season_mlb_baserunning %>%

  # FanGraphs — join by player + season only (team_abbr naming differs across sources)
  # For traded players with multiple rows, keep the first (typically season total)
  dplyr::left_join(
    player_season_fg_baserunning %>%
      dplyr::distinct(mlbam_id, season, .keep_all = TRUE) %>%
      dplyr::select(-team_abbr),
    by = c("mlbam_id", "season")
  ) %>%

  # Statcast — season total, join on player + season only
  dplyr::left_join(
    player_season_statcast_baserunning %>%
      dplyr::select(-team_abbr) %>%
      dplyr::distinct(mlbam_id, season, .keep_all = TRUE),
    by = c("mlbam_id", "season")
  ) %>%

  # Lahman — season total, join on player + season only
  dplyr::left_join(
    player_season_lahman_baserunning %>%
      dplyr::select(-team_abbr) %>%
      dplyr::distinct(mlbam_id, season, .keep_all = TRUE),
    by = c("mlbam_id", "season")
  ) %>%

  # Attach player names
  dplyr::left_join(
    player_master_ids %>%
      dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
      dplyr::select(mlbam_id, player_name, name_first, name_last, bbref_id),
    by = "mlbam_id"
  ) %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(player_name, player_name_mlb)
  ) %>%
  dplyr::select(-player_name_mlb) %>%

  dplyr::select(
    mlbam_id, player_name, name_first, name_last,
    bbref_id, season, team_abbr,
    dplyr::everything()
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(baserunning_master_season)

message("99_baserunning_master_join complete: ",
        nrow(baserunning_master_season),
        " player-season-team rows")
