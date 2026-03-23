# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_pitching_master_join.R
# ============================================================
# PURPOSE:
#   Join all season-level pitching sources into one master table.
#
# OUTPUT:
#   - pitching_master_season
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

# ------------------------------------------------------------
# Required Objects (must exist upstream)
# ------------------------------------------------------------
required_objects <- c(
  "player_season_mlb_pitching",
  "player_season_fg_pitching",
  "player_season_statcast_pitching",
  "player_season_lahman_pitching",
  "player_season_bbref_pitching_advanced"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Join (MLB Spine)
# ------------------------------------------------------------

pitching_master_season <- player_season_mlb_pitching %>%

  # FanGraphs — advanced metrics (FIP, xFIP, xERA, BABIP, LOB%, SIERA, fWAR, etc.)
  dplyr::left_join(
    player_season_fg_pitching,
    by = c("mlbam_id", "season", "team_abbr")
  ) %>%

  # Statcast (no team split)
  dplyr::left_join(
    player_season_statcast_pitching,
    by = c("mlbam_id", "season")
  ) %>%

  # Lahman (no team split)
  dplyr::left_join(
    player_season_lahman_pitching,
    by = c("mlbam_id", "season")
  ) %>%

  # BBRef advanced — ERA+, bWAR, H9, HR9, BB9, SO9, SO/W (no team split)
  dplyr::left_join(
    player_season_bbref_pitching_advanced,
    by = c("mlbam_id", "season")
  )

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(pitching_master_season)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("99_pitching_master_join complete: ",
        nrow(pitching_master_season),
        " rows in pitching_master_season.")