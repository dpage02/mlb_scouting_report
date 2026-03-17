# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_offense_master_join.R
# ============================================================
# PURPOSE:
#   Join all season-level offense sources into one master table.
#
# OUTPUT:
#   - offense_master_season
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

# ------------------------------------------------------------
# Required Objects (must exist upstream)
# ------------------------------------------------------------
required_objects <- c(
  "player_season_mlb_offense",
  "player_season_fg_offense",
  "player_season_statcast_offense",
  "player_season_lahman_offense"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Begin Join (MLB Spine)
# ------------------------------------------------------------

offense_master_season <- player_season_mlb_offense %>%
  
  # Fangraphs
  left_join(
    player_season_fg_offense,
    by = c("mlbam_id", "season", "team_abbr")
  ) %>%
  
  # Statcast
  left_join(
    player_season_statcast_offense,
    by = c("mlbam_id", "season")
  ) %>%
  
  # Lahman
  left_join(
    player_season_lahman_offense,
    by = c("mlbam_id", "season")
  )

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(offense_master_season)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("99_offense_master_join complete: ",
        nrow(offense_master_season),
        " rows in offense_master_season.")