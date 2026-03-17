# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_baserunning_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs baserunning metrics.
#   Selects ONLY the baserunning-specific columns so there
#   is no overlap with the offense FanGraphs module.
#
# KEY METRICS:
#   BsR  — total baserunning runs above average
#   wSB  — weighted stolen base runs
#   UBR  — ultimate base running (all non-SB baserunning)
#   wGDP — grounded into double play runs
#   Spd  — speed score
#
# DATA SOURCE:
#   FanGraphs via baseballr::fg_batter_leaders()
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_fg_baserunning
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

test_fg <- tryCatch(
  baseballr::fg_batter_leaders(
    startseason = season_complete,
    endseason   = season_complete,
    qual        = "0",
    type        = "8",
    pageitems   = "10"
  ),
  error = function(e) NULL
)

if (is.null(test_fg) || nrow(test_fg) == 0) {
  message("No FanGraphs data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
}

fg_raw <- baseballr::fg_batter_leaders(
  startseason = season_complete,
  endseason   = season_complete,
  qual        = "0",
  type        = "1",
  pageitems   = "10000"
)

# ------------------------------------------------------------
# Select baserunning columns only
# Avoids duplicating what is already in offense FG module
# ------------------------------------------------------------

baserunning_cols <- c(
  "playerid", "xMLBAMID", "team_name_abb", "Season",
  "BaseRunning", "wBsR", "Spd",
  "SB", "CS"
)

player_season_fg_baserunning <- fg_raw %>%
  dplyr::select(dplyr::any_of(baserunning_cols)) %>%
  dplyr::mutate(
    mlbam_id  = as.integer(xMLBAMID),
    fg_id     = as.character(playerid),
    season    = as.integer(season_complete),
    team_abbr = team_name_abb
  ) %>%
  dplyr::select(-playerid, -xMLBAMID, -team_name_abb,
                -dplyr::any_of("Season")) %>%
  dplyr::rename_with(
    ~ paste0("fg_", .x),
    -c(mlbam_id, fg_id, season, team_abbr)
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, season, team_abbr, fg_id, dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_fg_baserunning)

message("02_fangraphs_baserunning_season complete: ",
        nrow(player_season_fg_baserunning),
        " player-season-team rows for season ", season_complete)
