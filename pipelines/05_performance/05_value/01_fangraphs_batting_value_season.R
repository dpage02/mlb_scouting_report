# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_fangraphs_batting_value_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs WAR and value metrics for batters.
#   Uses type="1" (same FanGraphs endpoint as the baserunning
#   module), but selects only value/WAR columns.
#
# KEY METRICS:
#   WAR     — FanGraphs wins above replacement (fWAR)
#   Dollars — Dollar value estimate ($/WAR market rate)
#   Off     — Offensive runs above average
#   Def     — Defensive runs above average
#
# DATA SOURCE:
#   FanGraphs via baseballr::fg_batter_leaders(type = "1")
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_fg_batting_value
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
    type        = "1",
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
# Select value columns only
# ------------------------------------------------------------

value_cols <- c(
  "playerid", "xMLBAMID", "team_name_abb", "Season",
  "WAR", "Dollars", "Off", "Def"
)

player_season_fg_batting_value <- fg_raw %>%
  dplyr::select(dplyr::any_of(value_cols)) %>%
  dplyr::mutate(
    mlbam_id     = as.integer(xMLBAMID),
    fg_id        = as.character(playerid),
    season       = as.integer(season_complete),
    team_abbr    = team_name_abb,
    player_type  = "batter"
  ) %>%
  dplyr::select(-playerid, -xMLBAMID, -team_name_abb,
                -dplyr::any_of("Season")) %>%
  dplyr::rename_with(
    ~ paste0("fg_", .x),
    -c(mlbam_id, fg_id, season, team_abbr, player_type)
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, season, team_abbr, fg_id, player_type,
                dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_fg_batting_value)

message("01_fangraphs_batting_value_season complete: ",
        nrow(player_season_fg_batting_value),
        " batter-season-team rows for season ", season_complete)
