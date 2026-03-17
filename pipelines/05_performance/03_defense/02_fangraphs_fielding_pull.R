# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_fielding_pull.R
# ============================================================
# PURPOSE:
#   Construct league-wide FanGraphs fielding fact table.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# DATA SOURCE:
#   FanGraphs fielding leaderboard via baseballr::fg_fielder_leaders()
#
# SEASON LOGIC:
#   season_complete = target_season - 1
#   Falls back if no data returned.
#
# OUTPUT:
#   player_season_fg_defense
#
# NOTES:
#   - All stat columns are prefixed with fg_
#   - Rows with no mlbam_id (xMLBAMID) are dropped — can't join downstream
#   - FanGraphs returns season totals, so team_abbr reflects the
#     last team or "2TM" for traded players
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

test_fg <- tryCatch(
  baseballr::fg_fielder_leaders(
    startseason = season_complete,
    endseason   = season_complete,
    pos         = "all",
    qual        = "0"
  ),
  error = function(e) NULL
)

if (is.null(test_fg) || nrow(test_fg) == 0) {
  message("No FanGraphs fielding data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
}

fg_raw <- baseballr::fg_fielder_leaders(
  startseason = season_complete,
  endseason   = season_complete,
  pos         = "all",
  qual        = "0"
)

# ------------------------------------------------------------
# Standardize
# Mirrors the pattern used in 02_fangraphs_offense_season.R:
#   - Extract identity columns explicitly
#   - Drop source name columns
#   - Prefix all stat columns with fg_
# ------------------------------------------------------------

player_season_fg_defense <- fg_raw %>%
  dplyr::mutate(
    mlbam_id         = as.integer(xMLBAMID),
    fg_id            = as.character(playerid),
    season           = as.integer(season_complete),
    team_abbr        = team_name_abb,
    primary_position = Pos
  ) %>%
  # Drop source identifier columns before prefixing
  dplyr::select(
    -playerid, -xMLBAMID, -team_name_abb, -Pos,
    -dplyr::any_of(c("PlayerName", "Season", "Name", "team_name"))
  ) %>%
  # Prefix all remaining stat columns with fg_
  dplyr::rename_with(
    ~ paste0("fg_", .x),
    -c(mlbam_id, season, team_abbr, fg_id, primary_position)
  ) %>%
  # Drop players with no mlbam_id — they cannot join downstream
  dplyr::filter(!is.na(mlbam_id)) %>%
  # Drop FanGraphs metadata columns that are not stats
  dplyr::select(-dplyr::any_of(c(
    "fg_PlayerNameRoute", "fg_SeasonMin", "fg_SeasonMax",
    "fg_playerId", "fg_playerid", "fg_AlyAgg"
  ))) %>%
  # FanGraphs returns one row per position played — reduce to primary
  # (position with most innings: fg_Inn). If Inn is missing, take first row.
  dplyr::group_by(mlbam_id, season, team_abbr) %>%
  dplyr::arrange(dplyr::desc(dplyr::coalesce(fg_Inn, 0L)), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(mlbam_id, season, team_abbr, fg_id, primary_position, dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_fg_defense)

message("02_fangraphs_fielding_pull complete: ",
        nrow(player_season_fg_defense),
        " player-season-team rows for season ", season_complete)
