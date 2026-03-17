# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_mlb_fielding_pull.R
# ============================================================
# PURPOSE:
#   Construct league-wide season-level MLB fielding fact table.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#   primary_position = position with most games played
#
# DATA SOURCE:
#   MLB Stats API via baseballr::mlb_stats()
#
# SEASON LOGIC:
#   season_complete = target_season - 1
#   Falls back one more year if no data returned.
#
# OUTPUT:
#   player_season_mlb_defense
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

mlb_raw <- baseballr::mlb_stats(
  stat_type   = "season",
  stat_group  = "fielding",
  player_pool = "all",
  season      = season_complete,
  sport_id    = 1,
  limit       = 5000
)

if (is.null(mlb_raw) || !"player_id" %in% colnames(mlb_raw) || nrow(mlb_raw) == 0) {
  message("No MLB fielding data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  mlb_raw <- baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "fielding",
    player_pool = "all",
    season      = season_complete,
    sport_id    = 1,
    limit       = 5000
  )
}

# ------------------------------------------------------------
# Standardize columns
# ------------------------------------------------------------

mlb_std <- mlb_raw %>%
  dplyr::transmute(
    mlbam_id          = as.integer(player_id),
    player_name_mlb   = player_full_name,
    season            = as.integer(season_complete),
    team_name_raw     = team_name,
    position          = position_abbreviation,

    mlb_games_fielding   = as.integer(games),
    mlb_games_started    = as.integer(games_started),
    mlb_innings_fielding = as.numeric(innings),
    mlb_putouts          = as.integer(put_outs),
    mlb_assists          = as.integer(assists),
    mlb_errors           = as.integer(errors),
    mlb_double_plays     = as.integer(double_plays),
    mlb_fielding_pct     = as.numeric(fielding)
  ) %>%
  # Normalize full team name to canonical 3-letter abbreviation
  dplyr::left_join(
    team_ids %>% dplyr::select(team_name, team_abbr),
    by = c("team_name_raw" = "team_name")
  ) %>%
  dplyr::mutate(team_abbr = dplyr::coalesce(team_abbr, team_name_raw)) %>%
  dplyr::select(-team_name_raw)

# ------------------------------------------------------------
# Reduce to primary position per player-team
# Use innings (position-specific) not games (player total) to identify
# primary position. A 1B who emergency-pitched has 800 inn at 1B, 1 at P.
# ------------------------------------------------------------

player_season_mlb_defense <- mlb_std %>%
  dplyr::group_by(mlbam_id, season, team_abbr) %>%
  dplyr::slice_max(mlb_innings_fielding, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(primary_position = position) %>%
  dplyr::select(mlbam_id, season, team_abbr, primary_position, dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_mlb_defense)

message("01_mlb_fielding_pull complete: ",
        nrow(player_season_mlb_defense),
        " player-season-team rows for season ", season_complete)
