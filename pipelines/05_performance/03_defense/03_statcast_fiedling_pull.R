# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 03_statcast_fielding_pull.R
# ============================================================
# PURPOSE:
#   Construct Statcast OAA fielding fact table (season totals).
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT" — Statcast OAA is a season total,
#   not split by team. Mirrors the Statcast offense pattern.
#
# DATA SOURCE:
#   Baseball Savant via baseballr::statcast_leaderboards()
#   leaderboard = "outs_above_average"
#
# SEASON LOGIC:
#   season_complete = target_season - 1
#   Falls back if no data returned.
#
# OUTPUT:
#   player_season_statcast_defense
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

test_sc <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "outs_above_average",
    year        = season_complete
  ),
  error = function(e) NULL
)

if (is.null(test_sc) || nrow(test_sc) == 0) {
  message("No Statcast OAA data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
}

sc_raw <- baseballr::statcast_leaderboards(
  leaderboard = "outs_above_average",
  year        = season_complete
)

# ------------------------------------------------------------
# Standardize
# ------------------------------------------------------------

player_season_statcast_defense <- sc_raw %>%
  dplyr::transmute(
    mlbam_id         = as.integer(player_id),
    season           = as.integer(season_complete),
    team_abbr        = "TOT",
    primary_position = primary_pos_formatted,

    sc_oaa            = outs_above_average,
    sc_runs_prevented = fielding_runs_prevented,
    sc_oaa_infront    = outs_above_average_infront,
    sc_oaa_3b_side    = outs_above_average_lateral_toward3bline,
    sc_oaa_1b_side    = outs_above_average_lateral_toward1bline,
    sc_oaa_back       = outs_above_average_behind,
    sc_oaa_vs_rhh     = outs_above_average_rhh,
    sc_oaa_vs_lhh     = outs_above_average_lhh
  ) %>%
  dplyr::filter(!is.na(mlbam_id))

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_statcast_defense)

message("03_statcast_fielding_pull complete: ",
        nrow(player_season_statcast_defense),
        " player-season rows for season ", season_complete)
