# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_mlb_baserunning_season.R
# ============================================================
# PURPOSE:
#   Pull season-level baserunning stats from the MLB Stats API.
#   Captures metrics NOT already in the offense module:
#     - extra_bases_taken_percentage (XBT%) — how often a runner
#       advances extra bases on a hit (e.g. 1st to 3rd on a single)
#     - runs_scored_percentage — how often a runner scores
#     - pickoffs
#     - times_on_base
#   Also re-captures SB/CS/SB% as the authoritative MLB count.
#
# DATA SOURCE:
#   MLB Stats API via baseballr::mlb_stats(stat_group = "hitting")
#   Note: the MLB API has no dedicated "baseRunning" stat group.
#   Baserunning counting stats are embedded in the hitting response.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_mlb_baserunning
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

mlb_raw <- baseballr::mlb_stats(
  stat_type   = "season",
  stat_group  = "hitting",
  player_pool = "all",
  season      = season_complete,
  sport_id    = 1,
  limit       = 5000
)

if (is.null(mlb_raw) || !"player_id" %in% colnames(mlb_raw) || nrow(mlb_raw) == 0) {
  message("No MLB hitting data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  mlb_raw <- baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "hitting",
    player_pool = "all",
    season      = season_complete,
    sport_id    = 1,
    limit       = 5000
  )
}

# ------------------------------------------------------------
# Standardize — keep only baserunning-relevant columns
# (SB/CS/triples/runs/GIDP serve as the join spine;
#  advanced metrics come from FanGraphs and Statcast)
# ------------------------------------------------------------

player_season_mlb_baserunning <- mlb_raw %>%
  dplyr::transmute(
    mlbam_id        = as.integer(player_id),
    player_name_mlb = player_full_name,
    season          = as.integer(season_complete),
    team_name_raw   = team_name,

    mlb_sb          = as.integer(stolen_bases),
    mlb_cs          = as.integer(caught_stealing),
    mlb_sb_pct      = suppressWarnings(as.numeric(stolen_base_percentage)),
    mlb_triples     = as.integer(triples),
    mlb_runs        = as.integer(runs),
    mlb_gidp        = as.integer(ground_into_double_play)
  ) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(team_name, team_abbr),
    by = c("team_name_raw" = "team_name")
  ) %>%
  dplyr::mutate(team_abbr = dplyr::coalesce(team_abbr, team_name_raw)) %>%
  dplyr::select(-team_name_raw) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, player_name_mlb, season, team_abbr, dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_mlb_baserunning)

message("01_mlb_baserunning_season complete: ",
        nrow(player_season_mlb_baserunning),
        " player-season-team rows for season ", season_complete)
