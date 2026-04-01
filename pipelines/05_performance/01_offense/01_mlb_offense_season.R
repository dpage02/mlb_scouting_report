# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_mlb_offense_season.R
# ============================================================
# PURPOSE:
#   Construct league-wide season-level MLB offense fact table.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# DATA SOURCE:
#   MLB Stats API (league leaderboard)
#
# OUTPUT:
#   player_season_mlb_offense
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Pull League-Wide Season Hitting Leaderboard
# ------------------------------------------------------------

season_to_pull <- target_season

# Pull regular-season stats only by requesting gameType=R.
# This returns 0 rows during spring training (no R games yet),
# triggering the fallback below. If the parameter is unsupported,
# the tryCatch falls back to an unfiltered pull.
mlb_league_raw <- tryCatch(
  baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "hitting",
    player_pool = "all",
    season      = season_to_pull,
    game_type   = "R",
    limit       = 3000
  ),
  error = function(e) {
    baseballr::mlb_stats(
      stat_type   = "season",
      stat_group  = "hitting",
      player_pool = "all",
      season      = season_to_pull,
      limit       = 3000
    )
  }
)

# Fallback: if no meaningful regular-season data, use prior full season.
# Check max PA — any regular-season player will have more PA than any
# spring training player within a few weeks, making this robust.
max_pa <- if (
  "player_id" %in% names(mlb_league_raw) &&
  "plate_appearances" %in% names(mlb_league_raw) &&
  nrow(mlb_league_raw) > 0
) {
  max(suppressWarnings(as.integer(mlb_league_raw$plate_appearances)), na.rm = TRUE)
} else {
  0L
}

if (is.na(max_pa) || max_pa < 10) {
  message(season_to_pull, " hitting data appears to be preseason (max PA = ",
          max_pa, "). Falling back to ", season_to_pull - 1, " (last full season).")
  season_to_pull <- season_to_pull - 1L

  mlb_league_raw <- baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "hitting",
    player_pool = "all",
    season      = season_to_pull,
    limit       = 3000
  )
}

# ------------------------------------------------------------
# Build Fact Table
# ------------------------------------------------------------

player_season_mlb_offense <- mlb_league_raw %>%
  dplyr::transmute(
    mlbam_id = as.integer(player_id),
    season   = as.integer(season),
    team_id  = as.integer(team_id),

    # Counting stats
    mlb_pa   = as.integer(plate_appearances),
    mlb_ab   = as.integer(at_bats),
    mlb_h    = as.integer(hits),
    mlb_2b   = as.integer(doubles),
    mlb_3b   = as.integer(triples),
    mlb_hr   = as.integer(home_runs),
    mlb_bb   = as.integer(base_on_balls),
    mlb_ibb  = as.integer(intentional_walks),
    mlb_so   = as.integer(strike_outs),
    mlb_hbp  = as.integer(hit_by_pitch),
    mlb_sf   = as.integer(sac_flies),
    mlb_sh   = as.integer(sac_bunts),
    mlb_gidp = as.integer(ground_into_double_play),
    mlb_r    = as.integer(runs),
    mlb_rbi  = as.integer(rbi),
    mlb_tb   = as.integer(total_bases),

    # Rate stats
    mlb_avg   = as.numeric(avg),
    mlb_obp   = as.numeric(obp),
    mlb_slg   = as.numeric(slg),
    mlb_ops   = as.numeric(ops),
    mlb_babip = as.numeric(babip),

    # Baserunning
    mlb_sb     = as.integer(stolen_bases),
    mlb_cs     = as.integer(caught_stealing),
    mlb_sb_pct = as.numeric(stolen_base_percentage)
  ) %>%

  # Normalize team_abbr via team_ids
  # team_name from MLB Stats API = full name (e.g. "Atlanta Braves"), not abbr
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, team_abbr),
    by = c("team_id" = "mlbam_team_id")
  ) %>%
  dplyr::mutate(
    team_abbr = dplyr::coalesce(team_abbr, as.character(team_id))
  ) %>%
  dplyr::select(-team_id) %>%
  dplyr::select(mlbam_id, season, team_abbr, dplyr::everything())

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_mlb_offense)

message("01_mlb_offense_season complete: ",
        nrow(player_season_mlb_offense),
        " league-wide player-season rows for season ", season_to_pull)
