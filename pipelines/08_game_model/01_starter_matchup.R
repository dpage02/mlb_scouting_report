# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 01_starter_matchup.R
# ============================================================
# PURPOSE:
#   Build the starter matchup table — one row per side per game.
#   Joins probable pitchers from game_context with their
#   season stats from pitching_master_season.
#
# GRAIN:
#   One row per game_pk per side (home / away)
#   = 2 rows per game
#
# INPUT:
#   game_context          — game_pk, home/away pitcher IDs + names
#   pitching_master_season — season stats keyed on mlbam_id
#
# OUTPUT:
#   starter_matchup
# ============================================================

required_objects <- c("game_context", "pitching_master_season")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Pitcher season stats spine (deduplicated to one row per pitcher)
# Use total-season row for players who changed teams (team_abbr = "TOT")
# ------------------------------------------------------------

sp_stats <- pitching_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id,
    dplyr::any_of(c(
      # MLB Stats API
      "mlb_g", "mlb_gs", "mlb_ip", "mlb_era", "mlb_whip",
      "mlb_so", "mlb_bb", "mlb_hr", "mlb_sv",
      # FanGraphs
      "fg_ERA", "fg_WHIP", "fg_FIP", "fg_xFIP",
      "fg_K_9", "fg_BB_9", "fg_K_pct", "fg_BB_pct",
      "fg_WAR",
      # BBRef
      "bbref_ERA", "bbref_WHIP"
    ))
  )

# ------------------------------------------------------------
# Pivot game_context to long (one row per side per game)
# ------------------------------------------------------------

home_starters <- game_context %>%
  dplyr::filter(!is.na(home_pitcher_mlbam_id)) %>%
  dplyr::transmute(
    game_pk      = game_pk,
    game_date    = game_date,
    side         = "home",
    team_name    = home_team_name,
    mlbam_id     = as.integer(home_pitcher_mlbam_id),
    pitcher_name = home_pitcher_name
  )

away_starters <- game_context %>%
  dplyr::filter(!is.na(away_pitcher_mlbam_id)) %>%
  dplyr::transmute(
    game_pk      = game_pk,
    game_date    = game_date,
    side         = "away",
    team_name    = away_team_name,
    mlbam_id     = as.integer(away_pitcher_mlbam_id),
    pitcher_name = away_pitcher_name
  )

# ------------------------------------------------------------
# Stack and join season stats
# ------------------------------------------------------------

starter_matchup <- dplyr::bind_rows(home_starters, away_starters) %>%
  dplyr::left_join(sp_stats, by = "mlbam_id") %>%
  dplyr::arrange(game_pk, side)

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_games   <- dplyr::n_distinct(starter_matchup$game_pk)
n_missing <- sum(is.na(starter_matchup$mlb_era), na.rm = FALSE)

if (n_missing > 0) {
  message("NOTE: ", n_missing, " starters missing season stats ",
          "(Spring Training / new callup — expected)")
}

message("01_starter_matchup complete: ",
        nrow(starter_matchup), " rows | ",
        n_games, " games | ",
        sum(!is.na(starter_matchup$mlbam_id)), " starters identified")
