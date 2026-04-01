# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 03_bullpen_grid.R
# ============================================================
# PURPOSE:
#   Scope bullpen_context to today's games.
#   Produces one row per reliever per game_pk for both teams.
#
# NOTE ON SERIES USE:
#   Availability reflects current state entering the series.
#   For series previews, game 1 availability is accurate.
#   Games 2+ availability may shift based on prior game usage
#   but the pre-series workload (pitches_last_3_days,
#   appearances_last_7d) provides useful context regardless.
#
# GRAIN:
#   One row per pitcher per game_pk
#
# ROLE ORDER (for sorting):
#   CL > SU* > MID > LR > SP* (openers/swingmen)
#
# INPUT:
#   game_context    — game_pk, home/away team abbrs
#   bullpen_context — pitcher availability + season stats by team
#   team_ids        — mlbam_team_id ↔ team_abbr bridge
#
# OUTPUT:
#   bullpen_grid
# ============================================================

required_objects <- c("game_context", "bullpen_context", "team_ids", "starter_matchup")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Build team ↔ game bridge (same as lineup_context)
# ------------------------------------------------------------

home_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "home",
    team_id   = as.integer(home_team_id)
  )

away_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "away",
    team_id   = as.integer(away_team_id)
  )

team_game_bridge <- dplyr::bind_rows(home_games, away_games) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, team_abbr),
    by = c("team_id" = "mlbam_team_id")
  )

# ------------------------------------------------------------
# Role sort order for display
# ------------------------------------------------------------

role_order <- c(
  "CL"  = 1,
  "SU8" = 2, "SU7" = 3, "SU6" = 4, "SU" = 5,
  "MID" = 6,
  "LR"  = 7
)

# ------------------------------------------------------------
# Join bullpen_context to game bridge
# Exclude starters from their normal rotation slot
# (SP1-SP5 only included if fg_role suggests opener/swingman)
# ------------------------------------------------------------

bullpen_grid <- team_game_bridge %>%
  dplyr::left_join(
    bullpen_context %>%
      dplyr::filter(
        # Relief corps only: pure relievers + swingmen/openers
        # Exclude all SP roles — rotation covered in starter_matchup
        fg_position %in% c("RP", "SP/RP")
      ),
    by = "team_abbr"
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%

  # Exclude the probable starter for each game
  # (covers SP/RP swingmen who are starting today)
  dplyr::anti_join(
    starter_matchup %>% dplyr::select(game_pk, mlbam_id),
    by = c("game_pk", "mlbam_id")
  ) %>%
  dplyr::mutate(
    role_sort = dplyr::coalesce(role_order[fg_role], 99L)
  ) %>%
  dplyr::select(
    game_pk, game_date, side, team_abbr,
    mlbam_id, player_name, fg_role, fg_position, roster_type,
    availability, days_rest, pitches_yesterday,
    pitches_last_3_days, appearances_last_7d, consecutive_days,
    dplyr::any_of(c("last_outing_date")),
    dplyr::any_of(c("mlb_g", "mlb_ip", "mlb_era", "mlb_whip",
                    "mlb_so", "mlb_bb", "mlb_sv", "mlb_hld")),
    dplyr::any_of(c("fg_WAR", "fg_Dollars", "bbref_ERA", "bbref_WHIP")),
    role_sort
  ) %>%
  dplyr::arrange(game_pk, side, role_sort)

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_games     <- dplyr::n_distinct(bullpen_grid$game_pk)
n_available <- sum(bullpen_grid$availability %in% c("available", "fresh"), na.rm = TRUE)
n_limited   <- sum(bullpen_grid$availability == "limited",     na.rm = TRUE)
n_unavail   <- sum(bullpen_grid$availability == "unavailable", na.rm = TRUE)
n_injured   <- sum(bullpen_grid$availability == "injured",     na.rm = TRUE)

message("03_bullpen_grid complete: ",
        nrow(bullpen_grid), " pitcher-game rows | ",
        n_games, " games | ",
        n_available, " available/fresh | ",
        n_limited,   " limited | ",
        n_unavail,   " unavailable | ",
        n_injured,   " injured")
