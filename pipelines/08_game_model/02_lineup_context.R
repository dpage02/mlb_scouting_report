# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 02_lineup_context.R
# ============================================================
# PURPOSE:
#   Build the expected lineup for each team in today's games.
#   Uses FanGraphs depth chart batting slots (fg_role 1-9) as
#   the expected batting order, joined with season offense stats.
#
# NOTE:
#   Actual lineups are typically released 30-60 min before first
#   pitch. This uses the depth chart projection as the best
#   available pre-game estimate.
#
# GRAIN:
#   One row per batting slot per side per game_pk
#   = up to 18 rows per game (9 per side)
#
# INPUT:
#   game_context         — game_pk, home/away team IDs + names
#   depth_charts         — fg_role 1-9 = batting slot, team_abbr
#   offense_master_season — season stats keyed on mlbam_id
#   team_ids             — mlbam_team_id ↔ team_abbr bridge
#
# OUTPUT:
#   lineup_context
# ============================================================

required_objects <- c("game_context", "depth_charts",
                      "offense_master_season", "team_ids")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Batting order from depth charts (slots 1-9)
# ------------------------------------------------------------

batting_order <- depth_charts %>%
  dplyr::filter(
    fg_role %in% as.character(1:9),
    roster_type %in% c("mlb-sp", "mlb-bp", "mlb-sl")
  ) %>%
  dplyr::transmute(
    team_abbr    = team_abbr,
    batting_slot = as.integer(fg_role),
    mlbam_id     = mlbam_id,
    player_name  = player_name,
    fg_position  = fg_position
  )

# ------------------------------------------------------------
# Offense season stats (deduplicated to one row per player)
# Use highest PA row for players who changed teams
# ------------------------------------------------------------

off_stats <- offense_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id,
    dplyr::any_of(c(
      # MLB Stats API
      "mlb_pa", "mlb_avg", "mlb_obp", "mlb_slg", "mlb_ops",
      "mlb_hr", "mlb_rbi", "mlb_sb", "mlb_bb", "mlb_so",
      # FanGraphs
      "fg_wRC_plus", "fg_OBP", "fg_SLG", "fg_ISO",
      "fg_BB_pct", "fg_K_pct", "fg_BABIP", "fg_WAR",
      # BBRef
      "bbref_OPS", "bbref_PA"
    ))
  )

# ------------------------------------------------------------
# Build team ↔ game_pk bridge from game_context
# ------------------------------------------------------------

# Home teams
home_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "home",
    team_name = home_team_name,
    team_id   = as.integer(home_team_id)
  )

# Away teams
away_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "away",
    team_name = away_team_name,
    team_id   = as.integer(away_team_id)
  )

team_game_bridge <- dplyr::bind_rows(home_games, away_games) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, team_abbr),
    by = c("team_id" = "mlbam_team_id")
  )

# ------------------------------------------------------------
# Join batting order → game bridge → offense stats
# ------------------------------------------------------------

lineup_context <- team_game_bridge %>%
  dplyr::left_join(batting_order, by = "team_abbr") %>%
  dplyr::filter(!is.na(batting_slot)) %>%
  dplyr::left_join(off_stats, by = "mlbam_id") %>%
  dplyr::select(
    game_pk, game_date, side, team_name, team_abbr,
    batting_slot, mlbam_id, player_name, fg_position,
    dplyr::everything()
  ) %>%
  dplyr::arrange(game_pk, side, batting_slot)

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_games    <- dplyr::n_distinct(lineup_context$game_pk)
avg_lineup <- nrow(lineup_context) / max(n_games * 2, 1)

if (avg_lineup < 7) {
  warning("Average lineup size is ", round(avg_lineup, 1),
          " — expected ~9. Depth chart batting slots may be incomplete.")
}

message("02_lineup_context complete: ",
        nrow(lineup_context), " rows | ",
        n_games, " games | ",
        round(avg_lineup, 1), " avg batters per side")
