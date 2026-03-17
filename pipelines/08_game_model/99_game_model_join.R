# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 99_game_model_join.R
# ============================================================
# PURPOSE:
#   Validate all game model components and summarize coverage.
#   Does NOT merge into a single wide table — the three outputs
#   (starter_matchup, lineup_context, bullpen_grid) are kept
#   at their natural grains for the reporting layer.
#
# OUTPUTS (already built by prior scripts):
#   starter_matchup   — 2 rows per game (home SP, away SP)
#   lineup_context    — up to 18 rows per game (9 per side)
#   bullpen_grid      — N rows per game (all relievers, both teams)
# ============================================================

required_objects <- c("game_context", "starter_matchup",
                      "lineup_context", "bullpen_grid")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Coverage summary per game
# ------------------------------------------------------------

game_coverage <- game_context %>%
  dplyr::select(game_pk, game_date, home_team_name, away_team_name) %>%
  dplyr::left_join(
    starter_matchup %>%
      dplyr::group_by(game_pk) %>%
      dplyr::summarise(
        starters_identified = sum(!is.na(mlbam_id)),
        starters_with_stats = sum(!is.na(mlb_era)),
        .groups = "drop"
      ),
    by = "game_pk"
  ) %>%
  dplyr::left_join(
    lineup_context %>%
      dplyr::group_by(game_pk) %>%
      dplyr::summarise(
        lineup_slots_filled = dplyr::n_distinct(batting_slot[!is.na(mlbam_id)]),
        .groups = "drop"
      ),
    by = "game_pk"
  ) %>%
  dplyr::left_join(
    bullpen_grid %>%
      dplyr::group_by(game_pk) %>%
      dplyr::summarise(
        bullpen_arms       = dplyr::n(),
        bullpen_available  = sum(availability %in% c("available", "fresh"), na.rm = TRUE),
        bullpen_limited    = sum(availability == "limited",     na.rm = TRUE),
        bullpen_unavailable = sum(availability == "unavailable", na.rm = TRUE),
        .groups = "drop"
      ),
    by = "game_pk"
  )

# ------------------------------------------------------------
# Print summary
# ------------------------------------------------------------

message("=== 08_game_model coverage ===")
message(sprintf("%-6s  %-20s  %-20s  %s  %s  %s",
                "PK", "Away", "Home", "SPs", "Lineup", "Bullpen"))

for (i in seq_len(nrow(game_coverage))) {
  r <- game_coverage[i, ]
  message(sprintf("%-6s  %-20s  %-20s  %s/2  %s/18  %s arms",
    r$game_pk,
    substr(r$away_team_name, 1, 20),
    substr(r$home_team_name, 1, 20),
    dplyr::coalesce(r$starters_identified, 0L),
    dplyr::coalesce(r$lineup_slots_filled, 0L),
    dplyr::coalesce(r$bullpen_arms, 0L)
  ))
}

message("08_game_model complete: ",
        nrow(game_context), " games | ",
        nrow(starter_matchup), " starter rows | ",
        nrow(lineup_context), " lineup rows | ",
        nrow(bullpen_grid), " bullpen rows")
