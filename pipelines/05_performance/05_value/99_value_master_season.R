# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_value_master_season.R
# ============================================================
# PURPOSE:
#   Combine FanGraphs fWAR and Baseball Reference bWAR into one
#   master value table. Attaches player names from player_master_ids.
#
# JOIN LOGIC:
#   FG batters  + FG pitchers  → stacked by player_type
#   BBRef batters join on mlbam_id + season (team_abbr = TOT)
#   BBRef pitchers join on mlbam_id + season (team_abbr = TOT)
#
# NOTE ON GRAIN:
#   A two-way player (e.g. Shohei Ohtani) will appear as both
#   "batter" and "pitcher" — one row each, same team_abbr.
#   This is intentional: WAR values are separate by role.
#
# KEY VALUE METRICS:
#   fg_WAR     — FanGraphs fWAR for batters (FIP/UZR-based)
#   fg_Dollars — Dollar value estimate
#   NOTE: bref_daily_batter/pitcher do not expose WAR.
#         Pitcher fWAR unavailable due to fg_pitcher_leaders() bug.
#
# OUTPUT:
#   value_master_season
# ============================================================

# ------------------------------------------------------------
# Required Objects
# ------------------------------------------------------------

required_objects <- c(
  "player_season_fg_batting_value",
  "player_season_fg_pitching_value",
  "player_season_bbref_batting_value",
  "player_season_bbref_pitching_value"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Stack FG Batters + Pitchers (FG is the spine — team-level grain)
# Then join BBRef WAR by player_type so batter WAR goes to batters
# and pitcher WAR goes to pitchers (no column collision).
# ------------------------------------------------------------

bbref_batter_stats <- player_season_bbref_batting_value %>%
  dplyr::select(mlbam_id, season,
                dplyr::any_of(c("bbref_PA", "bbref_OPS", "bbref_SB", "bbref_CS"))) %>%
  dplyr::distinct(mlbam_id, season, .keep_all = TRUE)

fg_batters <- player_season_fg_batting_value %>%
  dplyr::left_join(bbref_batter_stats, by = c("mlbam_id", "season"))

# Use FG pitcher spine if available; fall back to BBRef pitcher spine
# (fg_pitcher_leaders() has a known bug in some baseballr versions)
bbref_pitcher_stats <- player_season_bbref_pitching_value %>%
  dplyr::select(mlbam_id, season,
                dplyr::any_of(c("bbref_G", "bbref_GS", "bbref_IP",
                                "bbref_ERA", "bbref_WHIP", "bbref_SO",
                                "bbref_BB", "bbref_HR", "bbref_BAbip",
                                "bbref_SO9", "bbref_SO.W"))) %>%
  dplyr::distinct(mlbam_id, season, .keep_all = TRUE)

if (nrow(player_season_fg_pitching_value) > 0) {
  fg_pitchers <- player_season_fg_pitching_value %>%
    dplyr::left_join(bbref_pitcher_stats, by = c("mlbam_id", "season"))
} else {
  message("Using BBRef as pitcher spine (FG pitcher data unavailable)")
  fg_pitchers <- player_season_bbref_pitching_value %>%
    dplyr::select(-dplyr::any_of(c("bbref_id", "player_type"))) %>%
    dplyr::mutate(player_type = "pitcher",
                  fg_id       = NA_character_)
}

value_master_season <- dplyr::bind_rows(fg_batters, fg_pitchers) %>%

  # Attach player names
  dplyr::left_join(
    player_master_ids %>%
      dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
      dplyr::select(mlbam_id, player_name, name_first, name_last, bbref_id),
    by = "mlbam_id"
  ) %>%

  dplyr::select(
    mlbam_id, player_name, name_first, name_last,
    bbref_id, season, team_abbr, fg_id, player_type,
    dplyr::any_of(c("fg_WAR", "bbref_WAR", "fg_Dollars")),
    dplyr::everything()
  ) %>%

  dplyr::arrange(team_abbr, player_type, dplyr::desc(fg_WAR))

# ------------------------------------------------------------
# Validate (each player_type subset separately)
# ------------------------------------------------------------

purrr::walk(c("batter", "pitcher"), function(pt) {
  subset_df <- value_master_season %>%
    dplyr::filter(player_type == pt) %>%
    dplyr::select(mlbam_id, season, team_abbr, dplyr::everything())
  validate_performance_table(subset_df)
})

message("99_value_master_season complete: ",
        nrow(value_master_season), " total rows (",
        sum(value_master_season$player_type == "batter"),  " batters, ",
        sum(value_master_season$player_type == "pitcher"), " pitchers)")
