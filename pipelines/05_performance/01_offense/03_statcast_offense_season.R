# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 03_statcast_offense_season.R
# ============================================================

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_to_pull <- target_season

test_sc <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "exit_velocity_barrels",
    year = season_to_pull,
    player_type = "batter",
    min_pa = 1
  ),
  error = function(e) NULL
)

if (is.null(test_sc) || nrow(test_sc) == 0) {
  message("No Statcast data for ", season_to_pull,
          ". Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1
}


# ------------------------------------------------------------
# Pull Leaderboards
# ------------------------------------------------------------

sc_ev <- baseballr::statcast_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = season_to_pull,
  player_type = "batter",
  min_pa = 1
)

sc_expected <- baseballr::statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = season_to_pull,
  player_type = "batter",
  min_pa = 1
)


# ------------------------------------------------------------
# Clean Function
# ------------------------------------------------------------

clean_sc <- function(df) {
  
  df %>%
    dplyr::rename(mlbam_id = player_id) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(-`last_name, first_name`, -year) %>%
    dplyr::rename_with(~ paste0("sc_", .x), -mlbam_id)
}


sc_ev_clean       <- clean_sc(sc_ev)
sc_expected_clean <- clean_sc(sc_expected)

# Sprint speed — ft/sec, competitive runs
sc_sprint <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "sprint_speed",
    year        = season_to_pull,
    player_type = "batter",
    min_pa      = 1
  ),
  error = function(e) {
    message("Sprint speed leaderboard failed: ", e$message)
    NULL
  }
)

sc_sprint_clean <- if (!is.null(sc_sprint) && nrow(sc_sprint) > 0) {
  clean_sc(sc_sprint)
} else {
  message("Sprint speed: no data for ", season_to_pull)
  NULL
}

# ------------------------------------------------------------
# Join
# ------------------------------------------------------------

player_season_statcast_offense <- sc_ev_clean %>%
  dplyr::left_join(sc_expected_clean, by = "mlbam_id") %>%
  dplyr::mutate(
    season    = season_to_pull,
    team_abbr = "TOT"
  ) %>%
  dplyr::select(mlbam_id, season, team_abbr, dplyr::everything())

if (!is.null(sc_sprint_clean)) {
  player_season_statcast_offense <- player_season_statcast_offense %>%
    dplyr::left_join(sc_sprint_clean, by = "mlbam_id", suffix = c("", "_dup")) %>%
    dplyr::select(-dplyr::ends_with("_dup"))
  message("Sprint speed joined: ",
          sum(!is.na(player_season_statcast_offense$sc_sprint_speed)),
          " players with data")
}


# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_statcast_offense)


# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("03_statcast_offense_season complete: ",
        nrow(player_season_statcast_offense),
        " league-wide rows created for season ",
        season_to_pull, ".")