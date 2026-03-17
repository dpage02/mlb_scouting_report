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
    player_type = "batter"
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
  player_type = "batter"
)

sc_expected <- baseballr::statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = season_to_pull,
  player_type = "batter"
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


sc_ev_clean <- clean_sc(sc_ev)
sc_expected_clean <- clean_sc(sc_expected)


# ------------------------------------------------------------
# Join
# ------------------------------------------------------------

player_season_statcast_offense <- sc_ev_clean %>%
  dplyr::left_join(sc_expected_clean, by = "mlbam_id") %>%
  dplyr::mutate(
    season = season_to_pull,
    team_abbr = "TOT"
  ) %>%
  dplyr::select(
    mlbam_id,
    season,
    team_abbr,
    dplyr::everything()
  )


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