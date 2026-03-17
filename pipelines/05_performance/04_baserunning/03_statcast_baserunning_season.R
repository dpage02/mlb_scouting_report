# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 03_statcast_baserunning_season.R
# ============================================================
# PURPOSE:
#   Pull Statcast baserunning and speed metrics from Baseball
#   Savant leaderboards. Three leaderboards are pulled:
#
#   1. sprint_speed — primary speed metric (ft/sec)
#      The gold standard for raw running ability.
#
#   2. running_splits_90_ft — home-to-first split times
#      Captures burst speed out of the batter's box.
#
#   3. baserunning — Statcast baserunning run value
#      Captures total baserunning run value including
#      extra base advancement and stolen base value.
#      Pulled with tryCatch — leaderboard name may vary.
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT" — Statcast metrics are season totals
#
# OUTPUT:
#   player_season_statcast_baserunning
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

test_sc <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "sprint_speed",
    year        = season_complete
  ),
  error = function(e) NULL
)

if (is.null(test_sc) || nrow(test_sc) == 0) {
  message("No Statcast sprint speed data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
}

# ------------------------------------------------------------
# Pull 1: Sprint Speed
# ------------------------------------------------------------

sc_sprint <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "sprint_speed",
    year        = season_complete
  ),
  error = function(e) {
    message("Sprint speed pull failed: ", e$message)
    NULL
  }
)

if (!is.null(sc_sprint) && nrow(sc_sprint) > 0) {
  sc_sprint_clean <- sc_sprint %>%
    dplyr::rename(mlbam_id = player_id) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(mlbam_id, dplyr::any_of(c(
      "sprint_speed", "hp_to_1b", "competitive_runs",
      "percentile", "sprint_speed_pct_rank"
    ))) %>%
    dplyr::rename_with(~ paste0("sc_", .x), -mlbam_id) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)
} else {
  message("Sprint speed data unavailable — creating empty table")
  sc_sprint_clean <- dplyr::tibble(mlbam_id = integer())
}

# ------------------------------------------------------------
# Pull 2: Running Splits (home to first)
# ------------------------------------------------------------

sc_splits <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "running_splits_90_ft",
    year        = season_complete
  ),
  error = function(e) {
    message("Running splits pull failed: ", e$message)
    NULL
  }
)

if (!is.null(sc_splits) && nrow(sc_splits) > 0) {
  splits_stat_cols <- c("median_hp_to_1b", "n_hp_to_1b", "hp_to_1b")

  sc_splits_clean <- sc_splits %>%
    dplyr::rename(mlbam_id = player_id) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(mlbam_id, dplyr::any_of(splits_stat_cols)) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)

  # Only rename if stat columns were actually captured
  cols_to_rename <- intersect(names(sc_splits_clean), splits_stat_cols)
  if (length(cols_to_rename) > 0) {
    sc_splits_clean <- sc_splits_clean %>%
      dplyr::rename_with(~ paste0("sc_", .x), .cols = dplyr::all_of(cols_to_rename))
  }
} else {
  message("Running splits data unavailable — skipping")
  sc_splits_clean <- dplyr::tibble(mlbam_id = integer())
}

# ------------------------------------------------------------
# Pull 3: Baserunning Run Value
# ------------------------------------------------------------

sc_brv <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "baserunning",
    year        = season_complete
  ),
  error = function(e) {
    message("Baserunning run value pull failed (leaderboard may require different name): ",
            e$message)
    NULL
  }
)

if (!is.null(sc_brv) && nrow(sc_brv) > 0) {
  brv_stat_cols <- c(
    "baserunning_run_value", "extra_bases_run_value",
    "basestealing_run_value", "runner_ev_run_value"
  )

  sc_brv_clean <- sc_brv %>%
    dplyr::rename(mlbam_id = player_id) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(mlbam_id, dplyr::any_of(brv_stat_cols)) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)

  # Only rename if stat columns were actually captured
  cols_to_rename <- intersect(names(sc_brv_clean), brv_stat_cols)
  if (length(cols_to_rename) > 0) {
    sc_brv_clean <- sc_brv_clean %>%
      dplyr::rename_with(~ paste0("sc_", .x), .cols = dplyr::all_of(cols_to_rename))
  }
} else {
  message("Baserunning run value data unavailable — skipping")
  sc_brv_clean <- dplyr::tibble(mlbam_id = integer())
}

# ------------------------------------------------------------
# Combine all Statcast baserunning sources
# ------------------------------------------------------------

player_season_statcast_baserunning <- sc_sprint_clean %>%
  dplyr::left_join(sc_splits_clean, by = "mlbam_id") %>%
  dplyr::left_join(sc_brv_clean,    by = "mlbam_id") %>%
  dplyr::mutate(
    season    = as.integer(season_complete),
    team_abbr = "TOT"
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, season, team_abbr, dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_statcast_baserunning)

message("03_statcast_baserunning_season complete: ",
        nrow(player_season_statcast_baserunning),
        " player-season rows for season ", season_complete)
