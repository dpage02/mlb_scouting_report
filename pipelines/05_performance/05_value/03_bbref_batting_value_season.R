# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 03_bbref_batting_value_season.R
# ============================================================
# PURPOSE:
#   Pull season-level Baseball Reference bWAR for batters.
#   bWAR uses DRS-based defense (vs UZR in fWAR), so it provides
#   an independent cross-check on player value.
#
# METHOD:
#   bref_daily_batter() returns cumulative season stats by date.
#   Pulling the full season range and taking the last entry per
#   player gives the final season-total bWAR.
#
# KEY METRICS:
#   bbref_PA  — plate appearances (season total, crosscheck)
#   bbref_OPS — OPS (crosscheck vs FanGraphs/MLB)
#   bbref_SB, bbref_CS — stolen bases
#   NOTE: bWAR is not available through bref_daily_batter().
#         WAR for batters comes from FanGraphs (fg_WAR).
#
# DATA SOURCE:
#   Baseball Reference via baseballr::bref_daily_batter()
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT" — BBRef accumulates across teams
#
# OUTPUT:
#   player_season_bbref_batting_value
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

season_start <- paste0(season_complete, "-03-20")
season_end   <- paste0(season_complete, "-11-05")

# ------------------------------------------------------------
# Pull BBRef Daily Batters (full season)
# ------------------------------------------------------------

bbref_raw <- tryCatch(
  baseballr::bref_daily_batter(
    t1 = season_start,
    t2 = season_end
  ),
  error = function(e) {
    message("BBRef batter pull failed: ", e$message)
    NULL
  }
)

if (is.null(bbref_raw) || nrow(bbref_raw) == 0) {
  message("No BBRef batter data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  season_start <- paste0(season_complete, "-03-20")
  season_end   <- paste0(season_complete, "-11-05")
  bbref_raw <- tryCatch(
    baseballr::bref_daily_batter(
      t1 = season_start,
      t2 = season_end
    ),
    error = function(e) {
      message("BBRef batter fallback also failed: ", e$message)
      NULL
    }
  )
}

# ------------------------------------------------------------
# Diagnostic (first run — confirm column names)
# ------------------------------------------------------------
message("BBRef daily batter columns: ", paste(names(bbref_raw), collapse = ", "))

# ------------------------------------------------------------
# Standardize — take final cumulative entry per player
# bref_daily_batter returns cumulative stats; last row = season total
# ------------------------------------------------------------

bbref_value_cols <- c(
  "bbref_id", "PA", "OPS", "SB", "CS"
)

player_season_bbref_batting_value <- bbref_raw %>%
  dplyr::select(dplyr::any_of(bbref_value_cols)) %>%
  # Last row per player = season-total cumulative stats
  dplyr::group_by(bbref_id) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup() %>%
  # Join to get mlbam_id
  dplyr::left_join(
    player_master_ids %>%
      dplyr::select(bbref_id, mlbam_id) %>%
      dplyr::filter(!is.na(bbref_id), !is.na(mlbam_id)) %>%
      dplyr::distinct(bbref_id, .keep_all = TRUE),
    by = "bbref_id"
  ) %>%
  dplyr::mutate(
    mlbam_id    = as.integer(mlbam_id),
    season      = as.integer(season_complete),
    team_abbr   = "TOT",
    player_type = "batter"
  ) %>%
  (\(df) {
    cols_to_rename <- intersect(names(df), c("PA", "OPS", "SB", "CS"))
    if (length(cols_to_rename) > 0) {
      df <- dplyr::rename_with(df, ~ paste0("bbref_", .x),
                               dplyr::all_of(cols_to_rename))
    }
    df
  })() %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, season, team_abbr, bbref_id, player_type,
                dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_bbref_batting_value)

message("03_bbref_batting_value_season complete: ",
        nrow(player_season_bbref_batting_value),
        " batter-season rows for season ", season_complete)
