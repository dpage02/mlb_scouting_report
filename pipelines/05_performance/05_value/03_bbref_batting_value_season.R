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
#   bbref_PA, bbref_AB, bbref_G, bbref_R
#   bbref_H, bbref_1B, bbref_2B, bbref_3B, bbref_HR, bbref_TB
#   bbref_RBI, bbref_SB, bbref_CS
#   bbref_BB, bbref_IBB, bbref_SO, bbref_HBP, bbref_SF, bbref_SH, bbref_GDP
#   bbref_BA, bbref_OBP, bbref_SLG, bbref_OPS, bbref_BAbip
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

# All stat columns available from bref_daily_batter()
# X1B = singles (bRef names it X1B due to R's column name rules)
bbref_stat_cols <- c(
  "PA", "AB", "G", "R",
  "H", "X1B", "X2B", "X3B", "HR", "TB",
  "RBI", "SB", "CS",
  "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP",
  "BA", "OBP", "SLG", "OPS", "BAbip"
)

player_season_bbref_batting_value <- bbref_raw %>%
  dplyr::select(bbref_id, dplyr::any_of(bbref_stat_cols)) %>%
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
  # Prefix all stat columns with bbref_, rename X1B -> 1B for clarity
  dplyr::rename_with(
    ~ paste0("bbref_", .x),
    dplyr::any_of(bbref_stat_cols)
  ) %>%
  dplyr::rename_with(
    ~ sub("bbref_X1B", "bbref_1B", .x),
    dplyr::any_of("bbref_X1B")
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, season, team_abbr, bbref_id, player_type,
                dplyr::everything())

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_bbref_batting_value)

bbref_cols_pulled <- names(player_season_bbref_batting_value)[
  grepl("^bbref_", names(player_season_bbref_batting_value)) &
  !names(player_season_bbref_batting_value) %in% c("bbref_id")
]
message("03_bbref_batting_value_season complete: ",
        nrow(player_season_bbref_batting_value),
        " batter-season rows for season ", season_complete,
        " | bbref stat cols: ", paste(bbref_cols_pulled, collapse = ", "))
