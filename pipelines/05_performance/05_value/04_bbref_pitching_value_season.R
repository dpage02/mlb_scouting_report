# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 04_bbref_pitching_value_season.R
# ============================================================
# PURPOSE:
#   Pull season-level Baseball Reference bWAR for pitchers.
#   bWAR for pitchers is RA9-based (actual runs allowed),
#   while fWAR is FIP-based (walks, strikeouts, HRs only).
#   Having both is useful: large fWAR vs bWAR gaps signal
#   pitchers whose results diverge from their peripherals.
#
# KEY METRICS:
#   bbref_IP, bbref_G, bbref_GS — workload
#   bbref_ERA, bbref_WHIP       — rate stats
#   bbref_SO, bbref_BB          — strikeouts and walks
#   bbref_HR                    — home runs allowed
#   bbref_BAbip                 — batting average on balls in play
#   NOTE: bref_daily_pitcher does not expose WAR.
#
# DATA SOURCE:
#   Baseball Reference via baseballr::bref_daily_pitcher()
#
# GRAIN:
#   One row per mlbam_id per season
#   team_abbr = "TOT" — BBRef accumulates across teams
#
# OUTPUT:
#   player_season_bbref_pitching_value
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

season_start <- paste0(season_complete, "-03-20")
season_end   <- paste0(season_complete, "-11-05")

# ------------------------------------------------------------
# Pull BBRef Daily Pitchers (full season)
# ------------------------------------------------------------

bbref_raw <- tryCatch(
  baseballr::bref_daily_pitcher(
    t1 = season_start,
    t2 = season_end
  ),
  error = function(e) {
    message("BBRef pitcher pull failed: ", e$message)
    NULL
  }
)

if (is.null(bbref_raw) || nrow(bbref_raw) == 0) {
  message("No BBRef pitcher data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  season_start <- paste0(season_complete, "-03-20")
  season_end   <- paste0(season_complete, "-11-05")
  bbref_raw <- tryCatch(
    baseballr::bref_daily_pitcher(
      t1 = season_start,
      t2 = season_end
    ),
    error = function(e) {
      message("BBRef pitcher fallback also failed: ", e$message)
      NULL
    }
  )
}

# ------------------------------------------------------------
# Diagnostic (first run — confirm column names)
# ------------------------------------------------------------
message("BBRef daily pitcher columns: ", paste(names(bbref_raw), collapse = ", "))

# ------------------------------------------------------------
# Standardize — take final cumulative entry per player
# ------------------------------------------------------------

bbref_value_cols <- c(
  "bbref_id",
  "G", "GS", "IP", "W", "L", "SV",
  "ERA", "WHIP", "SO", "BB", "HR",
  "BAbip", "SO9", "SO.W"
)

player_season_bbref_pitching_value <- bbref_raw %>%
  dplyr::select(dplyr::any_of(bbref_value_cols)) %>%
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
    player_type = "pitcher"
  ) %>%
  (\(df) {
    cols_to_rename <- intersect(names(df),
                                c("G", "GS", "IP", "W", "L", "SV",
                                  "ERA", "WHIP", "SO", "BB", "HR",
                                  "BAbip", "SO9", "SO.W"))
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

validate_performance_table(player_season_bbref_pitching_value)

message("04_bbref_pitching_value_season complete: ",
        nrow(player_season_bbref_pitching_value),
        " pitcher-season rows for season ", season_complete)
