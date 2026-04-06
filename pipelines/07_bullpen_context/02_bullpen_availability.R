# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 07_bullpen_context
# SCRIPT: 02_bullpen_availability.R
# ============================================================
# PURPOSE:
#   Compute raw fatigue metrics for each pitcher based on
#   recent pitching logs.
#
#   NOTE: Final availability classification (5-tier) is computed
#   in 99_bullpen_context_join.R after season stats and role
#   data are available for role-aware, month-aware thresholds.
#
# KEY OUTPUT COLUMNS:
#   mlbam_id              — pitcher ID
#   last_outing_date      — most recent appearance
#   days_rest             — days since last outing (NA if no recent app)
#   pitches_yesterday     — pitches thrown yesterday (0 if none)
#   pitches_last_3_days   — total pitches in last 3 days
#   appearances_last_7d   — total appearances in last 7 days
#   consecutive_days      — days pitched in a row entering today
#
# OUTPUT:
#   bullpen_availability
# ============================================================

if (!exists("recent_pitching_logs")) {
  stop("recent_pitching_logs not found — run 01_recent_pitching_logs.R first")
}

# ------------------------------------------------------------
# Aggregate per pitcher
# ------------------------------------------------------------

if (nrow(recent_pitching_logs) == 0) {

  message("No recent pitching log data — all pitchers marked as fresh")
  bullpen_availability <- dplyr::tibble(
    mlbam_id            = integer(),
    last_outing_date    = as.Date(character()),
    days_rest           = integer(),
    pitches_yesterday   = integer(),
    pitches_last_3_days = integer(),
    appearances_last_7d = integer(),
    consecutive_days    = integer()
  )

} else {

  bullpen_availability <- recent_pitching_logs %>%
    dplyr::group_by(mlbam_id) %>%
    dplyr::summarise(
      last_outing_date    = max(game_date),
      pitches_yesterday   = sum(pitches_thrown[days_ago == 1], na.rm = TRUE),
      pitches_2_days_ago  = sum(pitches_thrown[days_ago == 2], na.rm = TRUE),
      pitches_3_days_ago  = sum(pitches_thrown[days_ago == 3], na.rm = TRUE),
      pitches_last_3_days = sum(pitches_thrown[days_ago <= 3], na.rm = TRUE),
      appearances_last_7d = dplyr::n_distinct(game_date),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      days_rest = as.integer(target_date - last_outing_date),

      # Consecutive days pitched entering today (up to 3)
      consecutive_days = dplyr::case_when(
        pitches_yesterday > 0 & pitches_2_days_ago > 0 & pitches_3_days_ago > 0 ~ 3L,
        pitches_yesterday > 0 & pitches_2_days_ago > 0                           ~ 2L,
        pitches_yesterday > 0                                                     ~ 1L,
        TRUE                                                                      ~ 0L
      )
    ) %>%
    dplyr::select(mlbam_id, last_outing_date, days_rest,
                  pitches_yesterday, pitches_last_3_days,
                  appearances_last_7d, consecutive_days)

}

message("02_bullpen_availability complete: ",
        nrow(bullpen_availability), " pitcher-game rows | ",
        sum(bullpen_availability$pitches_yesterday > 0), " pitched yesterday | ",
        sum(bullpen_availability$consecutive_days >= 2), " on consecutive days | ",
        "availability classification deferred to 99_bullpen_context_join")
