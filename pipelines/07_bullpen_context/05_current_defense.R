# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 07_bullpen_context
# SCRIPT: 05_current_defense.R
# ============================================================
# PURPOSE:
#   Pull current-season fielding stats for today's lineup
#   players. Complements the prior-season defense metrics in
#   defense_master_season with early-season data.
#
# NOTE ON SAMPLE SIZE:
#   Early in the season (<50 inn), these numbers are noisy.
#   The display table labels them "YYYY (early)" so the reader
#   understands the context. OAA in particular needs 300+ inn
#   to stabilize — show it as a directional signal only.
#
# OUTPUT:
#   current_defense_stats
#   Columns: mlbam_id, cur_inn, cur_errors, cur_fld_pct, cur_oaa
# ============================================================

lineup_mlbam_ids <- if (exists("lineup_context") && nrow(lineup_context) > 0) {
  unique(as.integer(lineup_context$mlbam_id[!is.na(lineup_context$mlbam_id)]))
} else {
  integer(0)
}

# ── Current-season MLB fielding (Inn, E, Fld%) ──────────────

cur_mlb_raw <- tryCatch(
  baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "fielding",
    player_pool = "all",
    season      = target_season,
    sport_id    = 1,
    game_type   = "R",
    limit       = 5000
  ),
  error = function(e) {
    message("Current-season MLB fielding pull failed: ", e$message)
    NULL
  }
)

cur_mlb <- if (!is.null(cur_mlb_raw) && nrow(cur_mlb_raw) > 0 &&
               "player_id" %in% names(cur_mlb_raw)) {
  cur_mlb_raw %>%
    dplyr::transmute(
      mlbam_id  = as.integer(player_id),
      position  = position_abbreviation,
      cur_inn   = as.numeric(innings),
      cur_errors = as.integer(errors),
      cur_fld_pct = as.numeric(fielding)
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    # Keep primary position (most innings) per player
    dplyr::group_by(mlbam_id) %>%
    dplyr::slice_max(cur_inn, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(mlbam_id, cur_inn, cur_errors, cur_fld_pct)
} else {
  dplyr::tibble(
    mlbam_id    = integer(),
    cur_inn     = numeric(),
    cur_errors  = integer(),
    cur_fld_pct = numeric()
  )
}

# ── Current-season Statcast OAA ─────────────────────────────

cur_oaa_raw <- tryCatch(
  baseballr::statcast_leaderboards(
    leaderboard = "outs_above_average",
    year        = target_season
  ),
  error = function(e) {
    message("Current-season Statcast OAA pull failed: ", e$message)
    NULL
  }
)

cur_oaa <- if (!is.null(cur_oaa_raw) && nrow(cur_oaa_raw) > 0 &&
               "player_id" %in% names(cur_oaa_raw)) {
  cur_oaa_raw %>%
    dplyr::transmute(
      mlbam_id = as.integer(player_id),
      cur_oaa  = outs_above_average
    ) %>%
    dplyr::filter(!is.na(mlbam_id))
} else {
  dplyr::tibble(mlbam_id = integer(), cur_oaa = numeric())
}

# ── Join ────────────────────────────────────────────────────

current_defense_stats <- dplyr::full_join(cur_mlb, cur_oaa, by = "mlbam_id") %>%
  dplyr::filter(!is.na(mlbam_id))

message("05_current_defense complete: ",
        nrow(current_defense_stats), " players | season ", target_season,
        " | MLB fielding: ", nrow(cur_mlb), " rows",
        " | Statcast OAA: ", nrow(cur_oaa), " rows")
