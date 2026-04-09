# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_offense_master_join.R
# ============================================================
# PURPOSE:
#   Join all season-level offense sources into one master table.
#
# OUTPUT:
#   offense_master_season
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
#
# JOIN STRATEGY:
#   - MLB Stats API is the spine (has team_abbr per stint)
#   - FanGraphs: join on mlbam_id + season only (drop FG team_abbr
#     to avoid mismatch; take highest-PA row for traded players)
#   - Statcast / Lahman: player-level only (no team split)
#     Drop their team_abbr = "TOT" before joining to avoid
#     team_abbr.x / team_abbr.y conflicts
# ============================================================

# ------------------------------------------------------------
# Required Objects (must exist upstream)
# ------------------------------------------------------------

required_objects <- c(
  "player_season_mlb_offense",
  "player_season_fg_offense",
  "player_season_statcast_offense",
  "player_season_lahman_offense"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Prep FanGraphs: one row per mlbam_id + season
# For traded players who appear multiple times, keep highest PA
# FanGraphs returns PA uppercase → fg_PA after prefix
# ------------------------------------------------------------

pa_col <- intersect(c("fg_PA", "fg_pa"), names(player_season_fg_offense))[1]

fg_grouped <- player_season_fg_offense %>%
  dplyr::select(-team_abbr) %>%
  dplyr::group_by(mlbam_id, season)

fg_for_join <- if (!is.na(pa_col)) {
  dplyr::slice_max(fg_grouped, .data[[pa_col]], n = 1, with_ties = FALSE)
} else {
  dplyr::slice_head(fg_grouped, n = 1)
}

fg_for_join <- dplyr::ungroup(fg_for_join)

# ------------------------------------------------------------
# Begin Join (MLB Spine)
# ------------------------------------------------------------

offense_master_season <- player_season_mlb_offense %>%

  # FanGraphs — join by mlbam_id only (no season key).
  # Early in the season, FanGraphs falls back to prior-year data
  # (too few PA to compute wRC+), so the season in player_season_fg_offense
  # may be current_year-1 while the MLB spine is current_year.
  # Joining by mlbam_id only ensures advanced stats always attach.
  # Same pattern used by 99_pitching_master_join.R.
  dplyr::left_join(
    fg_for_join %>% dplyr::select(-season),
    by = "mlbam_id"
  ) %>%

  # Statcast (player-level, drop team_abbr = "TOT")
  dplyr::left_join(
    player_season_statcast_offense %>% dplyr::select(-team_abbr),
    by = c("mlbam_id", "season")
  ) %>%

  # Lahman (player-level, drop team_abbr = "TOT" and redundant lahman_yearID)
  dplyr::left_join(
    player_season_lahman_offense %>%
      dplyr::select(-team_abbr, -dplyr::any_of("lahman_yearID")),
    by = c("mlbam_id", "season")
  )

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(offense_master_season)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

wrc_plus_coverage <- if ("fg_wRC_plus" %in% names(offense_master_season)) {
  sum(!is.na(offense_master_season$fg_wRC_plus))
} else {
  "column missing"
}

message("99_offense_master_join complete: ",
        nrow(offense_master_season),
        " rows in offense_master_season | fg_wRC_plus non-NA: ",
        wrc_plus_coverage)
