# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_pitching_master_join.R
# ============================================================
# PURPOSE:
#   Join all season-level pitching sources into one master table.
#
# OUTPUT:
#   - pitching_master_season
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

# ------------------------------------------------------------
# Required Objects (must exist upstream)
# ------------------------------------------------------------
required_objects <- c(
  "player_season_mlb_pitching",
  "player_season_fg_pitching",
  "player_season_statcast_pitching",
  "player_season_lahman_pitching",
  "player_season_bbref_pitching_advanced"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Join (MLB Spine)
# ------------------------------------------------------------

# FanGraphs and BBRef/Statcast may be from the prior season when current season
# data is insufficient (early season). Join by mlbam_id only so best-available
# advanced stats are always attached regardless of season mismatch.

# fg_ip may be absent when the FanGraphs main pull fell back to an empty stub
fg_ip_vec <- if ("fg_ip" %in% names(player_season_fg_pitching)) {
  dplyr::coalesce(player_season_fg_pitching$fg_ip, 0)
} else {
  rep(0, nrow(player_season_fg_pitching))
}

fg_best <- player_season_fg_pitching %>%
  dplyr::arrange(mlbam_id, dplyr::desc(fg_ip_vec)) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::any_of(c("season", "team_abbr",
                                  "fg_g", "fg_gs", "fg_ip",
                                  "fg_era", "fg_whip")))

sc_best <- player_season_statcast_pitching %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::any_of(c("season")))

bbref_best <- player_season_bbref_pitching_advanced %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::any_of(c("season")))

pitching_master_season <- player_season_mlb_pitching %>%

  # FanGraphs — join by mlbam_id; best-available season already selected above
  dplyr::left_join(fg_best, by = "mlbam_id") %>%

  # Statcast (no team split)
  dplyr::left_join(sc_best, by = "mlbam_id") %>%

  # Lahman (no team split, same season as MLB)
  dplyr::left_join(
    player_season_lahman_pitching,
    by = c("mlbam_id", "season")
  ) %>%

  # BBRef advanced — join by mlbam_id; best-available season already selected above
  dplyr::left_join(bbref_best, by = "mlbam_id")

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(pitching_master_season)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("99_pitching_master_join complete: ",
        nrow(pitching_master_season),
        " rows in pitching_master_season.")