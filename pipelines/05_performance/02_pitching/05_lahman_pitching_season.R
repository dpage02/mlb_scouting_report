# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 05_lahman_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull Lahman season-level pitching stats (league-wide).
#
# OUTPUT:
#   - player_season_lahman_pitching
#
# GRAIN:
#   One row per mlbam_id / season
#
# NOTES:
#   Lahman Pitching can have multiple rows per player-season (stints/teams).
#   We aggregate to player-season totals to match master-join design.
# ============================================================

# ------------------------------------------------------------
# Required Objects
# ------------------------------------------------------------
required_objects <- c("player_master_ids", "target_season")
missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Determine Latest Lahman Season Available
# ------------------------------------------------------------
latest_lahman_season <- max(Lahman::Pitching$yearID, na.rm = TRUE)

season_to_pull <- if (target_season > latest_lahman_season) {
  message("Lahman Pitching data not available for ", target_season,
          ". Using latest Lahman season: ", latest_lahman_season, ".")
  latest_lahman_season
} else {
  target_season
}

# ------------------------------------------------------------
# Pull + Aggregate Lahman Pitching (player-season totals)
# ------------------------------------------------------------
lahman_pitching_season <- Lahman::Pitching %>%
  dplyr::filter(yearID == season_to_pull) %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    # Workload
    lah_g  = sum(G,  na.rm = TRUE),
    lah_gs = sum(GS, na.rm = TRUE),
    
    # W/L/SV
    lah_w  = sum(W,  na.rm = TRUE),
    lah_l  = sum(L,  na.rm = TRUE),
    lah_sv = sum(SV, na.rm = TRUE),
    
    # Batters / Outcomes Against
    lah_bf = sum(BFP, na.rm = TRUE),
    lah_h  = sum(H,   na.rm = TRUE),
    lah_r  = sum(R,   na.rm = TRUE),
    lah_er = sum(ER,  na.rm = TRUE),
    lah_hr = sum(HR,  na.rm = TRUE),
    lah_bb = sum(BB,  na.rm = TRUE),
    lah_so = sum(SO,  na.rm = TRUE),
    lah_hbp = sum(HBP, na.rm = TRUE),
    
    # Outs -> IP
    lah_ip_outs = sum(IPouts, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lah_ip = lah_ip_outs / 3,
    lah_era = dplyr::if_else(lah_ip > 0, (lah_er * 9) / lah_ip, NA_real_),
    season = as.integer(yearID)
  )

# ------------------------------------------------------------
# Map Lahman -> MLBAM via player_master_ids
# ------------------------------------------------------------
player_season_lahman_pitching <- lahman_pitching_season %>%
  dplyr::left_join(
    player_master_ids %>%
      dplyr::select(player_master_id, lahman_id, mlbam_id, player_name),
    by = c("playerID" = "lahman_id")
  ) %>%
  dplyr::transmute(
    mlbam_id,
    player_name,
    season,
    
    # Lahman pitching fields (prefixed)
    lah_g, lah_gs,
    lah_w, lah_l, lah_sv,
    lah_bf,
    lah_h, lah_r, lah_er, lah_hr, lah_bb, lah_so, lah_hbp,
    lah_ip, lah_era
  ) %>%
  dplyr::filter(!is.na(mlbam_id))

# ------------------------------------------------------------
# Validate Grain (mlbam_id / season)
# ------------------------------------------------------------
stopifnot(
  nrow(player_season_lahman_pitching) ==
    dplyr::n_distinct(paste0(player_season_lahman_pitching$mlbam_id, "_",
                             player_season_lahman_pitching$season))
)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------
message("05_lahman_pitching_season complete: ",
        nrow(player_season_lahman_pitching),
        " league-wide rows created for season ",
        season_to_pull, ".")