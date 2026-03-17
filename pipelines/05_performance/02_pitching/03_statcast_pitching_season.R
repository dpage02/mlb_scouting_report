# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 03_statcast_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull Statcast season-level pitching leaderboards.
#
# OUTPUT:
#   - player_season_statcast_pitching
#
# GRAIN:
#   One row per mlbam_id / season
# ============================================================


# ------------------------------------------------------------
# Determine Season (match MLB pull)
# ------------------------------------------------------------

season_to_pull <- unique(player_season_mlb_pitching$season)[1]


# ------------------------------------------------------------
# Pull Exit Velocity / Barrels (Pitcher)
# ------------------------------------------------------------

sc_ev <- baseballr::statcast_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = season_to_pull,
  player_type = "pitcher"
)


# ------------------------------------------------------------
# Pull Expected Statistics (Pitcher)
# ------------------------------------------------------------

sc_expected <- baseballr::statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = season_to_pull,
  player_type = "pitcher"
)


# ------------------------------------------------------------
# Clean EV Table
# ------------------------------------------------------------

sc_ev_clean <- sc_ev %>%
  dplyr::transmute(
    mlbam_id = player_id,
    season = year,
    
    # Quality of Contact Allowed
    sc_avg_ev_allowed = avg_hit_speed,
    sc_max_ev_allowed = max_hit_speed,
    sc_barrels_allowed = barrels,
    sc_barrel_pct_allowed = brl_percent,
    
    # Hard Contact Proxy
    sc_ev95plus_allowed = ev95plus,
    sc_ev95percent_allowed = ev95percent
  )


# ------------------------------------------------------------
# Clean Expected Table
# ------------------------------------------------------------

sc_expected_clean <- sc_expected %>%
  dplyr::transmute(
    mlbam_id = player_id,
    season = year,
    
    # Expected vs Actual
    sc_ba_allowed = ba,
    sc_xba_allowed = est_ba,
    sc_slg_allowed = slg,
    sc_xslg_allowed = est_slg,
    sc_woba_allowed = woba,
    sc_xwoba_allowed = est_woba,
    
    # Run Prevention
    sc_era = era,
    sc_xera = xera
  )

# ------------------------------------------------------------
# Combine Statcast Tables
# ------------------------------------------------------------

player_season_statcast_pitching <- sc_ev_clean %>%
  dplyr::left_join(
    sc_expected_clean,
    by = c("mlbam_id", "season")
  )


# ------------------------------------------------------------
# Validate (no team split for statcast)
# ------------------------------------------------------------

stopifnot(
  nrow(player_season_statcast_pitching) ==
    dplyr::n_distinct(player_season_statcast_pitching$mlbam_id)
)


# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("03_statcast_pitching_season complete: ",
        nrow(player_season_statcast_pitching),
        " league-wide rows created for season ",
        season_to_pull, ".")