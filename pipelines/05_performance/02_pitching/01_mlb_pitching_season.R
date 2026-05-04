# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_mlb_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull MLB season-level pitching stats (league-wide).
#
# OUTPUT:
#   - player_season_mlb_pitching
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

# ------------------------------------------------------------
# Determine Season To Pull
# ------------------------------------------------------------

season_to_pull <- target_season

# Pull regular-season stats only by requesting game_type = "R".
# This returns 0 rows during spring training (no R games played yet),
# triggering the prior-season fallback below.
# Falls back to unfiltered pull only if the game_type param is unsupported.
mlb_pitching_raw <- tryCatch(
  baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "pitching",
    player_pool = "all",
    season      = season_to_pull,
    game_type   = "R",
    limit       = 5000
  ),
  error = function(e) {
    baseballr::mlb_stats(
      stat_type   = "season",
      stat_group  = "pitching",
      player_pool = "all",
      season      = season_to_pull,
      limit       = 5000
    )
  }
)

# Use max IP as the signal: a pitcher with any regular-season starts will
# quickly accumulate IP that spring training appearances never reach.
max_ip <- if (nrow(mlb_pitching_raw) > 0 && "innings_pitched" %in% names(mlb_pitching_raw)) {
  max(suppressWarnings(as.numeric(mlb_pitching_raw$innings_pitched)), na.rm = TRUE)
} else {
  0
}

if (is.na(max_ip) || max_ip < 1) {
  message(season_to_pull, " pitching data appears to be preseason (max IP = ",
          round(max_ip, 1), "). Falling back to ", season_to_pull - 1,
          " (last full season).")
  season_to_pull <- target_season - 1L

  mlb_pitching_raw <- baseballr::mlb_stats(
    stat_type   = "season",
    stat_group  = "pitching",
    player_pool = "all",
    season      = season_to_pull,
    limit       = 5000
  )
}

# ------------------------------------------------------------
# Build Canonical Table
# ------------------------------------------------------------

cat("DEBUG 01_mlb_pitching_season: cols = ", paste(names(mlb_pitching_raw), collapse = ", "), "\n")

player_season_mlb_pitching <- mlb_pitching_raw %>%
  dplyr::transmute(

    # Keys
    mlbam_id      = as.integer(player_id),
    season        = as.integer(season_to_pull),
    team_name_raw = team_name,
    team_id       = team_id,
    
    # Workload
    mlb_g   = games_pitched,
    mlb_gs  = games_started,
    mlb_ip  = as.numeric(innings_pitched),
    mlb_bf  = batters_faced,
    
    # Results
    mlb_w   = wins,
    mlb_l   = losses,
    mlb_sv  = saves,
    mlb_hld = holds,
    
    # Run Prevention
    mlb_er  = earned_runs,
    mlb_r   = runs,
    mlb_era = as.numeric(era),
    mlb_whip = as.numeric(whip),
    
    # Contact
    mlb_h   = hits,
    mlb_hr  = home_runs,
    mlb_hbp = hit_by_pitch,
    
    # Discipline
    mlb_bb  = base_on_balls,
    mlb_ibb = intentional_walks,
    mlb_so  = strike_outs,
    
    # Batted Ball
    mlb_go  = ground_outs,
    mlb_ao  = air_outs,
    mlb_gidp = ground_into_double_play
  ) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(team_name, team_abbr),
    by = c("team_name_raw" = "team_name")
  ) %>%
  dplyr::mutate(team_abbr = dplyr::coalesce(team_abbr, team_name_raw)) %>%
  dplyr::select(-team_name_raw) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE)

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_mlb_pitching)

# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("01_mlb_pitching_season complete: ",
        nrow(player_season_mlb_pitching),
        " league-wide rows created for season ",
        season_to_pull, ".")