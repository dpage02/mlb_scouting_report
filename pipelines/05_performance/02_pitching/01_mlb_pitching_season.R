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

# Test if data exists for target season
mlb_test <- baseballr::mlb_stats(
  stat_type = "season",
  stat_group = "pitching",
  player_pool = "all",
  season = season_to_pull,
  limit = 1000
)

if (nrow(mlb_test) == 0) {
  message("No MLB pitching data found for ", season_to_pull,
          ". Falling back to previous season.")
  season_to_pull <- target_season - 1
}

# ------------------------------------------------------------
# Pull League-Wide Pitching
# ------------------------------------------------------------

mlb_pitching_raw <- baseballr::mlb_stats(
  stat_type = "season",
  stat_group = "pitching",
  player_pool = "all",
  season = season_to_pull,
  limit = 5000
)

# ------------------------------------------------------------
# Build Canonical Table
# ------------------------------------------------------------

player_season_mlb_pitching <- mlb_pitching_raw %>%
  dplyr::transmute(

    # Keys
    mlbam_id      = player_id,
    season        = as.integer(season),
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