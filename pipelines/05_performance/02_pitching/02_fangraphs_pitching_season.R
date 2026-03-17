# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_pitching_season.R
# ============================================================
# PURPOSE:
#   Build season-level Fangraphs pitching via game logs.
#
# OUTPUT:
#   - player_season_fg_pitching
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================


# ------------------------------------------------------------
# Determine Season (match MLB pull)
# ------------------------------------------------------------

season_to_pull <- unique(player_season_mlb_pitching$season)[1]


# ------------------------------------------------------------
# Get Relevant FG IDs (only MLB pitchers that season)
# ------------------------------------------------------------

fg_pitcher_ids <- player_master_ids %>%
  dplyr::filter(
    mlbam_id %in% player_season_mlb_pitching$mlbam_id,
    !is.na(fg_id)
  ) %>%
  dplyr::distinct(fg_id, mlbam_id, player_name)


# ------------------------------------------------------------
# Function: Pull + Aggregate One Pitcher
# ------------------------------------------------------------

pull_and_aggregate_pitcher <- function(fg_id, mlbam_id, player_name) {
  
  logs <- tryCatch(
    baseballr::fg_pitcher_game_logs(
      playerid = fg_id,
      year = season_to_pull
    ),
    error = function(e) return(NULL)
  )
  
  if (is.null(logs) || nrow(logs) == 0) return(NULL)
  
  logs %>%
    dplyr::group_by(Team, season) %>%
    dplyr::summarise(
      
      fg_g  = n(),
      fg_gs = sum(GS, na.rm = TRUE),
      fg_ip = sum(IP, na.rm = TRUE),
      fg_er = sum(ER, na.rm = TRUE),
      fg_h  = sum(H, na.rm = TRUE),
      fg_hr = sum(HR, na.rm = TRUE),
      fg_bb = sum(BB, na.rm = TRUE),
      fg_hbp = sum(HBP, na.rm = TRUE),
      fg_so = sum(SO, na.rm = TRUE),
      fg_bf = sum(TBF, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      fg_era = ifelse(fg_ip > 0, (fg_er * 9) / fg_ip, NA_real_),
      fg_whip = ifelse(fg_ip > 0, (fg_bb + fg_h) / fg_ip, NA_real_),
      fg_k_pct = ifelse(fg_bf > 0, fg_so / fg_bf, NA_real_),
      fg_bb_pct = ifelse(fg_bf > 0, fg_bb / fg_bf, NA_real_),
      fg_k_bb_pct = fg_k_pct - fg_bb_pct,
      
      fg_id = fg_id,
      mlbam_id = mlbam_id,
      player_name = player_name,
      team_abbr = Team
    )
}


# ------------------------------------------------------------
# Loop All Pitchers
# ------------------------------------------------------------

player_season_fg_pitching <- purrr::pmap_dfr(
  fg_pitcher_ids,
  pull_and_aggregate_pitcher
)


# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_fg_pitching)


# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message("02_fangraphs_pitching_season complete: ",
        nrow(player_season_fg_pitching),
        " rows created for season ",
        season_to_pull, ".")