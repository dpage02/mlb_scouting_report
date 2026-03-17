# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_offense_season.R
# ============================================================

# ------------------------------------------------------------
# Determine Fangraphs Season To Pull
# ------------------------------------------------------------

season_to_pull <- target_season

test_fg <- tryCatch(
  baseballr::fg_batter_leaders(
    qual = "0",
    startseason = as.character(season_to_pull),
    endseason   = as.character(season_to_pull),
    type = "8",
    pageitems = "10000"
  ),
  error = function(e) NULL
)

if (is.null(test_fg) || nrow(test_fg) == 0) {
  message("No FG data for ", season_to_pull,
          ". Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1
}

# ------------------------------------------------------------
# Pull Fangraphs Data
# ------------------------------------------------------------

fg_raw <- baseballr::fg_batter_leaders(
  qual = "0",
  startseason = as.character(season_to_pull),
  endseason   = as.character(season_to_pull),
  type = "8",
  pageitems = "10000"
)

# ------------------------------------------------------------
# Enforce Unique Crosswalk (Safety)
# ------------------------------------------------------------

player_master_ids <- player_master_ids %>%
  dplyr::distinct(fg_id, .keep_all = TRUE)

# ------------------------------------------------------------
# Clean & Join
# ------------------------------------------------------------

player_season_fg_offense <- fg_raw %>%
  
  mutate(
    fg_id     = as.integer(playerid),
    mlbam_id  = as.integer(xMLBAMID),
    team_abbr = team_name_abb
  ) %>%
  
  select(-playerid, -xMLBAMID, -team_name_abb) %>%
  
  rename_with(
    ~ paste0("fg_", .x),
    -c(fg_id, mlbam_id, team_abbr)
  ) %>%
  
  mutate(season = season_to_pull) %>%
  
  select(
    mlbam_id,
    season,
    team_abbr,
    fg_id,
    everything()
  )
# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_fg_offense)

# ------------------------------------------------------------
# Completion Message
# ------------------------------------------------------------

message("02_fangraphs_offense_season complete: ",
        nrow(player_season_fg_offense),
        " league-wide rows created for season ",
        season_to_pull, ".")