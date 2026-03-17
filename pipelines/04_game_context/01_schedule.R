# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 01_schedule.R
# ============================================================
# PURPOSE:
#   Construct the foundational schedule_context table.
#
# DEFAULT BEHAVIOR:
#   - Auto-selects next MLB game date (league-wide)
#
# OPTIONAL OVERRIDES:
#   - TARGET_DATE (Date or YYYY-MM-DD)
#   - TARGET_TEAM_ABBR (e.g. "ATL")
#   - GAME_TYPE_FILTER (e.g. "R", "S")
#
# OUTPUT:
#   - schedule_context (one row per game_pk)
#
# DESIGN:
#   League-wide by default.
#   Neutral. No Braves bias baked in.
# ============================================================

library(dplyr)
library(baseballr)
library(lubridate)

message("Running 01_schedule.R")

# ------------------------------------------------------------
# 1. Resolve Parameters
# ------------------------------------------------------------

if (!exists("TARGET_TEAM_ABBR")) {
  TARGET_TEAM_ABBR <- NULL
}

if (!exists("GAME_TYPE_FILTER")) {
  GAME_TYPE_FILTER <- NULL
}

today_date <- Sys.Date()
current_season <- year(today_date)

raw_schedule_full <- baseballr::mlb_schedule(
  season = current_season
)

if (nrow(raw_schedule_full) == 0) {
  stop("No schedule returned for season: ", current_season)
}

# ------------------------------------------------------------
# 2. Determine Target Date
# ------------------------------------------------------------

if (!exists("TARGET_DATE")) {
  
  # Auto-select next MLB game date (league-wide)
  next_game_date <- raw_schedule_full %>%
    filter(as.Date(official_date) >= today_date) %>%
    arrange(official_date) %>%
    slice(1) %>%
    pull(official_date)
  
  if (length(next_game_date) == 0) {
    stop("No upcoming games found in current season.")
  }
  
  target_date <- as.Date(next_game_date)
  
  message("Auto-selected next MLB game date: ", target_date)
  
} else {
  
  target_date <- as.Date(TARGET_DATE)
  message("Using manual TARGET_DATE: ", target_date)
  
}

target_season <- year(target_date)

# ------------------------------------------------------------
# 3. Filter Schedule To Target Date
# ------------------------------------------------------------

schedule_day <- raw_schedule_full %>%
  filter(as.Date(official_date) == target_date)

if (nrow(schedule_day) == 0) {
  stop("No MLB games found for date: ", target_date)
}

message("Games on target date: ", nrow(schedule_day))

# ------------------------------------------------------------
# 4. Optional Game Type Filter
# ------------------------------------------------------------

if (!is.null(GAME_TYPE_FILTER)) {
  
  message("Filtering for game type: ", GAME_TYPE_FILTER)
  
  schedule_day <- schedule_day %>%
    filter(game_type == GAME_TYPE_FILTER)
  
  if (nrow(schedule_day) == 0) {
    stop("No games found for specified game type on date.")
  }
}

# ------------------------------------------------------------
# 5. Optional Team Filter (ID-safe)
# ------------------------------------------------------------

if (!is.null(TARGET_TEAM_ABBR)) {
  
  message("Filtering for team: ", TARGET_TEAM_ABBR)
  
  target_team_id <- team_ids %>%
    filter(team_abbr == TARGET_TEAM_ABBR) %>%
    pull(mlbam_team_id)
  
  if (length(target_team_id) == 0) {
    stop("TARGET_TEAM_ABBR not found in team_ids.")
  }
  
  schedule_day <- schedule_day %>%
    filter(
      teams_home_team_id == target_team_id |
        teams_away_team_id == target_team_id
    )
  
  if (nrow(schedule_day) == 0) {
    stop("No games found for team on date.")
  }
}

# ------------------------------------------------------------
# 6. Construct schedule_context
# ------------------------------------------------------------

schedule_context <- schedule_day %>%
  transmute(
    game_pk          = game_pk,
    game_date        = as.Date(official_date),
    home_team_id     = teams_home_team_id,
    home_team_name   = teams_home_team_name,
    away_team_id     = teams_away_team_id,
    away_team_name   = teams_away_team_name,
    venue_id         = venue_id,
    venue_name       = venue_name,
    game_status      = status_detailed_state,
    day_night        = day_night,
    series_desc      = series_description,
    series_game_num  = series_game_number,
    games_in_series  = games_in_series,
    game_type        = game_type
  ) %>%
  distinct(game_pk, .keep_all = TRUE)

# ------------------------------------------------------------
# 7. Integrity Check
# ------------------------------------------------------------

stopifnot(
  nrow(schedule_context) ==
    n_distinct(schedule_context$game_pk)
)

message("schedule_context built: ", nrow(schedule_context), " game(s)")
message("01_schedule.R complete")
