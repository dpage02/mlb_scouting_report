# ============================================================
# mlb_scouting_report
# SCRIPT: run_pipeline_daily.R
# ============================================================
# PURPOSE:
#   Fast daily refresh — loads season stats from base cache,
#   then re-runs only what changes day-to-day:
#     rosters, game context, bullpen availability,
#     batting streaks, and game model.
#
# TYPICAL RUNTIME: 5-10 minutes (vs 30-60 for full rebuild)
#
# REQUIRES:
#   data/base_cache.rds — built by run_pipeline_base.R
#   If base cache is missing or stale (>7 days), run_all.R
#   will automatically trigger a full base rebuild first.
#
# OUTPUT:
#   data/pipeline_cache.rds  — used by reports at render time
# ============================================================

options(timeout = 300)

suppressMessages({
  source("pipelines/00_setup/00_load_packages.R")
  source("pipelines/00_setup/01_validate_enviroment.R")
  source("pipelines/00_setup/02_pipeline_config.R")  # sets target_date, target_season
})

cat("Starting daily pipeline...\n\n")

# ------------------------------------------------------------
# Load base cache (season stats + IDs)
# ------------------------------------------------------------

base_cache_path <- "data/base_cache.rds"

if (!file.exists(base_cache_path)) {
  stop(
    "Base cache not found. Run run_pipeline_base.R first.\n",
    "  source('run_pipeline_base.R')"
  )
}

base_age_days <- as.numeric(
  difftime(Sys.time(), file.mtime(base_cache_path), units = "days")
)

if (base_age_days > 7) {
  warning(sprintf(
    "Base cache is %.0f days old — season stats may be stale. ",
    base_age_days
  ), "Consider running run_pipeline_base.R to refresh.")
}

log_message(sprintf(
  "Loading base cache (%.1f MB, %.0f days old)...",
  file.size(base_cache_path) / 1e6,
  base_age_days
))

base_cache <- readRDS(base_cache_path)
list2env(base_cache, envir = .GlobalEnv)
rm(base_cache)

log_message("Base cache loaded")

# park_coordinates is static — source directly if not in cache
if (!exists("park_coordinates"))
  suppressMessages(source("pipelines/02_static_context/00_park_coordinates.R"))

# ------------------------------------------------------------
# 05_performance — stats refresh (MLB API + FanGraphs only)
# Statcast / Lahman / BBRef stay in base cache (slow to pull).
# Re-running just the fast sources updates wRC+, xFIP, ERA,
# and splits to reflect yesterday's games (~1 min).
# ------------------------------------------------------------

log_message("05 — Refreshing season stats (MLB API + FanGraphs)...")

suppressMessages({
  source("pipelines/05_performance/00_schema/00_grain_definition.R")
  source("pipelines/05_performance/00_schema/01_column_dictionary.R")

  # Offense: MLB API spine + FanGraphs advanced (wRC+, ISO, BB%, K%)
  source("pipelines/05_performance/01_offense/01_mlb_offense_season.R")
  source("pipelines/05_performance/01_offense/02_fangraphs_offense_season.R")
  source("pipelines/05_performance/01_offense/04_mlb_offense_splits.R")
  # Re-join with cached Statcast + Lahman from base cache
  source("pipelines/05_performance/01_offense/99_offense_master_join.R")
  source("pipelines/05_performance/01_offense/06_player_career_offense.R")

  # Pitching: MLB API spine + FanGraphs advanced (xFIP, FIP, K%, SIERA)
  source("pipelines/05_performance/02_pitching/01_mlb_pitching_season.R")
  source("pipelines/05_performance/02_pitching/02_fangraphs_pitching_season.R")
  source("pipelines/05_performance/02_pitching/04_mlb_pitching_splits.R")
  # Re-join with cached Statcast + Lahman + BBRef from base cache
  source("pipelines/05_performance/02_pitching/99_pitching_master_join.R")
  source("pipelines/05_performance/02_pitching/06_player_career_pitching.R")
})

log_message("05 — stats refresh complete")

# ------------------------------------------------------------
# 03_rosters — depth charts change with roster moves
# ------------------------------------------------------------

log_message("03 — Refreshing rosters...")
suppressMessages(source("pipelines/03_rosters/00_player_season_scope.R"))
log_message("03 — rosters complete")

# ------------------------------------------------------------
# 04_game_context — today's schedule, probables, weather
# ------------------------------------------------------------

log_message("04 — Building game context...")
suppressMessages({
  source("pipelines/04_game_context/01_schedule.R")
})
cat("Target date:", format(target_date, "%A, %B %d %Y"), "\n\n")
suppressMessages({
  source("pipelines/04_game_context/02_game_meta.R")
  source("pipelines/04_game_context/03_probable_pitchers.R")
  source("pipelines/04_game_context/04_umpire_assignments.R")
  source("pipelines/04_game_context/05_weather_forecast.R")
  source("pipelines/04_game_context/06_series_context.R")
  source("pipelines/04_game_context/99_game_context.R")
  source("pipelines/04_game_context/07_team_standings.R")
})
log_message("04 — game context complete")

# ------------------------------------------------------------
# 06_player_context — depth chart roles (needs fresh rosters)
# ------------------------------------------------------------

log_message("06 — Building player context...")
suppressMessages({
  source("pipelines/06_player_context/01_depth_charts.R")
  source("pipelines/06_player_context/99_player_context_join.R")
})
log_message("06 — player context complete")

# ------------------------------------------------------------
# 07_bullpen_context — recent logs, availability, batting streaks
# ------------------------------------------------------------

log_message("07 — Building bullpen context...")
suppressMessages({
  source("pipelines/07_bullpen_context/01_recent_pitching_logs.R")
  source("pipelines/07_bullpen_context/02_bullpen_availability.R")
  source("pipelines/07_bullpen_context/03_recent_batting_logs.R")
  source("pipelines/07_bullpen_context/05_current_defense.R")
  source("pipelines/07_bullpen_context/99_bullpen_context_join.R")
})
log_message("07 — bullpen context complete")

# ------------------------------------------------------------
# 08_game_model — matchups, lineups, splits
# ------------------------------------------------------------

log_message("08 — Building game model...")
suppressMessages({
  source("pipelines/08_game_model/01_starter_matchup.R")
  source("pipelines/08_game_model/02_lineup_context.R")
  source("pipelines/08_game_model/03_bullpen_grid.R")
  source("pipelines/08_game_model/04_matchup_splits.R")
  source("pipelines/08_game_model/99_game_model_join.R")
})
log_message("08 — game model complete")

# ------------------------------------------------------------
# Save pipeline cache (for report rendering)
# ------------------------------------------------------------

pipeline_cache_objects <- c(
  "target_date", "target_season", "team_ids",
  "game_context", "lineup_context", "starter_matchup", "bullpen_grid",
  "offense_master_season", "player_career_offense", "player_career_pitching",
  "pitching_master_season", "pitcher_arsenal",
  "defense_master_season", "lineup_context_splits", "starter_splits",
  "player_season_fg_pitching", "player_season_mlb_pitching",
  "player_master_ids", "depth_charts",
  "park_factors", "recent_batter_streaks", "current_defense_stats", "value_master_season",
  "player_season_mlb_offense_splits", "player_season_mlb_pitching_splits",
  "batter_pitch_type_stats",
  "steamer_projections",
  "team_standings"
)

pipeline_cache <- mget(
  pipeline_cache_objects[pipeline_cache_objects %in% ls()],
  envir = .GlobalEnv
)

if (!dir.exists("data")) dir.create("data")
saveRDS(pipeline_cache, "data/pipeline_cache.rds")

log_message(sprintf(
  "Pipeline cache saved — %d objects | %.1f MB",
  length(pipeline_cache),
  file.size("data/pipeline_cache.rds") / 1e6
))

log_message("Daily pipeline complete")
