# ============================================================
# mlb_scouting_report
# SCRIPT: run_pipeline_base.R
# ============================================================
# PURPOSE:
#   Build the slow-moving base layer: player IDs, season
#   performance stats (offense, pitching, defense, value),
#   and static context (park factors).
#
#   Run this once a week (or when you need fresh season stats).
#   Daily reports load from the base cache instead of re-running.
#
# WHAT THIS BUILDS:
#   01_ids           — permanent identity layer (team, player, park)
#   02_static_context — park factors (hard-coded), coordinates
#   05_performance   — season stats from MLB, FanGraphs, Statcast, BBRef
#
# WHAT IT DOES NOT RUN:
#   03_rosters, 04_game_context, 06-08 — those are daily
#
# OUTPUT:
#   data/base_cache.rds
# ============================================================

options(timeout = 300)

suppressMessages({
  source("pipelines/00_setup/00_load_packages.R")
  source("pipelines/00_setup/01_validate_enviroment.R")
  source("pipelines/00_setup/02_pipeline_config.R")
})

cat("Starting base pipeline build — IDs + season stats\n")
cat("(Run weekly or when you need fresh season stats)\n\n")

# ------------------------------------------------------------
# 01_ids
# ------------------------------------------------------------
log_message("01 — Building ID layer...")
suppressMessages({
  source("pipelines/01_ids/01_team_ids.R")
  source("pipelines/01_ids/02_player_master_ids.R")
  source("pipelines/01_ids/03_park_ids.R")
  source("pipelines/01_ids/04_umpire_ids_retrosheet.R")
  source("pipelines/01_ids/05_league_ids.R")
  source("pipelines/01_ids/99_write_id_tables.R")
})
log_message("01 — IDs complete")

# ------------------------------------------------------------
# 02_static_context
# ------------------------------------------------------------
suppressMessages(source("pipelines/02_static_context/00_park_coordinates.R"))
suppressMessages(source("pipelines/02_static_context/01_park_factors.R"))

# ------------------------------------------------------------
# 05_performance
# ------------------------------------------------------------
log_message("05 — Building offense...")
suppressMessages({
  source("pipelines/05_performance/00_schema/00_grain_definition.R")
  source("pipelines/05_performance/00_schema/01_column_dictionary.R")
  source("pipelines/05_performance/01_offense/01_mlb_offense_season.R")
  source("pipelines/05_performance/01_offense/02_fangraphs_offense_season.R")
  source("pipelines/05_performance/01_offense/03_statcast_offense_season.R")
  source("pipelines/05_performance/01_offense/04_mlb_offense_splits.R")
  source("pipelines/05_performance/01_offense/05_lahman_offense_season.R")
  source("pipelines/05_performance/01_offense/99_offense_master_join.R")
  source("pipelines/05_performance/01_offense/06_player_career_offense.R")
})
log_message("05 — offense complete")

log_message("05 — Building pitching...")
suppressMessages({
  source("pipelines/05_performance/02_pitching/01_mlb_pitching_season.R")
  source("pipelines/05_performance/02_pitching/02_fangraphs_pitching_season.R")
  source("pipelines/05_performance/02_pitching/03_statcast_pitching_season.R")
  source("pipelines/05_performance/02_pitching/04_mlb_pitching_splits.R")
  source("pipelines/05_performance/02_pitching/05_lahman_pitching_season.R")
  source("pipelines/05_performance/02_pitching/07_bbref_pitching_advanced.R")
  source("pipelines/05_performance/02_pitching/99_pitching_master_join.R")
  source("pipelines/05_performance/02_pitching/06_player_career_pitching.R")
  source("pipelines/05_performance/02_pitching/08_statcast_pitch_arsenal.R")
})
log_message("05 — pitching complete")

suppressMessages({
  source("pipelines/05_performance/04_baserunning/00_schema.R")
  source("pipelines/05_performance/04_baserunning/01_mlb_baserunning_season.R")
  source("pipelines/05_performance/04_baserunning/02_fangraphs_baserunning_season.R")
  source("pipelines/05_performance/04_baserunning/03_statcast_baserunning_season.R")
  source("pipelines/05_performance/04_baserunning/05_lahman_baserunning_season.R")
  source("pipelines/05_performance/04_baserunning/99_baserunning_master_join.R")
})

suppressMessages({
  source("pipelines/05_performance/05_value/00_schema.R")
  source("pipelines/05_performance/05_value/01_fangraphs_batting_value_season.R")
  source("pipelines/05_performance/05_value/02_fangraphs_pitching_value_season.R")
  source("pipelines/05_performance/05_value/03_bbref_batting_value_season.R")
  source("pipelines/05_performance/05_value/04_bbref_pitching_value_season.R")
  source("pipelines/05_performance/05_value/99_value_master_season.R")
})

suppressMessages({
  source("pipelines/05_performance/03_defense/00_schema.R")
  source("pipelines/05_performance/03_defense/01_mlb_fielding_pull.R")
  source("pipelines/05_performance/03_defense/02_fangraphs_fielding_pull.R")
  source("pipelines/05_performance/03_defense/03_statcast_fiedling_pull.R")
  source("pipelines/05_performance/03_defense/05_lahman_fielding_pull.R")
  source("pipelines/05_performance/03_defense/99_join_player_defense.R")
})
log_message("05 — performance complete")

# ------------------------------------------------------------
# Save base cache
# ------------------------------------------------------------

base_cache_objects <- c(
  "target_season",
  "team_ids", "player_master_ids",
  "park_factors",
  # Final joined tables
  "offense_master_season",   "player_career_offense",
  "pitching_master_season",  "player_career_pitching",
  "pitcher_arsenal",
  "defense_master_season",
  "value_master_season",
  # Fast component tables (re-run daily)
  "player_season_fg_pitching", "player_season_mlb_pitching",
  "player_season_mlb_offense_splits",
  "player_season_mlb_pitching_splits",
  # Slow component tables — cached so daily can skip re-pulling them
  # but still run master joins with fresh MLB API + FanGraphs spine
  "player_season_statcast_offense",
  "player_season_lahman_offense",
  "player_season_statcast_pitching",
  "player_season_lahman_pitching",
  "player_season_bbref_pitching_advanced",
  "player_season_fg_offense"
)

base_cache <- mget(
  base_cache_objects[base_cache_objects %in% ls()],
  envir = .GlobalEnv
)

if (!dir.exists("data")) dir.create("data")
saveRDS(base_cache, "data/base_cache.rds")

log_message(sprintf(
  "Base cache saved — %d objects, %.1f MB",
  length(base_cache),
  file.size("data/base_cache.rds") / 1e6
))

log_message("Base pipeline complete — run run_pipeline_daily.R to build today's games")
