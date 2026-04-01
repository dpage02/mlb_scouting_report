# ============================================================
# mlb_scouting_report
# PIPELINE ORCHESTRATOR
# SCRIPT: run_pipeline_full.R
# ============================================================
# PURPOSE:
#   Single entry-point to run the full scouting pipeline
#   from a clean R session.
#
# WHAT THIS SCRIPT DOES:
#   - Sources setup/config
#   - Builds ID layer
#   - Builds season roster universe
#   - Builds game schedule context
#
# WHAT THIS SCRIPT DOES NOT DO:
#   - Build performance metrics
#   - Build derived labels
#   - Generate reports
# ============================================================

suppressMessages({
  source("pipelines/00_setup/00_load_packages.R")
  source("pipelines/00_setup/01_validate_enviroment.R")
  source("pipelines/00_setup/02_pipeline_config.R")
})

cat("Starting mlb_scouting_report — FULL PIPELINE\n")

# ------------------------------------------------------------
# 01_ids (Permanent Identity Layer)
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

# ------------------------------------------------------------
# 02_static_context
# ------------------------------------------------------------

suppressMessages(source("pipelines/02_static_context/00_park_coordinates.R"))

# ------------------------------------------------------------
# 03_rosters (Season Scope Layer)
# ------------------------------------------------------------

log_message("03 — Building season roster universe...")

suppressMessages(source("pipelines/03_rosters/00_player_season_scope.R"))
log_message("03 — rosters complete")

# ------------------------------------------------------------
# 04_game_context (Event Layer)
# ------------------------------------------------------------

log_message("04 — Building schedule context...")

suppressMessages({
  source("pipelines/04_game_context/01_schedule.R")
  source("pipelines/04_game_context/02_game_meta.R")
  source("pipelines/04_game_context/03_probable_pitchers.R")
  source("pipelines/04_game_context/04_umpire_assignments.R")
  source("pipelines/04_game_context/05_weather_forecast.R")
  source("pipelines/04_game_context/06_series_context.R")
  source("pipelines/04_game_context/99_game_context.R")
})
log_message("04 — game context complete")

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
# 06_player_context
# ------------------------------------------------------------

log_message("06 — Building player context...")

suppressMessages({
  source("pipelines/06_player_context/01_depth_charts.R")
  source("pipelines/06_player_context/99_player_context_join.R")
})
log_message("06 — player context complete")

# ------------------------------------------------------------
# 07_bullpen_context
# ------------------------------------------------------------

log_message("07 — Building bullpen context...")

suppressMessages({
  source("pipelines/07_bullpen_context/01_recent_pitching_logs.R")
  source("pipelines/07_bullpen_context/02_bullpen_availability.R")
  source("pipelines/07_bullpen_context/99_bullpen_context_join.R")
})
log_message("07 — bullpen context complete")

# ------------------------------------------------------------
# 08_game_model
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
# Completion
# ------------------------------------------------------------

log_message("Pipeline complete")
