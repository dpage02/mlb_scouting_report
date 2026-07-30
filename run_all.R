# ============================================================
# mlb_scouting_report
# SCRIPT: run_all.R
# ============================================================
# PURPOSE:
#   Main daily entry point. Runs the pipeline and renders
#   all reports.
#
# PIPELINE STRATEGY:
#   DAILY (fast, 5-10 min):
#     Loads season stats from base cache, refreshes only
#     what changes day-to-day (rosters, game context,
#     weather, bullpen logs, batting streaks, lineups).
#
#   BASE REBUILD (slow, 30-60 min):
#     Re-pulls all season stats from MLB, FanGraphs,
#     Statcast, BBRef. Triggered automatically when:
#       - base cache is missing
#       - base cache is more than 7 days old
#     Or manually: source("run_pipeline_base.R")
#
# USAGE:
#   source("run_all.R")   — normal daily use
# ============================================================

options(timeout = 300)

# ------------------------------------------------------------
# Decide: daily refresh or full base rebuild?
# ------------------------------------------------------------

base_cache_path <- "data/base_cache.rds"

needs_base_rebuild <- !file.exists(base_cache_path) ||
  as.numeric(difftime(Sys.time(), file.mtime(base_cache_path), units = "days")) > 7

if (needs_base_rebuild) {
  if (!file.exists(base_cache_path)) {
    cat("Base cache not found — running full base build (one-time setup)...\n")
  } else {
    age <- round(as.numeric(difftime(Sys.time(), file.mtime(base_cache_path), units = "days")))
    cat(sprintf("Base cache is %d days old — refreshing season stats...\n", age))
  }
  source("run_pipeline_base.R")
} else {
  age_hrs <- round(as.numeric(difftime(Sys.time(), file.mtime(base_cache_path), units = "hours")))
  cat(sprintf("Base cache: %.0f days old — using cached season stats\n",
              as.numeric(difftime(Sys.time(), file.mtime(base_cache_path), units = "days"))))
}

# ------------------------------------------------------------
# Daily pipeline: rosters, game context, bullpen, lineups
# ------------------------------------------------------------

source("run_pipeline_daily.R")

# log_predictions.R logs today's predictions to predictions_log.csv and
# backfills actual scores for past pending games. Must run after
# run_pipeline_daily.R (needs game_context, starter_matchup,
# lineup_context, bullpen_grid) and before render_game_results.R
# (reads predictions_log.csv to know which games to render). This used
# to run from run_pipeline_phase01.R, which run_all.R stopped calling
# once the pipeline split into run_pipeline_base.R / run_pipeline_daily.R
# — log_predictions.R never got moved over, so predictions_log.csv never
# accumulated history and render_game_results.R always rendered nothing.
source("log_predictions.R")

# ------------------------------------------------------------
# Render reports
# ------------------------------------------------------------

source("09_reporting/render_report.R")
# render_game_results.R must run before render_recap.R: the daily recap
# links to each game's result_*.html page, and it checks file.exists()
# to skip the link gracefully when one isn't available rather than
# pointing at a 404 — that check only works if the result pages are
# already written to reports/ by the time the recap renders.
source("09_reporting/render_game_results.R")
source("09_reporting/render_recap.R")
source("09_reporting/render_accuracy.R")
source("09_reporting/render_deep_dives.R")
source("09_reporting/render_series.R")
source("push_to_web.R")
