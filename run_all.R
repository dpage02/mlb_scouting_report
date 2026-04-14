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

# ------------------------------------------------------------
# Render reports
# ------------------------------------------------------------

source("09_reporting/render_report.R")
source("09_reporting/render_deep_dives.R")
if (interactive()) {
  browseURL(file.path(getwd(), paste0("reports/scouting_", Sys.Date(), ".html")))
}
