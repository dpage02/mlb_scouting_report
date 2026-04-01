# Render only: skips the pipeline, uses existing cache
# Use this when tweaking display/layout — much faster than run_all.R
source("09_reporting/render_report.R")
source("09_reporting/render_deep_dives.R")
browseURL(file.path(getwd(), paste0("reports/scouting_", Sys.Date(), ".html")))
