# Full run: pipeline + cache + render
# Use this once a day (or when you need fresh data)
options(timeout = 300)  # 5 min download timeout (default 60s too short for multiple API calls)
invisible(capture.output(source("run_pipeline_phase01.R")))

pipeline_cache_objects <- c(
  "target_date", "target_season", "team_ids",
  "game_context", "lineup_context", "starter_matchup", "bullpen_grid",
  "offense_master_season", "player_career_offense", "player_career_pitching",
  "pitching_master_season", "pitcher_arsenal",
  "defense_master_season", "lineup_context_splits", "starter_splits",
  "player_season_fg_pitching", "player_season_mlb_pitching",
  "player_master_ids",
  "depth_charts"
)
pipeline_cache <- mget(
  pipeline_cache_objects[pipeline_cache_objects %in% ls()],
  envir = .GlobalEnv
)
if (!dir.exists("data")) dir.create("data")
saveRDS(pipeline_cache, "data/pipeline_cache.rds")

source("09_reporting/render_report.R")
source("09_reporting/render_deep_dives.R")
browseURL(file.path(getwd(), paste0("reports/scouting_", Sys.Date(), ".html")))
