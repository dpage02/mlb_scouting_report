# ============================================================
# patch_cache.R
# ============================================================
# PURPOSE:
#   Fast cache patch — skips the full pipeline.
#   Loads the existing cache, re-runs only:
#     1. player_master_ids  (Lahman + Chadwick, ~15s)
#     2. FanGraphs pitching (one API call, ~15s) → fg_WAR, fg_ERA_minus
#     3. pitcher_arsenal    (Statcast API, ~10s)
#     4. bbref_pitching_advanced  (BBRef scrape, ~10s)
#   Then patches pitching_master_season and starter_matchup,
#   saves the updated cache, and re-renders all reports.
#
# USE WHEN:
#   - ERA+/WAR showing blank in pitcher tables
#   - Pitch arsenal showing "not available"
#   - You DON'T want to re-run the full pipeline
#
# USE run_all.R WHEN:
#   - You need truly fresh game/lineup/roster data
# ============================================================

options(timeout = 300)  # 5 min download timeout (default 60s too short for multiple API calls)
message("=== patch_cache.R: loading existing cache ===")

if (!file.exists("data/pipeline_cache.rds")) {
  stop("No cache found at data/pipeline_cache.rds. Run run_all.R first.")
}

cache <- readRDS("data/pipeline_cache.rds")
list2env(cache, envir = .GlobalEnv)
rm(cache)

message("Cache loaded. Objects available: ", paste(sort(ls()), collapse = ", "))

# Schema helpers needed by FanGraphs script
source("pipelines/05_performance/00_schema/00_grain_definition.R")
source("pipelines/05_performance/00_schema/01_column_dictionary.R")

# ------------------------------------------------------------
# 1. Rebuild player_master_ids (needed for FG + BBRef joins)
# ------------------------------------------------------------
message("\n=== Step 1: player_master_ids ===")
source("pipelines/01_ids/02_player_master_ids.R")
message("player_master_ids: ", nrow(player_master_ids), " players | ",
        sum(!is.na(player_master_ids$fg_id)), " with fg_id | ",
        sum(!is.na(player_master_ids$bbref_id)), " with bbref_id")

# ------------------------------------------------------------
# 2. Re-run FanGraphs pitching (refreshes fg_WAR, fg_ERA_minus)
# ------------------------------------------------------------
message("\n=== Step 2: FanGraphs pitching ===")
source("pipelines/05_performance/02_pitching/02_fangraphs_pitching_season.R")
n_fg_war <- if ("fg_WAR" %in% names(player_season_fg_pitching))
  sum(!is.na(player_season_fg_pitching$fg_WAR)) else 0L
n_fg_era_minus <- if ("fg_ERA_minus" %in% names(player_season_fg_pitching))
  sum(!is.na(player_season_fg_pitching$fg_ERA_minus)) else 0L
message("player_season_fg_pitching: ", nrow(player_season_fg_pitching),
        " pitchers | fg_WAR non-NA: ", n_fg_war,
        " | fg_ERA_minus non-NA: ", n_fg_era_minus)

# ------------------------------------------------------------
# 3. Rebuild pitcher_arsenal (Statcast)
# ------------------------------------------------------------
message("\n=== Step 3: pitcher_arsenal (Statcast) ===")
source("pipelines/05_performance/02_pitching/08_statcast_pitch_arsenal.R")
message("pitcher_arsenal: ", nrow(pitcher_arsenal), " rows | ",
        dplyr::n_distinct(pitcher_arsenal$mlbam_id), " pitchers")

# ------------------------------------------------------------
# 4. Rebuild player_season_bbref_pitching_advanced (BBRef)
# ------------------------------------------------------------
message("\n=== Step 4: bbref_pitching_advanced (BBRef scrape) ===")
source("pipelines/05_performance/02_pitching/07_bbref_pitching_advanced.R")
n_bbref_era  <- if ("bbref_ERA_plus" %in% names(player_season_bbref_pitching_advanced))
  sum(!is.na(player_season_bbref_pitching_advanced$bbref_ERA_plus)) else 0L
n_bbref_war  <- if ("bbref_WAR" %in% names(player_season_bbref_pitching_advanced))
  sum(!is.na(player_season_bbref_pitching_advanced$bbref_WAR)) else 0L
message("player_season_bbref_pitching_advanced: ", nrow(player_season_bbref_pitching_advanced),
        " rows | ERA+: ", n_bbref_era, " | bWAR: ", n_bbref_war)

# ------------------------------------------------------------
# 5. Patch pitching_master_season with fresh FG + BBRef data
# ------------------------------------------------------------
message("\n=== Step 5: patching pitching_master_season ===")

fg_cols    <- names(player_season_fg_pitching)[!names(player_season_fg_pitching) %in%
                c("mlbam_id", "season", "team_abbr")]
bbref_cols <- c("bbref_ERA_plus", "bbref_FIP",
                "bbref_H9", "bbref_HR9", "bbref_BB9", "bbref_SO9",
                "bbref_SO_W", "bbref_WAR")

# Drop stale fg_ and bbref_ columns
pitching_master_season <- pitching_master_season %>%
  dplyr::select(-dplyr::any_of(c(fg_cols, bbref_cols)))

# Prepare fresh lookups — one row per mlbam_id
fg_ip_vec <- if ("fg_ip" %in% names(player_season_fg_pitching))
  dplyr::coalesce(player_season_fg_pitching$fg_ip, 0) else
  rep(0, nrow(player_season_fg_pitching))

fg_best <- player_season_fg_pitching %>%
  dplyr::arrange(mlbam_id, dplyr::desc(fg_ip_vec)) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::any_of(c("season", "team_abbr",
                                  "fg_g", "fg_gs", "fg_ip",
                                  "fg_era", "fg_whip")))

bbref_best <- player_season_bbref_pitching_advanced %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::any_of("season"))

# Join back in
pitching_master_season <- pitching_master_season %>%
  dplyr::left_join(fg_best,    by = "mlbam_id") %>%
  dplyr::left_join(bbref_best, by = "mlbam_id")

n_fg_war_master   <- if ("fg_WAR"        %in% names(pitching_master_season)) sum(!is.na(pitching_master_season$fg_WAR))        else 0L
n_fg_era_master   <- if ("fg_ERA_minus"  %in% names(pitching_master_season)) sum(!is.na(pitching_master_season$fg_ERA_minus))   else 0L
n_bbref_era_mast  <- if ("bbref_ERA_plus" %in% names(pitching_master_season)) sum(!is.na(pitching_master_season$bbref_ERA_plus)) else 0L
n_bbref_war_mast  <- if ("bbref_WAR"      %in% names(pitching_master_season)) sum(!is.na(pitching_master_season$bbref_WAR))      else 0L
message("pitching_master_season patched: ", nrow(pitching_master_season), " rows | ",
        "fg_WAR: ", n_fg_war_master, " | fg_ERA_minus: ", n_fg_era_master, " | ",
        "bbref_ERA+: ", n_bbref_era_mast, " | bbref_WAR: ", n_bbref_war_mast)

# ------------------------------------------------------------
# 6. Rebuild starter_matchup with fresh pitching stats
#    Also re-run 04_matchup_splits to restore pitch_hand
# ------------------------------------------------------------
message("\n=== Step 6: starter_matchup + splits ===")
source("pipelines/08_game_model/01_starter_matchup.R")
source("pipelines/08_game_model/04_matchup_splits.R")

# Diagnostic: check today's starters
starters_check <- starter_matchup %>%
  dplyr::select(game_pk, side, pitcher_name, mlbam_id,
                dplyr::any_of(c("fg_WAR", "fg_ERA_minus", "bbref_ERA_plus", "bbref_WAR")))
message("Today's starters:")
print(as.data.frame(starters_check))

# Check arsenal coverage for today's starters
if (nrow(pitcher_arsenal) > 0) {
  starter_ids <- unique(starter_matchup$mlbam_id[!is.na(starter_matchup$mlbam_id)])
  in_arsenal  <- sum(starter_ids %in% pitcher_arsenal$mlbam_id)
  message("Starters in arsenal: ", in_arsenal, "/", length(starter_ids))
} else {
  message("pitcher_arsenal is empty — Statcast call may have failed")
}

# ------------------------------------------------------------
# 7. Save updated cache
# ------------------------------------------------------------
message("\n=== Step 7: saving updated cache ===")

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
  "steamer_projections", "team_standings", "baserunning_master_season"
)
pipeline_cache <- mget(
  pipeline_cache_objects[pipeline_cache_objects %in% ls()],
  envir = .GlobalEnv
)
saveRDS(pipeline_cache, "data/pipeline_cache.rds")
message("Cache saved to data/pipeline_cache.rds")

# ------------------------------------------------------------
# 8. Render reports
# ------------------------------------------------------------
message("\n=== Step 8: rendering reports ===")
source("09_reporting/render_report.R")
source("09_reporting/render_deep_dives.R")
browseURL(file.path(getwd(), paste0("reports/scouting_", Sys.Date(), ".html")))

message("\n=== patch_cache.R complete ===")
