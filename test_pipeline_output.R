# ============================================================
# mlb_scouting_report
# SCRIPT: test_pipeline_output.R
# ============================================================
# PURPOSE:
#   Verify all pipeline objects needed for reporting are present
#   and populated correctly. Run this after run_pipeline_phase01.R
#   to confirm data quality before rendering the report.
#
# USAGE:
#   source("test_pipeline_output.R")  # assumes pipeline already ran
# ============================================================

cat("\n========================================================\n")
cat("  PIPELINE OUTPUT DIAGNOSTIC\n")
cat("  Run date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("========================================================\n\n")

pass <- 0
fail <- 0

check <- function(label, expr, note = NULL) {
  result <- tryCatch(expr, error = function(e) FALSE)
  status <- if (isTRUE(result)) {
    pass <<- pass + 1
    "[PASS]"
  } else {
    fail <<- fail + 1
    "[FAIL]"
  }
  msg <- paste(status, label)
  if (!is.null(note)) msg <- paste0(msg, " — ", note)
  cat(msg, "\n")
}

# ============================================================
# SECTION 1: Object Existence
# ============================================================
cat("--- Objects ---\n")

required_objects <- c(
  "game_context", "team_ids", "player_master_ids",
  "player_season_mlb_offense", "player_season_fg_offense",
  "player_season_statcast_offense", "offense_master_season",
  "player_season_mlb_pitching", "pitching_master_season",
  "depth_charts", "bullpen_context",
  "starter_matchup", "lineup_context", "bullpen_grid"
)

for (obj in required_objects) {
  check(
    paste0("object exists: ", obj),
    exists(obj) && is.data.frame(get(obj)) && nrow(get(obj)) > 0
  )
}

# ============================================================
# SECTION 2: Seasons Being Used
# ============================================================
cat("\n--- Season Check ---\n")

if (exists("player_season_mlb_offense") && nrow(player_season_mlb_offense) > 0) {
  mlb_off_season <- unique(player_season_mlb_offense$season)
  cat("[INFO] MLB offense season(s):", paste(mlb_off_season, collapse = ", "), "\n")
}

if (exists("player_season_fg_offense") && nrow(player_season_fg_offense) > 0) {
  fg_off_season <- unique(player_season_fg_offense$season)
  cat("[INFO] FanGraphs offense season(s):", paste(fg_off_season, collapse = ", "), "\n")
}

if (exists("player_season_mlb_pitching") && nrow(player_season_mlb_pitching) > 0) {
  mlb_pit_season <- unique(player_season_mlb_pitching$season)
  cat("[INFO] MLB pitching season(s):", paste(mlb_pit_season, collapse = ", "), "\n")
}

if (exists("pitching_master_season") && nrow(pitching_master_season) > 0) {
  fg_pit_season <- unique(pitching_master_season$season)
  cat("[INFO] Pitching master season(s):", paste(fg_pit_season, collapse = ", "), "\n")
}

if (exists("target_season")) {
  cat("[INFO] target_season:", target_season, "\n")
}

if (exists("target_date")) {
  cat("[INFO] target_date:", format(target_date, "%Y-%m-%d"), "\n")
}

# ============================================================
# SECTION 3: Row Counts
# ============================================================
cat("\n--- Row Counts ---\n")

count_check <- function(obj_name, min_rows = 1) {
  if (!exists(obj_name)) return(invisible(NULL))
  n <- nrow(get(obj_name))
  check(
    paste0(obj_name, ": ", n, " rows"),
    n >= min_rows,
    if (n < min_rows) paste0("expected >= ", min_rows)
  )
}

count_check("player_season_mlb_offense",   100)
count_check("player_season_fg_offense",    100)
count_check("player_season_statcast_offense", 50)
count_check("offense_master_season",       100)
count_check("player_season_mlb_pitching",  100)
count_check("pitching_master_season",      100)
count_check("game_context",                 1)
count_check("depth_charts",               100)
count_check("bullpen_context",             50)
count_check("starter_matchup",              1)
count_check("lineup_context",               9)
count_check("bullpen_grid",                10)

# ============================================================
# SECTION 4: Key Column Checks
# ============================================================
cat("\n--- Key Columns ---\n")

col_check <- function(obj_name, col_name) {
  if (!exists(obj_name)) return(invisible(NULL))
  df <- get(obj_name)
  check(
    paste0(obj_name, " has column: ", col_name),
    col_name %in% names(df)
  )
}

# FanGraphs wRC+ (the main bug we fixed)
col_check("player_season_fg_offense",  "fg_wRC_plus")
col_check("offense_master_season",     "fg_wRC_plus")
col_check("lineup_context",            "fg_wRC_plus")

# Offense spine
col_check("player_season_mlb_offense", "mlb_avg")
col_check("player_season_mlb_offense", "mlb_obp")
col_check("player_season_mlb_offense", "mlb_slg")
col_check("player_season_mlb_offense", "mlb_ops")
col_check("player_season_mlb_offense", "team_abbr")

# Pitching stats for starter table
col_check("pitching_master_season",    "mlb_era")
col_check("pitching_master_season",    "mlb_whip")
col_check("starter_matchup",           "mlb_era")
col_check("starter_matchup",           "mlb_whip")

# Bullpen grid
col_check("bullpen_grid",              "availability")
col_check("bullpen_grid",              "days_rest")
col_check("bullpen_grid",              "pitches_yesterday")

# Game context
col_check("game_context",              "game_pk")
col_check("game_context",              "home_team_name")
col_check("game_context",              "away_team_name")
col_check("game_context",              "venue_name")
col_check("game_context",              "home_plate_umpire")

# ============================================================
# SECTION 5: NA Coverage on Key Display Columns
# ============================================================
cat("\n--- NA Coverage (key display columns) ---\n")

coverage_check <- function(obj_name, col_name, min_pct = 0.5) {
  if (!exists(obj_name)) return(invisible(NULL))
  df <- get(obj_name)
  if (!col_name %in% names(df)) {
    cat("[SKIP]", obj_name, "$", col_name, "— column not found\n")
    return(invisible(NULL))
  }
  n_total  <- nrow(df)
  n_filled <- sum(!is.na(df[[col_name]]))
  pct      <- n_filled / n_total
  check(
    sprintf("%s$%s: %d/%d filled (%.0f%%)",
            obj_name, col_name, n_filled, n_total, pct * 100),
    pct >= min_pct,
    if (pct < min_pct) sprintf("expected >= %.0f%%", min_pct * 100)
  )
}

coverage_check("offense_master_season",    "fg_wRC_plus",  0.50)
coverage_check("offense_master_season",    "mlb_avg",      0.90)
coverage_check("offense_master_season",    "mlb_obp",      0.90)
coverage_check("offense_master_season",    "mlb_ops",      0.90)
coverage_check("lineup_context",           "fg_wRC_plus",  0.50)
coverage_check("lineup_context",           "mlb_avg",      0.50)
coverage_check("lineup_context",           "mlb_ops",      0.50)
coverage_check("pitching_master_season",   "mlb_era",      0.80)
coverage_check("pitching_master_season",   "mlb_whip",     0.80)
coverage_check("starter_matchup",          "pitcher_name", 0.50)
coverage_check("game_context",             "venue_name",   0.80)

# ============================================================
# SECTION 6: Game Model Coverage
# ============================================================
cat("\n--- Game Model Coverage ---\n")

if (exists("game_context") && exists("starter_matchup") &&
    exists("lineup_context") && exists("bullpen_grid")) {

  n_games     <- dplyr::n_distinct(game_context$game_pk)
  n_starters  <- dplyr::n_distinct(starter_matchup$game_pk)
  n_lu_games  <- dplyr::n_distinct(lineup_context$game_pk)
  n_bp_games  <- dplyr::n_distinct(bullpen_grid$game_pk)
  avg_lineup  <- nrow(lineup_context) / max(n_lu_games * 2, 1)
  avg_bullpen <- nrow(bullpen_grid)   / max(n_bp_games * 2, 1)

  cat(sprintf("[INFO] Games today: %d\n", n_games))
  cat(sprintf("[INFO] Games with starters:  %d / %d\n", n_starters, n_games))
  cat(sprintf("[INFO] Games with lineups:   %d / %d (avg %.1f batters/side)\n",
              n_lu_games, n_games, avg_lineup))
  cat(sprintf("[INFO] Games with bullpens:  %d / %d (avg %.1f arms/side)\n",
              n_bp_games, n_games, avg_bullpen))

  check("starter coverage >= 50%",  n_starters  >= n_games * 0.5)
  check("lineup coverage >= 50%",   n_lu_games  >= n_games * 0.5)
  check("bullpen coverage >= 50%",  n_bp_games  >= n_games * 0.5)
  check("avg lineup >= 7 batters",  avg_lineup  >= 7)
  check("avg bullpen >= 3 arms",    avg_bullpen >= 3)
}

# ============================================================
# SECTION 7: Team Abbreviation Sanity
# ============================================================
cat("\n--- Team Abbreviation Sanity ---\n")

bad_abbr_patterns <- c("SDP", "SFG", "KCR", "TBR", "ARI", "WSN")

if (exists("offense_master_season")) {
  bad_found <- offense_master_season$team_abbr[
    offense_master_season$team_abbr %in% bad_abbr_patterns
  ]
  check(
    "offense_master_season: no FG-style abbreviations (SDP, SFG, etc.)",
    length(bad_found) == 0,
    if (length(bad_found) > 0) paste("found:", paste(unique(bad_found), collapse = ", "))
  )
}

if (exists("lineup_context")) {
  lu_abbrs <- unique(lineup_context$team_abbr)
  check(
    paste0("lineup_context team abbrs: ", paste(sort(lu_abbrs), collapse = ", ")),
    length(lu_abbrs) > 0
  )
}

# ============================================================
# SUMMARY
# ============================================================
cat("\n========================================================\n")
cat(sprintf("  RESULT: %d passed, %d failed\n", pass, fail))
if (fail == 0) {
  cat("  All checks passed — ready to render report.\n")
} else {
  cat("  Fix failures above before rendering.\n")
}
cat("========================================================\n\n")
