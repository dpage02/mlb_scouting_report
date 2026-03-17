# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 06_series_context.R
# ============================================================
# PURPOSE:
#   Derive series-level flags for each game.
#
# OUTPUT:
#   - series_context
#
# DESIGN:
#   - Purely derived from schedule_context
#   - No external API calls
#   - One row per game_pk
# ============================================================

library(dplyr)

message("Running 06_series_context.R")

if (!exists("schedule_context")) {
  stop("schedule_context not found.")
}

series_context <- schedule_context %>%
  transmute(
    game_pk,
    series_length = games_in_series,
    game_in_series = series_game_num,
    is_series_opener = series_game_num == 1,
    is_series_finale = series_game_num == games_in_series,
    is_rubber_match = games_in_series == 3 & series_game_num == 3
  )

message("series_context built: ", nrow(series_context))
message("06_series_context.R complete")
