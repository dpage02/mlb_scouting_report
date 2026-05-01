# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 04_umpire_assignments.R
# ============================================================
# PURPOSE:
#   Attach umpire assignments to each game in schedule_context.
#
# OUTPUT:
#   - game_umpires (one row per game_pk)
#
# DESIGN:
#   - Safe in offseason
#   - Safe if assignments not published yet
#   - Does NOT error if empty
# ============================================================

library(dplyr)
library(baseballr)
library(purrr)

message("Running 04_umpire_assignments.R")

if (!exists("schedule_context")) {
  stop("schedule_context not found. Run 01_schedule.R first.")
}

# ------------------------------------------------------------
# Pull umpire assignments per game
# ------------------------------------------------------------

game_umpires <- map_dfr(
  schedule_context$game_pk,
  function(gpk) {
    
    ump_data <- tryCatch(
      baseballr::mlb_probables(gpk),
      error = function(e) return(NULL)
    )
    
    if (is.null(ump_data) || !is.data.frame(ump_data) || nrow(ump_data) == 0) {
      return(tibble(
        game_pk = gpk,
        home_plate_umpire = NA_character_,
        home_plate_umpire_id = NA_integer_
      ))
    }
    
    tibble(
      game_pk = gpk,
      home_plate_umpire = unique(ump_data$home_plate_full_name),
      home_plate_umpire_id = unique(ump_data$home_plate_id)
    )
  }
)

# ------------------------------------------------------------
# Integrity: one row per game
# ------------------------------------------------------------

game_umpires <- game_umpires %>%
  distinct(game_pk, .keep_all = TRUE)

message("game_umpires built: ", nrow(game_umpires), " rows")
message("04_umpire_assignments.R complete")
