# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 03_probable_pitchers.R
# ============================================================

library(dplyr)
library(baseballr)
library(purrr)

message("Running 03_probable_pitchers.R")

if (!exists("schedule_context")) {
  stop("schedule_context not found.")
}

raw_probables <- map_dfr(
  schedule_context$game_pk,
  function(gpk) {
    
    prob <- tryCatch(
      baseballr::mlb_probables(gpk),
      error = function(e) return(NULL)
    )
    
    if (is.null(prob) || nrow(prob) == 0) {
      return(tibble(
        game_pk = gpk,
        team_id = NA_integer_,
        pitcher_name = NA_character_,
        pitcher_mlbam_id = NA_integer_
      ))
    }
    
    prob %>%
      transmute(
        game_pk = game_pk,
        team_id = team_id,
        pitcher_name = fullName,
        pitcher_mlbam_id = id
      )
  }
)

# ------------------------------------------------------------
# Pivot to One Row Per Game
# ------------------------------------------------------------

game_probables <- raw_probables %>%
  left_join(
    schedule_context %>%
      select(game_pk, home_team_id, away_team_id),
    by = "game_pk"
  ) %>%
  mutate(
    role = case_when(
      team_id == home_team_id ~ "home",
      team_id == away_team_id ~ "away",
      TRUE ~ NA_character_
    )
  ) %>%
  select(game_pk, role, pitcher_name, pitcher_mlbam_id) %>%
  tidyr::pivot_wider(
    names_from = role,
    values_from = c(pitcher_name, pitcher_mlbam_id),
    names_glue = "{role}_{.value}"
  )

message("game_probables built: ", nrow(game_probables), " rows")
message("03_probable_pitchers.R complete")
