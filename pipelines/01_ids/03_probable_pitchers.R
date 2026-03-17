# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 03_probable_pitchers.R
# ============================================================

library(dplyr)
library(purrr)
library(baseballr)

message("Running 03_probable_pitchers.R")

# ------------------------------------------------------------
# Safety Checks
# ------------------------------------------------------------

if (!exists("game_meta")) {
  stop("game_meta not found. Run 02_game_meta.R first.")
}

if (!exists("player_master_ids")) {
  stop("player_master_ids not found. Run ID layer first.")
}

safe_probables <- purrr::possibly(
  baseballr::mlb_probables,
  otherwise = NULL
)

# ------------------------------------------------------------
# Pull Probables Per Game
# ------------------------------------------------------------

game_pitchers <- map_dfr(
  seq_len(nrow(game_meta)),
  function(i) {
    
    pk <- game_meta$game_pk[i]
    home_id <- game_meta$home_team_id[i]
    away_id <- game_meta$away_team_id[i]
    
    prob <- safe_probables(game_pk = pk)
    
    home_sp <- NA_real_
    away_sp <- NA_real_
    
    if (!is.null(prob) && nrow(prob) > 0) {
      
      home_sp <- prob %>%
        filter(team_id == home_id) %>%
        pull(id) %>%
        first()
      
      away_sp <- prob %>%
        filter(team_id == away_id) %>%
        pull(id) %>%
        first()
    }
    
    tibble(
      game_pk = pk,
      home_sp_mlbam = home_sp,
      away_sp_mlbam = away_sp
    )
  }
)

# ------------------------------------------------------------
# Join To Master IDs
# ------------------------------------------------------------

game_pitchers <- game_pitchers %>%
  left_join(
    player_master_ids %>%
      select(player_master_id, mlbam_id, player_name),
    by = c("home_sp_mlbam" = "mlbam_id")
  ) %>%
  rename(
    home_sp_id = player_master_id,
    home_sp_name = player_name
  ) %>%
  left_join(
    player_master_ids %>%
      select(player_master_id, mlbam_id, player_name),
    by = c("away_sp_mlbam" = "mlbam_id")
  ) %>%
  rename(
    away_sp_id = player_master_id,
    away_sp_name = player_name
  )

# ------------------------------------------------------------
# Integrity Check
# ------------------------------------------------------------

stopifnot(
  nrow(game_pitchers) ==
    n_distinct(game_pitchers$game_pk)
)

message("game_pitchers built: ", nrow(game_pitchers), " game(s)")
message("03_probable_pitchers.R complete")
