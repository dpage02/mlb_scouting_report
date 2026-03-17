# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 06_player_season_scope.R
# ============================================================
# PURPOSE:
#   Build the league-wide seasonal player universe.
#
# WHAT THIS SCRIPT DOES:
#   - Pulls all MLB 40-man rosters for the specified season
#   - Pulls full-season Statcast participation
#   - Constructs the season-level player universe
#   - Joins to player_master_ids for identity resolution
#   - Flags:
#       • on_40man
#       • appeared_in_games
#       • active_this_season
#   - Produces one row per mlbam_id
#
# WHAT THIS SCRIPT DOES NOT DO:
#   - Filter to a specific team
#   - Filter to a specific game
#   - Determine daily lineups
#   - Perform modeling or reporting
#
# INPUT TABLES REQUIRED:
#   - team_ids
#   - player_master_ids
#
# OUTPUT TABLE:
#   - player_season_scope
#
# DESIGN NOTES:
#   - League-wide by design (filtering occurs later)
#   - Season-scoped
#   - Identity-safe join on mlbam_id only
#   - Deterministic and reproducible
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(baseballr)

message("Running 06_player_season_scope.R")

season <- DEFAULT_SEASON

# ------------------------------------------------------------
# 1. LEAGUE-WIDE TEAM IDS (MUST BE 30)
# ------------------------------------------------------------

team_mlbam_ids <- unique(team_ids$mlbam_team_id)

if (length(team_mlbam_ids) != 30) {
  stop("team_ids is not properly built — expected 30 MLB teams.")
}

message("Teams in scope: ", length(team_mlbam_ids))

# ------------------------------------------------------------
# 2. 40-MAN ROSTERS
# ------------------------------------------------------------

message("Pulling 40-man rosters")

rosters_40man_raw <- purrr::map_dfr(
  team_mlbam_ids,
  ~ suppressMessages(
    baseballr::mlb_rosters(
      team_id     = .x,
      season      = season,
      roster_type = "40Man"
    )
  )
)

rosters_40man <- rosters_40man_raw %>%
  transmute(
    mlbam_id = person_id,
    on_40man = TRUE
  ) %>%
  distinct()

message("40-man count: ", nrow(rosters_40man))

# ------------------------------------------------------------
# 3. STATCAST PARTICIPATION (ONLY IF SEASON HAS DATA)
# ------------------------------------------------------------

message("Pulling Statcast participation")

season_games <- suppressMessages(
  baseballr::statcast_search(
    start_date = paste0(season, "-03-01"),
    end_date   = paste0(season, "-11-30")
  )
)

if (nrow(season_games) == 0) {
  message("No Statcast games found for season ", season)
  message("Building season scope using 40-man only.")
  
  game_players <- tibble(
    mlbam_id = numeric(),
    appeared_in_games = logical()
  )
} else {
  game_players <- season_games %>%
    select(batter, pitcher) %>%
    pivot_longer(cols = everything(), values_to = "mlbam_id") %>%
    distinct(mlbam_id) %>%
    mutate(appeared_in_games = TRUE)
}

message("Game participants: ", nrow(game_players))

# ------------------------------------------------------------
# 4. SEASON UNIVERSE
# ------------------------------------------------------------

season_universe <- bind_rows(
  rosters_40man %>% select(mlbam_id),
  game_players  %>% select(mlbam_id)
) %>%
  distinct()

message("Season universe size: ", nrow(season_universe))

# ------------------------------------------------------------
# 5. JOIN TO MASTER
# ------------------------------------------------------------

player_season_scope <- season_universe %>%
  left_join(player_master_ids, by = "mlbam_id") %>%
  left_join(rosters_40man,     by = "mlbam_id") %>%
  left_join(game_players,      by = "mlbam_id") %>%
  mutate(
    on_40man           = if_else(is.na(on_40man), FALSE, on_40man),
    appeared_in_games  = if_else(is.na(appeared_in_games), FALSE, appeared_in_games),
    active_this_season = on_40man | appeared_in_games
  ) %>%
  select(
    player_master_id,
    mlbam_id,
    player_name,
    on_40man,
    appeared_in_games,
    active_this_season
  )

# ------------------------------------------------------------
# 6. DIAGNOSTICS
# ------------------------------------------------------------

missing_master <- sum(is.na(player_season_scope$player_master_id))
message("Missing master IDs: ", missing_master)

stopifnot(
  nrow(player_season_scope) ==
    n_distinct(player_season_scope$mlbam_id)
)

message(
  "player_season_scope built: ",
  nrow(player_season_scope),
  " players for season ",
  season
)

message("06_player_season_scope.R complete")
