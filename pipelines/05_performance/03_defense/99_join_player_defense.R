# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 99_join_player_defense.R
# ============================================================
# PURPOSE:
#   Join all season-level defense sources into one master table.
#   Mirrors the pattern used by 99_offense_master_join.R and
#   99_pitching_master_join.R.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# JOIN LOGIC:
#   MLB is the spine (team-level, authoritative for position)
#   FanGraphs  → join on mlbam_id + season + team_abbr (team-level)
#   Statcast   → join on mlbam_id + season only (season total, TOT)
#   Lahman     → join on mlbam_id + season only (season total, TOT)
#
# OUTPUT:
#   defense_master_season
#   defense_master_position_players
#   defense_master_pitchers
#   defense_master_catchers
# ============================================================

# ------------------------------------------------------------
# Required Objects
# ------------------------------------------------------------

required_objects <- c(
  "player_season_mlb_defense",
  "player_season_fg_defense",
  "player_season_statcast_defense",
  "player_season_lahman_defense"
)

missing_objects <- required_objects[!required_objects %in% ls()]

if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Join (MLB Spine)
# ------------------------------------------------------------

defense_master_season <- player_season_mlb_defense %>%

  # FanGraphs — team-level, same grain as MLB spine
  # Safety dedup: ensure one row per mlbam_id + season + team_abbr
  dplyr::left_join(
    player_season_fg_defense %>%
      dplyr::select(-dplyr::any_of("primary_position")) %>%
      dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE),
    by = c("mlbam_id", "season", "team_abbr")
  ) %>%

  # Statcast — season total, join on player + season only
  # Safety dedup: ensure one row per mlbam_id + season
  dplyr::left_join(
    player_season_statcast_defense %>%
      dplyr::select(-team_abbr, -dplyr::any_of("primary_position")) %>%
      dplyr::distinct(mlbam_id, season, .keep_all = TRUE),
    by = c("mlbam_id", "season")
  ) %>%

  # Lahman — season total, join on player + season only
  # Safety dedup: ensure one row per mlbam_id + season
  dplyr::left_join(
    player_season_lahman_defense %>%
      dplyr::select(-team_abbr) %>%
      dplyr::distinct(mlbam_id, season, .keep_all = TRUE),
    by = c("mlbam_id", "season")
  ) %>%

  # Classify position group
  dplyr::mutate(
    position_group = dplyr::case_when(
      primary_position == "P" ~ "pitcher",
      primary_position == "C" ~ "catcher",
      TRUE                    ~ "position_player"
    )
  ) %>%

  # Attach player names from master ID spine
  # Only pull name/bbref_id — fg_id and lahman_id already exist from source joins
  dplyr::left_join(
    player_master_ids %>%
      dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
      dplyr::select(mlbam_id, player_name, name_first, name_last, bbref_id),
    by = "mlbam_id"
  ) %>%

  # For players not yet in Lahman/Chadwick, fall back to the MLB API name
  dplyr::mutate(
    player_name = dplyr::coalesce(player_name, player_name_mlb)
  ) %>%
  dplyr::select(-player_name_mlb) %>%

  dplyr::select(
    mlbam_id, player_name, name_first, name_last,
    bbref_id,
    season, team_abbr,
    primary_position, position_group,
    dplyr::everything()
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(defense_master_season)

# ------------------------------------------------------------
# Split by position group
# ------------------------------------------------------------

defense_master_position_players <- defense_master_season %>%
  dplyr::filter(position_group == "position_player")

defense_master_pitchers <- defense_master_season %>%
  dplyr::filter(position_group == "pitcher")

defense_master_catchers <- defense_master_season %>%
  dplyr::filter(position_group == "catcher")

message("99_join_player_defense complete: ",
        nrow(defense_master_season), " total rows | ",
        nrow(defense_master_position_players), " position players | ",
        nrow(defense_master_pitchers), " pitchers | ",
        nrow(defense_master_catchers), " catchers")
