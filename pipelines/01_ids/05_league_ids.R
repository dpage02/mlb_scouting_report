# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 05_league_ids.R
# ============================================================
# PURPOSE:
#   Build the canonical league_ids table
#   - One row per professional league used by the pipeline
#   - Stable reference table for joins and labeling
#
# DATASET CREATED:
#   - league_ids
#
# ASSUMPTIONS (validated interactively before writing this script):
#   Source: baseballr::mlb_teams()
#   Confirmed columns used:
#     league_id
#     league_name
#     sport_id
#     sport_name
#     season
#
# INPUTS:
#   - MLB team metadata (used only to enumerate leagues)
#
# OUTPUTS:
#   - league_ids (data frame; written later by 99_write_id_tables.R)
#   - league_ids
# ============================================================

# ---- Pull raw team data (used only for league enumeration) ----
raw_teams <- baseballr::mlb_teams()

# ---- Build canonical league_ids ----
league_ids <- raw_teams %>%
  dplyr::transmute(
    mlbam_league_id = league_id,
    league_name     = league_name,
    sport_id        = sport_id,
    sport_name      = sport_name,
    season          = season
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(mlbam_league_id, season) %>%
  dplyr::group_by(mlbam_league_id) %>%
  dplyr::summarise(
    league_name = dplyr::first(league_name),
    sport_id    = dplyr::first(sport_id),
    sport_name  = dplyr::first(sport_name),
    first_season = min(season, na.rm = TRUE),
    last_season  = max(season, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(mlbam_league_id) %>%
  dplyr::mutate(
    league_id = dplyr::row_number()
  ) %>%
  dplyr::relocate(league_id, .before = mlbam_league_id)

# ---- Basic integrity checks ----
# 1) Internal primary key uniqueness
stopifnot(!anyDuplicated(league_ids$league_id))

# 2) External MLBAM league ID uniqueness
stopifnot(!anyDuplicated(league_ids$mlbam_league_id))

# ---- Logging ----
log_message(glue::glue("league_ids built: {nrow(league_ids)} rows"))

# ============================================================
# END 05_league_ids.R
# ============================================================
