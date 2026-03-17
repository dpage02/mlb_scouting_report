# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 03_park_ids.R
# ============================================================
# PURPOSE:
#   Build the canonical park_ids table
#   - One row per physical MLB ballpark
#   - Stable reference table for park context and joins
#
# DATASET CREATED:
#   - park_ids
#
# ASSUMPTIONS (validated interactively before writing this script):
#   Source: baseballr::mlb_teams()
#   Confirmed columns used:
#     venue_id
#     venue_name
#     location_name
#     active
#     season
#
# INPUTS:
#   - MLB team metadata (used only to enumerate parks)
#
# OUTPUTS:
#   - park_ids (data frame; written later by 99_write_id_tables.R)
# ============================================================

# ---- Pull raw team data (used only for park enumeration) ----
raw_teams <- baseballr::mlb_teams()

# ---- Build canonical park_ids ----
# Using the current season 
current_season <- DEFAULT_SEASON

park_ids <- raw_teams %>%
  dplyr::transmute(
    mlbam_park_id = venue_id,
    park_name     = venue_name,
    city          = location_name,
    season        = season,
    team_active   = active
  ) %>%
  dplyr::filter(!is.na(mlbam_park_id)) %>%
  dplyr::distinct() %>%
  dplyr::group_by(mlbam_park_id) %>%
  dplyr::summarise(
    park_name    = dplyr::first(park_name),
    city         = dplyr::first(city),
    first_season = min(season, na.rm = TRUE),
    last_season  = max(season, na.rm = TRUE),
    is_active    = any(season == current_season & team_active),
    .groups = "drop"
  ) %>%
  dplyr::arrange(mlbam_park_id) %>%
  dplyr::mutate(
    park_id = dplyr::row_number()
  ) %>%
  dplyr::relocate(park_id, .before = mlbam_park_id)


# ---- Basic integrity checks ----
# 1) Internal primary key uniqueness
stopifnot(!anyDuplicated(park_ids$park_id))

# 2) External MLBAM park ID uniqueness
stopifnot(!anyDuplicated(park_ids$mlbam_park_id))

# 3) Active parks must have names
stopifnot(all(!is.na(park_ids$park_name[park_ids$is_active])))

# ---- Logging ----
log_message(glue::glue("park_ids built: {nrow(park_ids)} rows"))

# ============================================================
# END 03_park_ids.R
# ============================================================
