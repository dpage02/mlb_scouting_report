# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_pitching_value_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs WAR and value metrics for pitchers.
#   Uses fg_pitcher_leaders() with minimal parameters — the function
#   has known issues with pageitems/type parameters in some baseballr
#   versions. Falls back gracefully so bWAR (BBRef) still populates.
#
# KEY METRICS:
#   WAR     — FanGraphs WAR for pitchers (fWAR)
#   Dollars — Dollar value estimate ($/WAR market rate)
#   RAR     — Runs above replacement
#
# DATA SOURCE:
#   FanGraphs leaderboard API (direct httr call)
#   baseballr::fg_pitcher_leaders() has a known bug — direct call used instead.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_fg_pitching_value
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season
# ------------------------------------------------------------

season_complete <- target_season - 1

# fg_pitcher_leaders() has a known bug in some baseballr versions.
# Call the FanGraphs leaderboard API directly (same endpoint as fg_batter_leaders,
# with stats=pit instead of stats=bat).

fg_pitcher_leaders_direct <- function(season, qual = "0", type = "1") {
  url <- httr::modify_url(
    "https://www.fangraphs.com",
    path  = "/api/leaders/major-league/data",
    query = list(
      pos       = "all",
      stats     = "pit",
      lg        = "all",
      qual      = qual,
      type      = type,
      season    = season,
      season1   = season,
      ind       = "0",
      team      = "0",
      rost      = "0",
      age       = "0",
      filter    = "",
      players   = "0",
      pageitems = "2000000",
      pagenum   = "1"
    )
  )
  resp <- httr::GET(url, httr::timeout(60))
  if (httr::http_error(resp)) return(NULL)
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                               flatten = TRUE)
  if (!"data" %in% names(parsed)) return(NULL)
  dplyr::as_tibble(parsed$data)
}

fg_raw <- tryCatch(
  fg_pitcher_leaders_direct(season_complete),
  error = function(e) {
    message("FG pitcher direct pull failed for ", season_complete, ": ", e$message)
    NULL
  }
)

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs pitcher data for ", season_complete,
          ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  fg_raw <- tryCatch(
    fg_pitcher_leaders_direct(season_complete),
    error = function(e) {
      message("FG pitcher fallback also failed: ", e$message,
              "\nfWAR for pitchers will be NA in master.")
      NULL
    }
  )
}

# ------------------------------------------------------------
# Diagnostic — prints column names on a successful pull
# ------------------------------------------------------------
if (!is.null(fg_raw) && nrow(fg_raw) > 0) {
  message("FanGraphs pitcher columns: ", paste(names(fg_raw), collapse = ", "))
}

# ------------------------------------------------------------
# Select value columns only (any_of handles missing columns safely)
# ------------------------------------------------------------

value_cols <- c(
  "playerid", "xMLBAMID", "TeamNameAbb", "Season",
  "WAR", "Dollars", "RAR"
)

if (!is.null(fg_raw) && nrow(fg_raw) > 0) {

  player_season_fg_pitching_value <- fg_raw %>%
    dplyr::select(dplyr::any_of(value_cols)) %>%
    dplyr::mutate(
      mlbam_id    = as.integer(xMLBAMID),
      fg_id       = as.character(playerid),
      season      = as.integer(season_complete),
      team_abbr   = TeamNameAbb,
      player_type = "pitcher"
    ) %>%
    dplyr::select(-playerid, -xMLBAMID, -TeamNameAbb,
                  -dplyr::any_of("Season")) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      -c(mlbam_id, fg_id, season, team_abbr, player_type)
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, season, team_abbr, fg_id, player_type,
                  dplyr::everything())

} else {

  message("Creating empty fg pitching value table — fWAR will be NA in master.")
  player_season_fg_pitching_value <- dplyr::tibble(
    mlbam_id    = integer(),
    season      = integer(),
    team_abbr   = character(),
    fg_id       = character(),
    player_type = character()
  )

}

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

validate_performance_table(player_season_fg_pitching_value)

message("02_fangraphs_pitching_value_season complete: ",
        nrow(player_season_fg_pitching_value),
        " pitcher-season-team rows for season ", season_complete)
