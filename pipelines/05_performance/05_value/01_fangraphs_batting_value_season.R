# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_fangraphs_batting_value_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs WAR and value metrics for batters.
#   Bypasses baseballr::fg_batter_leaders() (known bug).
#
# KEY METRICS:
#   WAR, Dollars, Off, Def
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_fg_batting_value
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

season_complete <- target_season - 1

# ------------------------------------------------------------
# Direct FanGraphs API pull (type 1 = Advanced, has WAR/Dollars)
# ------------------------------------------------------------

pull_fg_batting_value_api <- function(yr) {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      query = list(
        age = "", pos = "all", stats = "bat", lg = "all",
        season = yr, season1 = yr, ind = "0", qual = "0",
        type = "1", pageitems = "2000000", pagenum = "1", rost = "0"
      ),
      httr::timeout(60)
    ),
    error = function(e) {
      message("FanGraphs batting value API failed (yr=", yr, "): ", e$message)
      NULL
    }
  )
  if (is.null(resp) || httr::http_error(resp)) return(NULL)
  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !"data" %in% names(parsed)) return(NULL)
  result <- tryCatch(dplyr::as_tibble(parsed$data), error = function(e) NULL)
  if (is.null(result) || nrow(result) == 0) return(NULL)
  result
}

fg_raw <- pull_fg_batting_value_api(season_complete)

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs data for ", season_complete, ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  fg_raw <- pull_fg_batting_value_api(season_complete)
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("FanGraphs batting value unavailable. Creating empty table.")
  player_season_fg_batting_value <- dplyr::tibble(
    mlbam_id = integer(), season = integer(), team_abbr = character(),
    fg_id = character(), player_type = character()
  )
} else {

  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id"), names(fg_raw))[1]
  team_col  <- intersect(c("team_name_abb", "Team"), names(fg_raw))[1]

  player_season_fg_batting_value <- fg_raw %>%
    dplyr::mutate(
      mlbam_id    = as.integer(if (!is.na(mlbam_col)) .data[[mlbam_col]] else NA_integer_),
      fg_id       = as.character(playerid),
      season      = as.integer(season_complete),
      team_abbr   = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_,
      player_type = "batter"
    ) %>%
    dplyr::select(
      mlbam_id, fg_id, season, team_abbr, player_type,
      dplyr::any_of(c("WAR", "Dollars", "Off", "Def"))
    ) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      dplyr::any_of(c("WAR", "Dollars", "Off", "Def"))
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, season, team_abbr, fg_id, player_type, dplyr::everything())

  validate_performance_table(player_season_fg_batting_value)
}

message("01_fangraphs_batting_value_season complete: ",
        nrow(player_season_fg_batting_value),
        " batter-season rows for season ", season_complete)
