# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_baserunning_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs baserunning metrics.
#   Bypasses baseballr::fg_batter_leaders() (known bug).
#
# KEY METRICS:
#   BsR, wSB, Spd
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# OUTPUT:
#   player_season_fg_baserunning
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

season_complete <- target_season - 1

# ------------------------------------------------------------
# Direct FanGraphs API pull (type 1 = Advanced, has baserunning cols)
# ------------------------------------------------------------

pull_fg_baserunning_api <- function(yr) {
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
      message("FanGraphs baserunning API failed (yr=", yr, "): ", e$message)
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

fg_raw <- pull_fg_baserunning_api(season_complete)

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs data for ", season_complete, ". Falling back to ", season_complete - 1)
  season_complete <- season_complete - 1
  fg_raw <- pull_fg_baserunning_api(season_complete)
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("FanGraphs baserunning unavailable. Creating empty table.")
  player_season_fg_baserunning <- dplyr::tibble(
    mlbam_id = integer(), season = integer(), team_abbr = character(),
    fg_id = character()
  )
} else {

  # Log what cols came back (useful for diagnosing missing UBR / Spd)
  br_candidates <- c("BsR", "BaseRunning", "wBsR", "wSB", "UBR", "wGDP", "Spd", "SB", "CS")
  message("FanGraphs type=1 cols found: ",
          paste(intersect(br_candidates, names(fg_raw)), collapse = ", "),
          " | missing: ",
          paste(setdiff(br_candidates, names(fg_raw)), collapse = ", "))

  # If UBR or Spd are missing from type=1, try type=2 (Advanced batting page)
  if (!all(c("UBR", "Spd") %in% names(fg_raw))) {
    fg_raw2 <- pull_fg_baserunning_api(season_complete)  # placeholder — type=2 below
    fg_raw2 <- tryCatch({
      resp2 <- httr::GET(
        "https://www.fangraphs.com/api/leaders/major-league/data",
        query = list(
          age = "", pos = "all", stats = "bat", lg = "all",
          season = season_complete, season1 = season_complete, ind = "0", qual = "0",
          type = "2", pageitems = "2000000", pagenum = "1", rost = "0"
        ),
        httr::timeout(60)
      )
      if (httr::http_error(resp2)) NULL
      else {
        parsed2 <- jsonlite::fromJSON(httr::content(resp2, as = "text", encoding = "UTF-8"), flatten = TRUE)
        if ("data" %in% names(parsed2)) dplyr::as_tibble(parsed2$data) else NULL
      }
    }, error = function(e) NULL)

    if (!is.null(fg_raw2) && nrow(fg_raw2) > 0) {
      message("FanGraphs type=2 cols found: ",
              paste(intersect(br_candidates, names(fg_raw2)), collapse = ", "))
      # Pull any missing baserunning cols from type=2
      id_col2 <- intersect(c("xMLBAMID", "mlbam_id", "playerid"), names(fg_raw2))[1]
      id_col1 <- intersect(c("xMLBAMID", "mlbam_id", "playerid"), names(fg_raw))[1]
      extra_cols <- intersect(setdiff(br_candidates, names(fg_raw)), names(fg_raw2))
      if (length(extra_cols) > 0 && !is.na(id_col2) && !is.na(id_col1)) {
        fg_raw2_slim <- fg_raw2 %>%
          dplyr::select(dplyr::all_of(c(id_col2, extra_cols))) %>%
          dplyr::rename(!!id_col1 := !!id_col2)
        fg_raw <- dplyr::left_join(fg_raw, fg_raw2_slim, by = id_col1)
        message("Patched from type=2: ", paste(extra_cols, collapse = ", "))
      }
    }
  }

  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id"), names(fg_raw))[1]
  team_col  <- intersect(c("team_name_abb", "Team"), names(fg_raw))[1]

  baserunning_cols <- intersect(
    c("BsR", "BaseRunning", "wBsR", "wSB", "UBR", "wGDP", "Spd", "SB", "CS"),
    names(fg_raw)
  )

  player_season_fg_baserunning <- fg_raw %>%
    dplyr::mutate(
      mlbam_id  = as.integer(if (!is.na(mlbam_col)) .data[[mlbam_col]] else NA_integer_),
      fg_id     = as.character(playerid),
      season    = as.integer(season_complete),
      team_abbr = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_
    ) %>%
    dplyr::select(mlbam_id, fg_id, season, team_abbr, dplyr::all_of(baserunning_cols)) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      dplyr::all_of(baserunning_cols)
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, season, team_abbr, fg_id, dplyr::everything())

  validate_performance_table(player_season_fg_baserunning)
}

message("02_fangraphs_baserunning_season complete: ",
        nrow(player_season_fg_baserunning),
        " rows for season ", season_complete)
