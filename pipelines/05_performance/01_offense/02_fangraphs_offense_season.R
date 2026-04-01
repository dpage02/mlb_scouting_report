# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_offense_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs batting stats (type 8 dashboard).
#   Bypasses baseballr::fg_batter_leaders() which has a known bug
#   ("object 'leaders' not found"). Uses httr directly instead.
#
# OUTPUT:
#   player_season_fg_offense
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

season_to_pull <- unique(player_season_mlb_offense$season)[1]

# ------------------------------------------------------------
# Direct FanGraphs API pull — bypasses fg_batter_leaders()
# ------------------------------------------------------------

pull_fg_batting_api <- function(yr, type_num = 8) {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      query = list(
        age = "", pos = "all", stats = "bat", lg = "all",
        season = yr, season1 = yr, ind = "0", qual = "0",
        type = as.character(type_num),
        pageitems = "2000000", pagenum = "1", rost = "0"
      ),
      httr::timeout(60)
    ),
    error = function(e) {
      message("FanGraphs batting API failed (type=", type_num, ", yr=", yr, "): ", e$message)
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

# ------------------------------------------------------------
# Pull type 8 dashboard — fall back to prior season if sparse
# ------------------------------------------------------------

fg_raw <- pull_fg_batting_api(season_to_pull, type_num = 8)

if (is.null(fg_raw) || nrow(fg_raw) < 100) {
  message("FanGraphs batting data insufficient for ", season_to_pull,
          " (", if (is.null(fg_raw)) "NULL" else nrow(fg_raw), " rows). ",
          "Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1
  fg_raw <- pull_fg_batting_api(season_to_pull, type_num = 8)
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("FanGraphs batting completely unavailable. Creating empty table.")
  player_season_fg_offense <- dplyr::tibble(
    mlbam_id = integer(), season = integer(), team_abbr = character()
  )
} else {

  message("FanGraphs batting pull (type 8): ", nrow(fg_raw), " rows")

  # Extract IDs and team name BEFORE prefixing
  # mlbam_id: try xMLBAMID directly, fall back to player_master_ids join
  mlbam_col  <- intersect(c("xMLBAMID", "mlbam_id"), names(fg_raw))[1]
  team_col   <- intersect(c("team_name", "Team"), names(fg_raw))[1]

  fg_work <- fg_raw %>%
    dplyr::mutate(
      mlbam_id       = as.integer(if (!is.na(mlbam_col)) .data[[mlbam_col]] else NA_integer_),
      fg_id          = as.character(playerid),
      .team_name_raw = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_
    )

  # If xMLBAMID not available, join via player_master_ids
  if (is.na(mlbam_col) || all(is.na(fg_work$mlbam_id))) {
    fg_work <- fg_work %>%
      dplyr::select(-mlbam_id) %>%
      dplyr::left_join(
        player_master_ids %>%
          dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
          dplyr::distinct(fg_id, .keep_all = TRUE) %>%
          dplyr::select(fg_id, mlbam_id),
        by = c("playerid" = "fg_id")
      ) %>%
      dplyr::mutate(mlbam_id = as.integer(mlbam_id))
  }

  # Drop raw identity columns, then prefix stat columns with fg_
  drop_raw <- c("playerid", "xMLBAMID", "team_name_abb", "team_name", "Team",
                "Season", "Name", "PlayerName", mlbam_col, team_col)

  # Normalize wRC+ before prefix
  names(fg_work) <- dplyr::case_when(
    names(fg_work) %in% c("wRC+", "wRC.", "wRC_plus") ~ "wRC_plus",
    names(fg_work) == "BB%"   ~ "BB_pct",
    names(fg_work) == "K%"    ~ "K_pct",
    names(fg_work) == "K-BB%" ~ "K_BB_pct",
    TRUE ~ names(fg_work)
  )

  player_season_fg_offense <- fg_work %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::select(-dplyr::any_of(drop_raw)) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      -c(mlbam_id, fg_id, .team_name_raw)
    ) %>%
    dplyr::left_join(
      team_ids %>% dplyr::select(team_name, team_abbr),
      by = c(".team_name_raw" = "team_name")
    ) %>%
    dplyr::mutate(
      team_abbr = dplyr::coalesce(team_abbr, .team_name_raw),
      season    = as.integer(season_to_pull)
    ) %>%
    dplyr::select(-.team_name_raw) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, season, team_abbr, fg_id, dplyr::everything())

  # ------------------------------------------------------------
  # Additional FanGraphs type pulls
  # ------------------------------------------------------------

  pull_fg_batter_extra <- function(type_num) {
    raw <- pull_fg_batting_api(season_to_pull, type_num = type_num)
    if (is.null(raw) || nrow(raw) == 0) {
      message("FG batter type ", type_num, ": no data")
      return(NULL)
    }
    names(raw) <- gsub("\\+", "_plus", gsub("%", "_pct", gsub("-", "_", names(raw))))

    mlbam_col_e <- intersect(c("xMLBAMID", "mlbam_id"), names(raw))[1]
    if (is.na(mlbam_col_e)) return(NULL)

    drop_cols <- c(
      "playerid", "xMLBAMID", "team_name_abb", "team_name", "Team",
      "Season", "Name", "PlayerName",
      "G", "PA", "AB", "AVG", "OBP", "SLG", "OPS",
      "HR", "R", "RBI", "SB", "BB_pct", "K_pct",
      "ISO", "BABIP", "wOBA", "wRC_plus", "wRC.", "WAR",
      "BsR", "Off", "Def", "xwOBA", "Age", "AgeRng"
    )

    result <- raw %>%
      dplyr::mutate(mlbam_id = suppressWarnings(as.integer(.data[[mlbam_col_e]]))) %>%
      dplyr::filter(!is.na(mlbam_id)) %>%
      dplyr::select(-dplyr::any_of(drop_cols)) %>%
      dplyr::rename_with(~ paste0("fg_", .x), -mlbam_id) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    message("FG batter type ", type_num, ": ", ncol(result) - 1,
            " new columns for ", nrow(result), " players")
    result
  }

  for (extra in list(
    pull_fg_batter_extra(1),   # Advanced: Spd, UBR, wSB, wRAA
    pull_fg_batter_extra(2),   # Batted Ball: Hard%, GB%, FB%, IFFB%
    pull_fg_batter_extra(5)    # Plate Discipline: O-Swing%, Z-Contact%, CSW%
  )) {
    if (!is.null(extra)) {
      player_season_fg_offense <- player_season_fg_offense %>%
        dplyr::left_join(extra, by = "mlbam_id", suffix = c("", "_dup")) %>%
        dplyr::select(-dplyr::ends_with("_dup"))
    }
  }

  validate_performance_table(player_season_fg_offense)

} # end if fg_raw available

message("02_fangraphs_offense_season complete: ",
        nrow(player_season_fg_offense),
        " rows for season ", season_to_pull,
        " | fg_wRC_plus non-NA: ",
        if ("fg_wRC_plus" %in% names(player_season_fg_offense))
          sum(!is.na(player_season_fg_offense$fg_wRC_plus)) else 0)
