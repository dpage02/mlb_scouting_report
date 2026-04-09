# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_offense_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs batting stats.
#   Uses a single type 3 pull which returns all ~475 columns
#   in one request: standard, advanced, batted ball, pitch type
#   run values, plate discipline, swing mechanics, Stuff+ faced, etc.
#   Bypasses baseballr::fg_batter_leaders() (known bug).
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
# Direct FanGraphs API pull
# type 3 returns all ~475 columns in a single request
# ------------------------------------------------------------

pull_fg_batting_api <- function(yr, type_num = 3) {
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
# Pull — fall back to prior season if wRC+ coverage is sparse
# (Early season: FG returns rows but wRC+ is NA for most players)
# ------------------------------------------------------------

fg_raw <- pull_fg_batting_api(season_to_pull, type_num = 3)

.wrc_col_raw <- if (!is.null(fg_raw))
  intersect(c("wRC+", "wRC.", "wRC_plus"), names(fg_raw))[1] else NA_character_
.wrc_coverage <- if (!is.na(.wrc_col_raw))
  sum(!is.na(fg_raw[[.wrc_col_raw]])) else 0L

if (is.null(fg_raw) || nrow(fg_raw) < 100 || .wrc_coverage < 150) {
  message("FanGraphs batting insufficient for ", season_to_pull,
          " (", if (is.null(fg_raw)) "NULL" else nrow(fg_raw), " rows | ",
          .wrc_coverage, " with wRC+). Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1
  fg_raw <- pull_fg_batting_api(season_to_pull, type_num = 3)
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("FanGraphs batting completely unavailable. Creating empty table.")
  player_season_fg_offense <- dplyr::tibble(
    mlbam_id = integer(), season = integer(), team_abbr = character()
  )
} else {

  message("FanGraphs batting pull (type 3): ", nrow(fg_raw), " rows | ",
          ncol(fg_raw), " columns")

  # Extract identity columns before name normalization
  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id"), names(fg_raw))[1]
  team_col  <- intersect(c("TeamNameAbb", "Team", "team_name"), names(fg_raw))[1]

  fg_work <- fg_raw %>%
    dplyr::mutate(
      tmp_mlbam_id = suppressWarnings(as.integer(
        if (!is.na(mlbam_col)) .data[[mlbam_col]] else NA_integer_
      )),
      tmp_team  = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_,
      tmp_fg_id = as.character(playerid)
    )

  # If xMLBAMID not available, join via player_master_ids
  if (is.na(mlbam_col) || all(is.na(fg_work$tmp_mlbam_id))) {
    fg_work <- fg_work %>%
      dplyr::select(-tmp_mlbam_id) %>%
      dplyr::left_join(
        player_master_ids %>%
          dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
          dplyr::distinct(fg_id, .keep_all = TRUE) %>%
          dplyr::select(fg_id, mlbam_id),
        by = c("playerid" = "fg_id")
      ) %>%
      dplyr::rename(tmp_mlbam_id = mlbam_id) %>%
      dplyr::mutate(tmp_mlbam_id = as.integer(tmp_mlbam_id))
  }

  # ------------------------------------------------------------
  # Normalize column names
  # Handle special cases first, then generic cleanup
  # ------------------------------------------------------------

  names(fg_work) <- dplyr::case_when(
    names(fg_work) == "-WPA"                         ~ "neg_WPA",
    names(fg_work) == "+WPA"                         ~ "pos_WPA",
    names(fg_work) == "1B"                           ~ "X1B",
    names(fg_work) == "K/9"                          ~ "K_per_9",
    names(fg_work) == "BB/9"                         ~ "BB_per_9",
    names(fg_work) == "H/9"                          ~ "H_per_9",
    names(fg_work) == "HR/9"                         ~ "HR_per_9",
    names(fg_work) == "K/BB"                         ~ "K_per_BB",
    names(fg_work) == "LOB%"                         ~ "LOB_pct",
    names(fg_work) == "HR/FB"                        ~ "HR_per_FB",
    names(fg_work) == "GB/FB"                        ~ "GB_per_FB",
    names(fg_work) == "FB%1"                         ~ "FB_usage_pct",
    names(fg_work) %in% c("wRC+", "wRC.", "wRC_plus") ~ "wRC_plus",
    names(fg_work) == "BB%"                          ~ "BB_pct",
    names(fg_work) == "K%"                           ~ "K_pct",
    names(fg_work) == "K-BB%"                        ~ "K_BB_pct",
    names(fg_work) == "C+SwStr%"                     ~ "C_plusSwStr_pct",
    names(fg_work) == "ERA-"                         ~ "ERA_minus",
    names(fg_work) == "FIP-"                         ~ "FIP_minus",
    names(fg_work) == "xFIP-"                        ~ "xFIP_minus",
    TRUE ~ names(fg_work)
  )

  # Generic cleanup: %, +, remaining special chars → underscores
  names(fg_work) <- gsub("%", "_pct",   gsub("\\+", "_plus", names(fg_work)))
  names(fg_work) <- gsub("[^A-Za-z0-9_]", "_", names(fg_work))
  names(fg_work) <- gsub("_+", "_", gsub("^_|_$", "", names(fg_work)))

  # Identity/metadata columns to drop before prefixing
  drop_raw <- c(
    "playerid", "xMLBAMID", "Name", "PlayerName", "PlayerNameRoute",
    "Team", "TeamName", "TeamNameAbb", "teamid", "playerTeamId",
    "Season", "SeasonMin", "SeasonMax", "Pos", "positionDB", "position",
    "Bats", "AgeR", "TG", "TPA", "Q"
  )

  player_season_fg_offense <- fg_work %>%
    dplyr::filter(!is.na(tmp_mlbam_id)) %>%
    dplyr::select(-dplyr::any_of(drop_raw)) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      -c(tmp_mlbam_id, tmp_team, tmp_fg_id)
    ) %>%
    dplyr::rename(mlbam_id = tmp_mlbam_id, fg_id = tmp_fg_id) %>%
    dplyr::left_join(
      team_ids %>% dplyr::select(team_name, team_abbr),
      by = c("tmp_team" = "team_name")
    ) %>%
    dplyr::mutate(
      team_abbr = dplyr::coalesce(team_abbr, tmp_team),
      season    = as.integer(season_to_pull)
    ) %>%
    dplyr::select(-tmp_team) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, season, team_abbr, fg_id, dplyr::everything())

  validate_performance_table(player_season_fg_offense)

}

message("02_fangraphs_offense_season complete: ",
        nrow(player_season_fg_offense),
        " rows for season ", season_to_pull,
        " | columns: ", ncol(player_season_fg_offense),
        " | fg_wRC_plus non-NA: ",
        if ("fg_wRC_plus" %in% names(player_season_fg_offense))
          sum(!is.na(player_season_fg_offense$fg_wRC_plus)) else 0)
