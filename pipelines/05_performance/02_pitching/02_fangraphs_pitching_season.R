# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs pitching stats.
#   Uses a single type 3 pull which returns all ~544 columns
#   in one request: standard, advanced, batted ball, pitch type
#   run values, plate discipline, Stuff+/Location+/Pitching+,
#   PitchingBot scores, per-pitch movement, etc.
#   Bypasses baseballr::fg_pitcher_leaders() (known bug).
#
# OUTPUT:
#   player_season_fg_pitching
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
# ============================================================

season_to_pull <- unique(player_season_mlb_pitching$season)[1]

# ------------------------------------------------------------
# Direct FanGraphs API pull
# type 3 returns all ~544 pitching columns in a single request
# ------------------------------------------------------------

pull_fg_pitching_api <- function(yr, type_num = 3) {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      query = list(
        age = "", pos = "all", stats = "pit", lg = "all",
        season = yr, season1 = yr, ind = "0", qual = "0",
        type = as.character(type_num),
        pageitems = "2000000", pagenum = "1", rost = "0"
      ),
      httr::timeout(60)
    ),
    error = function(e) {
      message("FanGraphs pitching API failed (type=", type_num, ", yr=", yr, "): ", e$message)
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
# Pull — fall back to prior season if data is sparse
# ------------------------------------------------------------

fg_raw <- pull_fg_pitching_api(season_to_pull, type_num = 3)

if (is.null(fg_raw) || nrow(fg_raw) < 50) {
  message("FanGraphs pitching insufficient for ", season_to_pull,
          " (", if (is.null(fg_raw)) "NULL" else nrow(fg_raw), " rows). ",
          "Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1L
  fg_raw <- pull_fg_pitching_api(season_to_pull, type_num = 3)
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("FanGraphs pitching completely unavailable. Creating empty table.")
  player_season_fg_pitching <- dplyr::tibble(
    mlbam_id = integer(), season = integer(), team_abbr = character()
  )
} else {

  message("FanGraphs pitching pull (type 3): ", nrow(fg_raw), " rows | ",
          ncol(fg_raw), " columns")

  # Extract identity columns before name normalization
  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id"), names(fg_raw))[1]
  team_col  <- intersect(c("TeamNameAbb", "Team", "team_name"), names(fg_raw))[1]

  fg_work <- fg_raw %>%
    dplyr::mutate(
      tmp_mlbam_id = suppressWarnings(as.integer(
        if (!is.na(mlbam_col)) .data[[mlbam_col]] else NA_integer_
      )),
      tmp_team = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_
    )

  # If xMLBAMID not available, join via player_master_ids
  if (is.na(mlbam_col) || all(is.na(fg_work$tmp_mlbam_id))) {
    fg_id_map <- player_master_ids %>%
      dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
      dplyr::distinct(fg_id, .keep_all = TRUE) %>%
      dplyr::select(fg_id, mlbam_id)

    fg_work <- fg_work %>%
      dplyr::select(-tmp_mlbam_id) %>%
      dplyr::left_join(fg_id_map, by = c("playerid" = "fg_id")) %>%
      dplyr::rename(tmp_mlbam_id = mlbam_id) %>%
      dplyr::mutate(tmp_mlbam_id = as.integer(tmp_mlbam_id))
  }

  # ------------------------------------------------------------
  # Normalize column names
  # Handle special cases first, then generic cleanup
  # ------------------------------------------------------------

  names(fg_work) <- dplyr::case_when(
    names(fg_work) == "-WPA"                           ~ "neg_WPA",
    names(fg_work) == "+WPA"                           ~ "pos_WPA",
    names(fg_work) == "K/9"                            ~ "K_per_9",
    names(fg_work) == "BB/9"                           ~ "BB_per_9",
    names(fg_work) == "H/9"                            ~ "H_per_9",
    names(fg_work) == "HR/9"                           ~ "HR_per_9",
    names(fg_work) == "K/BB"                           ~ "K_per_BB",
    names(fg_work) == "LOB%"                           ~ "LOB_pct",
    names(fg_work) == "HR/FB"                          ~ "HR_per_FB",
    names(fg_work) == "GB/FB"                          ~ "GB_per_FB",
    names(fg_work) == "FB%1"                           ~ "FB_usage_pct",
    names(fg_work) == "K%"                             ~ "K_pct",
    names(fg_work) == "BB%"                            ~ "BB_pct",
    names(fg_work) == "K-BB%"                          ~ "K_BB_pct",
    names(fg_work) == "C+SwStr%"                       ~ "C_plusSwStr_pct",
    names(fg_work) == "ERA-"                           ~ "ERA_minus",
    names(fg_work) == "FIP-"                           ~ "FIP_minus",
    names(fg_work) == "xFIP-"                          ~ "xFIP_minus",
    names(fg_work) == "Start-IP"                       ~ "Start_IP",
    names(fg_work) == "Relief-IP"                      ~ "Relief_IP",
    names(fg_work) == "RA9-Wins"                       ~ "RA9_Wins",
    names(fg_work) == "LOB-Wins"                       ~ "LOB_Wins",
    names(fg_work) == "BIP-Wins"                       ~ "BIP_Wins",
    names(fg_work) == "BS-Wins"                        ~ "BS_Wins",
    names(fg_work) == "RS/9"                           ~ "RS_per_9",
    names(fg_work) == "E-F"                            ~ "E_F",
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
    "Throws", "AgeR", "TG", "TIP", "Q"
  )

  player_season_fg_pitching <- fg_work %>%
    dplyr::filter(!is.na(tmp_mlbam_id)) %>%
    dplyr::select(-dplyr::any_of(drop_raw)) %>%
    dplyr::rename_with(
      ~ paste0("fg_", .x),
      -c(tmp_mlbam_id, tmp_team)
    ) %>%
    dplyr::rename(mlbam_id = tmp_mlbam_id) %>%
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
    dplyr::select(mlbam_id, season, team_abbr, dplyr::everything())

  validate_performance_table(player_season_fg_pitching)

}

message("02_fangraphs_pitching_season complete: ",
        nrow(player_season_fg_pitching),
        " rows for season ", season_to_pull,
        " | columns: ", ncol(player_season_fg_pitching),
        " | fg_WAR non-NA: ",
        if ("fg_WAR" %in% names(player_season_fg_pitching))
          sum(!is.na(player_season_fg_pitching$fg_WAR)) else 0)
