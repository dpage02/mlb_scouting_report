# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 06_player_context
# SCRIPT: 01_depth_charts.R
# ============================================================
# PURPOSE:
#   Pull FanGraphs depth charts for all 30 MLB teams.
#   Provides authoritative pitcher role assignments (SP1-SP5,
#   CL, SU8, SU7, MID, LR) and position player lineup slots.
#
# METHOD:
#   Uses FanGraphs depth chart API directly (httr).
#   First call pulls TeamList to get all 30 FG team IDs.
#   Then loops all teams, stacks results.
#
# KEY COLUMNS:
#   mlbam_id     — joins to all performance tables
#   fg_role      — SP1/SP2/SP3/SP4/SP5/CL/SU8/SU7/MID/LR/etc.
#   fg_position  — SP/RP/C/1B/2B/SS/3B/OF/DH
#   roster_type  — mlb-sp/mlb-bp/mlb-sl/il-sp/il-rp/off-sp/off-rp
#   fg_team_abbr — FanGraphs team abbreviation
#
# GRAIN:
#   One row per player per team (players on IL included)
#
# OUTPUT:
#   depth_charts
# ============================================================

# ------------------------------------------------------------
# Get all FanGraphs team IDs from initial call
# ------------------------------------------------------------

.fg_headers <- httr::add_headers(
  `User-Agent`      = paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                             "AppleWebKit/537.36 (KHTML, like Gecko) ",
                             "Chrome/124.0.0.0 Safari/537.36"),
  `Accept`          = "application/json, text/plain, */*",
  `Accept-Language` = "en-US,en;q=0.9",
  `Referer`         = "https://www.fangraphs.com/depthcharts.aspx"
)

fg_depth_raw <- tryCatch(
  httr::GET(
    "https://www.fangraphs.com/api/depth-charts/data",
    query   = list(teamid = "16", position = "ALL"),
    .fg_headers,
    httr::timeout(60)
  ),
  error = function(e) {
    message("FanGraphs depth chart API failed: ", e$message)
    NULL
  }
)

# Helper: fall back to depth_charts from the most recent pipeline cache
.depth_charts_from_cache <- function() {
  for (path in c("data/pipeline_cache.rds", "data/base_cache.rds")) {
    if (!file.exists(path)) next
    cached <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(cached) || !"depth_charts" %in% names(cached)) next
    dc <- cached$depth_charts
    if (is.null(dc) || nrow(dc) == 0) next
    message("  Using depth_charts from ", path, " (",
            nrow(dc), " players, ",
            dplyr::n_distinct(dc$fg_team_abbr), " teams)")
    return(dc)
  }
  NULL
}

if (is.null(fg_depth_raw) || httr::http_error(fg_depth_raw)) {
  message("WARNING: Cannot reach FanGraphs depth chart API. Trying base cache fallback...")
  fallback <- .depth_charts_from_cache()
  if (!is.null(fallback)) {
    depth_charts <- fallback
    message("01_depth_charts complete: ", nrow(depth_charts),
            " players (from base cache fallback)")
  } else {
    depth_charts <- dplyr::tibble(
      mlbam_id     = integer(),
      player_name  = character(),
      team_abbr    = character(),
      fg_team_abbr = character(),
      fg_role      = character(),
      fg_position  = character(),
      roster_type  = character()
    )
    message("01_depth_charts complete: 0 players (API unavailable, no base cache)")
  }
} else {

parsed_init <- jsonlite::fromJSON(
  httr::content(fg_depth_raw, as = "text", encoding = "UTF-8"),
  flatten = TRUE
)

# Extract all 30 FG team IDs from the TeamList
fg_team_list <- dplyr::as_tibble(parsed_init$TeamList) %>%
  dplyr::select(
    fg_team_id   = TeamId,
    fg_team_name = FullName,
    fg_team_abbr = AbbName
  )

message("FanGraphs team list: ", nrow(fg_team_list), " teams found")

# ------------------------------------------------------------
# Pull depth chart for each team
# ------------------------------------------------------------

pull_team_depth_chart <- function(fg_team_id, fg_team_abbr, fg_team_name) {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/depth-charts/data",
      query   = list(teamid = fg_team_id, position = "ALL"),
      .fg_headers,
      httr::timeout(30)
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::http_error(resp)) {
    message("Depth chart pull failed for team ", fg_team_abbr)
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(parsed) || !"Roster" %in% names(parsed)) return(NULL)

  roster <- dplyr::as_tibble(parsed$Roster)

  if (nrow(roster) == 0 || !"mlbamid" %in% names(roster)) return(NULL)

  roster %>%
    dplyr::select(
      mlbam_id     = mlbamid,
      player_name  = player,
      fg_role      = role,
      fg_position  = position,
      roster_type  = type,
      dplyr::any_of(c(
        "proj_pit_WAR", "proj_bat_WAR",
        "proj_pit_ERA", "proj_pit_IP", "proj_pit_GS", "proj_pit_SV",
        "proj_bat_WAR", "proj_bat_OPS", "proj_bat_wRC+"
      ))
    ) %>%
    dplyr::mutate(
      mlbam_id     = as.integer(mlbam_id),
      fg_team_id   = as.integer(fg_team_id),
      fg_team_abbr = fg_team_abbr,
      fg_team_name = fg_team_name
    )
}

# Loop all 30 teams
depth_charts_raw <- purrr::pmap_dfr(
  list(fg_team_list$fg_team_id,
       fg_team_list$fg_team_abbr,
       fg_team_list$fg_team_name),
  pull_team_depth_chart
)

# ------------------------------------------------------------
# Clean and join team_abbr from team_ids
# ------------------------------------------------------------

depth_charts <- depth_charts_raw %>%
  # Join on full team name — reliable across FG and MLB ID systems
  # (avoids SDP/SF, SFG/SF type abbreviation mismatches)
  dplyr::left_join(
    team_ids %>% dplyr::select(team_name, team_abbr, mlbam_team_id),
    by = c("fg_team_name" = "team_name")
  ) %>%
  dplyr::mutate(
    team_abbr = dplyr::coalesce(team_abbr, fg_team_abbr)
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::distinct(mlbam_id, fg_team_abbr, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id, player_name, team_abbr, fg_team_abbr,
    fg_role, fg_position, roster_type,
    dplyr::everything()
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_teams    <- dplyr::n_distinct(depth_charts$fg_team_abbr)
n_pitchers <- sum(depth_charts$fg_position %in% c("SP", "RP", "SP/RP"), na.rm = TRUE)

if (n_teams < 28) {
  warning("Only ", n_teams, " teams in depth chart — expected 30")
}

# If the API responded but returned nothing useful, try base cache
if (n_teams == 0) {
  message("WARNING: Depth chart loop returned 0 teams. Trying base cache fallback...")
  fallback <- .depth_charts_from_cache()
  if (!is.null(fallback)) {
    depth_charts <- fallback
    n_teams    <- dplyr::n_distinct(depth_charts$fg_team_abbr)
    n_pitchers <- sum(depth_charts$fg_position %in% c("SP", "RP", "SP/RP"), na.rm = TRUE)
    message("01_depth_charts complete: ", nrow(depth_charts),
            " players across ", n_teams, " teams (from base cache fallback)")
  }
}

message("01_depth_charts complete: ",
        nrow(depth_charts), " players across ", n_teams, " teams (",
        n_pitchers, " pitchers)")

} # end else (API available)
