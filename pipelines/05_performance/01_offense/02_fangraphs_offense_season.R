# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_offense_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs batting stats (type 8 dashboard).
#   Fixes wRC+ column naming and normalizes team_abbr via full
#   team name lookup (avoids SDP/SFG FG abbreviation mismatches).
#
# OUTPUT:
#   player_season_fg_offense
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

# ------------------------------------------------------------
# Determine Season (match MLB pull)
# ------------------------------------------------------------

season_to_pull <- unique(player_season_mlb_offense$season)[1]

# ------------------------------------------------------------
# Pull FanGraphs Batting Leaderboard (type 8 = dashboard)
# ------------------------------------------------------------

fg_raw <- tryCatch(
  baseballr::fg_batter_leaders(
    qual        = "0",
    startseason = as.character(season_to_pull),
    endseason   = as.character(season_to_pull),
    type        = "8",
    pageitems   = "10000"
  ),
  error = function(e) NULL
)

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs data for ", season_to_pull,
          ". Falling back to ", season_to_pull - 1)
  season_to_pull <- season_to_pull - 1

  fg_raw <- baseballr::fg_batter_leaders(
    qual        = "0",
    startseason = as.character(season_to_pull),
    endseason   = as.character(season_to_pull),
    type        = "8",
    pageitems   = "10000"
  )
}

# ------------------------------------------------------------
# Fix wRC+ Column Name Before Generic Prefix
# R converts "+" to "." in column names → wRC+ becomes wRC.
# Rename to wRC_plus so it survives the generic fg_ prefix cleanly
# ------------------------------------------------------------

fg_raw <- fg_raw %>%
  dplyr::rename_with(~ ifelse(.x %in% c("wRC.", "wRC+"), "wRC_plus", .x))

# ------------------------------------------------------------
# Build Fact Table
# ------------------------------------------------------------

player_season_fg_offense <- fg_raw %>%

  dplyr::mutate(
    fg_id    = as.integer(playerid),
    mlbam_id = as.integer(xMLBAMID)
  ) %>%

  # Drop raw identifier columns we've already extracted
  # Drop Season (we set season = season_to_pull), Name (not needed)
  # Keep team_name (full name) — it will become fg_team_name after prefix
  dplyr::select(-playerid, -xMLBAMID, -team_name_abb,
                -dplyr::any_of(c("Season", "Name", "PlayerName"))) %>%

  # Prefix all remaining columns with fg_
  # This converts team_name → fg_team_name, wRC_plus → fg_wRC_plus, etc.
  dplyr::rename_with(
    ~ paste0("fg_", .x),
    -c(fg_id, mlbam_id)
  ) %>%

  # Normalize team_abbr via full team name (avoids SDP/SFG FG abbreviation issues)
  dplyr::left_join(
    team_ids %>% dplyr::select(team_name, team_abbr),
    by = c("fg_team_name" = "team_name")
  ) %>%

  dplyr::mutate(
    team_abbr = dplyr::coalesce(team_abbr, fg_team_name),
    season    = as.integer(season_to_pull)
  ) %>%

  dplyr::select(-fg_team_name) %>%

  dplyr::filter(!is.na(mlbam_id)) %>%

  dplyr::select(mlbam_id, season, team_abbr, fg_id, dplyr::everything())

# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

# ------------------------------------------------------------
# Additional FanGraphs type pulls
# Each type returns columns the type 8 dashboard doesn't include.
# We strip identity/duplicate columns and join on mlbam_id.
# ------------------------------------------------------------

pull_fg_batter_extra <- function(type_num, season_val) {
  raw <- tryCatch(
    baseballr::fg_batter_leaders(
      qual        = "0",
      startseason = as.character(season_val),
      endseason   = as.character(season_val),
      type        = as.character(type_num),
      pageitems   = "10000"
    ),
    error = function(e) {
      message("FG batter type ", type_num, " failed: ", e$message)
      NULL
    }
  )

  if (is.null(raw) || nrow(raw) == 0) {
    message("FG batter type ", type_num, ": no data")
    return(NULL)
  }

  # Drop identity columns and columns already carried from type 8
  drop_cols <- c(
    "playerid", "xMLBAMID", "team_name_abb", "team_name", "Season",
    "Name", "PlayerName", "G", "PA", "AB",
    "AVG", "OBP", "SLG", "OPS", "HR", "R", "RBI", "SB",
    "BB.", "K.", "ISO", "BABIP", "wOBA", "wRC.", "WAR",
    "BsR", "Off", "Def", "xwOBA"
  )

  result <- raw %>%
    dplyr::mutate(mlbam_id = suppressWarnings(as.integer(xMLBAMID))) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::rename_with(~ paste0("fg_", .x), -mlbam_id) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)

  message("FG batter type ", type_num, ": ", ncol(result) - 1,
          " new columns for ", nrow(result), " players")
  result
}

extra_types_offense <- list(
  t1 = pull_fg_batter_extra(1, season_to_pull),   # Advanced: Spd, UBR, wSB, wRC, wRAA
  t2 = pull_fg_batter_extra(2, season_to_pull),   # Batted Ball: Hard%, Soft%, Med%, IFFB%, HR/FB
  t3 = pull_fg_batter_extra(3, season_to_pull),   # Win Probability: WPA, RE24, Clutch, pLI
  t5 = pull_fg_batter_extra(5, season_to_pull),   # Plate Discipline: O-Swing%, Z-Contact%, CSW%
  t6 = pull_fg_batter_extra(6, season_to_pull),   # Value breakdown: RAR, Dollars, Positional
  t7 = pull_fg_batter_extra(7, season_to_pull)    # Pitch values: wFB, wSL, wCB, wCH, etc.
)

for (extra in extra_types_offense) {
  if (!is.null(extra)) {
    player_season_fg_offense <- player_season_fg_offense %>%
      dplyr::left_join(extra, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup"))
  }
}

validate_performance_table(player_season_fg_offense)

# ------------------------------------------------------------
# Completion Message
# ------------------------------------------------------------

message("02_fangraphs_offense_season complete: ",
        nrow(player_season_fg_offense),
        " league-wide rows created for season ", season_to_pull,
        " | fg_wRC_plus present: ",
        "fg_wRC_plus" %in% names(player_season_fg_offense),
        " | non-NA wRC+: ",
        sum(!is.na(player_season_fg_offense$fg_wRC_plus)))
