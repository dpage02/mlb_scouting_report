# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs pitching leaderboard (league-wide).
#   Bypasses baseballr::fg_pitcher_leaders() which has a known bug
#   ("object 'leaders' not found"). Uses httr directly instead,
#   following the same pattern as 01_depth_charts.R.
#
# OUTPUT:
#   player_season_fg_pitching
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

season_to_pull <- unique(player_season_mlb_pitching$season)[1]

# ------------------------------------------------------------
# Direct FanGraphs API pull — bypasses fg_pitcher_leaders()
# type 8  = Dashboard  (ERA, FIP, xFIP, xERA, SIERA, WAR, K%, BB%, etc.)
# type 1  = Advanced   (ERA-, FIP-, xFIP-, K%, BB%, K-BB%)
# type 2  = Batted Ball (GB%, LD%, FB%, IFFB%, HR/FB, Hard%, Med%, Soft%)
# type 5  = Plate Disc. (O-Swing%, Z-Swing%, Zone%, SwStr%, CSW%)
# qual=0  → all pitchers, no IP minimum
# ind=0   → aggregate across teams for multi-team players
# ------------------------------------------------------------

pull_fg_pitching_api <- function(yr, type_num = 8) {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      query = list(
        age       = "",
        pos       = "all",
        stats     = "pit",
        lg        = "all",
        season    = yr,
        season1   = yr,
        ind       = "0",
        qual      = "0",
        type      = as.character(type_num),
        pageitems = "2000000",
        pagenum   = "1",
        rost      = "0"
      ),
      httr::timeout(60)
    ),
    error = function(e) {
      message("FanGraphs pitching API failed (type=", type_num, ", yr=", yr, "): ", e$message)
      NULL
    }
  )

  if (is.null(resp) || httr::http_error(resp)) {
    message("FanGraphs pitching API error (type=", type_num, ", yr=", yr, ")")
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    ),
    error = function(e) {
      message("FanGraphs JSON parse failed: ", e$message)
      NULL
    }
  )

  if (is.null(parsed) || !"data" %in% names(parsed)) return(NULL)

  result <- tryCatch(dplyr::as_tibble(parsed$data), error = function(e) NULL)
  if (is.null(result) || nrow(result) == 0) return(NULL)
  result
}

# ------------------------------------------------------------
# Pull dashboard (type 8) — main metrics + WAR
# Fall back to prior season if current is too sparse
# ------------------------------------------------------------

fg_raw <- pull_fg_pitching_api(season_to_pull, type_num = 8)

if (is.null(fg_raw) || nrow(fg_raw) < 50) {
  fallback_yr <- season_to_pull - 1L
  message("FanGraphs pitching data insufficient for ", season_to_pull,
          " (", if (is.null(fg_raw)) "NULL" else nrow(fg_raw), " rows). ",
          "Falling back to ", fallback_yr, ".")
  fg_raw <- pull_fg_pitching_api(fallback_yr, type_num = 8)
  if (!is.null(fg_raw) && nrow(fg_raw) >= 50) {
    season_to_pull <- fallback_yr
  } else {
    fg_raw <- NULL
  }
}

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs pitching data available. Creating empty table.")
  player_season_fg_pitching <- dplyr::tibble(
    mlbam_id  = integer(),
    season    = integer(),
    team_abbr = character()
  )
} else {

  message("FanGraphs pitching pull (type 8): ", nrow(fg_raw), " rows | columns: ",
          paste(names(fg_raw), collapse = ", "))

  # ------------------------------------------------------------
  # Normalize column names — FanGraphs uses %, /, - in names
  # ------------------------------------------------------------
  names(fg_raw) <- dplyr::case_when(
    names(fg_raw) == "LOB%"    ~ "LOB_pct",
    names(fg_raw) == "K/9"     ~ "K_per_9",
    names(fg_raw) == "BB/9"    ~ "BB_per_9",
    names(fg_raw) == "H/9"     ~ "H_per_9",
    names(fg_raw) == "HR/9"    ~ "HR_per_9",
    names(fg_raw) == "K%"      ~ "K_pct",
    names(fg_raw) == "BB%"     ~ "BB_pct",
    names(fg_raw) == "K-BB%"   ~ "K_BB_pct",
    names(fg_raw) == "ERA-"    ~ "ERA_minus",
    names(fg_raw) == "FIP-"    ~ "FIP_minus",
    names(fg_raw) == "xFIP-"   ~ "xFIP_minus",
    TRUE                       ~ names(fg_raw)
  )

  # Helper: safe numeric column extraction
  safe_num <- function(df, ...) {
    cols <- c(...)
    found <- intersect(cols, names(df))
    if (length(found) == 0) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[found[1]]]))
  }

  # ------------------------------------------------------------
  # Join to player_master_ids to get mlbam_id
  # FanGraphs returns fg player ID as "playerid"
  # ------------------------------------------------------------
  fg_joined <- fg_raw %>%
    dplyr::left_join(
      player_master_ids %>%
        dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
        dplyr::distinct(fg_id, .keep_all = TRUE) %>%
        dplyr::select(fg_id, mlbam_id),
      by = c("playerid" = "fg_id")
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::mutate(
      mlbam_id  = as.integer(mlbam_id),
      season    = as.integer(season_to_pull),
      team_abbr = dplyr::coalesce(as.character(Team), "TOT")
    )

  # ------------------------------------------------------------
  # Build canonical table with fg_ prefix
  # ------------------------------------------------------------
  player_season_fg_pitching <- dplyr::tibble(
    mlbam_id  = fg_joined$mlbam_id,
    season    = fg_joined$season,
    team_abbr = fg_joined$team_abbr,

    # Workload
    fg_g  = suppressWarnings(as.integer(safe_num(fg_joined, "G"))),
    fg_gs = suppressWarnings(as.integer(safe_num(fg_joined, "GS"))),
    fg_ip = safe_num(fg_joined, "IP"),

    # Classic rates
    fg_era  = safe_num(fg_joined, "ERA"),
    fg_whip = safe_num(fg_joined, "WHIP"),

    # Advanced ERA estimators
    fg_FIP   = safe_num(fg_joined, "FIP"),
    fg_xFIP  = safe_num(fg_joined, "xFIP"),
    fg_xERA  = safe_num(fg_joined, "xERA"),
    fg_SIERA = safe_num(fg_joined, "SIERA"),

    # BABIP / Strand rate
    fg_BABIP   = safe_num(fg_joined, "BABIP"),
    fg_LOB_pct = safe_num(fg_joined, "LOB_pct"),

    # Per-9 rates
    fg_K_9  = safe_num(fg_joined, "K_per_9",  "K.9"),
    fg_BB_9 = safe_num(fg_joined, "BB_per_9", "BB.9"),
    fg_H_9  = safe_num(fg_joined, "H_per_9",  "H.9"),
    fg_HR_9 = safe_num(fg_joined, "HR_per_9", "HR.9"),

    # Rate percentages
    fg_K_pct    = safe_num(fg_joined, "K_pct",    "SO_pct"),
    fg_BB_pct   = safe_num(fg_joined, "BB_pct"),
    fg_K_BB_pct = safe_num(fg_joined, "K_BB_pct"),

    # Value
    fg_WAR       = safe_num(fg_joined, "WAR"),
    fg_ERA_minus = safe_num(fg_joined, "ERA_minus", "ERA-")
  ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE)

  message("fg_WAR non-NA: ", sum(!is.na(player_season_fg_pitching$fg_WAR)),
          " | fg_ERA_minus non-NA: ", sum(!is.na(player_season_fg_pitching$fg_ERA_minus)))

  # ------------------------------------------------------------
  # Pull additional stat types and join on mlbam_id
  # ------------------------------------------------------------

  fg_id_map <- player_master_ids %>%
    dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
    dplyr::distinct(fg_id, .keep_all = TRUE) %>%
    dplyr::select(fg_id, mlbam_id)

  pull_fg_pitcher_extra <- function(type_num) {
    raw <- pull_fg_pitching_api(season_to_pull, type_num = type_num)
    if (is.null(raw) || nrow(raw) == 0) {
      message("FG pitcher type ", type_num, ": no data")
      return(NULL)
    }

    # Normalize special characters in column names
    names(raw) <- gsub("%", "_pct", gsub("/", "_per_", gsub("-", "_", names(raw))))

    drop_cols <- c(
      "playerid", "Season", "Name", "PlayerName", "Team", "Tm",
      "G", "GS", "IP", "W", "L", "SV",
      "ERA", "FIP", "xFIP", "SIERA", "xERA",
      "WHIP", "BABIP", "WAR", "K.9", "BB.9", "HR.9",
      "LOB.", "GB.", "Age", "AgeRng"
    )

    result <- raw %>%
      dplyr::left_join(fg_id_map, by = c("playerid" = "fg_id")) %>%
      dplyr::filter(!is.na(mlbam_id)) %>%
      dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
      dplyr::select(-dplyr::any_of(drop_cols), -dplyr::any_of("playerid")) %>%
      dplyr::rename_with(~ paste0("fg_", .x), -mlbam_id) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    message("FG pitcher type ", type_num, ": ", ncol(result) - 1,
            " new columns for ", nrow(result), " pitchers")
    result
  }

  # Type 1 = Advanced (ERA-, FIP-, K%, BB%)
  # Type 2 = Batted Ball (Hard%, GB%, FB%, IFFB%, HR/FB)
  # Type 5 = Plate Discipline (O-Swing%, SwStr%, CSW%, Zone%)
  extra_types <- list(
    pull_fg_pitcher_extra(1),
    pull_fg_pitcher_extra(2),
    pull_fg_pitcher_extra(5)
  )

  for (extra in extra_types) {
    if (!is.null(extra)) {
      player_season_fg_pitching <- player_season_fg_pitching %>%
        dplyr::left_join(extra, by = "mlbam_id", suffix = c("", "_dup")) %>%
        dplyr::select(-dplyr::ends_with("_dup"))
    }
  }
}

validate_performance_table(player_season_fg_pitching)

message("02_fangraphs_pitching_season complete: ",
        nrow(player_season_fg_pitching),
        " rows for season ", season_to_pull, ".")
