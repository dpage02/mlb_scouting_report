# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 02_fangraphs_pitching_season.R
# ============================================================
# PURPOSE:
#   Pull season-level FanGraphs pitching leaderboard (league-wide).
#   Uses fg_pitcher_leaders() — a single leaderboard call that
#   returns all pitchers with full advanced metrics:
#   FIP, xFIP, xERA, BABIP, LOB%, SIERA, K/9, BB/9, fWAR, etc.
#
# OUTPUT:
#   player_season_fg_pitching
#
# GRAIN:
#   One row per mlbam_id / season / team_abbr
# ============================================================

season_to_pull <- unique(player_season_mlb_pitching$season)[1]

# ------------------------------------------------------------
# Pull FanGraphs Leaderboard
# qual = "0"  → all pitchers, no IP minimum
# ind  = "0"  → aggregate across teams for multi-team players
# ------------------------------------------------------------

fg_raw <- tryCatch(
  baseballr::fg_pitcher_leaders(
    startseason = season_to_pull,
    endseason   = season_to_pull,
    qual        = "0",
    ind         = "0"
  ),
  error = function(e) {
    message("FanGraphs pitcher leaders pull failed: ", e$message)
    NULL
  }
)

if (is.null(fg_raw) || nrow(fg_raw) == 0) {
  message("No FanGraphs pitching data for ", season_to_pull, ". Creating empty table.")
  player_season_fg_pitching <- dplyr::tibble(
    mlbam_id  = integer(),
    season    = integer(),
    team_abbr = character()
  )
} else {

  message("FanGraphs pitching pull: ", nrow(fg_raw), " rows | columns: ",
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
    fg_K_pct    = safe_num(fg_joined, "K_pct"),
    fg_BB_pct   = safe_num(fg_joined, "BB_pct"),
    fg_K_BB_pct = safe_num(fg_joined, "K_BB_pct"),

    # Value
    fg_WAR = safe_num(fg_joined, "WAR")
  ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, season, team_abbr, .keep_all = TRUE)

  # Diagnostic: which advanced columns populated
  advanced_cols <- c("fg_FIP", "fg_xFIP", "fg_xERA", "fg_SIERA",
                     "fg_BABIP", "fg_LOB_pct", "fg_WAR")
  fill_rates <- sapply(advanced_cols, function(col) {
    round(mean(!is.na(player_season_fg_pitching[[col]])) * 100)
  })
  message("Advanced stat fill rates: ",
          paste(names(fill_rates), fill_rates, sep = "=", collapse = "% | "), "%")
}

# ------------------------------------------------------------
# Additional FanGraphs pitcher type pulls
# Same pattern as offense — strip identity/dup cols, join on mlbam_id.
# ------------------------------------------------------------

pull_fg_pitcher_extra <- function(type_num, season_val, id_map) {
  raw <- tryCatch(
    baseballr::fg_pitcher_leaders(
      qual        = "0",
      startseason = as.character(season_val),
      endseason   = as.character(season_val),
      type        = as.character(type_num),
      ind         = "0",
      pageitems   = "10000"
    ),
    error = function(e) {
      message("FG pitcher type ", type_num, " failed: ", e$message)
      NULL
    }
  )

  if (is.null(raw) || nrow(raw) == 0) {
    message("FG pitcher type ", type_num, ": no data")
    return(NULL)
  }

  drop_cols <- c(
    "playerid", "Season", "Name", "PlayerName", "Team", "Tm",
    "G", "GS", "IP", "W", "L", "SV",
    "ERA", "FIP", "xFIP", "SIERA", "xERA",
    "WHIP", "BABIP", "WAR", "K.9", "BB.9", "HR.9",
    "LOB.", "GB."
  )

  result <- raw %>%
    dplyr::left_join(
      id_map,
      by = c("playerid" = "fg_id")
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(-dplyr::any_of(drop_cols), -playerid) %>%
    dplyr::rename_with(~ paste0("fg_", .x), -mlbam_id) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)

  message("FG pitcher type ", type_num, ": ", ncol(result) - 1,
          " new columns for ", nrow(result), " pitchers")
  result
}

fg_id_map <- player_master_ids %>%
  dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
  dplyr::distinct(fg_id, .keep_all = TRUE) %>%
  dplyr::select(fg_id, mlbam_id)

extra_types_pitching <- list(
  t1 = pull_fg_pitcher_extra(1, season_to_pull, fg_id_map),  # Advanced: ERA-, FIP-, K%, BB%, K/BB
  t2 = pull_fg_pitcher_extra(2, season_to_pull, fg_id_map),  # Batted Ball: Hard%, Soft%, Med%, IFFB%
  t3 = pull_fg_pitcher_extra(3, season_to_pull, fg_id_map),  # Win Probability: WPA, RE24, Clutch, pLI
  t5 = pull_fg_pitcher_extra(5, season_to_pull, fg_id_map),  # Plate Discipline: O-Swing%, CSW%, SwStr%
  t7 = pull_fg_pitcher_extra(7, season_to_pull, fg_id_map)   # Pitch values: wFB, wSL, wCB, wCH, etc.
)

for (extra in extra_types_pitching) {
  if (!is.null(extra)) {
    player_season_fg_pitching <- player_season_fg_pitching %>%
      dplyr::left_join(extra, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup"))
  }
}

validate_performance_table(player_season_fg_pitching)

message("02_fangraphs_pitching_season complete: ",
        nrow(player_season_fg_pitching),
        " rows for season ", season_to_pull, ".")
