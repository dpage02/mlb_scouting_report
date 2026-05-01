# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 08_statcast_pitch_arsenal.R
# ============================================================
# PURPOSE:
#   Pull per-pitcher, per-pitch-type arsenal data from Baseball
#   Savant. Provides usage%, velocity, spin rate, movement,
#   whiff%, put-away%, hard-hit%, xBA, xwOBA, and RV/100
#   for every pitch each pitcher throws.
#
# OUTPUT:
#   pitcher_arsenal
#
# GRAIN:
#   One row per mlbam_id per pitch_type per season
#
# ENDPOINTS:
#   PRIMARY (outcomes + usage):
#     /leaderboard/pitch-arsenal-stats  — long format, one row per
#     pitcher × pitch type. Returns whiff%, put_away%, HH%, K%,
#     xBA, xwOBA, RV/100. Values confirmed non-NA for full seasons.
#
#   SUPPLEMENT (movement):
#     /pitch-arsenals?type=avg_speed|avg_spin|avg_break_x|avg_break_z_induced
#     Wide format, pivoted to long. Values confirmed non-NA.
#     NOTE: outcome metric types (whiff_pct, zone_pct, etc.) from
#     this endpoint return all-NA values — use primary endpoint instead.
# ============================================================

season_to_pull <- unique(player_season_mlb_pitching$season)[1]

# ------------------------------------------------------------
# Pitch type display name lookup
# ------------------------------------------------------------

pitch_name_map <- c(
  FF = "4-Seam FB",  FA = "4-Seam FB",  SI = "Sinker",
  FT = "Sinker",     FC = "Cutter",      FS = "Splitter",
  FO = "Forkball",   SL = "Slider",      ST = "Sweeper",
  SV = "Slurve",     CU = "Curveball",   KC = "Knuckle-Curve",
  CH = "Changeup",   SC = "Screwball",   KN = "Knuckleball",
  EP = "Eephus",     UN = "Unknown"
)

# ------------------------------------------------------------
# PRIMARY: long-format arsenal stats (outcomes + usage)
# ------------------------------------------------------------

fetch_arsenal_long <- function(yr, min_p = 5) {
  url <- paste0(
    "https://baseballsavant.mlb.com/leaderboard/pitch-arsenal-stats",
    "?year=", yr, "&type=pitcher&min=", min_p, "&pos=&team=&csv=true"
  )
  tryCatch(
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("Arsenal long fetch failed (yr=", yr, "): ", e$message)
      NULL
    }
  )
}

# ------------------------------------------------------------
# SUPPLEMENT: movement metrics from wide endpoint
# (avg_speed, avg_spin, h_break, v_break confirmed working)
# ------------------------------------------------------------

fetch_movement_long <- function(yr) {
  fetch_wide <- function(type_str) {
    url <- paste0(
      "https://baseballsavant.mlb.com/pitch-arsenals",
      "?year=", yr, "&min=50&type=", type_str, "&hand=&csv=true"
    )
    tryCatch(
      readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
  }

  pivot_wide <- function(df, value_col_name) {
    if (is.null(df) || nrow(df) == 0 || !"pitcher" %in% names(df)) return(NULL)
    suffix      <- paste0("_", value_col_name)
    metric_cols <- grep(paste0("^[a-z]+", suffix, "$"), names(df), value = TRUE)
    if (length(metric_cols) == 0) return(NULL)
    df %>%
      dplyr::select(pitcher, dplyr::all_of(metric_cols)) %>%
      tidyr::pivot_longer(cols = -pitcher, names_to = "col", values_to = value_col_name) %>%
      dplyr::mutate(
        mlbam_id   = as.integer(pitcher),
        pitch_code = toupper(sub(paste0(suffix, "$"), "", col))
      ) %>%
      dplyr::filter(!is.na(.data[[value_col_name]])) %>%
      dplyr::select(mlbam_id, pitch_code, dplyr::all_of(value_col_name))
  }

  speed_long  <- pivot_wide(fetch_wide("avg_speed"),          "avg_speed")
  spin_long   <- pivot_wide(fetch_wide("avg_spin"),           "avg_spin")
  hbreak_long <- pivot_wide(fetch_wide("avg_break_x"),        "avg_break_x")
  vbreak_long <- pivot_wide(fetch_wide("avg_break_z_induced"),"avg_break_z_induced")

  result <- speed_long
  for (extra in list(spin_long, hbreak_long, vbreak_long)) {
    if (!is.null(extra) && !is.null(result)) {
      result <- dplyr::left_join(result, extra, by = c("mlbam_id", "pitch_code"))
    }
  }
  result
}

# ------------------------------------------------------------
# Fetch data for season_to_pull
# ------------------------------------------------------------

raw_long   <- fetch_arsenal_long(season_to_pull)
n_pitchers <- if (!is.null(raw_long)) dplyr::n_distinct(raw_long$player_id) else 0L

# Fallback if current season is too sparse (< 100 pitchers = early season)
if (n_pitchers < 100) {
  fallback_yr <- season_to_pull - 1L
  message("Arsenal data sparse for ", season_to_pull,
          " (", n_pitchers, " pitchers). Falling back to ", fallback_yr, ".")
  raw_long <- fetch_arsenal_long(fallback_yr)
  if (!is.null(raw_long) && nrow(raw_long) > 0) season_to_pull <- fallback_yr
  n_pitchers <- if (!is.null(raw_long)) dplyr::n_distinct(raw_long$player_id) else 0L
}

# ------------------------------------------------------------
# Build pitcher_arsenal
# ------------------------------------------------------------

if (is.null(raw_long) || n_pitchers == 0) {

  message("No pitch arsenal data available. Creating empty table.")
  pitcher_arsenal <- dplyr::tibble(
    mlbam_id         = integer(),
    season           = integer(),
    pitch_code       = character(),
    pitch_name       = character(),
    n_pitches        = integer(),
    usage_pct        = numeric(),
    avg_speed        = numeric(),
    avg_spin         = numeric(),
    h_break          = numeric(),
    v_break          = numeric(),
    whiff_pct        = numeric(),
    put_away         = numeric(),
    hard_hit_pct     = numeric(),
    k_pct            = numeric(),
    xba              = numeric(),
    xwoba            = numeric(),
    run_value_per100 = numeric()
  )

} else {

  message("Pitch arsenal: ", n_pitchers, " pitchers for ", season_to_pull)

  # ── Normalise long-format outcome table ─────────────────────
  # pitch_usage is 0-100; whiff/put_away/hard_hit/k_percent are 0-100
  pitcher_arsenal <- raw_long %>%
    dplyr::mutate(
      mlbam_id         = as.integer(player_id),
      season           = as.integer(season_to_pull),
      pitch_code       = as.character(pitch_type),
      n_pitches        = as.integer(pitches),
      usage_pct        = pitch_usage    / 100,
      whiff_pct        = whiff_percent  / 100,
      put_away         = put_away       / 100,
      hard_hit_pct     = hard_hit_percent / 100,
      k_pct            = if ("k_percent" %in% names(raw_long)) k_percent / 100 else NA_real_,
      xba              = dplyr::coalesce(
                           if ("est_ba"   %in% names(raw_long)) est_ba   else NA_real_,
                           if ("xba"      %in% names(raw_long)) xba      else NA_real_),
      xwoba            = dplyr::coalesce(
                           if ("est_woba" %in% names(raw_long)) est_woba else NA_real_,
                           if ("xwoba"    %in% names(raw_long)) xwoba    else NA_real_),
      run_value_per100 = dplyr::coalesce(
                           if ("run_value_per_100" %in% names(raw_long)) run_value_per_100 else NA_real_,
                           if ("run_value_per100"  %in% names(raw_long)) run_value_per100  else NA_real_),
      pitch_name       = purrr::map_chr(pitch_code, function(code) {
                           nm <- unname(pitch_name_map[code])
                           if (is.na(nm)) code else nm
                         })
    ) %>%
    dplyr::filter(!is.na(usage_pct), usage_pct >= 0.005) %>%
    dplyr::select(
      mlbam_id, season, pitch_code, pitch_name, n_pitches, usage_pct,
      dplyr::any_of(c("whiff_pct", "put_away", "hard_hit_pct", "k_pct",
                       "xba", "xwoba", "run_value_per100"))
    ) %>%
    dplyr::arrange(mlbam_id, dplyr::desc(usage_pct))

  # ── Join movement metrics ────────────────────────────────────
  movement <- fetch_movement_long(season_to_pull)
  if (!is.null(movement)) {
    movement <- movement %>%
      dplyr::mutate(
        h_break = if ("avg_break_x"          %in% names(.)) avg_break_x          else NA_real_,
        v_break = if ("avg_break_z_induced"   %in% names(.)) avg_break_z_induced  else NA_real_
      ) %>%
      dplyr::select(mlbam_id, pitch_code,
                    dplyr::any_of(c("avg_speed", "avg_spin", "h_break", "v_break")))

    pitcher_arsenal <- dplyr::left_join(
      pitcher_arsenal, movement, by = c("mlbam_id", "pitch_code")
    )
  }

  # ── Reorder columns sensibly ─────────────────────────────────
  pitcher_arsenal <- pitcher_arsenal %>%
    dplyr::select(
      mlbam_id, season, pitch_code, pitch_name, n_pitches, usage_pct,
      dplyr::any_of(c("avg_speed", "avg_spin", "h_break", "v_break",
                       "whiff_pct", "put_away", "hard_hit_pct", "k_pct",
                       "xba", "xwoba", "run_value_per100"))
    )
}

message("08_statcast_pitch_arsenal complete: ",
        nrow(pitcher_arsenal), " pitcher-pitch rows | ",
        dplyr::n_distinct(pitcher_arsenal$mlbam_id), " pitchers | ",
        "season ", season_to_pull)
