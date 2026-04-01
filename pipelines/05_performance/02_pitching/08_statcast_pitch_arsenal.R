# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 08_statcast_pitch_arsenal.R
# ============================================================
# PURPOSE:
#   Pull per-pitcher, per-pitch-type arsenal data from Baseball
#   Savant. Provides usage%, velocity, spin rate, and whiff%
#   for every pitch each pitcher throws.
#
# OUTPUT:
#   pitcher_arsenal
#
# GRAIN:
#   One row per mlbam_id per pitch_type per season
#
# NOTE ON API FORMAT:
#   Baseball Savant pitch-arsenals endpoint returns WIDE format:
#     type=n_       → columns n_ff, n_si, n_fc, ...  (pitch counts)
#     type=avg_speed → columns ff_avg_speed, si_avg_speed, ...
#     type=avg_spin  → columns ff_avg_spin,  si_avg_spin,  ...
#     type=whiff_pct → columns ff_whiff_pct, si_whiff_pct, ...
#   We fetch each metric separately and pivot to long before joining.
#   Valid min values: 50, 100, 250, 500, 750, 1000, ...
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
# Helper: fetch one metric type from Baseball Savant
# ------------------------------------------------------------

fetch_arsenal_type <- function(yr, type_str, min_p = 50) {
  url <- paste0(
    "https://baseballsavant.mlb.com/pitch-arsenals",
    "?year=", yr,
    "&min=", min_p,
    "&type=", type_str,
    "&hand=&csv=true"
  )
  tryCatch(
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("Arsenal fetch failed (type=", type_str, ", yr=", yr, "): ", e$message)
      NULL
    }
  )
}

# ------------------------------------------------------------
# Helper: pivot wide metric table to long
#   Counts:  n_ff, n_si, ...        → pitch_code = FF, SI, ...
#   Others:  ff_avg_speed, ...      → pitch_code = FF, SI, ...
# ------------------------------------------------------------

pivot_arsenal_wide <- function(df, value_col_name, is_count = FALSE) {
  if (is.null(df) || nrow(df) == 0 || !"pitcher" %in% names(df)) return(NULL)

  if (is_count) {
    metric_cols <- grep("^n_[a-z]+$", names(df), value = TRUE)
    get_code    <- function(col) toupper(sub("^n_", "", col))
  } else {
    suffix      <- paste0("_", value_col_name)
    metric_cols <- grep(paste0("^[a-z]+", suffix, "$"), names(df), value = TRUE)
    get_code    <- function(col) toupper(sub(paste0(suffix, "$"), "", col))
  }

  if (length(metric_cols) == 0) return(NULL)

  df %>%
    dplyr::select(pitcher, dplyr::all_of(metric_cols)) %>%
    tidyr::pivot_longer(
      cols      = -pitcher,
      names_to  = "col",
      values_to = value_col_name
    ) %>%
    dplyr::mutate(
      mlbam_id   = as.integer(pitcher),
      pitch_code = get_code(col)
    ) %>%
    dplyr::filter(!is.na(.data[[value_col_name]])) %>%
    dplyr::select(mlbam_id, pitch_code, dplyr::all_of(value_col_name))
}

# ------------------------------------------------------------
# Pull all metric types for a given season
# ------------------------------------------------------------

pull_all_arsenal <- function(yr) {
  list(
    counts    = fetch_arsenal_type(yr, "n_"),
    avg_speed = fetch_arsenal_type(yr, "avg_speed"),
    avg_spin  = fetch_arsenal_type(yr, "avg_spin"),
    whiff_pct = fetch_arsenal_type(yr, "whiff_pct")
  )
}

metrics <- pull_all_arsenal(season_to_pull)
n_pitchers <- if (!is.null(metrics$counts)) nrow(metrics$counts) else 0L

# Fallback if current season is too sparse (< 100 pitchers = early season)
if (n_pitchers < 100) {
  fallback_yr <- season_to_pull - 1L
  message("Pitch arsenal data insufficient for ", season_to_pull,
          " (", n_pitchers, " pitchers). Falling back to ", fallback_yr, ".")
  metrics <- pull_all_arsenal(fallback_yr)
  n_pitchers <- if (!is.null(metrics$counts)) nrow(metrics$counts) else 0L
  if (n_pitchers > 0) season_to_pull <- fallback_yr
}

# ------------------------------------------------------------
# Build long-format arsenal table
# ------------------------------------------------------------

if (n_pitchers == 0) {
  message("No pitch arsenal data available. Creating empty table.")
  pitcher_arsenal <- dplyr::tibble(
    mlbam_id  = integer(),
    season    = integer(),
    pitch_code = character(),
    pitch_name = character(),
    n_pitches  = integer(),
    usage_pct  = numeric(),
    avg_speed  = numeric(),
    avg_spin   = numeric(),
    whiff_pct  = numeric()
  )
} else {
  message("Pitch arsenal: ", n_pitchers, " pitchers for ", season_to_pull)

  # Counts → long + compute usage_pct
  counts_long <- pivot_arsenal_wide(metrics$counts, "n_pitches", is_count = TRUE)
  if (!is.null(counts_long)) {
    counts_long <- counts_long %>%
      dplyr::mutate(n_pitches = as.integer(n_pitches)) %>%
      dplyr::group_by(mlbam_id) %>%
      dplyr::mutate(usage_pct = n_pitches / sum(n_pitches, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  # Other metrics → long
  speed_long <- pivot_arsenal_wide(metrics$avg_speed, "avg_speed",  is_count = FALSE)
  spin_long  <- pivot_arsenal_wide(metrics$avg_spin,  "avg_spin",   is_count = FALSE)
  whiff_long <- pivot_arsenal_wide(metrics$whiff_pct, "whiff_pct",  is_count = FALSE)

  # Join all metrics
  pitcher_arsenal <- counts_long
  for (extra in list(speed_long, spin_long, whiff_long)) {
    if (!is.null(extra)) {
      pitcher_arsenal <- dplyr::left_join(
        pitcher_arsenal, extra, by = c("mlbam_id", "pitch_code")
      )
    }
  }

  pitcher_arsenal <- pitcher_arsenal %>%
    dplyr::mutate(
      season     = as.integer(season_to_pull),
      pitch_name = purrr::map_chr(pitch_code, function(code) {
        nm <- unname(pitch_name_map[code])
        if (is.na(nm)) code else nm
      }),
      # whiff_pct from Savant is already 0-100; convert to decimal
      whiff_pct = dplyr::if_else(
        !is.na(whiff_pct) & whiff_pct > 1, whiff_pct / 100, whiff_pct
      )
    ) %>%
    dplyr::filter(!is.na(usage_pct), usage_pct >= 0.005) %>%
    dplyr::select(
      mlbam_id, season, pitch_code, pitch_name,
      n_pitches, usage_pct,
      dplyr::any_of(c("avg_speed", "avg_spin", "whiff_pct"))
    ) %>%
    dplyr::arrange(mlbam_id, dplyr::desc(usage_pct))
}

message("08_statcast_pitch_arsenal complete: ",
        nrow(pitcher_arsenal), " pitcher-pitch rows | ",
        dplyr::n_distinct(pitcher_arsenal$mlbam_id), " pitchers | ",
        "season ", season_to_pull)
