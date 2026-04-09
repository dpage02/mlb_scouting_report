# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 07_statcast_batter_vs_pitch.R
# ============================================================
# PURPOSE:
#   Pull per-batter, per-pitch-type performance stats from
#   Baseball Savant and aggregate across the last 3 seasons
#   for stable career-level sample sizes.
#
# OUTPUT:
#   batter_pitch_type_stats
#
# GRAIN:
#   One row per mlbam_id per pitch_code (career aggregate)
#
# NOTE ON DATA SOURCE:
#   Uses Savant statcast_search with group_by=name-pitch_type
#   (NOT pitch-arsenals, which only serves pitcher data).
#   One request per season; Savant returns aggregated stats
#   per batter per pitch type directly.
# ============================================================

career_seasons <- (target_season - 2L):target_season

if (!exists("pitch_name_map")) {
  pitch_name_map <- c(
    FF = "4-Seam FB",  FA = "4-Seam FB",  SI = "Sinker",
    FT = "Sinker",     FC = "Cutter",      FS = "Splitter",
    FO = "Forkball",   SL = "Slider",      ST = "Sweeper",
    SV = "Slurve",     CU = "Curveball",   KC = "Knuckle-Curve",
    CH = "Changeup",   SC = "Screwball",   KN = "Knuckleball",
    EP = "Eephus",     UN = "Unknown"
  )
}

# Safe weighted mean: drops NA pairs before computing
.wmean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

# ------------------------------------------------------------
# Fetch one season from Savant statcast search
# Returns long-format: mlbam_id, pitch_code, n_pitches,
#   xba, xwoba, whiff_pct, hard_hit_pct, run_value_per100
# ------------------------------------------------------------

fetch_batter_vs_pitch_season <- function(yr) {
  url <- paste0(
    "https://baseballsavant.mlb.com/statcast_search",
    "?hfGT=R%7C",
    "&hfSea=", yr, "%7C",
    "&player_type=batter",
    "&group_by=name-pitch_type",
    "&min_pitches=0&min_results=0&min_pas=0",
    "&type=details&csv=true"
  )

  df <- tryCatch(
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("Batter vs pitch fetch failed (yr=", yr, "): ", e$message)
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) {
    message("  No batter vs pitch data for ", yr)
    return(NULL)
  }

  # Savant uses player_id in this view
  id_col <- intersect(c("player_id", "batter"), names(df))[1]
  pt_col <- intersect(c("pitch_type"), names(df))[1]
  if (is.na(id_col) || is.na(pt_col)) {
    message("  Unexpected column names in batter vs pitch response for ", yr,
            ": ", paste(names(df), collapse = ", "))
    return(NULL)
  }

  result <- df %>%
    dplyr::select(
      mlbam_id   = dplyr::all_of(id_col),
      pitch_code = dplyr::all_of(pt_col),
      dplyr::any_of(c(
        "pitches",
        "xba", "xwoba",
        "whiff_percent", "hard_hit_percent",
        "run_value_per100"
      ))
    ) %>%
    dplyr::mutate(
      mlbam_id   = as.integer(mlbam_id),
      pitch_code = toupper(as.character(pitch_code)),
      season     = as.integer(yr)
    ) %>%
    dplyr::filter(!is.na(mlbam_id), nchar(pitch_code) > 0, pitch_code != "NA")

  # Standardise column names
  if ("pitches" %in% names(result))
    result <- dplyr::rename(result, n_pitches = pitches)
  if ("whiff_percent" %in% names(result))
    result <- dplyr::rename(result, whiff_pct = whiff_percent)
  if ("hard_hit_percent" %in% names(result))
    result <- dplyr::rename(result, hard_hit_pct = hard_hit_percent)

  # Ensure n_pitches is integer
  if ("n_pitches" %in% names(result))
    result <- dplyr::mutate(result, n_pitches = as.integer(n_pitches))

  # Normalise pct fields from 0-100 → decimal
  if ("whiff_pct" %in% names(result))
    result <- dplyr::mutate(result,
      whiff_pct = dplyr::if_else(!is.na(whiff_pct) & whiff_pct > 1, whiff_pct / 100, whiff_pct))
  if ("hard_hit_pct" %in% names(result))
    result <- dplyr::mutate(result,
      hard_hit_pct = dplyr::if_else(!is.na(hard_hit_pct) & hard_hit_pct > 1, hard_hit_pct / 100, hard_hit_pct))

  message("  ", yr, ": ", nrow(result), " batter-pitch rows, ",
          dplyr::n_distinct(result$mlbam_id), " batters")
  result
}

# ------------------------------------------------------------
# Pull all career seasons and aggregate
# ------------------------------------------------------------

message("Batter vs pitch type: pulling seasons ",
        paste(career_seasons, collapse = ", "), "...")

seasons_list <- lapply(career_seasons, fetch_batter_vs_pitch_season)
seasons_list <- Filter(Negate(is.null), seasons_list)

if (length(seasons_list) == 0) {
  message("No batter vs pitch data available. Creating empty table.")
  batter_pitch_type_stats <- dplyr::tibble(
    mlbam_id         = integer(),
    pitch_code       = character(),
    pitch_name       = character(),
    n_pitches        = integer(),
    xba              = numeric(),
    xwoba            = numeric(),
    whiff_pct        = numeric(),
    hard_hit_pct     = numeric(),
    run_value_per100 = numeric()
  )
} else {
  rate_cols <- c("xba", "xwoba", "whiff_pct", "hard_hit_pct", "run_value_per100")

  all_seasons <- dplyr::bind_rows(seasons_list)

  # Add any rate cols that may be missing
  for (col in rate_cols) {
    if (!col %in% names(all_seasons))
      all_seasons[[col]] <- NA_real_
  }
  if (!"n_pitches" %in% names(all_seasons))
    all_seasons$n_pitches <- NA_integer_

  batter_pitch_type_stats <- all_seasons %>%
    dplyr::group_by(mlbam_id, pitch_code) %>%
    dplyr::summarise(
      n_pitches        = sum(n_pitches, na.rm = TRUE),
      xba              = .wmean(xba,              n_pitches),
      xwoba            = .wmean(xwoba,            n_pitches),
      whiff_pct        = .wmean(whiff_pct,        n_pitches),
      hard_hit_pct     = .wmean(hard_hit_pct,     n_pitches),
      run_value_per100 = .wmean(run_value_per100, n_pitches),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_pitches  = as.integer(n_pitches),
      pitch_name = purrr::map_chr(pitch_code, function(code) {
        nm <- unname(pitch_name_map[code])
        if (is.na(nm)) code else nm
      })
    ) %>%
    dplyr::filter(!is.na(n_pitches), n_pitches >= 50L) %>%
    dplyr::select(
      mlbam_id, pitch_code, pitch_name, n_pitches,
      dplyr::any_of(rate_cols)
    ) %>%
    dplyr::arrange(mlbam_id, dplyr::desc(n_pitches))

  message("07_statcast_batter_vs_pitch complete: ",
          nrow(batter_pitch_type_stats), " batter-pitch rows | ",
          dplyr::n_distinct(batter_pitch_type_stats$mlbam_id), " batters | ",
          "career aggregate (", paste(career_seasons, collapse = "\u2013"), ")")
}
