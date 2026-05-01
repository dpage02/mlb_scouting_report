# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 07_statcast_batter_vs_pitch.R
# ============================================================
# PURPOSE:
#   Build per-batter, per-pitch-type run value stats from
#   FanGraphs pitch RV columns already in offense_master_season.
#   Pivots the wide fg_wXX_C columns to long format.
#
# OUTPUT:
#   batter_pitch_type_stats
#
# GRAIN:
#   One row per mlbam_id per pitch_code
#
# DATA SOURCE:
#   offense_master_season (fg_wFB_C, fg_wSL_C, fg_wCT_C, etc.)
#   from 02_fangraphs_offense_season.R type-3 pull.
#   Run value per 100 pitches, batter perspective.
#
# NOTE:
#   FanGraphs pitch RV lumps 4-seam + 2-seam + generic fastball
#   into wFB/C (mapped to code FF). Sweeper (ST) uses pfxwST/C
#   if present. Sinker (SI) uses pfxwSI/C if present.
# ============================================================

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

# FanGraphs fg_wXX_C column → Statcast pitch code mapping.
# One canonical code per FG column (display purposes).
fg_rv_pitch_map <- c(
  fg_wFB_C      = "FF",   # FG "fastball" bucket: 4-seam + 2-seam + FA
  fg_wCT_C      = "FC",   # Cutter
  fg_wSL_C      = "SL",   # Slider
  fg_wCB_C      = "CU",   # Curveball (FG uses CB abbreviation)
  fg_wCH_C      = "CH",   # Changeup
  fg_wSF_C      = "FS",   # Splitter (FG uses SF)
  fg_wKN_C      = "KN",   # Knuckleball
  fg_pfxwSI_C   = "SI",   # Sinker (pfx series — supplement when available)
  fg_pfxwST_C   = "ST",   # Sweeper (pfx series — supplement when available)
  fg_pfxwKC_C   = "KC"    # Knuckle-curve (pfx series)
)

# ----------------------------------------------------------------
# Build from FG data in offense_master_season
# ----------------------------------------------------------------

if (!exists("offense_master_season") || nrow(offense_master_season) == 0) {
  message("07_statcast_batter_vs_pitch: offense_master_season unavailable. Creating empty table.")
  batter_pitch_type_stats <- dplyr::tibble(
    mlbam_id         = integer(),
    pitch_code       = character(),
    pitch_name       = character(),
    run_value_per100 = numeric()
  )
} else {

  # Keep only columns that are present in offense_master_season
  available_map <- fg_rv_pitch_map[
    names(fg_rv_pitch_map) %in% names(offense_master_season)
  ]

  if (length(available_map) == 0) {
    message("07_statcast_batter_vs_pitch: no FG pitch RV columns found. Creating empty table.")
    batter_pitch_type_stats <- dplyr::tibble(
      mlbam_id         = integer(),
      pitch_code       = character(),
      pitch_name       = character(),
      run_value_per100 = numeric()
    )
  } else {

    # One row per player (highest-PA stint for traded players)
    oms_base <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(names(available_map)))

    # Pivot to long: one row per mlbam_id per pitch_code
    batter_pitch_type_stats <- tidyr::pivot_longer(
      oms_base,
      cols      = dplyr::any_of(names(available_map)),
      names_to  = "fg_col",
      values_to = "run_value_per100"
    ) %>%
      dplyr::filter(!is.na(run_value_per100)) %>%
      dplyr::mutate(
        pitch_code = unname(available_map[fg_col]),
        pitch_name = purrr::map_chr(pitch_code, function(code) {
          nm <- unname(pitch_name_map[code])
          if (is.na(nm)) code else nm
        })
      ) %>%
      dplyr::select(mlbam_id, pitch_code, pitch_name, run_value_per100) %>%
      dplyr::arrange(mlbam_id, pitch_code)

    message("07_statcast_batter_vs_pitch complete: ",
            nrow(batter_pitch_type_stats), " batter-pitch rows | ",
            dplyr::n_distinct(batter_pitch_type_stats$mlbam_id), " batters | ",
            "FG pitch RV source (", length(available_map), " pitch types)")
  }
}
