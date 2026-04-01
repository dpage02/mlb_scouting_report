# ============================================================
# mlb_scouting_report
# REPORTING HELPERS — Deep Dive
# SCRIPT: _deepdive_helpers.R
# ============================================================
# PURPOSE:
#   gt builder functions for the game deep dive report.
#
# FUNCTIONS:
#   make_lineup_full_gt(gpk, side)      — full stat table per side
#   make_career_offense_gt(gpk, side)   — multi-year batting history per side
#   make_career_pitching_gt(gpk)        — multi-year pitching history for starters
#   make_pitcher_full_gt(gpk)           — batted ball + pitch discipline for starters
#   make_lineup_splits_gt(gpk, side)    — matchup split table per side
#   make_starter_splits_gt(gpk)         — starter platoon splits
#   make_lineup_defense_gt(gpk, side)   — defense table (traditional + UZR/DRS + OAA)
#   make_arsenal_gt(gpk)                — pitch arsenal for today's starters
#   make_season_context_gt(gpk, side)   — current vs prior season per lineup player
#   make_bullpen_deep_gt(gpk, side)     — full season stats for bullpen arms
#   make_bullpen_arsenal_gt(gpk, side)  — pitch arsenal for bullpen arms
#   make_rotation_gt(gpk, side)         — projected rotation (SP1-SP5) with season stats
# ============================================================

library(gt)
library(dplyr)

# ------------------------------------------------------------
# Full Lineup — "Nerd Heaven" analytical breakdown
# Returns a NAMED LIST of gt tables (3 bands):
#   1. Overall                  — traditional rates + counting + advanced
#   2. Plate Discipline & Contact — outcomes, swing decisions, contact/whiff rates
#   3. Quality, Profile & Value — EV/barrel/expected, batted ball shape, run value/speed/clutch
#
# The QMD caller iterates over the list and renders each table
# with its section label as a sub-heading.
# ------------------------------------------------------------

make_lineup_full_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  # Season label — appended to subtitles so it's clear when showing prior-year stats
  stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
    unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))
  season_label <- paste0(" · ", stats_yr, " season")

  raw <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot)

  full_stats <- offense_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(
      mlbam_id,
      dplyr::any_of(c(
        # Overall
        "mlb_pa", "mlb_avg", "mlb_obp", "mlb_slg", "mlb_ops",
        "mlb_iso", "mlb_babip",
        "fg_wOBA", "fg_wRC_plus",
        "mlb_hr", "mlb_rbi", "mlb_bb", "mlb_so", "mlb_sb",
        # Approach — outcome rates (FG type 8 / type 1)
        "fg_K_pct", "fg_BB_pct", "fg_K.BB.",     # normalized names from type 8
        "fg_K.", "fg_BB.",                         # fallback dot names
        # Approach — swing decisions (FG type 5)
        "fg_O.Swing.", "fg_Z.Swing.", "fg_Zone.", "fg_F.Strike.",
        # Contact ability (FG type 5)
        "fg_SwStr.", "fg_CSW.", "fg_CStr.",
        "fg_Contact.", "fg_O.Contact.", "fg_Z.Contact.",
        # Contact quality — Statcast
        "sc_avg_hit_speed", "sc_brl_percent", "sc_ev95percent",
        # Contact quality — FanGraphs (type 2)
        "fg_Hard.", "fg_Med.", "fg_Soft.",
        # Expected (Statcast)
        "sc_est_ba", "sc_est_slg", "sc_est_woba",
        # Batted ball type (FG type 2)
        "fg_GB.", "fg_LD.", "fg_FB.", "fg_IFFB.", "fg_HR.FB",
        # Batted ball direction (FG type 2)
        "fg_Pull.", "fg_Cent.", "fg_Oppo.",
        # Value / run context (FG type 1 + 3)
        "fg_wRAA", "fg_wRC", "fg_Spd",
        "fg_WPA", "fg_RE24", "fg_Clutch", "fg_pLI",
        # Speed (Statcast sprint)
        "sc_sprint_speed"
      ))
    )

  raw <- raw %>%
    dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
    dplyr::select(-dplyr::ends_with("_dup"))

  # ── Helpers ─────────────────────────────────────────────────

  # Base display tibble — slot / name / position
  base_cols <- function() {
    dplyr::tibble(`#` = raw$batting_slot, Name = raw$player_name, Pos = raw$fg_position)
  }

  # Add a column from raw if the source column exists.
  # Tries multiple source_cols in order, uses the first match.
  add_col <- function(df, col_name, source_cols) {
    for (sc in source_cols) {
      if (sc %in% names(raw)) {
        df[[col_name]] <- raw[[sc]]
        return(df)
      }
    }
    df
  }

  # Standard gt finishing: missing text, col widths, font
  base_gt_opts <- function(tbl) {
    tbl %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
      gt::cols_width(`#` ~ gt::px(28), Pos ~ gt::px(38)) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")
  }

  # ── Section 1: OVERALL ──────────────────────────────────────
  d1 <- base_cols()
  d1 <- add_col(d1, "PA",    c("mlb_pa"))
  d1 <- add_col(d1, "AVG",   c("mlb_avg"))
  d1 <- add_col(d1, "OBP",   c("mlb_obp"))
  d1 <- add_col(d1, "SLG",   c("mlb_slg"))
  d1 <- add_col(d1, "OPS",   c("mlb_ops"))
  d1 <- add_col(d1, "ISO",   c("mlb_iso"))
  d1 <- add_col(d1, "BABIP", c("mlb_babip"))
  d1 <- add_col(d1, "wOBA",  c("fg_wOBA"))
  d1 <- add_col(d1, "wRC+",  c("fg_wRC_plus"))
  d1 <- add_col(d1, "HR",    c("mlb_hr"))
  d1 <- add_col(d1, "RBI",   c("mlb_rbi"))
  d1 <- add_col(d1, "BB",    c("mlb_bb"))
  d1 <- add_col(d1, "K",     c("mlb_so"))
  d1 <- add_col(d1, "SB",    c("mlb_sb"))

  t1 <- d1 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Overall")),
      subtitle = paste0("Traditional rates, counting stats, and advanced value", season_label)
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO", "BABIP", "wOBA")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("wRC+", "PA", "HR", "RBI", "BB", "K", "SB")),
      decimals = 0
    ) %>%
    gt::tab_spanner(
      label   = "Rate",
      columns = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO", "BABIP"))
    ) %>%
    gt::tab_spanner(
      label   = "Advanced",
      columns = dplyr::any_of(c("wOBA", "wRC+"))
    ) %>%
    gt::tab_spanner(
      label   = "Counting",
      columns = dplyr::any_of(c("PA", "HR", "RBI", "BB", "K", "SB"))
    ) %>%
    base_gt_opts()

  # ── Band 2: PLATE DISCIPLINE & CONTACT ──────────────────────
  # Merges former sections 2 (Plate Approach) + 3 (Contact Ability)
  d23 <- base_cols()
  d23 <- add_col(d23, "K%",       c("fg_K_pct",  "fg_K."))
  d23 <- add_col(d23, "BB%",      c("fg_BB_pct", "fg_BB."))
  d23 <- add_col(d23, "K-BB%",    c("fg_K.BB."))
  d23 <- add_col(d23, "Chase%",   c("fg_O.Swing."))
  d23 <- add_col(d23, "Z-Swing%", c("fg_Z.Swing."))
  d23 <- add_col(d23, "Zone%",    c("fg_Zone."))
  d23 <- add_col(d23, "F-Str%",   c("fg_F.Strike."))
  d23 <- add_col(d23, "Contact%", c("fg_Contact."))
  d23 <- add_col(d23, "Z-Con%",   c("fg_Z.Contact."))
  d23 <- add_col(d23, "O-Con%",   c("fg_O.Contact."))
  d23 <- add_col(d23, "SwStr%",   c("fg_SwStr."))
  d23 <- add_col(d23, "CSW%",     c("fg_CSW."))
  d23 <- add_col(d23, "CStr%",    c("fg_CStr."))

  t23 <- d23 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Plate Discipline & Contact")),
      subtitle = paste0("Selectivity and swing decisions (left) · what happens when he swings (right)", season_label)
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "K-BB%", "Chase%", "Z-Swing%",
                                  "Zone%", "F-Str%", "Contact%", "Z-Con%",
                                  "O-Con%", "SwStr%", "CSW%", "CStr%")),
      decimals = 1
    ) %>%
    gt::tab_spanner(
      label   = "Outcomes",
      columns = dplyr::any_of(c("K%", "BB%", "K-BB%"))
    ) %>%
    gt::tab_spanner(
      label   = "Swing Decisions",
      columns = dplyr::any_of(c("Chase%", "Z-Swing%", "Zone%", "F-Str%"))
    ) %>%
    gt::tab_spanner(
      label   = "Contact Rate",
      columns = dplyr::any_of(c("Contact%", "Z-Con%", "O-Con%"))
    ) %>%
    gt::tab_spanner(
      label   = "Miss / Strike",
      columns = dplyr::any_of(c("SwStr%", "CSW%", "CStr%"))
    ) %>%
    base_gt_opts()

  # ── Band 3: QUALITY, PROFILE & VALUE ────────────────────────
  # Merges former sections 4 (Contact Quality) + 5 (Batted Ball) + 6 (Value & Context)
  d456 <- base_cols()
  d456 <- add_col(d456, "EV",     c("sc_avg_hit_speed"))
  d456 <- add_col(d456, "HH%",    c("sc_ev95percent"))
  d456 <- add_col(d456, "Brl%",   c("sc_brl_percent"))
  d456 <- add_col(d456, "Hard%",  c("fg_Hard."))
  d456 <- add_col(d456, "Med%",   c("fg_Med."))
  d456 <- add_col(d456, "Soft%",  c("fg_Soft."))
  d456 <- add_col(d456, "xBA",    c("sc_est_ba"))
  d456 <- add_col(d456, "xSLG",   c("sc_est_slg"))
  d456 <- add_col(d456, "xwOBA",  c("sc_est_woba"))
  d456 <- add_col(d456, "GB%",    c("fg_GB."))
  d456 <- add_col(d456, "LD%",    c("fg_LD."))
  d456 <- add_col(d456, "FB%",    c("fg_FB."))
  d456 <- add_col(d456, "IFFB%",  c("fg_IFFB."))
  d456 <- add_col(d456, "HR/FB",  c("fg_HR.FB"))
  d456 <- add_col(d456, "Pull%",  c("fg_Pull."))
  d456 <- add_col(d456, "Cent%",  c("fg_Cent."))
  d456 <- add_col(d456, "Oppo%",  c("fg_Oppo."))
  d456 <- add_col(d456, "wRAA",   c("fg_wRAA"))
  d456 <- add_col(d456, "wRC",    c("fg_wRC"))
  d456 <- add_col(d456, "Spd",    c("fg_Spd"))
  d456 <- add_col(d456, "Sprint", c("sc_sprint_speed"))
  d456 <- add_col(d456, "WPA",    c("fg_WPA"))
  d456 <- add_col(d456, "RE24",   c("fg_RE24"))
  d456 <- add_col(d456, "Clutch", c("fg_Clutch"))
  d456 <- add_col(d456, "pLI",    c("fg_pLI"))

  t456 <- d456 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Quality, Profile & Value")),
      subtitle = paste0("How hard / where the ball goes · expected stats · run value and context", season_label)
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("EV")),
      decimals = 1
    ) %>%
    # Statcast HH% and Brl% stored as whole numbers (e.g. 42.1 not 0.421)
    gt::fmt_number(
      columns  = dplyr::any_of(c("HH%", "Brl%")),
      decimals = 1
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = dplyr::any_of(c("HH%", "Brl%"))),
      fn = function(x) dplyr::if_else(x == "—", "—", paste0(x, "%"))
    ) %>%
    # FG Hard%/Med%/Soft% stored as decimals (0.42 = 42%)
    gt::fmt_percent(
      columns  = dplyr::any_of(c("Hard%", "Med%", "Soft%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("xBA", "xSLG", "xwOBA")),
      decimals = 3
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%",
                                  "HR/FB", "Pull%", "Cent%", "Oppo%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("wRAA", "wRC", "WPA", "RE24", "Clutch",
                                  "Spd", "Sprint")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("pLI")),
      decimals = 2
    ) %>%
    gt::tab_spanner(
      label   = "Statcast",
      columns = dplyr::any_of(c("EV", "HH%", "Brl%"))
    ) %>%
    gt::tab_spanner(
      label   = "FanGraphs",
      columns = dplyr::any_of(c("Hard%", "Med%", "Soft%"))
    ) %>%
    gt::tab_spanner(
      label   = "Expected",
      columns = dplyr::any_of(c("xBA", "xSLG", "xwOBA"))
    ) %>%
    gt::tab_spanner(
      label   = "BB Type",
      columns = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%", "HR/FB"))
    ) %>%
    gt::tab_spanner(
      label   = "Direction",
      columns = dplyr::any_of(c("Pull%", "Cent%", "Oppo%"))
    ) %>%
    gt::tab_spanner(
      label   = "Run Value",
      columns = dplyr::any_of(c("wRAA", "wRC"))
    ) %>%
    gt::tab_spanner(
      label   = "Speed",
      columns = dplyr::any_of(c("Spd", "Sprint"))
    ) %>%
    gt::tab_spanner(
      label   = "Leverage / Clutch",
      columns = dplyr::any_of(c("WPA", "RE24", "Clutch", "pLI"))
    ) %>%
    base_gt_opts()

  # ── Return list — drop bands with no data columns ────────────
  # (only `#`, Name, Pos means no stats landed for that band)
  tables <- list(
    "Overall"                    = list(d = d1,   t = t1),
    "Plate Discipline & Contact" = list(d = d23,  t = t23),
    "Quality, Profile & Value"   = list(d = d456, t = t456)
  )

  out <- lapply(tables, function(x) {
    if (ncol(x$d) > 3) x$t else NULL
  })
  Filter(Negate(is.null), out)
}

# ------------------------------------------------------------
# Career Batting History — multi-year longitudinal per side
# ------------------------------------------------------------

make_career_offense_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  lineup <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot) %>%
    dplyr::select(mlbam_id, batting_slot, player_name)

  if (!exists("player_career_offense") || nrow(player_career_offense) == 0) {
    return(
      dplyr::tibble(Note = "Career data not available.") %>%
        gt::gt() %>%
        gt::tab_header(title = gt::md(paste0("**", team, "** — Career Batting History")))
    )
  }

  career <- lineup %>%
    dplyr::left_join(player_career_offense, by = "mlbam_id") %>%
    dplyr::arrange(batting_slot, season) %>%
    dplyr::filter(!is.na(season))

  if (nrow(career) == 0) {
    return(
      dplyr::tibble(Note = "No career history found for this lineup.") %>%
        gt::gt() %>%
        gt::tab_header(title = gt::md(paste0("**", team, "** — Career Batting History")))
    )
  }

  # Career totals row per player
  # NOTE: weighted means must be computed BEFORE hist_pa is redefined as a
  # scalar sum — dplyr::summarise() lets later exprs reference earlier results,
  # so if hist_pa = sum(...) appears first it becomes length-1 and breaks
  # weighted.mean(..., w = pmax(hist_pa, 0)) which expects a vector weight.
  totals <- career %>%
    dplyr::group_by(batting_slot, player_name) %>%
    dplyr::summarise(
      mlbam_id    = dplyr::first(mlbam_id),
      season      = "Career",
      # Weighted rates first — hist_pa is still the original group vector here
      hist_avg    = weighted.mean(hist_avg, w = pmax(hist_pa, 0), na.rm = TRUE),
      hist_obp    = weighted.mean(hist_obp, w = pmax(hist_pa, 0), na.rm = TRUE),
      hist_slg    = weighted.mean(hist_slg, w = pmax(hist_pa, 0), na.rm = TRUE),
      hist_ops    = weighted.mean(hist_ops, w = pmax(hist_pa, 0), na.rm = TRUE),
      hist_iso    = weighted.mean(hist_iso, w = pmax(hist_pa, 0), na.rm = TRUE),
      hist_bb_pct = sum(hist_bb, na.rm = TRUE) / sum(hist_pa, na.rm = TRUE),
      hist_k_pct  = sum(hist_so, na.rm = TRUE) / sum(hist_pa, na.rm = TRUE),
      fg_wRC_plus = if ("fg_wRC_plus" %in% names(career))
                     weighted.mean(fg_wRC_plus, w = pmax(hist_pa, 0), na.rm = TRUE)
                   else NA_real_,
      fg_wOBA     = if ("fg_wOBA" %in% names(career))
                     weighted.mean(fg_wOBA, w = pmax(hist_pa, 0), na.rm = TRUE)
                   else NA_real_,
      # Counting totals after — these redefine hist_pa/bb/so as scalars
      hist_pa     = sum(hist_pa,  na.rm = TRUE),
      hist_r      = sum(hist_r,   na.rm = TRUE),
      hist_2b     = sum(hist_2b,  na.rm = TRUE),
      hist_3b     = sum(hist_3b,  na.rm = TRUE),
      hist_hr     = sum(hist_hr,  na.rm = TRUE),
      hist_rbi    = sum(hist_rbi, na.rm = TRUE),
      hist_bb     = sum(hist_bb,  na.rm = TRUE),
      hist_so     = sum(hist_so,  na.rm = TRUE),
      hist_sb     = sum(hist_sb,  na.rm = TRUE),
      .groups = "drop"
    )

  career_full <- dplyr::bind_rows(
    career %>% dplyr::mutate(season = as.character(season)),
    totals
  ) %>%
    dplyr::arrange(batting_slot,
                   dplyr::if_else(season == "Career", 9999L, as.integer(season)))

  display <- dplyr::tibble(
    Name   = career_full$player_name,
    Season = career_full$season,
    PA     = career_full$hist_pa,
    R      = career_full$hist_r,
    `2B`   = career_full$hist_2b,
    `3B`   = career_full$hist_3b,
    HR     = career_full$hist_hr,
    RBI    = career_full$hist_rbi,
    BB     = career_full$hist_bb,
    K      = career_full$hist_so,
    SB     = career_full$hist_sb,
    AVG    = career_full$hist_avg,
    OBP    = career_full$hist_obp,
    SLG    = career_full$hist_slg,
    OPS    = career_full$hist_ops,
    ISO    = career_full$hist_iso,
    `BB%`  = career_full$hist_bb_pct,
    `K%`   = career_full$hist_k_pct
  )

  if ("fg_wRC_plus" %in% names(career_full)) display$`wRC+` <- career_full$fg_wRC_plus
  if ("fg_wOBA"    %in% names(career_full)) display$wOBA    <- career_full$fg_wOBA

  display %>%
    gt::gt(groupname_col = "Name") %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Career Batting History")),
      subtitle = "Last 5 seasons + current  ·  totals aggregated across teams"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO", "wOBA")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("PA", "R", "2B", "3B", "HR", "RBI", "BB", "K", "SB")),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("wRC+")),
      decimals = 0
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("BB%", "K%")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Counting",
      columns = dplyr::any_of(c("PA", "R", "2B", "3B", "HR", "RBI", "BB", "K", "SB"))
    ) %>%
    gt::tab_spanner(
      label   = "Rate",
      columns = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO", "BB%", "K%"))
    ) %>%
    gt::tab_spanner(
      label   = "Advanced",
      columns = dplyr::any_of(c("wRC+", "wOBA"))
    ) %>%
    gt::tab_style(
      style     = list(gt::cell_text(weight = "bold"),
                       gt::cell_fill(color = "#dce8ff")),
      locations = gt::cells_body(rows = Season == "Career")
    ) %>%
    gt::tab_options(
      table.font.size            = 12,
      heading.align              = "left",
      data_row.padding           = gt::px(3),
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4fa"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Career Pitching History — multi-year for today's starters
# ------------------------------------------------------------

make_career_pitching_gt <- function(gpk) {
  raw <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::select(mlbam_id, side, pitcher_name) %>%
    dplyr::mutate(side = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(side)

  if (nrow(raw) == 0) return(invisible(NULL))

  if (!exists("player_career_pitching") || nrow(player_career_pitching) == 0) {
    return(invisible(NULL))
  }

  career <- raw %>%
    dplyr::left_join(player_career_pitching, by = "mlbam_id") %>%
    dplyr::arrange(side, season) %>%
    dplyr::filter(!is.na(season))

  if (nrow(career) == 0) return(invisible(NULL))

  display <- dplyr::tibble(
    Pitcher = career$pitcher_name,
    Season  = career$season,
    G       = career$hist_g,
    GS      = career$hist_gs,
    IP      = career$hist_ip,
    ERA     = career$hist_era,
    WHIP    = career$hist_whip,
    K9      = career$hist_k9,
    BB9     = career$hist_bb9,
    `K/BB`  = career$hist_k_bb,
    H       = career$hist_h,
    HR      = career$hist_hr,
    BB      = career$hist_bb,
    K       = career$hist_so,
    SV      = career$hist_sv
  )

  if ("fg_FIP"     %in% names(career)) display$FIP     <- career$fg_FIP
  if ("fg_xFIP"    %in% names(career)) display$xFIP    <- career$fg_xFIP
  if ("fg_WAR"     %in% names(career)) display$WAR     <- career$fg_WAR
  if ("fg_BABIP"   %in% names(career)) display$BABIP   <- career$fg_BABIP
  if ("fg_LOB_pct" %in% names(career)) display$`LOB%`  <- career$fg_LOB_pct
  if ("fg_k_pct"   %in% names(career)) display$`K%`    <- career$fg_k_pct
  if ("fg_bb_pct"  %in% names(career)) display$`BB%`   <- career$fg_bb_pct

  display %>%
    gt::gt(groupname_col = "Pitcher") %>%
    gt::tab_header(
      title    = "Starting Pitcher — Career History",
      subtitle = "Last 5 seasons + current  ·  totals aggregated across teams"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP", "BABIP")),
      decimals = 2
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("IP", "K9", "BB9", "K/BB", "WAR")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("G", "GS", "H", "HR", "BB", "K", "SV")),
      decimals = 0
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("LOB%", "K%", "BB%")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Volume",
      columns = dplyr::any_of(c("G", "GS", "IP", "H", "HR", "BB", "K", "SV"))
    ) %>%
    gt::tab_spanner(
      label   = "Rates",
      columns = dplyr::any_of(c("ERA", "WHIP", "K9", "BB9", "K/BB"))
    ) %>%
    gt::tab_spanner(
      label   = "Advanced",
      columns = dplyr::any_of(c("FIP", "xFIP", "WAR"))
    ) %>%
    gt::tab_spanner(
      label   = "Context",
      columns = dplyr::any_of(c("BABIP", "LOB%", "K%", "BB%"))
    ) %>%
    gt::tab_options(
      table.font.size            = 12,
      heading.align              = "left",
      data_row.padding           = gt::px(3),
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4fa"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Lineup Splits — vs today's starter handedness
# ------------------------------------------------------------

make_lineup_splits_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
    unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))
  season_label <- paste0(" · ", stats_yr, " season")

  raw <- lineup_context_splits %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot)

  if (nrow(raw) == 0) {
    return(
      dplyr::tibble(Note = "No split data available for this game.") %>%
        gt::gt() %>%
        gt::tab_header(title = gt::md(paste0("**", team, "** — Splits")))
    )
  }

  split_label <- if (length(unique(raw$split_label)) == 1)
    unique(raw$split_label) else "Matchup Split"

  # Derive starter handedness note for subtitle
  hand_note <- dplyr::case_when(
    split_label == "vs RHP" ~ "Opposing starter throws RHP — showing vs RHP splits",
    split_label == "vs LHP" ~ "Opposing starter throws LHP — showing vs LHP splits",
    TRUE                    ~ paste0("Showing ", split_label, " splits")
  )

  display <- dplyr::tibble(
    `#`    = raw$batting_slot,
    Name   = raw$player_name,
    Pos    = raw$fg_position
  )

  # Split stats (applicable to today's matchup)
  if ("sp_pa"    %in% names(raw)) display$PA    <- raw$sp_pa
  if ("sp_avg"   %in% names(raw)) display$AVG   <- raw$sp_avg
  if ("sp_obp"   %in% names(raw)) display$OBP   <- raw$sp_obp
  if ("sp_slg"   %in% names(raw)) display$SLG   <- raw$sp_slg
  if ("sp_ops"   %in% names(raw)) display$OPS   <- raw$sp_ops
  if ("sp_hr"    %in% names(raw)) display$HR    <- raw$sp_hr
  if ("sp_bb"    %in% names(raw)) display$BB    <- raw$sp_bb
  if ("sp_so"    %in% names(raw)) display$K     <- raw$sp_so
  if ("sp_babip" %in% names(raw)) display$BABIP <- raw$sp_babip

  # Overall for reference
  if ("mlb_ops"    %in% names(raw)) display$`OPS (Overall)` <- raw$mlb_ops
  if ("fg_wRC_plus" %in% names(raw)) display$`wRC+`         <- raw$fg_wRC_plus

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Matchup Splits")),
      subtitle = paste0(hand_note, season_label)
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS",
                                  "BABIP", "OPS (Overall)")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("PA", "HR", "BB", "K", "wRC+")),
      decimals = 0
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = split_label,
      columns = dplyr::any_of(c("PA", "AVG", "OBP", "SLG", "OPS",
                                 "HR", "BB", "K", "BABIP"))
    ) %>%
    gt::tab_spanner(
      label   = "Season",
      columns = dplyr::any_of(c("OPS (Overall)", "wRC+"))
    ) %>%
    # Highlight rows where split OPS > overall OPS (favorable matchup)
    gt::tab_style(
      style     = gt::cell_fill(color = "#eafaea"),
      locations = gt::cells_body(
        rows = !is.na(OPS) & !is.na(`OPS (Overall)`) & OPS > `OPS (Overall)`
      )
    ) %>%
    gt::cols_width(`#` ~ gt::px(28), Pos ~ gt::px(38)) %>%
    gt::tab_options(
      table.font.size  = 12,
      heading.align    = "left",
      data_row.padding = gt::px(4),
      column_labels.font.weight = "bold"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Starter Platoon Splits
# Shows how each starter performs vs RHH and vs LHH
# ------------------------------------------------------------

make_starter_splits_gt <- function(gpk) {
  raw <- starter_splits %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(side = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(side)

  if (nrow(raw) == 0 || !"ps_era_vr" %in% names(raw)) {
    return(invisible(NULL))
  }

  display <- dplyr::tibble(
    Side    = dplyr::if_else(raw$side == "away", "Away SP", "Home SP"),
    Pitcher = dplyr::coalesce(raw$pitcher_name, "TBD"),
    Hand    = dplyr::coalesce(raw$pitch_hand, "?")
  )

  if ("ps_ip_vr"   %in% names(raw)) display$`IP vs R`   <- raw$ps_ip_vr
  if ("ps_era_vr"  %in% names(raw)) display$`ERA vs R`  <- raw$ps_era_vr
  if ("ps_whip_vr" %in% names(raw)) display$`WHIP vs R` <- raw$ps_whip_vr
  if ("ps_ops_vr"  %in% names(raw)) display$`OPS vs R`  <- raw$ps_ops_vr
  if ("ps_so_vr"   %in% names(raw)) display$`K vs R`    <- raw$ps_so_vr
  if ("ps_ip_vl"   %in% names(raw)) display$`IP vs L`   <- raw$ps_ip_vl
  if ("ps_era_vl"  %in% names(raw)) display$`ERA vs L`  <- raw$ps_era_vl
  if ("ps_whip_vl" %in% names(raw)) display$`WHIP vs L` <- raw$ps_whip_vl
  if ("ps_ops_vl"  %in% names(raw)) display$`OPS vs L`  <- raw$ps_ops_vl
  if ("ps_so_vl"   %in% names(raw)) display$`K vs L`    <- raw$ps_so_vl

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Starting Pitcher — Platoon Splits",
      subtitle = paste0("Performance vs right-handed and left-handed batters · ",
                        if (exists("offense_master_season") && nrow(offense_master_season) > 0)
                          paste0(unique(offense_master_season$season)[1], " season")
                        else format(Sys.Date(), "%Y"))
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA vs R", "WHIP vs R", "OPS vs R",
                                  "ERA vs L", "WHIP vs L", "OPS vs L")),
      decimals = 2
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("IP vs R", "IP vs L")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("K vs R", "K vs L")),
      decimals = 0
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "vs RHH",
      columns = dplyr::any_of(c("IP vs R", "ERA vs R", "WHIP vs R",
                                 "OPS vs R", "K vs R"))
    ) %>%
    gt::tab_spanner(
      label   = "vs LHH",
      columns = dplyr::any_of(c("IP vs L", "ERA vs L", "WHIP vs L",
                                 "OPS vs L", "K vs L"))
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = "#eaf2ff"),
      locations = gt::cells_body(rows = Side == "Home SP")
    ) %>%
    gt::tab_options(
      table.font.size  = 13,
      heading.align    = "left",
      data_row.padding = gt::px(5)
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Pitch Arsenal — per-pitch-type breakdown for today's starters
# Rows: pitch types sorted by usage%. Grouped by pitcher.
# ------------------------------------------------------------

make_arsenal_gt <- function(gpk) {
  if (!exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0) {
    return(invisible(NULL))
  }

  starters <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(side = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(side) %>%
    dplyr::select(mlbam_id, pitcher_name, side, team_name)

  if (nrow(starters) == 0) return(invisible(NULL))

  data <- starters %>%
    dplyr::left_join(pitcher_arsenal, by = "mlbam_id") %>%
    dplyr::filter(!is.na(pitch_code)) %>%
    dplyr::mutate(
      Pitcher = paste0(pitcher_name, " (", toupper(side), " — ", team_name, ")")
    ) %>%
    dplyr::arrange(side, dplyr::desc(usage_pct))

  if (nrow(data) == 0) return(invisible(NULL))

  display <- dplyr::tibble(
    Pitcher  = data$Pitcher,
    Pitch    = data$pitch_name,
    `Usage%` = data$usage_pct,
    `Velo`   = data$avg_speed
  )

  if ("avg_spin"        %in% names(data) && any(!is.na(data$avg_spin)))
    display$Spin <- data$avg_spin
  if ("h_break"         %in% names(data) && any(!is.na(data$h_break)))
    display$`H.Break` <- data$h_break
  if ("v_break"         %in% names(data) && any(!is.na(data$v_break)))
    display$`V.Break` <- data$v_break
  if ("whiff_pct"       %in% names(data) && any(!is.na(data$whiff_pct)))
    display$`Whiff%` <- data$whiff_pct
  if ("put_away"        %in% names(data) && any(!is.na(data$put_away)))
    display$`PutAway%` <- data$put_away
  if ("hard_hit_pct"    %in% names(data) && any(!is.na(data$hard_hit_pct)))
    display$`HH%` <- data$hard_hit_pct
  if ("xba"             %in% names(data) && any(!is.na(data$xba)))
    display$xBA <- data$xba
  if ("xwoba"           %in% names(data) && any(!is.na(data$xwoba)))
    display$xwOBA <- data$xwoba
  if ("run_value_per100" %in% names(data) && any(!is.na(data$run_value_per100)))
    display$`RV/100` <- data$run_value_per100

  season_yr <- if ("season" %in% names(pitcher_arsenal))
    unique(pitcher_arsenal$season)[1] else ""

  tbl <- display %>%
    gt::gt(groupname_col = "Pitcher") %>%
    gt::tab_header(
      title    = "Starting Pitcher — Pitch Arsenal",
      subtitle = paste0("Pitch mix, velocity, movement, and outcomes · ", season_yr, " season")
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("Usage%", "Whiff%", "PutAway%", "HH%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Velo")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Spin")),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("H.Break", "V.Break")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("xBA", "xwOBA")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("RV/100")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Pitch Characteristics",
      columns = dplyr::any_of(c("Velo", "Spin", "H.Break", "V.Break"))
    ) %>%
    gt::tab_spanner(
      label   = "Outcomes",
      columns = dplyr::any_of(c("Whiff%", "PutAway%", "HH%", "xBA", "xwOBA", "RV/100"))
    ) %>%
    gt::tab_options(
      table.font.size            = 12,
      heading.align              = "left",
      data_row.padding           = gt::px(4),
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4fa"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")

  # data_color requires non-NA values — apply conditionally
  if ("Velo" %in% names(display) && any(!is.na(display$Velo)))
    tbl <- tbl %>% gt::data_color(columns = "Velo",
                                   palette  = c("#cfe2ff", "#084298"),
                                   na_color = "white")
  if ("Usage%" %in% names(display) && any(!is.na(display[["Usage%"]])))
    tbl <- tbl %>% gt::data_color(columns = "Usage%",
                                   palette  = c("#f8f9fa", "#343a40"),
                                   na_color = "white")
  if ("RV/100" %in% names(display) && any(!is.na(display[["RV/100"]])))
    tbl <- tbl %>% gt::data_color(columns = "RV/100",
                                   palette  = c("#27ae60", "#f8f9fa", "#e74c3c"),
                                   na_color = "white")

  tbl
}

# ------------------------------------------------------------
# Lineup Defense — Traditional + Advanced + Statcast
# Shows prior-season defensive metrics for each lineup player
# ------------------------------------------------------------

make_lineup_defense_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  lineup <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot)

  if (!exists("defense_master_season") || nrow(defense_master_season) == 0) {
    return(
      dplyr::tibble(Note = "Defense data not available.") %>%
        gt::gt() %>%
        gt::tab_header(title = gt::md(paste0("**", team, "** — Defense")))
    )
  }

  # Most recent season per player
  defense <- defense_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(season)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(
      mlbam_id,
      dplyr::any_of(c(
        "mlb_innings_fielding", "mlb_errors", "mlb_fielding_pct",
        "fg_UZR", "fg_UZR_150", "fg_DRS", "fg_Def",
        "sc_oaa", "sc_runs_prevented"
      ))
    )

  raw <- lineup %>%
    dplyr::left_join(defense, by = "mlbam_id", suffix = c("", "_def"))

  display <- dplyr::tibble(
    `#`  = raw$batting_slot,
    Name = raw$player_name,
    Pos  = raw$fg_position
  )

  if ("mlb_innings_fielding" %in% names(raw)) display$Inn    <- raw$mlb_innings_fielding
  if ("mlb_errors"           %in% names(raw)) display$E      <- raw$mlb_errors
  if ("mlb_fielding_pct"     %in% names(raw)) display$`Fld%` <- raw$mlb_fielding_pct
  if ("fg_UZR"               %in% names(raw)) display$UZR    <- raw$fg_UZR
  if ("fg_UZR_150"           %in% names(raw)) display$`UZR/150` <- raw$fg_UZR_150
  if ("fg_DRS"               %in% names(raw)) display$DRS    <- raw$fg_DRS
  if ("fg_Def"               %in% names(raw)) display$Def    <- raw$fg_Def
  if ("sc_oaa"               %in% names(raw)) display$OAA    <- raw$sc_oaa
  if ("sc_runs_prevented"    %in% names(raw)) display$`Runs+`<- raw$sc_runs_prevented

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Defense")),
      subtitle = "Prior season defensive metrics"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Inn")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Fld%")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("UZR", "UZR/150", "DRS", "Def", "OAA", "Runs+")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("E")),
      decimals = 0
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Traditional",
      columns = dplyr::any_of(c("Inn", "E", "Fld%"))
    ) %>%
    gt::tab_spanner(
      label   = "Advanced",
      columns = dplyr::any_of(c("UZR", "UZR/150", "DRS", "Def"))
    ) %>%
    gt::tab_spanner(
      label   = "Statcast",
      columns = dplyr::any_of(c("OAA", "Runs+"))
    ) %>%
    gt::cols_width(`#` ~ gt::px(28), Pos ~ gt::px(38)) %>%
    gt::tab_options(
      table.font.size           = 12,
      heading.align             = "left",
      data_row.padding          = gt::px(4),
      column_labels.font.weight = "bold"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Full Pitcher Breakdown — advanced sections for today's starters
# Returns a NAMED LIST of gt tables (2 bands):
#   1. Batted Ball Profile — GB%, LD%, FB%, IFFB%, HR/FB%, Hard%, Soft%
#   2. Pitch Discipline Against — K%, BB%, Chase%, Z-Swing%, SwStr%, CSW%
# Requires starter_matchup to contain fg_* batted ball / discipline cols
# (sourced from type 2 and type 5 FanGraphs extra pulls via 01_starter_matchup.R).
# ------------------------------------------------------------

make_pitcher_full_gt <- function(gpk) {
  raw <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(side = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(side)

  if (nrow(raw) == 0) return(list())

  mlb_yr <- if (exists("pitching_master_season") && nrow(pitching_master_season) > 0)
    unique(pitching_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))
  fg_yr <- if (exists("player_season_fg_pitching") && nrow(player_season_fg_pitching) > 0 &&
               "season" %in% names(player_season_fg_pitching))
    unique(player_season_fg_pitching$season)[1] else mlb_yr
  season_label <- if (!is.null(fg_yr) && !is.na(fg_yr) && fg_yr != mlb_yr)
    paste0(" \u00b7 ", fg_yr, " advanced stats")
  else
    paste0(" \u00b7 ", mlb_yr, " season")

  base_cols <- function() {
    dplyr::tibble(
      Side    = dplyr::if_else(raw$side == "away", "Away SP", "Home SP"),
      Pitcher = dplyr::coalesce(raw$pitcher_name, "TBD"),
      Hand    = dplyr::coalesce(raw$pitch_hand, "\u2014")
    )
  }

  add_col <- function(df, col_name, source_cols) {
    for (sc in source_cols) {
      if (sc %in% names(raw)) {
        df[[col_name]] <- raw[[sc]]
        return(df)
      }
    }
    df
  }

  base_gt_opts <- function(tbl) {
    tbl %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_style(
        style     = gt::cell_fill(color = "#eaf2ff"),
        locations = gt::cells_body(rows = Side == "Home SP")
      ) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(5),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")
  }

  # ── Section 1: BATTED BALL PROFILE ─────────────────────────
  d1 <- base_cols()
  d1 <- add_col(d1, "GB%",   c("fg_GB."))
  d1 <- add_col(d1, "LD%",   c("fg_LD."))
  d1 <- add_col(d1, "FB%",   c("fg_FB."))
  d1 <- add_col(d1, "IFFB%", c("fg_IFFB."))
  d1 <- add_col(d1, "HR/FB", c("fg_HR.FB"))
  d1 <- add_col(d1, "Hard%", c("fg_Hard."))
  d1 <- add_col(d1, "Med%",  c("fg_Med."))
  d1 <- add_col(d1, "Soft%", c("fg_Soft."))
  d1 <- add_col(d1, "BABIP", c("fg_BABIP"))
  d1 <- add_col(d1, "LOB%",  c("fg_LOB_pct"))

  t1 <- d1 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Starting Pitchers \u2014 Batted Ball Profile",
      subtitle = paste0("How batters make contact against each starter", season_label)
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%", "HR/FB",
                                  "Hard%", "Med%", "Soft%", "LOB%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("BABIP")),
      decimals = 3
    ) %>%
    gt::tab_spanner(
      label   = "Batted Ball Type",
      columns = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%", "HR/FB"))
    ) %>%
    gt::tab_spanner(
      label   = "Contact Quality",
      columns = dplyr::any_of(c("Hard%", "Med%", "Soft%"))
    ) %>%
    gt::tab_spanner(
      label   = "Luck / Strand",
      columns = dplyr::any_of(c("BABIP", "LOB%"))
    ) %>%
    base_gt_opts()

  # ── Section 2: PLATE DISCIPLINE AGAINST ─────────────────────
  d2 <- base_cols()
  d2 <- add_col(d2, "K%",       c("fg_K_pct"))
  d2 <- add_col(d2, "BB%",      c("fg_BB_pct"))
  d2 <- add_col(d2, "K-BB%",    c("fg_K_BB_pct"))
  d2 <- add_col(d2, "Chase%",   c("fg_O.Swing."))
  d2 <- add_col(d2, "Z-Swing%", c("fg_Z.Swing."))
  d2 <- add_col(d2, "Zone%",    c("fg_Zone."))
  d2 <- add_col(d2, "F-Str%",   c("fg_F.Strike."))
  d2 <- add_col(d2, "SwStr%",   c("fg_SwStr."))
  d2 <- add_col(d2, "CSW%",     c("fg_CSW."))

  t2 <- d2 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Starting Pitchers \u2014 Pitch Discipline",
      subtitle = paste0("Strikeout/walk outcomes \u00b7 how batters react to each starter", season_label)
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "K-BB%", "Chase%", "Z-Swing%",
                                  "Zone%", "F-Str%", "SwStr%", "CSW%")),
      decimals = 1
    ) %>%
    gt::tab_spanner(
      label   = "Outcomes",
      columns = dplyr::any_of(c("K%", "BB%", "K-BB%"))
    ) %>%
    gt::tab_spanner(
      label   = "Batter Reactions",
      columns = dplyr::any_of(c("Chase%", "Z-Swing%", "Zone%", "F-Str%"))
    ) %>%
    gt::tab_spanner(
      label   = "Miss Rate",
      columns = dplyr::any_of(c("SwStr%", "CSW%"))
    ) %>%
    base_gt_opts()

  # ── Section 3: STATCAST AGAINST ─────────────────────────────
  d3 <- base_cols()
  d3 <- add_col(d3, "Avg EV",  c("sc_avg_ev_allowed"))
  d3 <- add_col(d3, "EV95",    c("sc_ev95percent_allowed"))
  d3 <- add_col(d3, "Brl%",    c("sc_barrel_pct_allowed"))
  d3 <- add_col(d3, "xBA",     c("sc_xba_allowed"))
  d3 <- add_col(d3, "xSLG",    c("sc_xslg_allowed"))
  d3 <- add_col(d3, "xwOBA",   c("sc_xwoba_allowed"))
  d3 <- add_col(d3, "wOBA",    c("sc_woba_allowed"))
  d3 <- add_col(d3, "xERA",    c("sc_xera"))

  t3 <- d3 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Starting Pitchers \u2014 Statcast Against",
      subtitle = paste0("Exit velocity, barrel rate, and expected stats allowed", season_label)
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Avg EV", "EV95")),
      decimals = 1
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("Brl%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("xBA", "xSLG", "xwOBA", "wOBA")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("xERA")),
      decimals = 2
    ) %>%
    gt::tab_spanner(
      label   = "Exit Velocity",
      columns = dplyr::any_of(c("Avg EV", "EV95", "Brl%"))
    ) %>%
    gt::tab_spanner(
      label   = "Expected Stats Allowed",
      columns = dplyr::any_of(c("xBA", "xSLG", "xwOBA", "wOBA", "xERA"))
    ) %>%
    base_gt_opts()

  # data_color requires non-NA values to determine domain — apply conditionally
  if ("xwOBA" %in% names(d3) && any(!is.na(d3$xwOBA))) {
    t3 <- t3 %>% gt::data_color(
      columns = "xwOBA",
      palette = c("#27ae60", "#f8f9fa", "#e74c3c"),
      na_color = "white"
    )
  }
  if ("Avg EV" %in% names(d3) && any(!is.na(d3[["Avg EV"]]))) {
    t3 <- t3 %>% gt::data_color(
      columns = "Avg EV",
      palette = c("#27ae60", "#f8f9fa", "#e74c3c"),
      na_color = "white"
    )
  }

  tables <- list(
    "Batted Ball Profile" = list(d = d1, t = t1),
    "Pitch Discipline"    = list(d = d2, t = t2),
    "Statcast Against"    = list(d = d3, t = t3)
  )

  out <- lapply(tables, function(x) {
    if (ncol(x$d) > 3) x$t else NULL
  })
  Filter(Negate(is.null), out)
}

# ------------------------------------------------------------
# Season in Context — current vs prior season for lineup
# Grouped by player; rows = current season + last 2 prior seasons
# Uses offense_master_season for current and player_career_offense
# for historical seasons.
# ------------------------------------------------------------

make_season_context_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  lineup <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot) %>%
    dplyr::select(mlbam_id, batting_slot, player_name)

  if (nrow(lineup) == 0) return(invisible(NULL))

  if (!exists("offense_master_season") || nrow(offense_master_season) == 0) {
    return(invisible(NULL))
  }

  cur_yr <- unique(offense_master_season$season)[1]

  # Helper: normalize K%/BB% column names
  rename_rate <- function(df) {
    if (!"fg_K_pct" %in% names(df) && "fg_K." %in% names(df))
      df <- dplyr::rename(df, fg_K_pct = `fg_K.`)
    if (!"fg_BB_pct" %in% names(df) && "fg_BB." %in% names(df))
      df <- dplyr::rename(df, fg_BB_pct = `fg_BB.`)
    df
  }

  # ── Current season ─────────────────────────────────────────
  current <- offense_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, dplyr::any_of(c(
      "mlb_pa", "mlb_avg", "mlb_obp", "mlb_slg", "mlb_ops",
      "mlb_iso", "mlb_hr", "mlb_rbi", "mlb_bb", "mlb_so",
      "fg_K_pct", "fg_K.", "fg_BB_pct", "fg_BB.", "fg_wRC_plus", "fg_wOBA"
    ))) %>%
    rename_rate() %>%
    dplyr::mutate(season = as.character(cur_yr))

  cur_rows <- lineup %>%
    dplyr::left_join(current, by = "mlbam_id") %>%
    dplyr::filter(!is.na(season))

  # ── Historical seasons ──────────────────────────────────────
  if (!exists("player_career_offense") || nrow(player_career_offense) == 0) {
    hist_rows <- cur_rows[0, ]
  } else {
    hist_rows <- lineup %>%
      dplyr::left_join(
        player_career_offense %>%
          dplyr::filter(season < cur_yr) %>%
          dplyr::arrange(mlbam_id, dplyr::desc(season)) %>%
          dplyr::group_by(mlbam_id) %>%
          dplyr::slice_head(n = 2) %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            mlbam_id  = mlbam_id,
            season    = as.character(season),
            mlb_pa    = hist_pa,
            mlb_avg   = hist_avg,
            mlb_obp   = hist_obp,
            mlb_slg   = hist_slg,
            mlb_ops   = hist_ops,
            mlb_iso   = hist_iso,
            mlb_hr    = hist_hr,
            mlb_rbi   = hist_rbi,
            mlb_bb    = hist_bb,
            mlb_so    = hist_so,
            fg_K_pct  = hist_so  / pmax(hist_pa, 1),
            fg_BB_pct = hist_bb  / pmax(hist_pa, 1),
            fg_wRC_plus = if ("fg_wRC_plus" %in% names(player_career_offense)) fg_wRC_plus else NA_real_,
            fg_wOBA     = if ("fg_wOBA"     %in% names(player_career_offense)) fg_wOBA     else NA_real_
          ),
        by = "mlbam_id"
      ) %>%
      dplyr::filter(!is.na(season))
  }

  # ── Bind, keeping only shared columns ──────────────────────
  shared_cols <- intersect(names(cur_rows), names(hist_rows))
  if (length(shared_cols) < 3 || nrow(cur_rows) == 0) return(invisible(NULL))

  all_rows <- dplyr::bind_rows(
    cur_rows  %>% dplyr::select(dplyr::all_of(shared_cols)),
    hist_rows %>% dplyr::select(dplyr::all_of(shared_cols))
  ) %>%
    dplyr::arrange(batting_slot, dplyr::desc(as.integer(season)))

  if (nrow(all_rows) == 0) return(invisible(NULL))

  display <- dplyr::tibble(Name = all_rows$player_name, Season = all_rows$season)
  if ("mlb_pa"      %in% names(all_rows)) display$PA    <- as.integer(all_rows$mlb_pa)
  if ("mlb_avg"     %in% names(all_rows)) display$AVG   <- all_rows$mlb_avg
  if ("mlb_obp"     %in% names(all_rows)) display$OBP   <- all_rows$mlb_obp
  if ("mlb_slg"     %in% names(all_rows)) display$SLG   <- all_rows$mlb_slg
  if ("mlb_ops"     %in% names(all_rows)) display$OPS   <- all_rows$mlb_ops
  if ("mlb_iso"     %in% names(all_rows)) display$ISO   <- all_rows$mlb_iso
  if ("mlb_hr"      %in% names(all_rows)) display$HR    <- as.integer(all_rows$mlb_hr)
  if ("mlb_rbi"     %in% names(all_rows)) display$RBI   <- as.integer(all_rows$mlb_rbi)
  if ("fg_K_pct"    %in% names(all_rows)) display$`K%`  <- all_rows$fg_K_pct
  if ("fg_BB_pct"   %in% names(all_rows)) display$`BB%` <- all_rows$fg_BB_pct
  if ("fg_wRC_plus" %in% names(all_rows)) display$`wRC+` <- all_rows$fg_wRC_plus
  if ("fg_wOBA"     %in% names(all_rows)) display$wOBA  <- all_rows$fg_wOBA

  display %>%
    gt::gt(groupname_col = "Name") %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Season in Context")),
      subtitle = "Current season vs prior seasons \u00b7 rows newest first"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO", "wOBA")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("PA", "HR", "RBI", "wRC+")),
      decimals = 0
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
    gt::tab_style(
      style     = list(gt::cell_text(weight = "bold"),
                       gt::cell_fill(color = "#e8f4fd")),
      locations = gt::cells_body(rows = Season == as.character(cur_yr))
    ) %>%
    gt::tab_spanner(label = "Rate",
                    columns = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS", "ISO"))) %>%
    gt::tab_spanner(label = "Counting",
                    columns = dplyr::any_of(c("PA", "HR", "RBI"))) %>%
    gt::tab_spanner(label = "Advanced",
                    columns = dplyr::any_of(c("K%", "BB%", "wRC+", "wOBA"))) %>%
    gt::tab_options(
      table.font.size            = 12,
      heading.align              = "left",
      data_row.padding           = gt::px(3),
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4fa"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Bullpen Deep Dive — full season pitching stats for bullpen arms
# Includes FanGraphs advanced stats where available.
# ------------------------------------------------------------

make_bullpen_deep_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  grid <- bullpen_grid %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::filter(!fg_role %in% c("SP")) %>%
    dplyr::arrange(role_sort) %>%
    dplyr::select(mlbam_id, player_name, fg_role, availability)

  if (nrow(grid) == 0) return(invisible(NULL))

  if (exists("pitching_master_season") && nrow(pitching_master_season) > 0) {
    pit_stats <- pitching_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(c(
        "mlb_g", "mlb_ip", "mlb_era", "mlb_whip",
        "mlb_so", "mlb_bb", "mlb_sv", "mlb_hld",
        "fg_K_pct", "fg_BB_pct", "fg_K_BB_pct",
        "fg_FIP", "fg_xFIP", "fg_SIERA",
        "fg_BABIP", "fg_LOB_pct", "fg_WAR"
      )))
    raw <- grid %>% dplyr::left_join(pit_stats, by = "mlbam_id")
  } else {
    raw <- grid
  }

  stats_yr <- if (exists("pitching_master_season") && nrow(pitching_master_season) > 0)
    unique(pitching_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

  display <- dplyr::tibble(Role = raw$fg_role, Name = raw$player_name, Status = raw$availability)
  if ("mlb_g"       %in% names(raw)) display$G     <- as.integer(raw$mlb_g)
  if ("mlb_sv"      %in% names(raw)) display$SV    <- as.integer(raw$mlb_sv)
  if ("mlb_hld"     %in% names(raw)) display$HLD   <- as.integer(raw$mlb_hld)
  if ("mlb_ip"      %in% names(raw)) display$IP    <- raw$mlb_ip
  if ("mlb_era"     %in% names(raw)) display$ERA   <- raw$mlb_era
  if ("mlb_whip"    %in% names(raw)) display$WHIP  <- raw$mlb_whip
  if ("fg_K_pct"    %in% names(raw)) display$`K%`  <- raw$fg_K_pct
  if ("fg_BB_pct"   %in% names(raw)) display$`BB%` <- raw$fg_BB_pct
  if ("fg_K_BB_pct" %in% names(raw)) display$`K-BB%` <- raw$fg_K_BB_pct
  if ("fg_FIP"      %in% names(raw)) display$FIP   <- raw$fg_FIP
  if ("fg_xFIP"     %in% names(raw)) display$xFIP  <- raw$fg_xFIP
  if ("fg_SIERA"    %in% names(raw)) display$SIERA <- raw$fg_SIERA
  if ("fg_BABIP"    %in% names(raw)) display$BABIP <- raw$fg_BABIP
  if ("fg_LOB_pct"  %in% names(raw)) display$`LOB%` <- raw$fg_LOB_pct
  if ("fg_WAR"      %in% names(raw)) display$WAR   <- raw$fg_WAR

  tbl <- display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Bullpen Season Stats")),
      subtitle = paste0("Full season pitching stats for all bullpen arms \u00b7 ", stats_yr)
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP", "SIERA", "BABIP")),
      decimals = 2
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("IP", "WAR")), decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("G", "SV", "HLD")), decimals = 0) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "K-BB%", "LOB%")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
    gt::tab_spanner(label = "Volume",
                    columns = dplyr::any_of(c("G", "SV", "HLD", "IP"))) %>%
    gt::tab_spanner(label = "Results",
                    columns = dplyr::any_of(c("ERA", "WHIP"))) %>%
    gt::tab_spanner(label = "Estimators",
                    columns = dplyr::any_of(c("FIP", "xFIP", "SIERA"))) %>%
    gt::tab_spanner(label = "Discipline",
                    columns = dplyr::any_of(c("K%", "BB%", "K-BB%"))) %>%
    gt::tab_spanner(label = "Context",
                    columns = dplyr::any_of(c("BABIP", "LOB%", "WAR"))) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = avail_colors["fresh"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "fresh")
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = avail_colors["available"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "available")
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = avail_colors["limited"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "limited")
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = avail_colors["unavailable"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "unavailable")
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = avail_colors["injured"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "injured")
    ) %>%
    gt::tab_options(
      table.font.size           = 12,
      heading.align             = "left",
      data_row.padding          = gt::px(4),
      column_labels.font.weight = "bold"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")

  if ("ERA" %in% names(display) && any(!is.na(display$ERA)))
    tbl <- tbl %>% gt::data_color(
      columns  = "ERA",
      palette  = c("#27ae60", "#f8f9fa", "#e74c3c"),
      na_color = "white"
    )

  tbl
}

# ------------------------------------------------------------
# Bullpen Arsenal — pitch mix and characteristics for all relievers
# Grouped by pitcher, rows sorted by usage%.
# ------------------------------------------------------------

make_bullpen_arsenal_gt <- function(gpk, side_filter) {
  if (!exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0) {
    return(invisible(NULL))
  }

  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  grid <- bullpen_grid %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::filter(!fg_role %in% c("SP")) %>%
    dplyr::arrange(role_sort) %>%
    dplyr::select(mlbam_id, player_name, fg_role)

  if (nrow(grid) == 0) return(invisible(NULL))

  data <- grid %>%
    dplyr::left_join(pitcher_arsenal, by = "mlbam_id") %>%
    dplyr::filter(!is.na(pitch_code)) %>%
    dplyr::mutate(Pitcher = paste0(player_name, " (", fg_role, ")")) %>%
    dplyr::arrange(match(mlbam_id, grid$mlbam_id), dplyr::desc(usage_pct))

  if (nrow(data) == 0) return(invisible(NULL))

  season_yr <- if ("season" %in% names(pitcher_arsenal)) unique(pitcher_arsenal$season)[1] else ""

  display <- dplyr::tibble(
    Pitcher  = data$Pitcher,
    Pitch    = data$pitch_name,
    `Usage%` = data$usage_pct,
    Velo     = data$avg_speed
  )
  if ("avg_spin"         %in% names(data) && any(!is.na(data$avg_spin)))
    display$Spin <- data$avg_spin
  if ("h_break"          %in% names(data) && any(!is.na(data$h_break)))
    display$`H.Break` <- data$h_break
  if ("v_break"          %in% names(data) && any(!is.na(data$v_break)))
    display$`V.Break` <- data$v_break
  if ("whiff_pct"        %in% names(data) && any(!is.na(data$whiff_pct)))
    display$`Whiff%` <- data$whiff_pct
  if ("put_away"         %in% names(data) && any(!is.na(data$put_away)))
    display$`PutAway%` <- data$put_away
  if ("hard_hit_pct"     %in% names(data) && any(!is.na(data$hard_hit_pct)))
    display$`HH%` <- data$hard_hit_pct
  if ("xba"              %in% names(data) && any(!is.na(data$xba)))
    display$xBA <- data$xba
  if ("xwoba"            %in% names(data) && any(!is.na(data$xwoba)))
    display$xwOBA <- data$xwoba
  if ("run_value_per100" %in% names(data) && any(!is.na(data$run_value_per100)))
    display$`RV/100` <- data$run_value_per100

  tbl <- display %>%
    gt::gt(groupname_col = "Pitcher") %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Bullpen Pitch Arsenal")),
      subtitle = paste0("Pitch mix, velocity, movement, and outcomes \u00b7 ", season_yr, " season")
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("Usage%", "Whiff%", "PutAway%", "HH%")),
      decimals = 1
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("Velo")), decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("Spin")), decimals = 0) %>%
    gt::fmt_number(columns = dplyr::any_of(c("H.Break", "V.Break")), decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("xBA", "xwOBA")), decimals = 3) %>%
    gt::fmt_number(columns = dplyr::any_of(c("RV/100")), decimals = 1) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
    gt::tab_spanner(label = "Pitch Characteristics",
                    columns = dplyr::any_of(c("Velo", "Spin", "H.Break", "V.Break"))) %>%
    gt::tab_spanner(label = "Outcomes",
                    columns = dplyr::any_of(c("Whiff%", "PutAway%", "HH%", "xBA", "xwOBA", "RV/100"))) %>%
    gt::tab_options(
      table.font.size            = 12,
      heading.align              = "left",
      data_row.padding           = gt::px(4),
      column_labels.font.weight  = "bold",
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4fa"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")

  if ("Velo" %in% names(display) && any(!is.na(display$Velo)))
    tbl <- tbl %>% gt::data_color(columns = "Velo",
                                   palette  = c("#cfe2ff", "#084298"),
                                   na_color = "white")
  if ("Usage%" %in% names(display) && any(!is.na(display[["Usage%"]])))
    tbl <- tbl %>% gt::data_color(columns = "Usage%",
                                   palette  = c("#f8f9fa", "#343a40"),
                                   na_color = "white")
  if ("RV/100" %in% names(display) && any(!is.na(display[["RV/100"]])))
    tbl <- tbl %>% gt::data_color(columns = "RV/100",
                                   palette  = c("#27ae60", "#f8f9fa", "#e74c3c"),
                                   na_color = "white")

  tbl
}

# ------------------------------------------------------------
# Rotation — SP1-SP5 from FanGraphs depth chart + season stats
# Shows projected rotation order with full pitching metrics.
# ------------------------------------------------------------

make_rotation_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  if (!exists("depth_charts") || nrow(depth_charts) == 0) {
    return(invisible(NULL))
  }

  # Resolve team_abbr for this side
  team_id <- if (side_filter == "home") game$home_team_id else game$away_team_id
  t_abbr  <- tryCatch(
    team_ids %>% dplyr::filter(mlbam_team_id == team_id) %>%
      dplyr::pull(team_abbr) %>% dplyr::first(),
    error = function(e) NA_character_
  )
  if (is.na(t_abbr) || length(t_abbr) == 0) return(invisible(NULL))

  # SP1-SP5 from depth chart, sorted by rotation slot
  rotation <- depth_charts %>%
    dplyr::filter(
      team_abbr == t_abbr,
      stringr::str_starts(fg_role, "SP")
    ) %>%
    dplyr::mutate(
      slot_num = suppressWarnings(as.integer(stringr::str_extract(fg_role, "\\d+")))
    ) %>%
    dplyr::filter(!is.na(slot_num), slot_num <= 5) %>%
    dplyr::arrange(slot_num) %>%
    dplyr::select(mlbam_id, player_name, fg_role,
                  dplyr::any_of(c("proj_pit_ERA", "proj_pit_IP", "proj_pit_WAR")))

  if (nrow(rotation) == 0) return(invisible(NULL))

  # Join season stats
  if (exists("pitching_master_season") && nrow(pitching_master_season) > 0) {
    pit_stats <- pitching_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(c(
        "mlb_gs", "mlb_ip", "mlb_era", "mlb_whip",
        "fg_K_pct", "fg_BB_pct", "fg_FIP", "fg_xFIP", "fg_SIERA",
        "fg_BABIP", "fg_LOB_pct", "fg_WAR",
        "bbref_ERA_plus", "bbref_WAR"
      )))
    rotation <- rotation %>% dplyr::left_join(pit_stats, by = "mlbam_id")
  }

  stats_yr <- if (exists("pitching_master_season") && nrow(pitching_master_season) > 0)
    unique(pitching_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

  display <- dplyr::tibble(Slot = rotation$fg_role, Name = rotation$player_name)
  if ("mlb_gs"    %in% names(rotation)) display$GS   <- as.integer(rotation$mlb_gs)
  if ("mlb_ip"    %in% names(rotation)) display$IP   <- rotation$mlb_ip
  if ("mlb_era"   %in% names(rotation)) display$ERA  <- rotation$mlb_era
  if ("mlb_whip"  %in% names(rotation)) display$WHIP <- rotation$mlb_whip
  # ERA+ — prefer bbref_ERA_plus; derive from fg_ERA_minus if unavailable
  era_plus_rot <- dplyr::coalesce(
    if ("bbref_ERA_plus" %in% names(rotation)) rotation$bbref_ERA_plus else rep(NA_real_, nrow(rotation)),
    if ("fg_ERA_minus"   %in% names(rotation) && any(!is.na(rotation$fg_ERA_minus)))
      round(10000 / rotation$fg_ERA_minus) else rep(NA_real_, nrow(rotation))
  )
  era_plus_rot <- dplyr::if_else(is.finite(era_plus_rot), era_plus_rot, NA_real_)
  if (any(!is.na(era_plus_rot))) display$`ERA+` <- era_plus_rot

  # WAR — prefer fg_WAR (current season), fall back to bbref_WAR
  has_fwar <- "fg_WAR"    %in% names(rotation)
  has_bwar <- "bbref_WAR" %in% names(rotation)
  if (has_fwar || has_bwar) {
    war_vec <- dplyr::coalesce(
      if (has_fwar) rotation$fg_WAR    else rep(NA_real_, nrow(rotation)),
      if (has_bwar) rotation$bbref_WAR else rep(NA_real_, nrow(rotation))
    )
    war_col <- if (has_fwar && any(!is.na(rotation$fg_WAR))) "fWAR" else "bWAR"
    display[[war_col]] <- war_vec
  }

  if ("fg_K_pct"   %in% names(rotation)) display$`K%`  <- rotation$fg_K_pct
  if ("fg_BB_pct"  %in% names(rotation)) display$`BB%` <- rotation$fg_BB_pct
  if ("fg_FIP"     %in% names(rotation)) display$FIP   <- rotation$fg_FIP
  if ("fg_xFIP"    %in% names(rotation)) display$xFIP  <- rotation$fg_xFIP
  if ("fg_SIERA"   %in% names(rotation)) display$SIERA <- rotation$fg_SIERA
  if ("fg_BABIP"   %in% names(rotation)) display$BABIP <- rotation$fg_BABIP
  if ("fg_LOB_pct" %in% names(rotation)) display$`LOB%` <- rotation$fg_LOB_pct

  if ("proj_pit_ERA" %in% names(rotation) && any(!is.na(rotation$proj_pit_ERA)))
    display$`Proj ERA` <- rotation$proj_pit_ERA
  if ("proj_pit_IP"  %in% names(rotation) && any(!is.na(rotation$proj_pit_IP)))
    display$`Proj IP`  <- rotation$proj_pit_IP

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Projected Rotation")),
      subtitle = paste0("SP1\u2013SP5 from FanGraphs depth chart \u00b7 ",
                        stats_yr, " season stats")
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP",
                                              "SIERA", "BABIP", "Proj ERA")),
                   decimals = 2) %>%
    gt::fmt_number(columns = dplyr::any_of(c("IP", "fWAR", "bWAR",
                                              "Proj IP")), decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("GS", "ERA+")), decimals = 0) %>%
    gt::fmt_percent(columns = dplyr::any_of(c("K%", "BB%", "LOB%")),
                    decimals = 1) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
    gt::tab_spanner(label = "Volume",
                    columns = dplyr::any_of(c("GS", "IP"))) %>%
    gt::tab_spanner(label = "Results",
                    columns = dplyr::any_of(c("ERA", "ERA+", "WHIP"))) %>%
    gt::tab_spanner(label = "Estimators",
                    columns = dplyr::any_of(c("FIP", "xFIP", "SIERA"))) %>%
    gt::tab_spanner(label = "Discipline",
                    columns = dplyr::any_of(c("K%", "BB%"))) %>%
    gt::tab_spanner(label = "Context",
                    columns = dplyr::any_of(c("BABIP", "LOB%", "fWAR", "bWAR"))) %>%
    gt::tab_spanner(label = "Projection",
                    columns = dplyr::any_of(c("Proj ERA", "Proj IP"))) %>%
    gt::tab_options(
      table.font.size           = 12,
      heading.align             = "left",
      data_row.padding          = gt::px(4),
      column_labels.font.weight = "bold"
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}
