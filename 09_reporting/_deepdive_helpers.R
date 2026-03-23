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
#   make_lineup_splits_gt(gpk, side)    — matchup split table per side
#   make_starter_splits_gt(gpk)         — starter platoon splits
#   make_lineup_defense_gt(gpk, side)   — defense table (traditional + UZR/DRS + OAA)
# ============================================================

library(gt)
library(dplyr)

# ------------------------------------------------------------
# Full Lineup — "Nerd Heaven" analytical breakdown
# Returns a NAMED LIST of gt tables, one per analytical section:
#   1. Overall        — traditional rates + counting + advanced
#   2. Plate Approach — walk/strikeout/swing decisions
#   3. Contact Ability — contact rate, whiff rate
#   4. Contact Quality — exit velocity, barrel, expected stats
#   5. Batted Ball    — GB/LD/FB/direction profile
#   6. Value & Context — run value, speed, leverage/clutch
#
# The QMD caller iterates over the list and renders each table
# with its section label as a sub-heading.
# ------------------------------------------------------------

make_lineup_full_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

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
      subtitle = "Traditional rates, counting stats, and advanced value"
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

  # ── Section 2: PLATE APPROACH ───────────────────────────────
  d2 <- base_cols()
  d2 <- add_col(d2, "K%",       c("fg_K_pct",  "fg_K."))
  d2 <- add_col(d2, "BB%",      c("fg_BB_pct", "fg_BB."))
  d2 <- add_col(d2, "K-BB%",    c("fg_K.BB."))
  d2 <- add_col(d2, "Chase%",   c("fg_O.Swing."))
  d2 <- add_col(d2, "Z-Swing%", c("fg_Z.Swing."))
  d2 <- add_col(d2, "Zone%",    c("fg_Zone."))
  d2 <- add_col(d2, "F-Str%",   c("fg_F.Strike."))

  t2 <- d2 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Plate Approach")),
      subtitle = "How selective is he? Does he chase? Does he see strikes?"
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "K-BB%",
                                  "Chase%", "Z-Swing%", "Zone%", "F-Str%")),
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
    base_gt_opts()

  # ── Section 3: CONTACT ABILITY ──────────────────────────────
  d3 <- base_cols()
  d3 <- add_col(d3, "Contact%",   c("fg_Contact."))
  d3 <- add_col(d3, "Z-Con%",     c("fg_Z.Contact."))
  d3 <- add_col(d3, "O-Con%",     c("fg_O.Contact."))
  d3 <- add_col(d3, "SwStr%",     c("fg_SwStr."))
  d3 <- add_col(d3, "CSW%",       c("fg_CSW."))
  d3 <- add_col(d3, "CStr%",      c("fg_CStr."))

  t3 <- d3 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Contact Ability")),
      subtitle = "What happens when he swings? Called strikes + whiffs (CSW%) is a key pitcher metric to track"
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("Contact%", "Z-Con%", "O-Con%",
                                  "SwStr%", "CSW%", "CStr%")),
      decimals = 1
    ) %>%
    gt::tab_spanner(
      label   = "Contact Rate",
      columns = dplyr::any_of(c("Contact%", "Z-Con%", "O-Con%"))
    ) %>%
    gt::tab_spanner(
      label   = "Miss / Strike Rate",
      columns = dplyr::any_of(c("SwStr%", "CSW%", "CStr%"))
    ) %>%
    base_gt_opts()

  # ── Section 4: CONTACT QUALITY ──────────────────────────────
  d4 <- base_cols()
  d4 <- add_col(d4, "EV",    c("sc_avg_hit_speed"))
  d4 <- add_col(d4, "HH%",   c("sc_ev95percent"))
  d4 <- add_col(d4, "Brl%",  c("sc_brl_percent"))
  d4 <- add_col(d4, "Hard%", c("fg_Hard."))
  d4 <- add_col(d4, "Med%",  c("fg_Med."))
  d4 <- add_col(d4, "Soft%", c("fg_Soft."))
  d4 <- add_col(d4, "xBA",   c("sc_est_ba"))
  d4 <- add_col(d4, "xSLG",  c("sc_est_slg"))
  d4 <- add_col(d4, "xwOBA", c("sc_est_woba"))

  t4 <- d4 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Contact Quality")),
      subtitle = "When he makes contact, how hard does he hit it? Expected stats remove luck on balls in play"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("EV")),
      decimals = 1
    ) %>%
    # Statcast HH% and Brl% are stored as whole numbers (e.g. 42.1 not 0.421)
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
    base_gt_opts()

  # ── Section 5: BATTED BALL PROFILE ──────────────────────────
  d5 <- base_cols()
  d5 <- add_col(d5, "GB%",   c("fg_GB."))
  d5 <- add_col(d5, "LD%",   c("fg_LD."))
  d5 <- add_col(d5, "FB%",   c("fg_FB."))
  d5 <- add_col(d5, "IFFB%", c("fg_IFFB."))
  d5 <- add_col(d5, "HR/FB", c("fg_HR.FB"))
  d5 <- add_col(d5, "Pull%", c("fg_Pull."))
  d5 <- add_col(d5, "Cent%", c("fg_Cent."))
  d5 <- add_col(d5, "Oppo%", c("fg_Oppo."))

  t5 <- d5 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Batted Ball Profile")),
      subtitle = "Where does the ball go? IFFB% = infield pop-up rate; HR/FB = home run rate on fly balls"
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%",
                                  "HR/FB", "Pull%", "Cent%", "Oppo%")),
      decimals = 1
    ) %>%
    gt::tab_spanner(
      label   = "Type",
      columns = dplyr::any_of(c("GB%", "LD%", "FB%", "IFFB%", "HR/FB"))
    ) %>%
    gt::tab_spanner(
      label   = "Direction",
      columns = dplyr::any_of(c("Pull%", "Cent%", "Oppo%"))
    ) %>%
    base_gt_opts()

  # ── Section 6: VALUE & CONTEXT ───────────────────────────────
  d6 <- base_cols()
  d6 <- add_col(d6, "wRAA",   c("fg_wRAA"))
  d6 <- add_col(d6, "wRC",    c("fg_wRC"))
  d6 <- add_col(d6, "Spd",    c("fg_Spd"))
  d6 <- add_col(d6, "Sprint", c("sc_sprint_speed"))
  d6 <- add_col(d6, "WPA",    c("fg_WPA"))
  d6 <- add_col(d6, "RE24",   c("fg_RE24"))
  d6 <- add_col(d6, "Clutch", c("fg_Clutch"))
  d6 <- add_col(d6, "pLI",    c("fg_pLI"))

  t6 <- d6 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** — Value & Context")),
      subtitle = "Run value above average · speed · leverage and clutch performance"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("wRAA", "wRC", "WPA", "RE24", "Clutch")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("Spd", "Sprint")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("pLI")),
      decimals = 2
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

  # ── Return list — drop sections with no data columns ────────
  # (only `#`, Name, Pos means no stats landed for that section)
  tables <- list(
    "Overall"          = list(d = d1, t = t1),
    "Plate Approach"   = list(d = d2, t = t2),
    "Contact Ability"  = list(d = d3, t = t3),
    "Contact Quality"  = list(d = d4, t = t4),
    "Batted Ball"      = list(d = d5, t = t5),
    "Value & Context"  = list(d = d6, t = t6)
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
    AVG    = career_full$hist_avg,
    OBP    = career_full$hist_obp,
    SLG    = career_full$hist_slg,
    OPS    = career_full$hist_ops,
    HR     = career_full$hist_hr,
    RBI    = career_full$hist_rbi,
    BB     = career_full$hist_bb,
    K      = career_full$hist_so,
    SB     = career_full$hist_sb,
    ISO    = career_full$hist_iso,
    `BB%`  = career_full$hist_bb_pct,
    `K%`   = career_full$hist_k_pct
  )

  if ("fg_wRC_plus" %in% names(career_full)) display$`wRC+` <- career_full$fg_wRC_plus
  if ("fg_wOBA"    %in% names(career_full)) display$wOBA   <- career_full$fg_wOBA

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
      columns  = dplyr::any_of(c("PA", "HR", "RBI", "BB", "K", "SB", "wRC+")),
      decimals = 0
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("BB%", "K%")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Counting",
      columns = dplyr::any_of(c("PA", "HR", "RBI", "BB", "K", "SB"))
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
    Side    = dplyr::if_else(career$side == "away", "Away SP", "Home SP"),
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

  if ("fg_FIP"  %in% names(career)) display$FIP  <- career$fg_FIP
  if ("fg_xFIP" %in% names(career)) display$xFIP <- career$fg_xFIP
  if ("fg_WAR"  %in% names(career)) display$WAR  <- career$fg_WAR

  display %>%
    gt::gt(groupname_col = "Pitcher") %>%
    gt::tab_header(
      title    = "Starting Pitcher — Career History",
      subtitle = "Last 5 seasons + current  ·  totals aggregated across teams"
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP")),
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
    gt::tab_style(
      style     = gt::cell_fill(color = "#eaf2ff"),
      locations = gt::cells_body(rows = Side == "Home SP")
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
      subtitle = hand_note
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
      subtitle = "Performance vs right-handed and left-handed batters"
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
