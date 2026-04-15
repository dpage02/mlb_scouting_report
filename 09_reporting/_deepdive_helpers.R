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
        "mlb_pa", "mlb_avg", "mlb_obp", "mlb_slg", "mlb_ops",
        "fg_wRC_plus", "mlb_hr", "mlb_rbi",
        "fg_K_pct", "fg_BB_pct", "fg_K.", "fg_BB.",
        "fg_O.Swing.", "fg_SwStr.", "fg_CSW.",
        "sc_avg_hit_speed", "sc_brl_percent", "sc_ev95percent",
        "sc_est_ba", "sc_est_slg", "sc_est_woba",
        "fg_GB.", "fg_LD.", "fg_FB.", "fg_HR.FB",
        "fg_Pull.", "fg_Cent.", "fg_Oppo.",
        "sc_sprint_speed"
      ))
    )

  raw <- raw %>%
    dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
    dplyr::select(-dplyr::ends_with("_dup"))

  # ── Helpers ─────────────────────────────────────────────────

  add_col <- function(df, col_name, source_cols) {
    for (sc in source_cols) {
      if (sc %in% names(raw)) { df[[col_name]] <- raw[[sc]]; return(df) }
    }
    df
  }

  base_gt_opts <- function(tbl, has_pos = TRUE) {
    tbl %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      {
        if (has_pos)
          gt::cols_width(., `#` ~ gt::px(28), Pos ~ gt::px(38))
        else
          gt::cols_width(., `#` ~ gt::px(28))
      } %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")
  }

  # ── Table A: OVERVIEW ────────────────────────────────────────
  # Core batting line + advanced value. Pos shown here only.
  dA <- dplyr::tibble(`#` = raw$batting_slot, Name = raw$player_name, Pos = raw$fg_position)
  dA <- add_col(dA, "PA",   c("mlb_pa"))
  dA <- add_col(dA, "AVG",  c("mlb_avg"))
  dA <- add_col(dA, "OBP",  c("mlb_obp"))
  dA <- add_col(dA, "SLG",  c("mlb_slg"))
  dA <- add_col(dA, "OPS",  c("mlb_ops"))
  dA <- add_col(dA, "wRC+", c("fg_wRC_plus"))
  dA <- add_col(dA, "HR",   c("mlb_hr"))
  dA <- add_col(dA, "RBI",  c("mlb_rbi"))

  tA <- dA %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Overview")),
      subtitle = paste0("Core production", season_label)
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS")), decimals = 3) %>%
    gt::fmt_number(columns = dplyr::any_of(c("wRC+", "PA", "HR", "RBI")), decimals = 0) %>%
    gt::tab_spanner(label = "Batting Line", columns = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS"))) %>%
    gt::tab_spanner(label = "Value",        columns = dplyr::any_of(c("wRC+", "HR", "RBI", "PA"))) %>%
    base_gt_opts(has_pos = TRUE)

  # ── Table B: ADVANCED ────────────────────────────────────────
  # Plate discipline + batted ball profile.
  dB <- dplyr::tibble(`#` = raw$batting_slot, Name = raw$player_name)
  dB <- add_col(dB, "K%",     c("fg_K_pct",  "fg_K."))
  dB <- add_col(dB, "BB%",    c("fg_BB_pct", "fg_BB."))
  dB <- add_col(dB, "Chase%", c("fg_O.Swing."))
  dB <- add_col(dB, "SwStr%", c("fg_SwStr."))
  dB <- add_col(dB, "CSW%",   c("fg_CSW."))
  dB <- add_col(dB, "GB%",    c("fg_GB."))
  dB <- add_col(dB, "LD%",    c("fg_LD."))
  dB <- add_col(dB, "FB%",    c("fg_FB."))
  dB <- add_col(dB, "HR/FB",  c("fg_HR.FB"))
  dB <- add_col(dB, "Pull%",  c("fg_Pull."))
  dB <- add_col(dB, "Cent%",  c("fg_Cent."))
  dB <- add_col(dB, "Oppo%",  c("fg_Oppo."))

  tB <- dB %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Advanced")),
      subtitle = paste0("Plate discipline \u00b7 batted ball profile", season_label)
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "Chase%", "SwStr%", "CSW%",
                                  "GB%", "LD%", "FB%", "HR/FB",
                                  "Pull%", "Cent%", "Oppo%")),
      decimals = 1
    ) %>%
    gt::tab_spanner(label = "Discipline",  columns = dplyr::any_of(c("K%", "BB%", "Chase%", "SwStr%", "CSW%"))) %>%
    gt::tab_spanner(label = "Batted Ball", columns = dplyr::any_of(c("GB%", "LD%", "FB%", "HR/FB"))) %>%
    gt::tab_spanner(label = "Direction",   columns = dplyr::any_of(c("Pull%", "Cent%", "Oppo%"))) %>%
    base_gt_opts(has_pos = FALSE)

  # ── Table C: STAT NERD ───────────────────────────────────────
  # Statcast contact quality + expected stats + sprint speed.
  dC <- dplyr::tibble(`#` = raw$batting_slot, Name = raw$player_name)
  dC <- add_col(dC, "EV",     c("sc_avg_hit_speed"))
  dC <- add_col(dC, "HH%",    c("sc_ev95percent"))
  dC <- add_col(dC, "Brl%",   c("sc_brl_percent"))
  dC <- add_col(dC, "xBA",    c("sc_est_ba"))
  dC <- add_col(dC, "xSLG",   c("sc_est_slg"))
  dC <- add_col(dC, "xwOBA",  c("sc_est_woba"))
  dC <- add_col(dC, "Sprint", c("sc_sprint_speed"))

  tC <- dC %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Statcast")),
      subtitle = paste0("Contact quality \u00b7 expected stats \u00b7 sprint speed", season_label)
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("EV")), decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("HH%", "Brl%")), decimals = 1) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = dplyr::any_of(c("HH%", "Brl%"))),
      fn = function(x) dplyr::if_else(x == "\u2014", "\u2014", paste0(x, "%"))
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("xBA", "xSLG", "xwOBA")), decimals = 3) %>%
    gt::fmt_number(columns = dplyr::any_of(c("Sprint")), decimals = 1) %>%
    gt::tab_spanner(label = "Contact Quality", columns = dplyr::any_of(c("EV", "HH%", "Brl%"))) %>%
    gt::tab_spanner(label = "Expected",        columns = dplyr::any_of(c("xBA", "xSLG", "xwOBA"))) %>%
    gt::tab_spanner(label = "Speed",           columns = dplyr::any_of(c("Sprint"))) %>%
    base_gt_opts(has_pos = FALSE)

  # ── Return — drop tables where no stat columns landed ────────
  out <- list()
  if (ncol(dA) > 3) out[["Overview"]]   <- tA
  if (ncol(dB) > 2) out[["Advanced"]]   <- tB
  if (ncol(dC) > 2) out[["Stat Nerd"]]  <- tC
  out
}

# ------------------------------------------------------------
# Bootstrap 5 tab wrapper for the three-tier lineup view
# uid must be unique per team per page (e.g. paste0(gpk, side))
# ------------------------------------------------------------

make_lineup_tabset_html <- function(tables, uid) {
  tab_names <- names(tables)
  if (length(tab_names) == 0) return("")

  slugify <- function(nm) gsub("[^a-z0-9]", "", tolower(nm))

  # Nav buttons
  btns <- vapply(seq_along(tab_names), function(i) {
    nm   <- tab_names[[i]]
    slug <- slugify(nm)
    active   <- if (i == 1) " active"  else ""
    selected <- if (i == 1) "true" else "false"
    sprintf(
      '<li class="nav-item" role="presentation"><button class="nav-link%s" id="%s-%s-tab" data-bs-toggle="tab" data-bs-target="#%s-%s" type="button" role="tab" aria-controls="%s-%s" aria-selected="%s" style="font-size:0.82rem;padding:5px 14px;">%s</button></li>',
      active, slug, uid, slug, uid, slug, uid, selected, nm
    )
  }, character(1))

  # Pane content
  panes <- vapply(seq_along(tab_names), function(i) {
    nm   <- tab_names[[i]]
    slug <- slugify(nm)
    show <- if (i == 1) " show active" else ""
    sprintf(
      '<div class="tab-pane fade%s" id="%s-%s" role="tabpanel" aria-labelledby="%s-%s-tab" style="padding-top:10px;">%s</div>',
      show, slug, uid, slug, uid, gt::as_raw_html(tables[[nm]])
    )
  }, character(1))

  paste0(
    '<ul class="nav nav-tabs" id="lineup-', uid, '" role="tablist" style="margin-bottom:0;">',
    paste(btns,  collapse = ""),
    '</ul>',
    '<div class="tab-content" id="lineup-', uid, '-content">',
    paste(panes, collapse = ""),
    '</div>'
  )
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
        gt::tab_header(title = gt::md(paste0("**", team, "** \u2014 Defense")))
    )
  }

  # Prior-season metrics (most recent completed season per player)
  prior_yr <- max(defense_master_season$season, na.rm = TRUE)
  prior_def <- defense_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(season)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(
      mlbam_id,
      dplyr::any_of(c(
        "fg_DRS", "fg_UZR", "fg_UZR.150", "fg_UZR_150",  # both dot and underscore variants
        "fg_ARM", "sc_oaa", "sc_runs_prevented"
      ))
    ) %>%
    # Normalise UZR/150 column name — FanGraphs uses dot, we may have either
    {
      df <- .
      if (!"fg_UZR.150" %in% names(df) && "fg_UZR_150" %in% names(df))
        dplyr::rename(df, `fg_UZR.150` = fg_UZR_150)
      else df
    }

  # Current-season metrics (small sample, directional signal)
  cur_def <- if (exists("current_defense_stats") && nrow(current_defense_stats) > 0) {
    current_defense_stats %>%
      dplyr::filter(mlbam_id %in% lineup$mlbam_id) %>%
      dplyr::select(mlbam_id,
                    dplyr::any_of(c("cur_inn", "cur_errors", "cur_fld_pct", "cur_oaa")))
  } else {
    dplyr::tibble(mlbam_id = integer())
  }

  raw <- lineup %>%
    dplyr::left_join(prior_def, by = "mlbam_id", suffix = c("", "_p")) %>%
    dplyr::left_join(cur_def,   by = "mlbam_id", suffix = c("", "_c"))

  cur_yr    <- as.integer(format(Sys.Date(), "%Y"))
  prior_lbl <- paste0(prior_yr, " Season")
  cur_lbl   <- paste0(cur_yr, " (early)")

  display <- dplyr::tibble(
    `#`  = raw$batting_slot,
    Name = raw$player_name,
    Pos  = raw$fg_position
  )

  # Prior season — advanced metrics
  if ("fg_DRS"     %in% names(raw)) display$DRS     <- raw$fg_DRS
  if ("fg_UZR"     %in% names(raw)) display$UZR     <- raw$fg_UZR
  if ("fg_UZR.150" %in% names(raw)) display$`UZR/150` <- raw$`fg_UZR.150`
  if ("fg_ARM"     %in% names(raw)) display$ARM     <- raw$fg_ARM
  if ("sc_oaa"     %in% names(raw)) display$OAA     <- raw$sc_oaa

  # Current season — traditional + early OAA
  if ("cur_inn"     %in% names(raw)) display$Inn     <- raw$cur_inn
  if ("cur_errors"  %in% names(raw)) display$E       <- raw$cur_errors
  if ("cur_fld_pct" %in% names(raw)) display$`Fld%`  <- raw$cur_fld_pct
  if ("cur_oaa"     %in% names(raw)) display$`OAA*`  <- raw$cur_oaa

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "** \u2014 Defense")),
      subtitle = paste0(prior_lbl, " advanced metrics \u00b7 ", cur_lbl, " fielding (*OAA small sample)")
    ) %>%
    gt::fmt_number(columns = dplyr::any_of(c("Inn")),             decimals = 1) %>%
    gt::fmt_number(columns = dplyr::any_of(c("Fld%")),            decimals = 3) %>%
    gt::fmt_number(columns = dplyr::any_of(c("E")),               decimals = 0) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("DRS", "UZR", "UZR/150", "ARM", "OAA", "OAA*")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
    gt::tab_spanner(
      label   = prior_lbl,
      columns = dplyr::any_of(c("DRS", "UZR", "UZR/150", "ARM", "OAA"))
    ) %>%
    gt::tab_spanner(
      label   = cur_lbl,
      columns = dplyr::any_of(c("Inn", "E", "Fld%", "OAA*"))
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

  # ── Section 1: COMBINED PROFILE (Outcomes + Batted Ball + Luck) ──
  d1 <- base_cols()
  # Strikeout / walk outcomes
  d1 <- add_col(d1, "K%",       c("fg_K_pct"))
  d1 <- add_col(d1, "BB%",      c("fg_BB_pct"))
  d1 <- add_col(d1, "K-BB%",    c("fg_K_BB_pct"))
  d1 <- add_col(d1, "SwStr%",   c("fg_SwStr_pct",  "fg_SwStr."))
  d1 <- add_col(d1, "CSW%",     c("fg_C_plusSwStr_pct", "fg_CSW."))
  # Batter swing decisions
  d1 <- add_col(d1, "Chase%",   c("fg_O_Swing_pct", "fg_O.Swing."))
  d1 <- add_col(d1, "Z-Swing%", c("fg_Z_Swing_pct", "fg_Z.Swing."))
  d1 <- add_col(d1, "Zone%",    c("fg_Zone_pct",    "fg_Zone."))
  # Batted ball shape
  d1 <- add_col(d1, "GB%",      c("fg_GB_pct",      "fg_GB."))
  d1 <- add_col(d1, "LD%",      c("fg_LD_pct",      "fg_LD."))
  d1 <- add_col(d1, "FB%",      c("fg_FB_pct",      "fg_FB."))
  d1 <- add_col(d1, "HR/FB",    c("fg_HR_per_FB",   "fg_HR.FB"))
  d1 <- add_col(d1, "Hard%",    c("fg_Hard_pct",    "fg_Hard."))
  # Luck / strand
  d1 <- add_col(d1, "BABIP",    c("fg_BABIP"))
  d1 <- add_col(d1, "LOB%",     c("fg_LOB_pct"))

  t1 <- d1 %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Starting Pitchers \u2014 Profile",
      subtitle = paste0("Strikeout/walk outcomes \u00b7 batter reactions \u00b7 batted ball shape", season_label)
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("K%", "BB%", "K-BB%", "SwStr%", "CSW%",
                                  "Chase%", "Z-Swing%", "Zone%",
                                  "GB%", "LD%", "FB%", "HR/FB", "Hard%", "LOB%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("BABIP")),
      decimals = 3
    ) %>%
    gt::tab_spanner(
      label   = "Outcomes",
      columns = dplyr::any_of(c("K%", "BB%", "K-BB%", "SwStr%", "CSW%"))
    ) %>%
    gt::tab_spanner(
      label   = "Batter Reactions",
      columns = dplyr::any_of(c("Chase%", "Z-Swing%", "Zone%"))
    ) %>%
    gt::tab_spanner(
      label   = "Batted Ball",
      columns = dplyr::any_of(c("GB%", "LD%", "FB%", "HR/FB", "Hard%"))
    ) %>%
    gt::tab_spanner(
      label   = "Luck / Strand",
      columns = dplyr::any_of(c("BABIP", "LOB%"))
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
    gt::fmt_number(
      columns  = dplyr::any_of(c("Brl%")),
      decimals = 1
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = dplyr::any_of(c("Brl%"))),
      fn = function(x) dplyr::if_else(x == "\u2014", "\u2014", paste0(x, "%"))
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
    "Pitcher Profile"  = list(d = d1, t = t1),
    "Statcast Against" = list(d = d3, t = t3)
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
      style     = gt::cell_fill(color = avail_colors["doubtful"], alpha = 0.85),
      locations = gt::cells_body(columns = "Status", rows = Status == "doubtful")
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

# ------------------------------------------------------------
# Pitcher Matchup — SP arsenal vs opposing lineup splits
#
# Returns an HTML string (two panels per direction):
#   Left  — SP's pitch arsenal (pitch type, usage%, velo, whiff%, xwOBA, RV/100)
#   Right — Opposing lineup's batting splits vs SP's handedness (AVG/OBP/SLG/OPS/HR/K/wRC+)
#
# Streak badges shown in the batter table:
#   🔥 hot bat (last-7 OPS ≥ .900)  ⚡ 5+ game hit streak  ❄ cold bat
# ------------------------------------------------------------

make_pitcher_matchup_html <- function(gpk) {

  game_row <- game_context %>% dplyr::filter(game_pk == gpk)
  if (nrow(game_row) == 0) return('<p style="color:#888;">Game not found.</p>')

  starters <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(.side_f = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(.side_f)

  if (nrow(starters) == 0) {
    return('<p style="color:#888; font-style:italic;">Starter data not available.</p>')
  }

  # ── Sub-helper: per-SP arsenal gt ───────────────────────────
  .sp_arsenal_tbl <- function(sp_id, sp_name) {
    if (!exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0) return(NULL)
    ars <- dplyr::tibble(mlbam_id = as.integer(sp_id)) %>%
      dplyr::left_join(pitcher_arsenal, by = "mlbam_id") %>%
      dplyr::filter(!is.na(pitch_code)) %>%
      dplyr::arrange(dplyr::desc(usage_pct))
    if (nrow(ars) == 0) return(NULL)

    season_yr <- if ("season" %in% names(ars)) ars$season[1] else ""

    disp <- dplyr::tibble(
      Pitch  = ars$pitch_name,
      `Use%` = ars$usage_pct,
      Velo   = ars$avg_speed
    )
    if ("whiff_pct"        %in% names(ars) && any(!is.na(ars$whiff_pct)))
      disp$`Whiff%` <- ars$whiff_pct
    if ("put_away"         %in% names(ars) && any(!is.na(ars$put_away)))
      disp$`Put%`   <- ars$put_away
    if ("xba"              %in% names(ars) && any(!is.na(ars$xba)))
      disp$xBA      <- ars$xba
    if ("xwoba"            %in% names(ars) && any(!is.na(ars$xwoba)))
      disp$xwOBA    <- ars$xwoba
    if ("run_value_per100" %in% names(ars) && any(!is.na(ars$run_value_per100)))
      disp$`RV/100` <- ars$run_value_per100

    tbl <- disp %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", sp_name, "** \u2014 Arsenal")),
        subtitle = paste0(season_yr, " season \u00b7 sorted by usage")
      ) %>%
      gt::fmt_percent(
        columns  = dplyr::any_of(c("Use%", "Whiff%", "Put%")),
        decimals = 1
      ) %>%
      gt::fmt_number(columns = dplyr::any_of(c("Velo")),   decimals = 1) %>%
      gt::fmt_number(columns = dplyr::any_of(c("xBA", "xwOBA")), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("RV/100")), decimals = 1) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Pitch",
        id      = paste0("arsenal_pitch_", gsub("[^A-Za-z0-9]", "_", sp_name)),
        columns = dplyr::any_of(c("Velo", "Use%", "Whiff%", "Put%"))
      ) %>%
      gt::tab_spanner(
        label   = "Outcomes",
        id      = paste0("arsenal_outcomes_", gsub("[^A-Za-z0-9]", "_", sp_name)),
        columns = dplyr::any_of(c("xBA", "xwOBA", "RV/100"))
      ) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    if ("RV/100" %in% names(disp) && any(!is.na(disp[["RV/100"]])))
      tbl <- tbl %>% gt::data_color(
        columns  = "RV/100",
        palette  = c("#27ae60", "#f8f9fa", "#e74c3c"),
        na_color = "white"
      )
    tbl
  }

  # ── Sub-helper: opposing lineup vs SP handedness gt ─────────
  .batters_vs_hand_tbl <- function(bat_side, sp_hand, sp_name) {
    split_code_val <- if (toupper(dplyr::coalesce(sp_hand, "R")) == "L") "vl" else "vr"
    hand_label     <- if (split_code_val == "vl") "vs LHP" else "vs RHP"

    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == bat_side) %>%
      dplyr::arrange(batting_slot)
    if (nrow(lineup) == 0) return(NULL)

    bat_team <- if (bat_side == "home") game_row$home_team_name[1] else game_row$away_team_name[1]

    # Splits vs SP handedness
    splits_data <- if (exists("player_season_mlb_offense_splits") &&
                       nrow(player_season_mlb_offense_splits) > 0) {
      player_season_mlb_offense_splits %>%
        dplyr::filter(split_code == split_code_val,
                      mlbam_id  %in% lineup$mlbam_id) %>%
        dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id,
                      dplyr::any_of(c("mlb_pa", "mlb_avg", "mlb_obp",
                                      "mlb_slg", "mlb_ops", "mlb_hr", "mlb_so")))
    } else {
      dplyr::tibble(mlbam_id = integer(0))
    }

    # Overall wRC+, SwStr%, CSW% (splits don't carry these)
    wrc_data <- if (exists("offense_master_season") && nrow(offense_master_season) > 0) {
      offense_master_season %>%
        dplyr::filter(mlbam_id %in% lineup$mlbam_id) %>%
        dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, dplyr::any_of(c("fg_wRC_plus", "fg_SwStr.", "fg_CSW.")))
    } else {
      dplyr::tibble(mlbam_id = integer(0))
    }

    # Recent streak badges
    streak_data <- if (exists("recent_batter_streaks") &&
                       nrow(recent_batter_streaks) > 0) {
      recent_batter_streaks %>%
        dplyr::filter(mlbam_id %in% lineup$mlbam_id) %>%
        dplyr::select(mlbam_id,
                      dplyr::any_of(c("hit_streak", "is_hot", "is_cold")))
    } else {
      dplyr::tibble(mlbam_id = integer(0))
    }

    base <- lineup %>%
      dplyr::select(mlbam_id, batting_slot, player_name, fg_position) %>%
      dplyr::left_join(splits_data, by = "mlbam_id") %>%
      dplyr::left_join(wrc_data,   by = "mlbam_id") %>%
      dplyr::left_join(streak_data, by = "mlbam_id")

    # Streak badge
    get_flag <- function(is_hot, is_cold, hit_streak) {
      hot  <- isTRUE(is_hot)
      cold <- isTRUE(is_cold)
      streak_ok <- !is.na(hit_streak) && hit_streak >= 5L
      dplyr::case_when(
        hot  & streak_ok ~ "\U1F525\u26a1",
        hot              ~ "\U1F525",
        streak_ok        ~ "\u26a1",
        cold             ~ "\u2744",
        TRUE             ~ ""
      )
    }

    hs_vec   <- if ("hit_streak" %in% names(base)) base$hit_streak else rep(NA_integer_, nrow(base))
    hot_vec  <- if ("is_hot"     %in% names(base)) base$is_hot     else rep(FALSE, nrow(base))
    cold_vec <- if ("is_cold"    %in% names(base)) base$is_cold    else rep(FALSE, nrow(base))

    disp <- dplyr::tibble(
      `#`  = base$batting_slot,
      Name = base$player_name,
      Pos  = base$fg_position,
      Flag = mapply(get_flag, hot_vec, cold_vec, hs_vec, USE.NAMES = FALSE)
    )

    if ("mlb_pa"      %in% names(base)) disp$PA     <- base$mlb_pa
    if ("mlb_avg"     %in% names(base)) disp$AVG    <- base$mlb_avg
    if ("mlb_obp"     %in% names(base)) disp$OBP    <- base$mlb_obp
    if ("mlb_slg"     %in% names(base)) disp$SLG    <- base$mlb_slg
    if ("mlb_ops"     %in% names(base)) disp$OPS    <- base$mlb_ops
    if ("mlb_hr"      %in% names(base)) disp$HR     <- base$mlb_hr
    if ("mlb_so"      %in% names(base)) disp$K      <- base$mlb_so
    if ("fg_wRC_plus" %in% names(base)) disp$`wRC+` <- base$fg_wRC_plus
    if ("fg_SwStr."   %in% names(base)) disp$`SwStr%` <- base$fg_SwStr.
    if ("fg_CSW."     %in% names(base)) disp$`CSW%`   <- base$fg_CSW.

    disp %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", bat_team, "** \u2014 ", hand_label)),
        subtitle = paste0("Batting splits vs ", sp_name)
      ) %>%
      gt::fmt_number(
        columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS")),
        decimals = 3
      ) %>%
      gt::fmt_number(
        columns  = dplyr::any_of(c("PA", "HR", "K", "wRC+")),
        decimals = 0
      ) %>%
      gt::fmt_percent(
        columns  = dplyr::any_of(c("SwStr%", "CSW%")),
        decimals = 1
      ) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::cols_width(
        `#`  ~ gt::px(28),
        Pos  ~ gt::px(38),
        Flag ~ gt::px(30)
      ) %>%
      gt::tab_spanner(
        label   = "Split Stats",
        columns = dplyr::any_of(c("PA", "AVG", "OBP", "SLG", "OPS", "HR", "K", "wRC+"))
      ) %>%
      gt::tab_spanner(
        label   = "Approach",
        columns = dplyr::any_of(c("SwStr%", "CSW%"))
      ) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")
  }

  # ── Build one HTML block per matchup direction ───────────────
  sections <- lapply(seq_len(nrow(starters)), function(i) {
    sp      <- starters[i, ]
    bat_side <- if (sp$side == "away") "home" else "away"
    sp_hand  <- dplyr::coalesce(sp$pitch_hand, "R")
    hand_str <- if (toupper(sp_hand) == "L") "LHP" else "RHP"
    sp_team  <- if (sp$side == "away") game_row$away_team_name[1] else game_row$home_team_name[1]
    bat_team <- if (bat_side == "home") game_row$home_team_name[1] else game_row$away_team_name[1]

    ars_tbl <- tryCatch(.sp_arsenal_tbl(sp$mlbam_id, sp$pitcher_name),
                        error = function(e) NULL)
    bat_tbl <- tryCatch(.batters_vs_hand_tbl(bat_side, sp_hand, sp$pitcher_name), error = function(e) NULL)

    ars_html <- if (!is.null(ars_tbl)) gt::as_raw_html(ars_tbl) else
      '<p style="color:#888; font-style:italic; padding:8px;">Arsenal data not available.</p>'
    bat_html <- if (!is.null(bat_tbl)) gt::as_raw_html(bat_tbl) else
      '<p style="color:#888; font-style:italic; padding:8px;">Lineup data not available.</p>'

    paste0(
      '<div style="margin-bottom:2.5rem;">',
      '<h4 style="margin-top:0; margin-bottom:0.75rem; color:#2c3e50;">',
      sp$pitcher_name, ' (', sp_team, ' \u2014 ', hand_str, ') facing ', bat_team,
      '</h4>',
      '<div style="display:flex; gap:1.5rem; flex-wrap:wrap; align-items:flex-start;">',
      '<div style="flex:0 0 auto; min-width:280px; max-width:400px;">',
      ars_html,
      '</div>',
      '<div style="flex:1; min-width:340px;">',
      bat_html,
      '</div>',
      '</div>',
      '</div>'
    )
  })

  paste0(
    '<div class="pitcher-matchup-section">',
    paste(sections, collapse = "\n"),
    '</div>'
  )
}

# ============================================================
# make_batter_vs_pitch_html
# ============================================================
# For each starter, shows how the opposing lineup performs
# against each of that pitcher's specific pitch types.
# Displayed as Bootstrap pill tabs — one tab per pitch type,
# each containing a gt table of batter stats.
#
# Requires batter_pitch_type_stats (from base cache).
# Gracefully shows unavailable message if not present.
# ============================================================

make_batter_vs_pitch_html <- function(gpk) {

  # FanGraphs type-4 pitch run-value columns (run value per 100 pitches, batter perspective)
  # Statcast pitch code → FanGraphs column name in offense_master_season
  # FG uses different abbreviations from Statcast in several cases:
  #   FF/FA  → fg_wFB_C   (FG lumps fastballs as "FB")
  #   FC     → fg_wCT_C   (FG uses CT for cutter)
  #   CU     → fg_wCB_C   (FG uses CB for curveball)
  #   FS     → fg_wSF_C   (FG uses SF for split-finger)
  #   ST     → fg_pfxwST_C (sweeper only in pfx series)
  #   SI/KC  → pfx series  (standard FG lacks separate column)
  .pitch_fg_map <- c(
    FF = "fg_wFB_C",      # four-seam → FG generic fastball
    FA = "fg_wFB_C",      # fastball
    FT = "fg_wFB_C",      # two-seam (lumped with fastball in FG)
    SI = "fg_pfxwSI_C",   # sinker → pfx series
    FC = "fg_wCT_C",      # cutter → FG uses CT
    SL = "fg_wSL_C",      # slider
    ST = "fg_pfxwST_C",   # sweeper → pfx series (only location)
    CU = "fg_wCB_C",      # curveball → FG uses CB
    CB = "fg_wCB_C",      # curveball (Statcast alt code)
    CH = "fg_wCH_C",      # changeup
    FS = "fg_wSF_C",      # split-finger → FG uses SF
    KC = "fg_pfxwKC_C",   # knuckle-curve → pfx series
    KN = "fg_wKN_C"       # knuckleball
  )
  fg_rv_cols <- unique(unname(.pitch_fg_map))

  has_fg_data <- exists("offense_master_season") &&
                 any(fg_rv_cols %in% names(offense_master_season))

  if (!has_fg_data)
    return('<p style="color:#888; font-style:italic;">Batter vs. pitch type data not available \u2014 run base pipeline to build.</p>')

  game_row <- game_context %>% dplyr::filter(game_pk == gpk)
  if (nrow(game_row) == 0) return('<p style="color:#888;">Game not found.</p>')

  starters <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(.side_f = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(.side_f)

  if (nrow(starters) == 0)
    return('<p style="color:#888; font-style:italic;">Starter data not available.</p>')

  # Detect plate discipline column names (type 6 pull → _pct suffix;
  # type 8 dashboard may use period notation — try both)
  swstr_col <- intersect(c("fg_SwStr_pct", "fg_SwStr."), names(offense_master_season))[1]
  csw_col   <- intersect(c("fg_C_plusSwStr_pct", "fg_CSW_pct", "fg_CSW."), names(offense_master_season))[1]
  pd_cols   <- stats::na.omit(c(swstr_col, csw_col))

  # Pre-extract batter FG pitch RV columns + plate discipline once
  batter_rv <- offense_master_season %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, dplyr::any_of(c(fg_rv_cols, pd_cols)))

  # Bat side (L/R/S) from Lahman::People via player_master_ids crosswalk
  bats_lookup <- tryCatch({
    Lahman::People %>%
      dplyr::select(lahman_id = playerID, bats) %>%
      dplyr::inner_join(
        player_master_ids %>% dplyr::select(lahman_id, mlbam_id),
        by = "lahman_id"
      ) %>%
      dplyr::filter(!is.na(bats), !is.na(mlbam_id)) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, bats_code = bats)
  }, error = function(e) dplyr::tibble(mlbam_id = integer(), bats_code = character()))

  sections <- lapply(seq_len(nrow(starters)), function(i) {
    sp           <- starters[i, ]
    bat_side     <- if (sp$side == "away") "home" else "away"
    bat_team     <- if (bat_side == "home") game_row$home_team_name[1] else game_row$away_team_name[1]
    sp_team      <- if (sp$side == "away") game_row$away_team_name[1] else game_row$home_team_name[1]

    # SP's pitch arsenal — top 5 by usage
    if (!exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0) return("")
    sp_pitches <- dplyr::tibble(mlbam_id = as.integer(sp$mlbam_id)) %>%
      dplyr::left_join(pitcher_arsenal, by = "mlbam_id") %>%
      dplyr::filter(!is.na(pitch_code)) %>%
      dplyr::arrange(dplyr::desc(usage_pct)) %>%
      dplyr::slice_head(n = 5)
    if (nrow(sp_pitches) == 0) return("")

    # Keep only pitches that have a corresponding FG column present in batter_rv
    sp_pitches <- sp_pitches %>%
      dplyr::mutate(.fg_col = .pitch_fg_map[pitch_code]) %>%
      dplyr::filter(!is.na(.fg_col), .fg_col %in% names(batter_rv))
    if (nrow(sp_pitches) == 0) return("")

    # Opposing lineup
    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == bat_side) %>%
      dplyr::arrange(batting_slot)
    if (nrow(lineup) == 0) return("")

    # vs SP-hand OPS from lineup_context_splits (same game_pk + batter side)
    splits_side <- if (exists("lineup_context_splits") && nrow(lineup_context_splits) > 0) {
      lineup_context_splits %>%
        dplyr::filter(game_pk == gpk, side == bat_side) %>%
        dplyr::select(mlbam_id, sp_ops, split_label)
    } else {
      dplyr::tibble(mlbam_id = integer(), sp_ops = numeric(), split_label = character())
    }
    split_lbl <- {
      lbl <- splits_side$split_label[!is.na(splits_side$split_label)]
      if (length(lbl) > 0) lbl[1] else "vs SP"
    }

    # Batters with their RV columns + bat side + season stats + splits
    base_batters <- lineup %>%
      dplyr::select(
        mlbam_id, batting_slot, player_name, fg_position,
        dplyr::any_of(c("mlb_pa", "fg_wRC_plus"))
      ) %>%
      dplyr::left_join(batter_rv,   by = "mlbam_id") %>%
      dplyr::left_join(bats_lookup, by = "mlbam_id") %>%
      dplyr::left_join(splits_side %>% dplyr::select(mlbam_id, sp_ops), by = "mlbam_id")

    # ── Wide table: batters as rows, pitch types as columns ──────────────────
    # Fixed context columns first, then one column per pitch type.
    wide <- base_batters %>%
      dplyr::transmute(
        `#`      = batting_slot,
        B        = bats_code,
        Name     = player_name,
        Pos      = fg_position,
        PA       = suppressWarnings(as.integer(mlb_pa)),
        wrc_plus = suppressWarnings(as.numeric(fg_wRC_plus)),
        vs_sp    = suppressWarnings(as.numeric(sp_ops)),
        swstr    = suppressWarnings(as.numeric(
          if (!is.na(swstr_col) && swstr_col %in% names(base_batters))
            base_batters[[swstr_col]] else NA_real_
        )),
        csw      = suppressWarnings(as.numeric(
          if (!is.na(csw_col) && csw_col %in% names(base_batters))
            base_batters[[csw_col]] else NA_real_
        ))
      )

    for (j in seq_len(nrow(sp_pitches))) {
      key <- paste0("pt_", sp_pitches$pitch_code[j])
      wide[[key]] <- suppressWarnings(as.numeric(base_batters[[sp_pitches$.fg_col[j]]]))
    }

    pitch_keys <- paste0("pt_", sp_pitches$pitch_code)

    # Column labels: bold pitch name + usage% + velocity
    pitch_labels <- setNames(
      lapply(seq_len(nrow(sp_pitches)), function(j) {
        pn  <- sp_pitches$pitch_name[j]
        pct <- sprintf("%.0f%%", sp_pitches$usage_pct[j] * 100)
        vlo <- if (!is.na(sp_pitches$avg_speed[j]))
                 sprintf("%.0fmph", sp_pitches$avg_speed[j]) else ""
        gt::html(paste0(
          "<span style='font-weight:600;'>", pn, "</span><br>",
          "<small style='color:#666;font-weight:normal;'>", pct,
          if (nchar(vlo)) paste0(" \u00b7 ", vlo) else "", "</small>"
        ))
      }),
      pitch_keys
    )

    # All column labels (context + pitch)
    col_labels <- c(
      list(
        B        = "B",
        PA       = "PA",
        wrc_plus = "wRC+",
        vs_sp    = gt::html(paste0("<small>", split_lbl, "</small><br><small style='color:#666;'>OPS</small>")),
        swstr    = gt::html("SwStr%"),
        csw      = gt::html("CSW%")
      ),
      pitch_labels
    )

    tbl <- wide %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", sp$pitcher_name, "** (", sp_team,
                                 ") \u2014 **", bat_team, "** lineup")),
        subtitle = gt::html(paste0(
          "RV/100 (FanGraphs) per pitch type \u00b7 PA = season sample \u00b7 ",
          "<span style='color:#27ae60;'>\u25a0 green = batter advantage</span>  ",
          "<span style='color:#c0392b;'>\u25a0 red = pitcher advantage</span>"
        ))
      ) %>%
      gt::cols_label(.list = col_labels) %>%
      gt::fmt_number(columns = dplyr::any_of(pitch_keys), decimals = 1) %>%
      gt::fmt_number(columns = "vs_sp",    decimals = 3) %>%
      gt::fmt_percent(columns = dplyr::any_of(c("swstr", "csw")), decimals = 1) %>%
      gt::fmt_integer(columns = dplyr::any_of(c("wrc_plus", "PA"))) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::cols_width(
        `#`      ~ gt::px(28),
        B        ~ gt::px(24),
        Pos      ~ gt::px(38),
        Name     ~ gt::px(140),
        PA       ~ gt::px(44),
        wrc_plus ~ gt::px(50),
        vs_sp    ~ gt::px(58),
        swstr    ~ gt::px(54),
        csw      ~ gt::px(54)
      ) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold",
        column_labels.padding     = gt::px(6)
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    # Color each pitch column independently with symmetric domain
    for (key in pitch_keys) {
      rv_vals <- wide[[key]]
      rv_vals <- rv_vals[!is.na(rv_vals) & is.finite(rv_vals)]
      if (length(rv_vals) >= 2) {
        domain_abs <- max(abs(rv_vals), na.rm = TRUE)
        if (domain_abs > 0)
          tbl <- tbl %>% gt::data_color(
            columns = key,
            palette = c("#f8d7da", "#f8f9fa", "#d4edda"),
            domain  = c(-domain_abs, domain_abs)
          )
      }
    }

    # Color wRC+ column (100 = league avg; green above, red below)
    wrc_vals <- wide$wrc_plus[!is.na(wide$wrc_plus) & is.finite(wide$wrc_plus)]
    if (length(wrc_vals) >= 2) {
      wrc_abs <- max(abs(wrc_vals - 100), na.rm = TRUE)
      if (wrc_abs > 0)
        tbl <- tbl %>% gt::data_color(
          columns = "wrc_plus",
          palette = c("#f8d7da", "#f8f9fa", "#d4edda"),
          domain  = c(100 - wrc_abs, 100 + wrc_abs)
        )
    }

    # Color SwStr% and CSW% — higher = more pitcher-friendly (red high, green low)
    for (pd_key in intersect(c("swstr", "csw"), names(wide))) {
      pd_vals <- wide[[pd_key]]
      pd_vals <- pd_vals[!is.na(pd_vals) & is.finite(pd_vals)]
      if (length(pd_vals) >= 2)
        tbl <- tbl %>% gt::data_color(
          columns  = pd_key,
          palette  = c("#d4edda", "#f8f9fa", "#f8d7da"),
          domain   = c(min(pd_vals), max(pd_vals))
        )
    }

    paste0(
      '<div style="margin-bottom:2.5rem; overflow-x:auto;">',
      gt::as_raw_html(tbl),
      '</div>'
    )
  })

  paste0(
    '<div class="batter-vs-pitch-section">',
    paste(sections, collapse = "\n"),
    '</div>'
  )
}

# ============================================================
# BATTER INTELLIGENCE REPORT — Functions
# Added for mlb_hitting.qmd rebuild
# ============================================================

# ------------------------------------------------------------
# Lineup Overview Cards (HTML)
# Returns an HTML string with two side-by-side team cards.
# ------------------------------------------------------------

make_lineup_overview_html <- function(gpk) {
  tryCatch({
    game <- game_context %>% dplyr::filter(game_pk == gpk)
    if (nrow(game) == 0) return("")

    stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
      unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

    # Deduplicated offense stats
    full_stats <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    .make_card <- function(side_filter) {
      team_name <- if (side_filter == "home") game$home_team_name else game$away_team_name

      lineup <- lineup_context %>%
        dplyr::filter(game_pk == gpk, side == side_filter) %>%
        dplyr::arrange(batting_slot)

      if (nrow(lineup) == 0) return("")

      raw <- lineup %>%
        dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
        dplyr::select(-dplyr::ends_with("_dup"))

      # Resolve K% and BB% column names
      k_col  <- intersect(c("fg_K_pct", "fg_K."),  names(raw))[1]
      bb_col <- intersect(c("fg_BB_pct", "fg_BB."), names(raw))[1]

      pa_vec  <- dplyr::coalesce(raw$mlb_pa, 0L)
      pa_safe <- pmax(pa_vec, 1L)

      wrc_vals <- if ("fg_wRC_plus" %in% names(raw)) raw$fg_wRC_plus else rep(NA_real_, nrow(raw))
      k_vals   <- if (!is.na(k_col))  raw[[k_col]]  else rep(NA_real_, nrow(raw))
      bb_vals  <- if (!is.na(bb_col)) raw[[bb_col]] else rep(NA_real_, nrow(raw))
      iso_vals <- if ("fg_ISO" %in% names(raw)) raw$fg_ISO else rep(NA_real_, nrow(raw))
      hh_vals  <- if ("sc_ev95percent" %in% names(raw)) raw$sc_ev95percent else rep(NA_real_, nrow(raw))
      brl_vals <- if ("sc_brl_percent" %in% names(raw)) raw$sc_brl_percent else rep(NA_real_, nrow(raw))

      # PA-weighted aggregates
      n_valid <- function(x) sum(!is.na(x) & is.finite(x))

      safe_wmean <- function(x, w) {
        ok <- !is.na(x) & is.finite(x)
        if (sum(ok) == 0) return(NA_real_)
        weighted.mean(x[ok], w[ok])
      }

      team_wrc  <- safe_wmean(wrc_vals,  pa_safe)
      team_k    <- safe_wmean(k_vals,    pa_safe)
      team_bb   <- safe_wmean(bb_vals,   pa_safe)
      team_iso  <- safe_wmean(iso_vals,  pa_safe)
      team_hh   <- if (n_valid(hh_vals)  > 0) mean(hh_vals,  na.rm = TRUE) else NA_real_
      team_brl  <- if (n_valid(brl_vals) > 0) mean(brl_vals, na.rm = TRUE) else NA_real_

      # Lineup identity classification
      identity <- dplyr::case_when(
        !is.na(team_iso) & !is.na(team_bb) & team_iso >= .175 & team_bb >= .090 ~ "Power-heavy, patient",
        !is.na(team_iso) & !is.na(team_bb) & team_iso >= .175 & team_bb <  .080 ~ "Power-heavy, aggressive",
        !is.na(team_bb)  & !is.na(team_k)  & team_bb  >= .090 & team_k  <  .200 ~ "Patient, contact-first",
        !is.na(team_k)   & !is.na(team_iso) & team_k   >= .240 & team_iso >= .150 ~ "High-strikeout, power",
        !is.na(team_k)   & !is.na(team_bb) & team_k   <  .200 & team_bb <  .080 ~ "Aggressive contact",
        TRUE ~ "Balanced"
      )

      # Strength callout
      strength <- dplyr::case_when(
        !is.na(team_wrc) & team_wrc >= 110               ~ "Above-avg offense (wRC+)",
        !is.na(team_iso) & team_iso >= .175               ~ "Power lineup (ISO)",
        !is.na(team_bb)  & team_bb  >= .090               ~ "Patient lineup (BB%)",
        !is.na(team_k)   & team_k   <  .190               ~ "Contact-oriented (low K%)",
        TRUE                                               ~ "Competitive lineup"
      )

      # Weakness callout
      weakness <- dplyr::case_when(
        !is.na(team_k)  & team_k  >= .240 ~ "High strikeout risk (K%)",
        !is.na(team_iso) & team_iso < .130 ~ "Limited power (ISO)",
        !is.na(team_bb)  & team_bb  < .070 ~ "Aggressive, limited walks (BB%)",
        TRUE                               ~ "No glaring weakness identified"
      )

      # Formatting helpers
      fmt_wrc  <- if (!is.na(team_wrc))  as.character(round(team_wrc))  else "\u2014"
      fmt_k    <- if (!is.na(team_k))    sprintf("%.1f%%", team_k  * 100) else "\u2014"
      fmt_bb   <- if (!is.na(team_bb))   sprintf("%.1f%%", team_bb * 100) else "\u2014"
      fmt_iso  <- if (!is.na(team_iso))  sprintf(".%03d", round(team_iso * 1000)) else "\u2014"
      fmt_hh   <- if (!is.na(team_hh))   sprintf("%.1f%%", team_hh)  else "\u2014"
      fmt_brl  <- if (!is.na(team_brl))  sprintf("%.1f%%", team_brl) else "\u2014"

      wrc_color <- dplyr::case_when(
        is.na(team_wrc)    ~ "#2c3e50",
        team_wrc >= 115    ~ "#27ae60",
        team_wrc >= 100    ~ "#2c3e50",
        TRUE               ~ "#e74c3c"
      )

      stat_box <- function(val, label, color = "#2c3e50") {
        paste0(
          '<div style="text-align:center; background:#f8f9fa; border-radius:4px; padding:6px 4px;">',
          '<div style="font-size:18px; font-weight:700; color:', color, ';">', val, '</div>',
          '<div style="font-size:10px; color:#888;">', label, '</div>',
          '</div>'
        )
      }

      paste0(
        '<div style="flex:1; min-width:260px; background:white; border-radius:8px; ',
        'border:2px solid #1a73e8; padding:14px 16px;">',
        '<div style="font-size:15px; font-weight:700; color:#1a3c6e; margin-bottom:8px;">',
        gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", team_name))),
        '</div>',
        '<div style="font-size:13px; color:#555; margin-bottom:6px;">',
        '<em>', identity, '</em>',
        '</div>',
        '<div style="display:grid; grid-template-columns:1fr 1fr 1fr; gap:6px; margin-bottom:8px;">',
        stat_box(fmt_wrc, "wRC+", wrc_color),
        stat_box(fmt_k,   "K%"),
        stat_box(fmt_bb,  "BB%"),
        stat_box(fmt_iso, "ISO"),
        stat_box(fmt_hh,  "HH%"),
        stat_box(fmt_brl, "Brl%"),
        '</div>',
        '<div style="font-size:12px; color:#27ae60; margin-bottom:2px;">&#10003; ', strength, '</div>',
        '<div style="font-size:12px; color:#e74c3c;">&#9888; ', weakness, '</div>',
        '</div>'
      )
    }

    away_card <- .make_card("away")
    home_card <- .make_card("home")

    paste0(
      '<div style="display:flex; gap:1.5rem; flex-wrap:wrap; margin-bottom:2rem;">',
      away_card, home_card,
      '</div>'
    )
  }, error = function(e) {
    paste0('<p style="color:#888; font-style:italic;">Lineup overview unavailable: ',
           conditionMessage(e), '</p>')
  })
}

# ------------------------------------------------------------
# Batter Intelligence Tables — Named list of 3 gt tables
# Tab 1: Overview, Tab 2: Discipline & Contact, Tab 3: Batted Ball
# ------------------------------------------------------------

make_batter_intelligence_gt <- function(gpk, side_filter) {
  tryCatch({
    game <- game_context %>% dplyr::filter(game_pk == gpk)
    team <- if (side_filter == "home") game$home_team_name else game$away_team_name

    stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
      unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))
    season_label <- paste0(stats_yr, " season")

    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == side_filter) %>%
      dplyr::arrange(batting_slot)

    if (nrow(lineup) == 0) return(list())

    full_stats <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    raw <- lineup %>%
      dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup"))

    # Helper: resolve column with fallback variants
    col1 <- function(df, candidates) {
      found <- intersect(candidates, names(df))
      if (length(found) == 0) rep(NA_real_, nrow(df)) else df[[found[1]]]
    }

    base_opts <- function(tbl) {
      tbl %>%
        gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
        gt::tab_options(
          table.font.size           = 12,
          heading.align             = "left",
          data_row.padding          = gt::px(4),
          column_labels.font.weight = "bold"
        ) %>%
        gt::opt_stylize(style = 1, color = "blue")
    }

    # ── TAB 1: Overview ──────────────────────────────────────────────
    k_col  <- intersect(c("fg_K_pct",  "fg_K."),  names(raw))[1]
    bb_col <- intersect(c("fg_BB_pct", "fg_BB."), names(raw))[1]

    d1 <- dplyr::tibble(
      `#`   = raw$batting_slot,
      Name  = raw$player_name,
      Pos   = raw$fg_position,
      PA    = dplyr::coalesce(raw$mlb_pa, NA_integer_),
      `wRC+`= if ("fg_wRC_plus" %in% names(raw)) raw$fg_wRC_plus else NA_real_,
      ISO   = if ("fg_ISO"      %in% names(raw)) raw$fg_ISO      else NA_real_,
      wOBA  = if ("fg_wOBA"     %in% names(raw)) raw$fg_wOBA     else NA_real_,
      `BB%` = if (!is.na(bb_col)) raw[[bb_col]] else NA_real_,
      `K%`  = if (!is.na(k_col))  raw[[k_col]]  else NA_real_
    )

    t1 <- d1 %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** \u2014 Batter Intelligence")),
        subtitle = paste0("Core production \u00b7 ", season_label)
      ) %>%
      gt::fmt_integer(columns = dplyr::any_of(c("PA"))) %>%
      gt::fmt_number(columns  = dplyr::any_of(c("wRC+")), decimals = 0) %>%
      gt::fmt_number(columns  = dplyr::any_of(c("ISO", "wOBA")), decimals = 3) %>%
      gt::fmt_percent(columns = dplyr::any_of(c("BB%", "K%")), decimals = 1) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Value",
        columns = dplyr::any_of(c("wRC+", "ISO", "wOBA"))
      ) %>%
      gt::tab_spanner(
        label   = "Approach",
        columns = dplyr::any_of(c("BB%", "K%"))
      ) %>%
      gt::cols_width(`#` ~ gt::px(28), Pos ~ gt::px(38)) %>%
      base_opts()

    # Apply data_color to wRC+ if sufficient values
    wrc_vals <- d1[["wRC+"]]
    if (sum(!is.na(wrc_vals) & is.finite(wrc_vals)) >= 2)
      t1 <- t1 %>% gt::data_color(
        columns  = "wRC+",
        palette  = c("#f8d7da", "#f8f9fa", "#d4edda"),
        domain   = c(70, 140),
        na_color = "white"
      )

    # ── TAB 2: Discipline & Contact ──────────────────────────────────
    chase_col  <- intersect(c("fg_O.Swing.", "fg_O_Swing_pct"),    names(raw))[1]
    zcon_col   <- intersect(c("fg_Z.Contact.", "fg_Z_Contact_pct"), names(raw))[1]
    swstr_col  <- intersect(c("fg_SwStr.", "fg_SwStr_pct"),         names(raw))[1]
    csw_col    <- intersect(c("fg_CSW.", "fg_C_plusSwStr_pct"),     names(raw))[1]
    fstrike_col<- intersect(c("fg_F.Strike.", "fg_F_Strike_pct"),   names(raw))[1]

    d2 <- dplyr::tibble(
      `#`        = raw$batting_slot,
      Name       = raw$player_name,
      `Chase%`   = if (!is.na(chase_col))   raw[[chase_col]]   else NA_real_,
      `ZCon%`    = if (!is.na(zcon_col))    raw[[zcon_col]]    else NA_real_,
      `SwStr%`   = if (!is.na(swstr_col))   raw[[swstr_col]]   else NA_real_,
      `CSW%`     = if (!is.na(csw_col))     raw[[csw_col]]     else NA_real_,
      `F-Strike%`= if (!is.na(fstrike_col)) raw[[fstrike_col]] else NA_real_,
      EV         = if ("sc_avg_hit_speed"  %in% names(raw)) raw$sc_avg_hit_speed  else NA_real_,
      `HH%`      = if ("sc_ev95percent"    %in% names(raw)) raw$sc_ev95percent    else NA_real_,
      `Brl%`     = if ("sc_brl_percent"    %in% names(raw)) raw$sc_brl_percent    else NA_real_,
      BABIP      = if ("fg_BABIP"          %in% names(raw)) raw$fg_BABIP          else NA_real_
    )

    t2 <- d2 %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** \u2014 Batter Intelligence")),
        subtitle = paste0("Plate discipline & contact quality \u00b7 ", season_label)
      ) %>%
      gt::fmt_percent(
        columns  = dplyr::any_of(c("Chase%", "ZCon%", "SwStr%", "CSW%", "F-Strike%")),
        decimals = 1
      ) %>%
      gt::fmt_number(columns = dplyr::any_of(c("EV")),    decimals = 1) %>%
      gt::fmt_number(columns = dplyr::any_of(c("BABIP")), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("HH%", "Brl%")), decimals = 1) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = dplyr::any_of(c("HH%", "Brl%"))),
        fn = function(x) {
          ifelse(x == "\u2014" | is.na(x), "\u2014",
                 paste0(gsub("\\s+$", "", x), "%"))
        }
      ) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Plate Discipline",
        columns = dplyr::any_of(c("Chase%", "ZCon%", "SwStr%", "CSW%", "F-Strike%"))
      ) %>%
      gt::tab_spanner(
        label   = "Contact Quality",
        columns = dplyr::any_of(c("EV", "HH%", "Brl%", "BABIP"))
      ) %>%
      gt::cols_width(`#` ~ gt::px(28)) %>%
      base_opts()

    # data_color: SwStr% (red=high, green=low) — only if sufficient values
    swstr_vals <- d2[["SwStr%"]]
    if (sum(!is.na(swstr_vals) & is.finite(swstr_vals)) >= 2)
      t2 <- t2 %>% gt::data_color(
        columns  = "SwStr%",
        palette  = c("#d4edda", "#f8f9fa", "#f8d7da"),
        domain   = c(0.04, 0.16),
        na_color = "white"
      )

    chase_vals <- d2[["Chase%"]]
    if (sum(!is.na(chase_vals) & is.finite(chase_vals)) >= 2)
      t2 <- t2 %>% gt::data_color(
        columns  = "Chase%",
        palette  = c("#d4edda", "#f8f9fa", "#f8d7da"),
        domain   = c(0.20, 0.45),
        na_color = "white"
      )

    # ── TAB 3: Batted Ball & Profile ─────────────────────────────────
    d3 <- dplyr::tibble(
      `#`     = raw$batting_slot,
      Name    = raw$player_name,
      `GB%`   = col1(raw, c("fg_GB_pct",    "fg_GB.")),
      `LD%`   = col1(raw, c("fg_LD_pct",    "fg_LD.")),
      `FB%`   = col1(raw, c("fg_FB_pct",    "fg_FB.")),
      `HR/FB` = col1(raw, c("fg_HR_per_FB", "fg_HR.FB")),
      `Pull%` = col1(raw, c("fg_Pull_pct",  "fg_Pull.")),
      `Cent%` = col1(raw, c("fg_Cent_pct",  "fg_Cent.")),
      `Oppo%` = col1(raw, c("fg_Oppo_pct",  "fg_Oppo."))
    )

    t3 <- d3 %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** \u2014 Batter Intelligence")),
        subtitle = paste0("Batted ball profile & direction \u00b7 ", season_label)
      ) %>%
      gt::fmt_percent(
        columns  = dplyr::any_of(c("GB%", "LD%", "FB%", "HR/FB", "Pull%", "Cent%", "Oppo%")),
        decimals = 1
      ) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Batted Ball Type",
        columns = dplyr::any_of(c("GB%", "LD%", "FB%", "HR/FB"))
      ) %>%
      gt::tab_spanner(
        label   = "Direction",
        columns = dplyr::any_of(c("Pull%", "Cent%", "Oppo%"))
      ) %>%
      gt::cols_width(`#` ~ gt::px(28)) %>%
      base_opts()

    ld_vals <- d3[["LD%"]]
    if (sum(!is.na(ld_vals) & is.finite(ld_vals)) >= 2)
      t3 <- t3 %>% gt::data_color(
        columns  = "LD%",
        palette  = c("#f8f9fa", "#d4edda"),
        domain   = c(0.16, 0.28),
        na_color = "white"
      )

    hrfb_vals <- d3[["HR/FB"]]
    if (sum(!is.na(hrfb_vals) & is.finite(hrfb_vals)) >= 2)
      t3 <- t3 %>% gt::data_color(
        columns  = "HR/FB",
        palette  = c("#f8f9fa", "#d4edda"),
        domain   = c(0.06, 0.22),
        na_color = "white"
      )

    list(
      "Overview"                    = t1,
      "Discipline & Contact"        = t2,
      "Batted Ball & Profile"       = t3
    )

  }, error = function(e) {
    list()
  })
}

# ------------------------------------------------------------
# Pitch Vulnerability — batter RV vs tonight's opposing SP arsenal
# ------------------------------------------------------------

make_pitch_vulnerability_gt <- function(gpk, side_filter) {
  tryCatch({
    game <- game_context %>% dplyr::filter(game_pk == gpk)
    team <- if (side_filter == "home") game$home_team_name else game$away_team_name

    # Identify opposing SP
    opp_side <- if (side_filter == "away") "home" else "away"
    sp_row <- starter_matchup %>%
      dplyr::filter(game_pk == gpk, side == opp_side)

    na_tbl <- function(msg) {
      dplyr::tibble(Note = msg) %>%
        gt::gt() %>%
        gt::tab_header(title = paste0(team, " \u2014 Pitch Vulnerability")) %>%
        gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
        gt::tab_options(table.font.size = 12, heading.align = "left",
                        data_row.padding = gt::px(4)) %>%
        gt::opt_stylize(style = 1, color = "blue")
    }

    if (nrow(sp_row) == 0) return(na_tbl("Opposing starter not found."))

    sp_id   <- sp_row$mlbam_id[1]
    sp_name <- dplyr::coalesce(sp_row$pitcher_name[1], "Unknown SP")

    if (!exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0)
      return(na_tbl(paste0("Arsenal data unavailable for ", sp_name, ".")))

    arsenal <- pitcher_arsenal %>%
      dplyr::filter(mlbam_id == sp_id) %>%
      dplyr::arrange(dplyr::desc(usage_pct)) %>%
      dplyr::slice_head(n = 4)

    if (nrow(arsenal) == 0)
      return(na_tbl(paste0("No arsenal data found for ", sp_name, ".")))

    # Map pitch codes to batter RV columns
    pitch_rv_map <- c(
      FF = "fg_wFB_C", FA = "fg_wFB_C", FT = "fg_wFB_C", SI = "fg_wFB_C",
      FC = "fg_wCT_C",
      SL = "fg_wSL_C", ST = "fg_pfxwST_C",
      CU = "fg_wCB_C", CB = "fg_wCB_C",
      CH = "fg_wCH_C",
      FS = "fg_wSF_C", FO = "fg_wSF_C"
    )

    stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
      unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

    full_stats <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == side_filter) %>%
      dplyr::arrange(batting_slot)

    raw <- lineup %>%
      dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup"))

    # Build display tibble
    display <- dplyr::tibble(
      `#`  = raw$batting_slot,
      Name = raw$player_name
    )

    pitch_col_names <- character(0)

    for (i in seq_len(nrow(arsenal))) {
      pc       <- as.character(arsenal$pitch_code[i])
      pname    <- dplyr::coalesce(arsenal$pitch_name[i], pc)
      usage    <- if (!is.na(arsenal$usage_pct[i])) sprintf("%.0f%%", arsenal$usage_pct[i] * 100) else "?"
      rv_col   <- pitch_rv_map[pc]
      col_label <- paste0(pname, "\n(", usage, ")")

      if (!is.na(rv_col) && rv_col %in% names(raw)) {
        display[[col_label]] <- raw[[rv_col]]
      } else {
        display[[col_label]] <- NA_real_
      }
      pitch_col_names <- c(pitch_col_names, col_label)
    }

    tbl <- display %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** vs **", sp_name, "**\u2019s Arsenal")),
        subtitle = paste0("Run value per 100 pitches (batter perspective) \u00b7 ", stats_yr, " season")
      ) %>%
      gt::fmt_number(
        columns  = dplyr::any_of(pitch_col_names),
        decimals = 1
      ) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_source_note(
        source_note = "Run value per 100 pitches faced (batter perspective). Positive = above average vs that pitch type. Scale: \u00b13.0 is significant."
      ) %>%
      gt::cols_width(`#` ~ gt::px(28)) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    # Apply data_color per pitch column
    for (cn in pitch_col_names) {
      if (cn %in% names(display)) {
        vals <- display[[cn]]
        if (sum(!is.na(vals) & is.finite(vals)) >= 2) {
          tbl <- tbl %>% gt::data_color(
            columns  = dplyr::all_of(cn),
            palette  = c("#f8d7da", "#f8f9fa", "#d4edda"),
            domain   = c(-3.0, 3.0),
            na_color = "white"
          )
        }
      }
    }

    tbl

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Pitch vulnerability unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_header(title = "Pitch Vulnerability") %>%
      gt::tab_options(table.font.size = 12, heading.align = "left",
                      data_row.padding = gt::px(4)) %>%
      gt::opt_stylize(style = 1, color = "blue")
  })
}

# ------------------------------------------------------------
# Sustainability — xStats vs actual (over/underperformers)
# ------------------------------------------------------------

make_sustainability_gt <- function(gpk, side_filter) {
  tryCatch({
    game <- game_context %>% dplyr::filter(game_pk == gpk)
    team <- if (side_filter == "home") game$home_team_name else game$away_team_name

    stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
      unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

    full_stats <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == side_filter) %>%
      dplyr::arrange(batting_slot)

    if (nrow(lineup) == 0) return(invisible(NULL))

    raw <- lineup %>%
      dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup")) %>%
      dplyr::filter(dplyr::coalesce(mlb_pa, 0L) >= 10)

    if (nrow(raw) == 0) {
      return(
        dplyr::tibble(Note = "No batters with 10+ PA in current season.") %>%
          gt::gt() %>%
          gt::tab_header(title = paste0(team, " \u2014 Sustainability")) %>%
          gt::tab_options(table.font.size = 12, heading.align = "left") %>%
          gt::opt_stylize(style = 1, color = "blue")
      )
    }

    avg_vals  <- if ("mlb_avg"      %in% names(raw)) raw$mlb_avg      else NA_real_
    xba_vals  <- if ("sc_est_ba"    %in% names(raw)) raw$sc_est_ba    else NA_real_
    woba_vals <- if ("fg_wOBA"      %in% names(raw)) raw$fg_wOBA      else NA_real_
    xwoba_vals<- if ("sc_est_woba"  %in% names(raw)) raw$sc_est_woba  else NA_real_
    babip_vals<- if ("fg_BABIP"     %in% names(raw)) raw$fg_BABIP     else NA_real_
    pa_vals   <- dplyr::coalesce(raw$mlb_pa, 0L)

    avg_diff  <- avg_vals  - xba_vals
    woba_diff <- woba_vals - xwoba_vals

    note_fn <- function(ad, wd, pa) {
      if (is.na(pa) || pa < 30) return("Small sample")
      dplyr::case_when(
        !is.na(ad) & !is.na(wd) & ad >  .030 & wd >  .020 ~ "Running hot \u2014 regression likely",
        !is.na(ad) & !is.na(wd) & ad < -.030 & wd < -.020 ~ "Unlucky \u2014 positive regression due",
        !is.na(ad) & ad >  .030                             ~ "AVG inflated by BABIP luck",
        !is.na(wd) & wd >  .020                             ~ "Contact timing running hot",
        !is.na(ad) & ad < -.030                             ~ "AVG suppressed \u2014 expect improvement",
        !is.na(wd) & wd < -.020                             ~ "Quality contact not converting",
        TRUE                                                 ~ "In line with expectations"
      )
    }

    notes <- mapply(note_fn, avg_diff, woba_diff, pa_vals, SIMPLIFY = TRUE)

    display <- dplyr::tibble(
      `#`          = raw$batting_slot,
      Name         = raw$player_name,
      AVG          = avg_vals,
      xBA          = xba_vals,
      `AVG Diff`   = avg_diff,
      wOBA         = woba_vals,
      xwOBA        = xwoba_vals,
      `wOBA Diff`  = woba_diff,
      BABIP        = babip_vals,
      Note         = notes
    )

    tbl <- display %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** \u2014 Sustainability")),
        subtitle = paste0("xStats vs actual \u00b7 ", stats_yr, " season (min 10 PA shown; note requires 30 PA)")
      ) %>%
      gt::fmt_number(
        columns  = dplyr::any_of(c("AVG", "xBA", "wOBA", "xwOBA", "BABIP")),
        decimals = 3
      ) %>%
      gt::fmt_number(
        columns  = dplyr::any_of(c("AVG Diff", "wOBA Diff")),
        decimals = 3
      ) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Batting Average",
        columns = dplyr::any_of(c("AVG", "xBA", "AVG Diff"))
      ) %>%
      gt::tab_spanner(
        label   = "Run Creation",
        columns = dplyr::any_of(c("wOBA", "xwOBA", "wOBA Diff"))
      ) %>%
      gt::cols_width(`#` ~ gt::px(28)) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    # data_color for diffs
    avg_diff_vals <- display[["AVG Diff"]]
    if (sum(!is.na(avg_diff_vals) & is.finite(avg_diff_vals)) >= 2)
      tbl <- tbl %>% gt::data_color(
        columns  = "AVG Diff",
        palette  = c("#f8d7da", "#f8f9fa", "#d4edda"),
        domain   = c(-0.050, 0.050),
        na_color = "white"
      )

    woba_diff_vals <- display[["wOBA Diff"]]
    if (sum(!is.na(woba_diff_vals) & is.finite(woba_diff_vals)) >= 2)
      tbl <- tbl %>% gt::data_color(
        columns  = "wOBA Diff",
        palette  = c("#f8d7da", "#f8f9fa", "#d4edda"),
        domain   = c(-0.050, 0.050),
        na_color = "white"
      )

    tbl

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Sustainability data unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_header(title = "Sustainability") %>%
      gt::tab_options(table.font.size = 12, heading.align = "left",
                      data_row.padding = gt::px(4)) %>%
      gt::opt_stylize(style = 1, color = "blue")
  })
}

# ------------------------------------------------------------
# Baserunning — SB/CS, sprint speed, and advanced value
# ------------------------------------------------------------

make_baserunning_gt <- function(gpk, side_filter) {
  tryCatch({
    game <- game_context %>% dplyr::filter(game_pk == gpk)
    team <- if (side_filter == "home") game$home_team_name else game$away_team_name

    stats_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
      unique(offense_master_season$season)[1] else as.integer(format(Sys.Date(), "%Y"))

    full_stats <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    lineup <- lineup_context %>%
      dplyr::filter(game_pk == gpk, side == side_filter) %>%
      dplyr::arrange(batting_slot)

    if (nrow(lineup) == 0) return(invisible(NULL))

    raw <- lineup %>%
      dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
      dplyr::select(-dplyr::ends_with("_dup"))

    # Supplement with baserunning master (contains fg_UBR, fg_Spd not in offense_master)
    if (exists("baserunning_master_season") && nrow(baserunning_master_season) > 0) {
      br_sup <- baserunning_master_season %>%
        dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_sb, 0L))) %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, dplyr::any_of(c("fg_UBR", "fg_Spd", "fg_wSB", "fg_wGDP")))
      raw <- raw %>%
        dplyr::left_join(br_sup, by = "mlbam_id", suffix = c("", "_br")) %>%
        dplyr::select(-dplyr::ends_with("_br"))
    }

    sb_vals  <- if ("mlb_sb"         %in% names(raw)) raw$mlb_sb         else NA_integer_
    cs_vals  <- if ("mlb_cs"         %in% names(raw)) raw$mlb_cs         else NA_integer_

    # SB% — use pre-computed column if available, else derive
    sbpct_vals <- if ("mlb_sb_pct" %in% names(raw)) {
      raw$mlb_sb_pct
    } else if (!all(is.na(sb_vals)) && !all(is.na(cs_vals))) {
      sb_safe <- dplyr::coalesce(sb_vals, 0L)
      cs_safe <- dplyr::coalesce(cs_vals, 0L)
      dplyr::if_else((sb_safe + cs_safe) > 0,
                     sb_safe / (sb_safe + cs_safe),
                     NA_real_)
    } else {
      NA_real_
    }

    col1_br <- function(candidates) {
      found <- intersect(candidates, names(raw))
      if (length(found) == 0) rep(NA_real_, nrow(raw)) else raw[[found[1]]]
    }

    display <- dplyr::tibble(
      `#`     = raw$batting_slot,
      Name    = raw$player_name,
      SB      = sb_vals,
      CS      = cs_vals,
      `SB%`   = sbpct_vals,
      Sprint  = col1_br(c("sc_sprint_speed")),
      Spd     = col1_br(c("fg_Spd", "fg_spd")),
      UBR     = col1_br(c("fg_UBR", "fg_ubr", "fg_UBR_x", "fg_UBR_y")),
      BsR     = col1_br(c("fg_wBsR", "fg_BsR", "fg_bsr", "fg_wBSR"))
    )

    tbl <- display %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team, "** \u2014 Baserunning")),
        subtitle = paste0("Speed, stolen base efficiency & extra-base value \u00b7 ", stats_yr, " season")
      ) %>%
      gt::fmt_integer(columns = dplyr::any_of(c("SB", "CS"))) %>%
      gt::fmt_percent(columns = dplyr::any_of(c("SB%")), decimals = 1) %>%
      gt::fmt_number(columns  = dplyr::any_of(c("Sprint", "Spd", "UBR", "BsR")), decimals = 1) %>%
      gt::fmt_missing(columns = dplyr::everything(), missing_text = "\u2014") %>%
      gt::tab_spanner(
        label   = "Stolen Bases",
        columns = dplyr::any_of(c("SB", "CS", "SB%"))
      ) %>%
      gt::tab_spanner(
        label   = "Speed",
        columns = dplyr::any_of(c("Sprint", "Spd"))
      ) %>%
      gt::tab_spanner(
        label   = "Baserunning Value",
        columns = dplyr::any_of(c("UBR", "BsR"))
      ) %>%
      gt::tab_source_note(
        source_note = "Sprint Speed: Statcast mph (elite \u2265 30). Spd: Bill James composite speed score. UBR: extra bases taken on BIP (excludes SB). BsR: total baserunning runs above average."
      ) %>%
      gt::cols_width(`#` ~ gt::px(28)) %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    # data_color: Sprint (green=fast)
    sprint_vals <- display[["Sprint"]]
    if (sum(!is.na(sprint_vals) & is.finite(sprint_vals)) >= 2)
      tbl <- tbl %>% gt::data_color(
        columns  = "Sprint",
        palette  = c("#f8f9fa", "#d4edda"),
        domain   = c(25, 32),
        na_color = "white"
      )

    # data_color: BsR
    bsr_vals <- display[["BsR"]]
    if (sum(!is.na(bsr_vals) & is.finite(bsr_vals)) >= 2)
      tbl <- tbl %>% gt::data_color(
        columns  = "BsR",
        palette  = c("#f8d7da", "#f8f9fa", "#d4edda"),
        domain   = c(-3, 3),
        na_color = "white"
      )

    tbl

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Baserunning data unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_header(title = "Baserunning") %>%
      gt::tab_options(table.font.size = 12, heading.align = "left",
                      data_row.padding = gt::px(4)) %>%
      gt::opt_stylize(style = 1, color = "blue")
  })
}
