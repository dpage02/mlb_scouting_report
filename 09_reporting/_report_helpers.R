# ============================================================
# mlb_scouting_report
# REPORTING HELPERS
# SCRIPT: _report_helpers.R
# ============================================================
# PURPOSE:
#   gt table builder functions for the Quarto scouting report.
#   Each function takes a game_pk (and optionally a side) and
#   returns a formatted gt table ready for HTML output.
#
# FUNCTIONS:
#   make_game_header_gt(gpk)         — venue, weather, umpire, series
#   make_starter_gt(gpk)             — both SPs with season stats
#   make_lineup_gt(gpk, side)        — batting order with offense stats
#   make_bullpen_gt(gpk, side)       — bullpen grid with availability colors
# ============================================================

library(gt)
library(dplyr)

# ------------------------------------------------------------
# Availability color palette
# ------------------------------------------------------------

avail_colors <- c(
  "fresh"       = "#27ae60",
  "available"   = "#a9dfbf",
  "limited"     = "#f39c12",
  "doubtful"    = "#e67e22",
  "unavailable" = "#e74c3c",
  "injured"     = "#d5d8dc"
)

avail_text_colors <- c(
  "fresh"       = "white",
  "available"   = "#1a5276",
  "limited"     = "white",
  "doubtful"    = "white",
  "unavailable" = "white",
  "injured"     = "#7f8c8d"
)

# ------------------------------------------------------------
# Game Header
# ------------------------------------------------------------

make_game_header_gt <- function(gpk) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)

  # Series description
  series_str <- if (!is.na(game$game_in_series) && !is.na(game$series_length)) {
    label <- paste0("Game ", game$game_in_series, " of ", game$series_length)
    flags <- c(
      if (isTRUE(game$is_series_opener))  "Series Opener",
      if (isTRUE(game$is_series_finale))  "Series Finale",
      if (isTRUE(game$is_rubber_match))   "Rubber Match"
    )
    if (length(flags) > 0) paste0(label, " — ", paste(flags, collapse = ", ")) else label
  } else "—"

  # Weather
  weather_str <- if (!is.na(game$game_temp_f)) {
    paste0(game$game_temp_f, "°F")
  } else "—"

  wind_str <- if (!is.na(game$wind_speed_mph)) {
    paste0(game$wind_speed_mph, " mph")
  } else "—"

  # Game type flags
  # MLB MLBAM league IDs: 103 = AL, 104 = NL
  league_label <- dplyr::case_when(
    !is.na(game$home_league_id) & game$home_league_id == 103 ~ "AL Game",
    !is.na(game$home_league_id) & game$home_league_id == 104 ~ "NL Game",
    TRUE ~ "League Game"
  )
  type_flags <- c(
    if (isTRUE(game$is_interleague))   "Interleague",
    if (isTRUE(game$is_division_game)) "Division Game",
    if (isTRUE(game$is_doubleheader))  "Doubleheader"
  )
  type_str <- if (length(type_flags) > 0) paste(type_flags, collapse = " · ") else league_label

  time_str <- if ("game_time" %in% names(game) && !is.na(game$game_time))
    game$game_time else "TBD"

  dplyr::tibble(
    Label = c("First Pitch", "Venue", "Weather", "Wind", "HP Umpire", "Series", "Game Type"),
    Value = c(
      time_str,
      dplyr::coalesce(game$venue_name, "—"),
      weather_str,
      wind_str,
      dplyr::coalesce(game$home_plate_umpire, "TBD"),
      series_str,
      type_str
    )
  ) %>%
    gt::gt() %>%
    gt::cols_label(Label = "", Value = "") %>%
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = Label)
    ) %>%
    gt::cols_width(Label ~ gt::px(110)) %>%
    gt::tab_options(
      column_labels.hidden      = TRUE,
      table.border.top.style    = "hidden",
      table.border.bottom.style = "hidden",
      table.font.size           = 13,
      data_row.padding          = gt::px(4)
    )
}

# ------------------------------------------------------------
# Starter Matchup
# ------------------------------------------------------------

make_starter_gt <- function(gpk) {
  raw <- starter_matchup %>%
    dplyr::filter(game_pk == gpk) %>%
    dplyr::mutate(side = factor(side, levels = c("away", "home"))) %>%
    dplyr::arrange(side)

  display <- dplyr::tibble(
    Side    = dplyr::if_else(raw$side == "away", "Away SP", "Home SP"),
    Pitcher = dplyr::coalesce(raw$pitcher_name, "TBD"),
    Hand    = dplyr::coalesce(raw$pitch_hand,   "—"),
    Team    = dplyr::coalesce(raw$team_name,    "—")
  )

  # Volume
  if ("mlb_gs" %in% names(raw)) display$GS <- raw$mlb_gs
  if ("mlb_ip" %in% names(raw)) display$IP <- raw$mlb_ip

  # Classic rates — ERA omitted (ERA+ covers it)
  if ("mlb_whip" %in% names(raw)) display$WHIP <- raw$mlb_whip

  # ERA context — prefer bbref_ERA_plus; derive from fg_ERA_minus if unavailable
  # ERA+ = 10000 / ERA-  (both centered at 100; higher ERA+ = better)
  # Guard against Inf (ERA = 0.00 early season) — treat as NA
  era_plus_vec <- dplyr::coalesce(
    if ("bbref_ERA_plus" %in% names(raw)) raw$bbref_ERA_plus else rep(NA_real_, nrow(raw)),
    if ("fg_ERA_minus"   %in% names(raw) && any(!is.na(raw$fg_ERA_minus)))
      round(10000 / raw$fg_ERA_minus) else rep(NA_real_, nrow(raw))
  )
  era_plus_vec <- dplyr::if_else(is.finite(era_plus_vec), era_plus_vec, NA_real_)
  if (any(!is.na(era_plus_vec))) display$`ERA+` <- era_plus_vec

  # ERA estimators
  if ("fg_FIP"   %in% names(raw)) display$FIP   <- raw$fg_FIP
  if ("fg_xFIP"  %in% names(raw)) display$xFIP  <- raw$fg_xFIP
  if ("fg_xERA"  %in% names(raw)) display$xERA  <- raw$fg_xERA
  if ("fg_SIERA" %in% names(raw)) display$SIERA <- raw$fg_SIERA

  # Batted ball / strand
  if ("fg_BABIP"   %in% names(raw)) display$BABIP   <- raw$fg_BABIP
  if ("fg_LOB_pct" %in% names(raw)) display$`LOB%`  <- raw$fg_LOB_pct

  # Per-9
  if ("fg_K_9"  %in% names(raw)) display$`K/9`  <- raw$fg_K_9
  if ("fg_BB_9" %in% names(raw)) display$`BB/9` <- raw$fg_BB_9
  if ("fg_HR_9" %in% names(raw)) display$`HR/9` <- raw$fg_HR_9

  # Rate %
  if ("fg_K_pct"    %in% names(raw)) display$`K%`    <- raw$fg_K_pct
  if ("fg_BB_pct"   %in% names(raw)) display$`BB%`   <- raw$fg_BB_pct
  if ("fg_K_BB_pct" %in% names(raw)) display$`K-BB%` <- raw$fg_K_BB_pct

  # Value — single WAR column, preferring fg_WAR (current season) then bbref_WAR
  war_vec <- dplyr::coalesce(
    if ("fg_WAR"    %in% names(raw)) raw$fg_WAR    else rep(NA_real_, nrow(raw)),
    if ("bbref_WAR" %in% names(raw)) raw$bbref_WAR else rep(NA_real_, nrow(raw))
  )
  # Use the label of whichever source actually has data; fall back to "WAR"
  war_label <- if (any(!is.na(war_vec))) {
    if ("fg_WAR" %in% names(raw) && any(!is.na(raw$fg_WAR))) "fWAR" else "bWAR"
  } else "WAR"
  display[[war_label]] <- war_vec

  tbl <- display %>%
    gt::gt() %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("IP")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP", "xERA", "SIERA",
                                  "BABIP", "K/9", "BB/9", "HR/9")),
      decimals = 2
    ) %>%
    gt::fmt_percent(
      columns  = dplyr::any_of(c("LOB%", "K%", "BB%", "K-BB%")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA+", "GS")),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("fWAR", "bWAR", "WAR")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Volume",
      columns = dplyr::any_of(c("GS", "IP"))
    ) %>%
    gt::tab_spanner(
      label   = "Results",
      columns = dplyr::any_of(c("ERA", "ERA+", "WHIP"))
    ) %>%
    gt::tab_spanner(
      label   = "Estimators",
      columns = dplyr::any_of(c("FIP", "xFIP", "xERA", "SIERA"))
    ) %>%
    gt::tab_spanner(
      label   = "Batted Ball",
      columns = dplyr::any_of(c("BABIP", "LOB%", "HR/9"))
    ) %>%
    gt::tab_spanner(
      label   = "Discipline",
      columns = dplyr::any_of(c("K/9", "BB/9", "K%", "BB%", "K-BB%"))
    ) %>%
    gt::tab_spanner(
      label   = "Value",
      columns = dplyr::any_of(c("fWAR", "bWAR", "WAR"))
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = "#eaf2ff"),
      locations = gt::cells_body(rows = Side == "Home SP")
    ) %>%
    gt::tab_options(
      table.font.size  = 12,
      heading.align    = "left",
      data_row.padding = gt::px(5)
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")

  tbl
}

# ------------------------------------------------------------
# Lineup
# ------------------------------------------------------------

make_lineup_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  raw <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(batting_slot)

  display <- dplyr::tibble(
    `#`  = raw$batting_slot,
    Name = raw$player_name,
    Pos  = raw$fg_position
  )

  if ("mlb_avg"    %in% names(raw)) display$AVG    <- raw$mlb_avg
  if ("mlb_obp"    %in% names(raw)) display$OBP    <- raw$mlb_obp
  if ("mlb_slg"    %in% names(raw)) display$SLG    <- raw$mlb_slg
  if ("mlb_ops"    %in% names(raw)) display$OPS    <- raw$mlb_ops
  if ("mlb_hr"     %in% names(raw)) display$HR     <- raw$mlb_hr
  if ("mlb_rbi"    %in% names(raw)) display$RBI    <- raw$mlb_rbi
  if ("fg_wRC_plus"%in% names(raw)) display$`wRC+` <- raw$fg_wRC_plus

  display %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md(paste0("**", team, "**")),
      subtitle = if (exists("offense_master_season") && nrow(offense_master_season) > 0)
                   paste0(unique(offense_master_season$season)[1], " season stats")
                 else NULL
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("AVG", "OBP", "SLG", "OPS")),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("wRC+")),
      decimals = 0
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::cols_width(`#` ~ gt::px(30), Pos ~ gt::px(40)) %>%
    gt::tab_options(
      table.font.size  = 12,
      heading.align    = "left",
      data_row.padding = gt::px(4)
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")
}

# ------------------------------------------------------------
# Bullpen Grid
# ------------------------------------------------------------

make_bullpen_gt <- function(gpk, side_filter) {
  game <- game_context %>% dplyr::filter(game_pk == gpk)
  team <- if (side_filter == "home") game$home_team_name else game$away_team_name

  raw <- bullpen_grid %>%
    dplyr::filter(game_pk == gpk, side == side_filter) %>%
    dplyr::arrange(role_sort)

  display <- dplyr::tibble(
    Role   = raw$fg_role,
    Name   = raw$player_name,
    Status = raw$availability,
    Rest   = raw$days_rest,
    `P/Y`  = raw$pitches_yesterday,
    `P/3`  = raw$pitches_last_3_days,
    `App`  = raw$appearances_last_7d
  )

  if ("mlb_ip"   %in% names(raw)) display$IP   <- raw$mlb_ip
  if ("mlb_era"  %in% names(raw)) display$ERA  <- raw$mlb_era
  if ("mlb_whip" %in% names(raw)) display$WHIP <- raw$mlb_whip
  if ("mlb_sv"   %in% names(raw)) display$SV   <- raw$mlb_sv
  if ("mlb_hld"  %in% names(raw)) display$HLD  <- raw$mlb_hld
  if ("fg_WAR"   %in% names(raw)) display$WAR  <- raw$fg_WAR

  tbl <- display %>%
    gt::gt() %>%
    gt::tab_header(title = gt::md(paste0("**", team, "** Bullpen"))) %>%
    gt::fmt_number(columns = dplyr::any_of(c("ERA", "WHIP")), decimals = 2) %>%
    gt::fmt_number(columns = dplyr::any_of(c("IP", "WAR")),  decimals = 1) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
    gt::tab_spanner(
      label   = "Current",
      columns = dplyr::any_of(c("Status", "Rest", "P/Y", "P/3", "App"))
    ) %>%
    gt::tab_spanner(
      label   = "2025 Season",
      columns = dplyr::any_of(c("IP", "ERA", "WHIP", "SV", "HLD", "WAR"))
    ) %>%
    gt::cols_width(Role ~ gt::px(45), Status ~ gt::px(85)) %>%
    gt::tab_options(
      table.font.size  = 12,
      heading.align    = "left",
      data_row.padding = gt::px(4)
    ) %>%
    gt::opt_stylize(style = 1, color = "blue")

  # Color code rows by availability status
  for (status in names(avail_colors)) {
    rows_idx <- which(display$Status == status)
    if (length(rows_idx) == 0) next
    tbl <- tbl %>%
      gt::tab_style(
        style     = list(
          gt::cell_fill(color = avail_colors[status]),
          gt::cell_text(color = avail_text_colors[status])
        ),
        locations = gt::cells_body(rows = Status == status)
      )
  }

  tbl
}
