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
  "unavailable" = "#e74c3c",
  "injured"     = "#d5d8dc"
)

avail_text_colors <- c(
  "fresh"       = "white",
  "available"   = "#1a5276",
  "limited"     = "white",
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
  type_flags <- c(
    if (isTRUE(game$is_interleague))   "Interleague",
    if (isTRUE(game$is_division_game)) "Division Game",
    if (isTRUE(game$is_doubleheader))  "Doubleheader"
  )
  type_str <- if (length(type_flags) > 0) paste(type_flags, collapse = " · ") else "—"

  dplyr::tibble(
    Label = c("Venue", "Weather", "Wind", "HP Umpire", "Series", "Game Type"),
    Value = c(
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

  # Build display tibble with human-readable column names
  display <- dplyr::tibble(
    Side    = dplyr::if_else(raw$side == "away", "Away SP", "Home SP"),
    Pitcher = dplyr::coalesce(raw$pitcher_name, "TBD"),
    Team    = dplyr::coalesce(raw$team_name, "—")
  )

  if ("mlb_gs"   %in% names(raw)) display$GS    <- raw$mlb_gs
  if ("mlb_ip"   %in% names(raw)) display$IP    <- raw$mlb_ip
  if ("mlb_era"  %in% names(raw)) display$ERA   <- raw$mlb_era
  if ("mlb_whip" %in% names(raw)) display$WHIP  <- raw$mlb_whip
  if ("fg_FIP"   %in% names(raw)) display$FIP   <- raw$fg_FIP
  if ("fg_xFIP"  %in% names(raw)) display$xFIP  <- raw$fg_xFIP
  if ("fg_K_9"   %in% names(raw)) display$`K/9` <- raw$fg_K_9
  if ("fg_BB_9"  %in% names(raw)) display$`BB/9`<- raw$fg_BB_9
  if ("fg_WAR"   %in% names(raw)) display$WAR   <- raw$fg_WAR

  tbl <- display %>%
    gt::gt() %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("IP")),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("ERA", "WHIP", "FIP", "xFIP", "K/9", "BB/9")),
      decimals = 2
    ) %>%
    gt::fmt_number(
      columns  = dplyr::any_of(c("WAR")),
      decimals = 1
    ) %>%
    gt::fmt_missing(columns = dplyr::everything(), missing_text = "—") %>%
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
    gt::tab_header(title = gt::md(paste0("**", team, "**"))) %>%
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
