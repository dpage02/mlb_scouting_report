# ============================================================
# mlb_scouting_report
# REPORTING HELPERS — Series Preview & Recap
# SCRIPT: _series_helpers.R
# ============================================================
# PURPOSE:
#   Helper functions for mlb_series_preview.qmd and
#   mlb_series_recap.qmd.
#
# FUNCTIONS:
#   make_series_header_html()        — series overview banner
#   make_series_team_overview_gt()   — W-L + team offense/pitching summary
#   make_series_batting_gt()         — full roster batting (starters + bench)
#   make_series_rotation_gt()        — probable starters per game
#   make_series_bullpen_gt()         — bullpen season stats for one team
#   make_series_recap_boxscores()    — game-by-game linescore from MLB API
#   make_series_recap_narrative()    — programmatic 3-paragraph narrative
# ============================================================

library(gt)
library(dplyr)
library(purrr)

# ============================================================
# SERIES PREVIEW HELPERS
# ============================================================

# ------------------------------------------------------------
# Series header banner
# Shows: away @ home · dates · venue · game count
# ------------------------------------------------------------

make_series_header_html <- function(ns) {
  # ns = braves_series_context$next_series list
  if (is.null(ns)) return("")

  dates_str <- if (length(ns$game_dates) > 1) {
    sprintf("%s \u2013 %s",
      format(min(ns$game_dates), "%b %d"),
      format(max(ns$game_dates), "%b %d, %Y"))
  } else {
    format(ns$game_dates[1], "%B %d, %Y")
  }

  game_label <- paste0(ns$n_games, "-game series")

  paste0(
    '<div style="background:linear-gradient(135deg,#1a2a4a 0%,#2c3e50 100%);',
    'color:white;padding:20px 24px;border-radius:8px;margin-bottom:1.5rem;">',
    '<div style="font-size:0.8rem;text-transform:uppercase;letter-spacing:1px;',
    'color:#aac4e0;margin-bottom:6px;">', game_label, '</div>',
    '<div style="font-size:1.6rem;font-weight:700;margin-bottom:4px;">',
    ns$away_team_name, ' <span style="color:#aac4e0;">@</span> ', ns$home_team_name,
    '</div>',
    '<div style="font-size:0.9rem;color:#c5d8ea;">',
    dates_str, ' &nbsp;&middot;&nbsp; ', ns$venue_name,
    '</div>',
    '</div>'
  )
}

# ------------------------------------------------------------
# Team season overview: W-L, RS/RA, team wRC+, team ERA
# One row per team, shown side-by-side via stacked gt table
# ------------------------------------------------------------

make_series_team_overview_gt <- function(team_id_1, team_id_2) {
  tryCatch({

    .team_row <- function(tid) {
      # Standings
      std <- if (exists("team_standings") && nrow(team_standings) > 0)
        team_standings %>% dplyr::filter(mlbam_team_id == tid)
      else
        dplyr::tibble()

      team_name  <- if (nrow(std) > 0) std$team_name[1]  else as.character(tid)
      wins       <- if (nrow(std) > 0) std$wins[1]       else NA_integer_
      losses     <- if (nrow(std) > 0) std$losses[1]     else NA_integer_
      rs         <- if (nrow(std) > 0) std$runs_scored[1] else NA_integer_
      ra         <- if (nrow(std) > 0) std$runs_allowed[1] else NA_integer_
      div_rank   <- if (nrow(std) > 0) std$division_rank[1] else NA_integer_
      run_diff   <- if (nrow(std) > 0) std$run_diff[1]   else NA_integer_
      gp         <- if (nrow(std) > 0) std$games_played[1] else NA_integer_

      record_str <- if (!is.na(wins) && !is.na(losses))
        paste0(wins, "-", losses) else "—"
      rdiff_str  <- if (!is.na(run_diff))
        paste0(if (run_diff >= 0) "+" else "", run_diff) else "—"

      # Team wRC+ (PA-weighted mean from offense_master_season)
      team_wrc <- tryCatch({
        if (!exists("offense_master_season") || nrow(offense_master_season) == 0)
          return(NA_real_)
        team_off <- offense_master_season %>%
          dplyr::filter(
            dplyr::coalesce(mlb_team_id, 0L) == tid |
            dplyr::coalesce(fg_team_id,  0L) == tid
          ) %>%
          dplyr::filter(!is.na(fg_wrc_plus), !is.na(mlb_pa), mlb_pa >= 25)
        if (nrow(team_off) == 0) return(NA_real_)
        round(sum(team_off$fg_wrc_plus * team_off$mlb_pa, na.rm = TRUE) /
              sum(team_off$mlb_pa, na.rm = TRUE))
      }, error = function(e) NA_real_)

      # Team ERA (SP + RP combined, for starters: role == "SP")
      team_era <- tryCatch({
        if (!exists("pitching_master_season") || nrow(pitching_master_season) == 0)
          return(NA_real_)
        team_pit <- pitching_master_season %>%
          dplyr::filter(
            dplyr::coalesce(mlb_team_id, 0L) == tid |
            dplyr::coalesce(fg_team_id,  0L) == tid,
            !is.na(mlb_era), !is.na(mlb_ip), mlb_ip >= 10
          )
        if (nrow(team_pit) == 0) return(NA_real_)
        total_er <- sum(team_pit$mlb_era * team_pit$mlb_ip / 9, na.rm = TRUE)
        total_ip <- sum(team_pit$mlb_ip, na.rm = TRUE)
        if (total_ip == 0) return(NA_real_)
        round(total_er / total_ip * 9, 2)
      }, error = function(e) NA_real_)

      dplyr::tibble(
        Team       = team_name,
        Record     = record_str,
        `Div Rank` = if (!is.na(div_rank)) as.character(div_rank) else "—",
        RS         = if (!is.na(rs)) rs else NA_integer_,
        RA         = if (!is.na(ra)) ra else NA_integer_,
        `Run Diff` = rdiff_str,
        `Team wRC+`= if (!is.na(team_wrc)) as.integer(team_wrc) else NA_integer_,
        `Team ERA` = if (!is.na(team_era)) team_era else NA_real_
      )
    }

    df <- dplyr::bind_rows(.team_row(team_id_1), .team_row(team_id_2))

    df %>%
      gt::gt() %>%
      gt::tab_header(title = "Season at a Glance") %>%
      gt::cols_align(align = "center", columns = -Team) %>%
      gt::cols_align(align = "left",   columns = Team) %>%
      gt::fmt_number(columns = `Team ERA`, decimals = 2) %>%
      gt::sub_missing(missing_text = "\u2014") %>%
      gt::tab_options(
        table.font.size      = 13,
        heading.align        = "left",
        data_row.padding     = gt::px(6),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue") %>%
      gt::data_color(
        columns = `Team wRC+`,
        palette = c("#d6eaf8", "#f8f9fa", "#d5f5e3"),
        domain  = c(85, 115),
        na_color = "white"
      )

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Team overview unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = 12)
  })
}

# ------------------------------------------------------------
# Full roster batting: starters (top 9 PA) + bench (PA >= 25)
# One table per team, header shows opposing SP name if known.
# ------------------------------------------------------------

make_series_batting_gt <- function(team_id, opp_sp_name = NULL) {
  tryCatch({

    if (!exists("offense_master_season") || nrow(offense_master_season) == 0)
      stop("offense_master_season unavailable")

    # Filter to this team's players
    team_off <- offense_master_season %>%
      dplyr::filter(
        dplyr::coalesce(mlb_team_id, 0L) == team_id |
        dplyr::coalesce(fg_team_id,  0L) == team_id
      ) %>%
      dplyr::filter(!is.na(mlb_pa), mlb_pa >= 10) %>%
      # One row per player (highest PA stint for traded players)
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      # Exclude pitchers: position flag or very low PA
      dplyr::filter(
        !dplyr::coalesce(tolower(fg_position), "") %in% c("p", "sp", "rp")
      ) %>%
      dplyr::arrange(dplyr::desc(dplyr::coalesce(mlb_pa, 0L)))

    if (nrow(team_off) == 0) stop("No position players found for team")

    # Tag starters (top 9 PA) vs bench (rest with PA >= 25)
    team_off <- team_off %>%
      dplyr::mutate(
        roster_role = dplyr::if_else(dplyr::row_number() <= 9, "Lineup", "Bench")
      ) %>%
      dplyr::filter(roster_role == "Lineup" | mlb_pa >= 25)

    # Resolve player names
    name_col <- intersect(
      c("fg_name", "player_name", "mlb_player_name", "name_full"),
      names(team_off)
    )[1]
    pos_col <- intersect(
      c("fg_position", "mlb_position", "position"),
      names(team_off)
    )[1]

    df <- team_off %>%
      dplyr::transmute(
        roster_role,
        Name    = if (!is.na(name_col)) .data[[name_col]] else as.character(mlbam_id),
        Pos     = if (!is.na(pos_col))  toupper(.data[[pos_col]]) else "—",
        PA      = dplyr::coalesce(as.integer(mlb_pa), NA_integer_),
        `wRC+`  = dplyr::coalesce(as.integer(fg_wrc_plus), NA_integer_),
        AVG     = dplyr::coalesce(as.numeric(mlb_avg), NA_real_),
        OBP     = dplyr::coalesce(as.numeric(mlb_obp), NA_real_),
        SLG     = dplyr::coalesce(as.numeric(mlb_slg), NA_real_),
        HR      = dplyr::coalesce(as.integer(mlb_hr), NA_integer_),
        ISO     = dplyr::coalesce(as.numeric(fg_iso), NA_real_),
        `BB%`   = dplyr::coalesce(as.numeric(fg_bb_pct), NA_real_),
        `K%`    = dplyr::coalesce(as.numeric(fg_k_pct),  NA_real_)
      )

    subtitle_txt <- if (!is.null(opp_sp_name) && !is.na(opp_sp_name))
      paste0("vs ", opp_sp_name, " \u00b7 2026 season")
    else
      "2026 season"

    tbl <- df %>%
      dplyr::select(-roster_role) %>%
      gt::gt() %>%
      gt::tab_header(subtitle = subtitle_txt) %>%
      gt::tab_row_group(
        label = "Bench",
        rows  = df$roster_role == "Bench"
      ) %>%
      gt::tab_row_group(
        label = "Lineup",
        rows  = df$roster_role == "Lineup"
      ) %>%
      gt::row_group_order(groups = c("Lineup", "Bench")) %>%
      gt::cols_align(align = "center", columns = -Name) %>%
      gt::cols_align(align = "left",   columns = Name) %>%
      gt::fmt_number(columns = c(AVG, OBP, SLG, ISO), decimals = 3) %>%
      gt::fmt_percent(columns = c(`BB%`, `K%`), decimals = 1) %>%
      gt::sub_missing(missing_text = "\u2014") %>%
      gt::tab_options(
        table.font.size           = 12,
        heading.align             = "left",
        data_row.padding          = gt::px(4),
        row_group.font.weight     = "bold",
        row_group.background.color = "#eaf2fb"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    # Color wRC+ where sample is sufficient
    wrc_vals <- df$`wRC+`[!is.na(df$`wRC+`)]
    if (length(wrc_vals) >= 3) {
      tbl <- tbl %>%
        gt::data_color(
          columns  = `wRC+`,
          palette  = c("#d6eaf8", "#f8f9fa", "#d5f5e3"),
          domain   = c(70, 160),
          na_color = "white"
        )
    }

    tbl

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Batting data unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = 12)
  })
}

# ------------------------------------------------------------
# Probable starters table: one row per game in the series
# Shows Tier-1 (confirmed) or TBD for unconfirmed slots.
# ------------------------------------------------------------

make_series_rotation_gt <- function(next_series, next_series_probables) {
  tryCatch({

    ns  <- next_series
    prb <- next_series_probables

    if (is.null(ns) || is.null(prb) || nrow(prb) == 0) {
      return(
        dplyr::tibble(Note = "Probable starters not yet available.") %>%
          gt::gt() %>%
          gt::tab_options(table.font.size = 12)
      )
    }

    # SP season stat lookup helper
    .sp_line <- function(pitcher_id) {
      if (is.na(pitcher_id) || !exists("pitching_master_season") ||
          nrow(pitching_master_season) == 0) return("")
      r <- pitching_master_season %>%
        dplyr::filter(mlbam_id == pitcher_id) %>%
        dplyr::slice(1)
      if (nrow(r) == 0) return("")
      era <- dplyr::coalesce(r$mlb_era[1], NA_real_)
      ip  <- dplyr::coalesce(r$mlb_ip[1],  NA_real_)
      fip <- dplyr::coalesce(r$fg_fip[1],  NA_real_)
      k9  <- dplyr::coalesce(r$fg_k_9[1],  NA_real_)
      parts <- c(
        if (!is.na(era)) paste0("ERA ", sprintf("%.2f", era)),
        if (!is.na(ip))  paste0(sprintf("%.0f", ip), " IP"),
        if (!is.na(fip)) paste0("FIP ", sprintf("%.2f", fip)),
        if (!is.na(k9))  paste0(sprintf("%.1f", k9), " K/9")
      )
      paste(parts, collapse = " \u00b7 ")
    }

    df <- prb %>%
      dplyr::arrange(game_date) %>%
      dplyr::mutate(
        Game        = paste0("Game ", dplyr::row_number()),
        Date        = format(game_date, "%a %b %d"),
        `Away SP`   = dplyr::if_else(
          !is.na(away_pitcher_name), away_pitcher_name, "TBD"
        ),
        `Away Stats`= purrr::map_chr(away_pitcher_mlbam_id, .sp_line),
        `Home SP`   = dplyr::if_else(
          !is.na(home_pitcher_name), home_pitcher_name, "TBD"
        ),
        `Home Stats`= purrr::map_chr(home_pitcher_mlbam_id, .sp_line)
      ) %>%
      dplyr::select(Game, Date, `Away SP`, `Away Stats`, `Home SP`, `Home Stats`)

    df %>%
      gt::gt() %>%
      gt::tab_header(
        title    = "Probable Starters",
        subtitle = paste0(ns$away_team_name, " @ ", ns$home_team_name)
      ) %>%
      gt::tab_spanner(label = ns$away_team_name, columns = c(`Away SP`, `Away Stats`)) %>%
      gt::tab_spanner(label = ns$home_team_name, columns = c(`Home SP`, `Home Stats`)) %>%
      gt::cols_align(align = "left") %>%
      gt::tab_style(
        style = gt::cell_text(color = "#888", font_size = gt::px(11), style = "italic"),
        locations = gt::cells_body(columns = c(`Away Stats`, `Home Stats`))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#888"),
        locations = gt::cells_body(
          columns = c(`Away SP`, `Home SP`),
          rows    = `Away SP` == "TBD" | `Home SP` == "TBD"
        )
      ) %>%
      gt::sub_missing(missing_text = "") %>%
      gt::tab_options(
        table.font.size      = 12,
        heading.align        = "left",
        data_row.padding     = gt::px(6),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Rotation data unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = 12)
  })
}

# ------------------------------------------------------------
# Series bullpen: season stats for one team's relief corps
# ------------------------------------------------------------

make_series_bullpen_gt <- function(team_id) {
  tryCatch({

    if (!exists("pitching_master_season") || nrow(pitching_master_season) == 0)
      stop("pitching_master_season unavailable")

    bp <- pitching_master_season %>%
      dplyr::filter(
        dplyr::coalesce(mlb_team_id, 0L) == team_id |
        dplyr::coalesce(fg_team_id,  0L) == team_id,
        dplyr::coalesce(tolower(fg_position), "") %in% c("rp", "rel", "reliever") |
        (!is.na(mlb_ip) & !is.na(mlb_gs) & mlb_gs == 0 & mlb_ip >= 5)
      ) %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::filter(!is.na(mlb_ip), mlb_ip >= 5) %>%
      dplyr::arrange(dplyr::coalesce(mlb_era, 99))

    if (nrow(bp) == 0) stop("No bullpen arms found")

    name_col <- intersect(
      c("fg_name", "player_name", "mlb_player_name", "name_full"),
      names(bp)
    )[1]

    df <- bp %>%
      dplyr::transmute(
        Name  = if (!is.na(name_col)) .data[[name_col]] else as.character(mlbam_id),
        G     = dplyr::coalesce(as.integer(mlb_g),   NA_integer_),
        IP    = dplyr::coalesce(as.numeric(mlb_ip),  NA_real_),
        ERA   = dplyr::coalesce(as.numeric(mlb_era), NA_real_),
        FIP   = dplyr::coalesce(as.numeric(fg_fip),  NA_real_),
        WHIP  = dplyr::coalesce(as.numeric(mlb_whip),NA_real_),
        `K%`  = dplyr::coalesce(as.numeric(fg_k_pct),NA_real_),
        `BB%` = dplyr::coalesce(as.numeric(fg_bb_pct),NA_real_),
        `K-BB%` = dplyr::if_else(
          !is.na(fg_k_pct) & !is.na(fg_bb_pct),
          fg_k_pct - fg_bb_pct, NA_real_
        )
      ) %>%
      dplyr::slice_head(n = 10)

    tbl <- df %>%
      gt::gt() %>%
      gt::cols_align(align = "center", columns = -Name) %>%
      gt::cols_align(align = "left",   columns = Name) %>%
      gt::fmt_number(columns = c(IP, ERA, FIP, WHIP), decimals = 2) %>%
      gt::fmt_percent(columns = c(`K%`, `BB%`, `K-BB%`), decimals = 1) %>%
      gt::sub_missing(missing_text = "\u2014") %>%
      gt::tab_options(
        table.font.size      = 12,
        heading.align        = "left",
        data_row.padding     = gt::px(4),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 1, color = "blue")

    era_vals <- df$ERA[!is.na(df$ERA)]
    if (length(era_vals) >= 3) {
      tbl <- tbl %>%
        gt::data_color(
          columns  = ERA,
          palette  = c("#d5f5e3", "#f8f9fa", "#f9ebea"),
          domain   = c(2.5, 5.5),
          na_color = "white"
        )
    }

    tbl

  }, error = function(e) {
    dplyr::tibble(Note = paste0("Bullpen data unavailable: ", conditionMessage(e))) %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = 12)
  })
}

# ============================================================
# SERIES RECAP HELPERS
# ============================================================

# ------------------------------------------------------------
# Fetch linescore + boxscore summary for a single game_pk
# Returns a list: score, decisions, top_hitters, top_pitchers
# ------------------------------------------------------------

.fetch_game_summary <- function(gpk) {
  tryCatch({
    bs <- baseballr::mlb_boxscore(gpk)
    if (is.null(bs)) return(NULL)

    # Linescore
    ls_raw <- tryCatch(
      httr::GET(
        sprintf("https://statsapi.mlb.com/api/v1/game/%d/linescore", as.integer(gpk)),
        httr::timeout(15)
      ),
      error = function(e) NULL
    )
    linescore <- tryCatch({
      parsed <- jsonlite::fromJSON(
        httr::content(ls_raw, "text", encoding = "UTF-8"), flatten = TRUE
      )
      list(
        away_runs = as.integer(parsed$teams$away$runs),
        home_runs = as.integer(parsed$teams$home$runs),
        away_hits = as.integer(parsed$teams$away$hits),
        home_hits = as.integer(parsed$teams$home$hits)
      )
    }, error = function(e) list(away_runs = NA, home_runs = NA, away_hits = NA, home_hits = NA))

    list(
      game_pk   = gpk,
      linescore = linescore,
      boxscore  = bs
    )
  }, error = function(e) {
    message("  .fetch_game_summary failed for gpk ", gpk, ": ", e$message)
    NULL
  })
}

# ------------------------------------------------------------
# Game-by-game linescore table for the series recap
# ------------------------------------------------------------

make_series_recap_boxscores <- function(cs) {
  # cs = braves_series_context$current_series
  tryCatch({
    if (is.null(cs) || length(cs$game_pks) == 0)
      stop("No game_pks for current series")

    rows <- purrr::map_dfr(
      seq_along(cs$game_pks),
      function(i) {
        gpk   <- cs$game_pks[i]
        gdate <- cs$game_dates[i]

        gs <- .fetch_game_summary(gpk)

        away_r <- if (!is.null(gs)) gs$linescore$away_runs else NA_integer_
        home_r <- if (!is.null(gs)) gs$linescore$home_runs else NA_integer_
        away_h <- if (!is.null(gs)) gs$linescore$away_hits else NA_integer_
        home_h <- if (!is.null(gs)) gs$linescore$home_hits else NA_integer_

        winner <- dplyr::case_when(
          !is.na(away_r) & !is.na(home_r) & away_r > home_r ~ cs$away_team_name,
          !is.na(away_r) & !is.na(home_r) & home_r > away_r ~ cs$home_team_name,
          TRUE ~ "—"
        )

        score_str <- if (!is.na(away_r) && !is.na(home_r))
          paste0(away_r, " - ", home_r) else "—"

        dplyr::tibble(
          Game   = paste0("Game ", i),
          Date   = format(gdate, "%a %b %d"),
          Score  = score_str,
          `Away` = cs$away_team_name,
          `Home` = cs$home_team_name,
          Winner = winner,
          away_r = away_r,
          home_r = home_r
        )
      }
    )

    # Series result
    braves_w <- sum(
      (rows$away_r > rows$home_r & cs$away_team_id == 144L) |
      (rows$home_r > rows$away_r & cs$home_team_id == 144L),
      na.rm = TRUE
    )
    opp_w <- cs$n_games - braves_w

    result_header <- paste0("Atlanta Braves ", braves_w, "-", opp_w, " vs ", cs$opponent_name)

    list(
      table_data    = rows,
      braves_wins   = braves_w,
      opp_wins      = opp_w,
      result_header = result_header
    )

  }, error = function(e) {
    message("make_series_recap_boxscores failed: ", e$message)
    NULL
  })
}

# ------------------------------------------------------------
# Programmatic 3-paragraph series recap narrative
# Para 1: Series result + overall story
# Para 2: Offensive highlights
# Para 3: Pitching highlights
# ------------------------------------------------------------

make_series_recap_narrative <- function(cs, recap_data) {
  tryCatch({
    if (is.null(recap_data)) return("")

    rows     <- recap_data$table_data
    bw       <- recap_data$braves_wins
    ow       <- recap_data$opp_wins
    opp_name <- cs$opponent_name
    n        <- cs$n_games

    # --- Paragraph 1: result ---
    result_phrase <- dplyr::case_when(
      bw == n  ~ paste0("swept the ", opp_name, " (", bw, "-", ow, ")"),
      ow == n  ~ paste0("were swept by the ", opp_name, " (", bw, "-", ow, ")"),
      bw > ow  ~ paste0("took the series from the ", opp_name, " (", bw, "-", ow, ")"),
      ow > bw  ~ paste0("dropped the series to the ", opp_name, " (", bw, "-", ow, ")"),
      TRUE     ~ paste0("split the series with the ", opp_name, " (", bw, "-", ow, ")")
    )

    # Run differential across series
    total_rs <- sum(
      dplyr::if_else(cs$away_team_id == 144L, rows$away_r, rows$home_r),
      na.rm = TRUE
    )
    total_ra <- sum(
      dplyr::if_else(cs$away_team_id == 144L, rows$home_r, rows$away_r),
      na.rm = TRUE
    )

    # Highest scoring game
    run_totals <- rows$away_r + rows$home_r
    peak_game_idx <- which.max(run_totals)
    peak_game <- if (length(peak_game_idx) > 0)
      paste0("Game ", peak_game_idx, " was the offensive high point with ",
             run_totals[peak_game_idx], " combined runs.")
    else ""

    p1 <- paste0(
      "The Atlanta Braves ", result_phrase, " at ",
      if (cs$braves_are_home) cs$venue_name else paste0(cs$venue_name, " in ", cs$opponent_name, " territory"),
      ". Across the ", n, "-game set, Atlanta scored ", total_rs, " run",
      if (total_rs != 1) "s" else "", " and allowed ", total_ra, ". ",
      peak_game
    )

    # --- Paragraph 2: offensive highlights (placeholder — no per-game batter data) ---
    run_avg  <- if (n > 0) round(total_rs / n, 1) else NA
    off_tone <- dplyr::case_when(
      !is.na(run_avg) & run_avg >= 5.5 ~ "productive",
      !is.na(run_avg) & run_avg >= 4.0 ~ "steady",
      !is.na(run_avg) & run_avg >= 2.5 ~ "inconsistent",
      TRUE ~ "quiet"
    )

    p2 <- paste0(
      "Offensively, Atlanta was ", off_tone, ", averaging ", run_avg, " runs per game. ",
      if (bw > ow)
        "The lineup delivered in key spots when it mattered most. "
      else if (ow > bw)
        "The lineup struggled to sustain pressure over the course of the series. "
      else
        "Offensive production came in bursts rather than consistently. ",
      "Full batting breakdowns for each game are available in the individual game deep dive pages."
    )

    # --- Paragraph 3: pitching highlights ---
    best_game_braves_idx <- which.min(
      dplyr::if_else(cs$away_team_id == 144L, rows$home_r, rows$away_r)
    )
    min_ra <- min(
      dplyr::if_else(cs$away_team_id == 144L, rows$home_r, rows$away_r),
      na.rm = TRUE
    )

    p3 <- paste0(
      "On the mound, Atlanta's best outing came in Game ", best_game_braves_idx,
      ", holding ", opp_name, " to just ", min_ra, " run",
      if (min_ra != 1) "s" else "", ". ",
      if (total_ra / n < 3.5)
        "The staff was largely sharp throughout, limiting the opponent's offense."
      else if (total_ra / n < 5)
        "Pitching was a mixed story — solid at times but unable to consistently shut down the opposition."
      else
        "Pitching struggled across the series, giving up too many runs to keep the Braves competitive."
    )

    # Wrap paragraphs
    paste0(
      '<div style="line-height:1.7; font-size:14px; color:#2c3e50;">',
      '<p>', p1, '</p>',
      '<p>', p2, '</p>',
      '<p>', p3, '</p>',
      '</div>'
    )

  }, error = function(e) {
    paste0('<p style="color:#888; font-style:italic;">',
           'Narrative unavailable: ', conditionMessage(e), '</p>')
  })
}
