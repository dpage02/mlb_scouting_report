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

    # Team defense (OAA/DRS) — one live FanGraphs team-leaderboard pull,
    # shared across both teams. team_name_abb uses FanGraphs' own
    # abbreviation scheme (e.g. "SDP" not "SD"), which can silently
    # mismatch this codebase's team_abbr convention — match on the full
    # team name (endsWith the FG mascot name) instead.
    fg_team_def <- tryCatch({
      season_yr <- if (exists("offense_master_season") && nrow(offense_master_season) > 0)
        as.character(unique(offense_master_season$season)[1]) else as.character(format(Sys.Date(), "%Y"))
      baseballr::fg_team_fielder(startseason = season_yr, endseason = season_yr, qual = "0")
    }, error = function(e) NULL)

    .team_defense <- function(tid) {
      tryCatch({
        if (is.null(fg_team_def) || !exists("team_ids")) return(list(oaa = NA_integer_, drs = NA_integer_))
        full_name <- team_ids %>% dplyr::filter(mlbam_team_id == tid) %>% dplyr::pull(team_name)
        if (length(full_name) == 0) return(list(oaa = NA_integer_, drs = NA_integer_))
        hit <- fg_team_def[endsWith(full_name[1], fg_team_def$team_name), ]
        if (nrow(hit) == 0) return(list(oaa = NA_integer_, drs = NA_integer_))
        list(oaa = as.integer(hit$OAA[1]), drs = as.integer(hit$DRS[1]))
      }, error = function(e) list(oaa = NA_integer_, drs = NA_integer_))
    }

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

      # Resolve team_abbr once — offense_master_season/pitching_master_season
      # have no mlb_team_id/fg_team_id columns, only team_abbr.
      # (function() {...})() wrappers below — a bare tryCatch({..return()..})
      # would otherwise have return() escape .team_row entirely instead of
      # just setting the local variable (return() targets the nearest
      # enclosing function in R, and tryCatch's block isn't one).
      tid_abbr <- tryCatch((function() {
        if (!exists("team_ids")) return(NA_character_)
        r <- team_ids %>% dplyr::filter(mlbam_team_id == tid)
        if (nrow(r) == 0) NA_character_ else r$team_abbr[1]
      })(), error = function(e) NA_character_)

      # Team wRC+ (PA-weighted mean from offense_master_season)
      team_wrc <- tryCatch((function() {
        if (!exists("offense_master_season") || nrow(offense_master_season) == 0 ||
            is.na(tid_abbr))
          return(NA_real_)
        team_off <- offense_master_season %>%
          dplyr::filter(team_abbr == tid_abbr) %>%
          dplyr::filter(!is.na(fg_wRC_plus), !is.na(mlb_pa), mlb_pa >= 25)
        if (nrow(team_off) == 0) return(NA_real_)
        round(sum(team_off$fg_wRC_plus * team_off$mlb_pa, na.rm = TRUE) /
              sum(team_off$mlb_pa, na.rm = TRUE))
      })(), error = function(e) NA_real_)

      # Team ERA (SP + RP combined, for starters: role == "SP")
      team_era <- tryCatch((function() {
        if (!exists("pitching_master_season") || nrow(pitching_master_season) == 0 ||
            is.na(tid_abbr))
          return(NA_real_)
        team_pit <- pitching_master_season %>%
          dplyr::filter(
            team_abbr == tid_abbr,
            !is.na(mlb_era), !is.na(mlb_ip), mlb_ip >= 10
          )
        if (nrow(team_pit) == 0) return(NA_real_)
        total_er <- sum(team_pit$mlb_era * team_pit$mlb_ip / 9, na.rm = TRUE)
        total_ip <- sum(team_pit$mlb_ip, na.rm = TRUE)
        if (total_ip == 0) return(NA_real_)
        round(total_er / total_ip * 9, 2)
      })(), error = function(e) NA_real_)

      # Team Baserunning — BsR (sum, counting stat) + SB/SB% (from raw
      # SB/CS totals, not an average of per-player rates).
      # (function() {...})() wrapper — a bare tryCatch({..return()..}) would
      # otherwise have return() escape .team_row entirely instead of just
      # setting br (return() targets the nearest enclosing function in R).
      br <- tryCatch((function() {
        if (!exists("baserunning_master_season") || nrow(baserunning_master_season) == 0 ||
            is.na(tid_abbr))
          return(list(bsr = NA_real_, sb = NA_integer_, sb_pct = NA_real_))
        team_br <- baserunning_master_season %>% dplyr::filter(team_abbr == tid_abbr)
        if (nrow(team_br) == 0) return(list(bsr = NA_real_, sb = NA_integer_, sb_pct = NA_real_))
        total_sb <- sum(team_br$mlb_sb, na.rm = TRUE)
        total_cs <- sum(team_br$mlb_cs, na.rm = TRUE)
        list(
          bsr    = round(sum(team_br$fg_wBsR, na.rm = TRUE), 1),
          sb     = as.integer(total_sb),
          sb_pct = if ((total_sb + total_cs) > 0) round(total_sb / (total_sb + total_cs), 3) else NA_real_
        )
      })(), error = function(e) list(bsr = NA_real_, sb = NA_integer_, sb_pct = NA_real_))

      def <- .team_defense(tid)

      dplyr::tibble(
        Team       = team_name,
        Record     = record_str,
        `Div Rank` = if (!is.na(div_rank)) as.character(div_rank) else "—",
        RS         = if (!is.na(rs)) rs else NA_integer_,
        RA         = if (!is.na(ra)) ra else NA_integer_,
        `Run Diff` = rdiff_str,
        `Team wRC+`= if (!is.na(team_wrc)) as.integer(team_wrc) else NA_integer_,
        `Team ERA` = if (!is.na(team_era)) team_era else NA_real_,
        `Team BsR` = br$bsr,
        `Team SB`  = br$sb,
        `Team SB%` = br$sb_pct,
        `Team OAA` = def$oaa,
        `Team DRS` = def$drs
      )
    }

    df <- dplyr::bind_rows(.team_row(team_id_1), .team_row(team_id_2))

    df %>%
      gt::gt() %>%
      gt::tab_header(title = "Season at a Glance") %>%
      gt::cols_align(align = "center", columns = -Team) %>%
      gt::cols_align(align = "left",   columns = Team) %>%
      gt::fmt_number(columns = `Team ERA`, decimals = 2) %>%
      gt::fmt_number(columns = `Team BsR`, decimals = 1) %>%
      gt::fmt_percent(columns = `Team SB%`, decimals = 1) %>%
      gt::sub_missing(missing_text = "\u2014") %>%
      gt::tab_spanner(label = "Offense",     columns = `Team wRC+`) %>%
      gt::tab_spanner(label = "Pitching",    columns = `Team ERA`) %>%
      gt::tab_spanner(label = "Baserunning", columns = c(`Team BsR`, `Team SB`, `Team SB%`)) %>%
      gt::tab_spanner(label = "Defense",     columns = c(`Team OAA`, `Team DRS`)) %>%
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
      ) %>%
      gt::data_color(
        columns = `Team BsR`,
        palette = c("#d6eaf8", "#f8f9fa", "#d5f5e3"),
        domain  = c(-10, 10),
        na_color = "white"
      ) %>%
      gt::data_color(
        columns = `Team OAA`,
        palette = c("#d6eaf8", "#f8f9fa", "#d5f5e3"),
        domain  = c(-15, 15),
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

    # offense_master_season has no mlb_team_id/fg_team_id column, only
    # team_abbr — resolve it from the MLBAM team_id via team_ids.
    team_abbr_match <- if (exists("team_ids")) {
      r <- team_ids %>% dplyr::filter(mlbam_team_id == team_id)
      if (nrow(r) == 0) NA_character_ else r$team_abbr[1]
    } else NA_character_
    if (is.na(team_abbr_match)) stop("Could not resolve team_abbr for team_id ", team_id)
    team_name_match <- if (exists("team_ids")) {
      r <- team_ids %>% dplyr::filter(mlbam_team_id == team_id)
      if (nrow(r) == 0) team_abbr_match else r$team_name[1]
    } else team_abbr_match

    # Filter to this team's players
    team_off <- offense_master_season %>%
      dplyr::filter(team_abbr == team_abbr_match) %>%
      dplyr::filter(!is.na(mlb_pa), mlb_pa >= 10) %>%
      # One row per player (highest PA stint for traded players)
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      # Exclude pitchers: position flag or very low PA
      dplyr::filter(
        !dplyr::coalesce(tolower(sc_position), "") %in% c("p", "sp", "rp")
      ) %>%
      dplyr::arrange(dplyr::desc(dplyr::coalesce(mlb_pa, 0L)))

    if (nrow(team_off) == 0) stop("No position players found for team")

    # Tag starters (top 9 PA) vs bench (rest with PA >= 25)
    team_off <- team_off %>%
      dplyr::mutate(
        roster_role = dplyr::if_else(dplyr::row_number() <= 9, "Lineup", "Bench")
      ) %>%
      dplyr::filter(roster_role == "Lineup" | mlb_pa >= 25)

    # offense_master_season's own name columns (lahman_player_name, etc.) are
    # unpopulated in this pipeline. player_master_ids is the primary
    # mlbam_id -> name lookup used elsewhere, but it's built from a
    # crosswalk that lags behind on very recent call-ups; depth_charts
    # (FanGraphs) has since proven more complete for those stragglers.
    # Row-level coalesce (not column-level intersect) so each player falls
    # back independently rather than one missing column blanking everyone.
    team_off$.pmi_name <- NA_character_
    if (exists("player_master_ids")) {
      pmi_lookup <- player_master_ids %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, player_name)
      team_off$.pmi_name <- pmi_lookup$player_name[match(team_off$mlbam_id, pmi_lookup$mlbam_id)]
    }
    team_off$.dc_name <- NA_character_
    if (exists("depth_charts")) {
      dc_lookup <- depth_charts %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, player_name)
      team_off$.dc_name <- dc_lookup$player_name[match(team_off$mlbam_id, dc_lookup$mlbam_id)]
    }
    team_off$.resolved_name <- dplyr::coalesce(team_off$.pmi_name, team_off$.dc_name)

    pos_col <- intersect(
      c("fg_position", "mlb_position", "position", "sc_position"),
      names(team_off)
    )[1]

    # Resolve wOBA column (FanGraphs normalizes to fg_wOBA after prefix)
    woba_col <- intersect(c("fg_wOBA", "fg_woba"), names(team_off))[1]

    df <- team_off %>%
      dplyr::transmute(
        roster_role,
        Name    = dplyr::coalesce(.resolved_name, as.character(mlbam_id)),
        Pos     = if (!is.na(pos_col))  toupper(.data[[pos_col]]) else "—",
        PA      = dplyr::coalesce(as.integer(mlb_pa), NA_integer_),
        HR      = dplyr::coalesce(as.integer(mlb_hr), NA_integer_),
        RBI     = dplyr::coalesce(as.integer(mlb_rbi), NA_integer_),
        `wRC+`  = dplyr::coalesce(as.integer(fg_wRC_plus), NA_integer_),
        wOBA    = if (!is.na(woba_col)) dplyr::coalesce(as.numeric(.data[[woba_col]]), NA_real_) else NA_real_,
        AVG     = dplyr::coalesce(as.numeric(mlb_avg), NA_real_),
        OBP     = dplyr::coalesce(as.numeric(mlb_obp), NA_real_),
        SLG     = dplyr::coalesce(as.numeric(mlb_slg), NA_real_),
        `BB%`   = dplyr::coalesce(as.numeric(fg_BB_pct), NA_real_),
        `K%`    = dplyr::coalesce(as.numeric(fg_K_pct),  NA_real_)
      )

    subtitle_txt <- if (!is.null(opp_sp_name) && !is.na(opp_sp_name))
      paste0("vs ", opp_sp_name, " \u00b7 2026 season")
    else
      "2026 season"

    tbl <- df %>%
      dplyr::select(-roster_role) %>%
      gt::gt() %>%
      gt::tab_header(
        title    = gt::md(paste0("**", team_name_match, "**")),
        subtitle = subtitle_txt
      ) %>%
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
      gt::fmt_number(columns = dplyr::any_of(c("wOBA", "AVG", "OBP", "SLG")), decimals = 3) %>%
      gt::fmt_percent(columns = dplyr::any_of(c("BB%", "K%")), decimals = 1) %>%
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
      fip <- dplyr::coalesce(r$fg_FIP[1],  NA_real_)
      k9  <- dplyr::coalesce(r$fg_K_per_9[1],  NA_real_)
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
        style = gt::cell_text(color = "#888", size = gt::px(11), style = "italic"),
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

    # pitching_master_season has no mlb_team_id/fg_team_id column, only
    # team_abbr — resolve it from the MLBAM team_id via team_ids.
    team_abbr_match <- if (exists("team_ids")) {
      r <- team_ids %>% dplyr::filter(mlbam_team_id == team_id)
      if (nrow(r) == 0) NA_character_ else r$team_abbr[1]
    } else NA_character_
    if (is.na(team_abbr_match)) stop("Could not resolve team_abbr for team_id ", team_id)

    # pitching_master_season has no position column — games-started == 0
    # with meaningful innings is a sufficient reliever definition on its own.
    bp <- pitching_master_season %>%
      dplyr::filter(
        team_abbr == team_abbr_match,
        !is.na(mlb_ip), !is.na(mlb_gs), mlb_gs == 0, mlb_ip >= 5
      ) %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::filter(!is.na(mlb_ip), mlb_ip >= 5) %>%
      dplyr::arrange(dplyr::coalesce(mlb_era, 99))

    if (nrow(bp) == 0) stop("No bullpen arms found")

    # pitching_master_season's own player_name column is unpopulated in this
    # pipeline. player_master_ids lags behind on very recent call-ups;
    # depth_charts (FanGraphs) has proven more complete for those stragglers.
    # Row-level coalesce so each player falls back independently.
    bp$.pmi_name <- NA_character_
    if (exists("player_master_ids")) {
      pmi_lookup <- player_master_ids %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, player_name)
      bp$.pmi_name <- pmi_lookup$player_name[match(bp$mlbam_id, pmi_lookup$mlbam_id)]
    }
    bp$.dc_name <- NA_character_
    if (exists("depth_charts")) {
      dc_lookup <- depth_charts %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, player_name)
      bp$.dc_name <- dc_lookup$player_name[match(bp$mlbam_id, dc_lookup$mlbam_id)]
    }
    bp$.resolved_name <- dplyr::coalesce(bp$.pmi_name, bp$.dc_name)

    df <- bp %>%
      dplyr::transmute(
        Name  = dplyr::coalesce(.resolved_name, as.character(mlbam_id)),
        G     = dplyr::coalesce(as.integer(mlb_g),   NA_integer_),
        IP    = dplyr::coalesce(as.numeric(mlb_ip),  NA_real_),
        ERA   = dplyr::coalesce(as.numeric(mlb_era), NA_real_),
        FIP   = dplyr::coalesce(as.numeric(fg_FIP),  NA_real_),
        WHIP  = dplyr::coalesce(as.numeric(mlb_whip),NA_real_),
        `K%`  = dplyr::coalesce(as.numeric(fg_K_pct),NA_real_),
        `BB%` = dplyr::coalesce(as.numeric(fg_BB_pct),NA_real_),
        `K-BB%` = dplyr::if_else(
          !is.na(fg_K_pct) & !is.na(fg_BB_pct),
          fg_K_pct - fg_BB_pct, NA_real_
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

# ------------------------------------------------------------
# Divisional standings for both teams' divisions
# ------------------------------------------------------------

make_series_standings_html <- function(team_id_1, team_id_2) {
  tryCatch({
    if (!exists("team_standings") || nrow(team_standings) == 0)
      return('<p style="color:#888;font-style:italic;">Standings unavailable.</p>')

    # Division name lookup (MLB standard IDs)
    .div_name <- function(did) {
      switch(as.character(did),
        "200" = "AL West",  "201" = "AL East",  "202" = "AL Central",
        "203" = "NL West",  "204" = "NL East",  "205" = "NL Central",
        paste0("Division ", did)
      )
    }

    div_of <- function(tid) {
      r <- team_standings %>% dplyr::filter(mlbam_team_id == tid)
      if (nrow(r) > 0 && "division_id" %in% names(r)) r$division_id[1] else NA_integer_
    }

    divs_to_show <- unique(na.omit(c(div_of(team_id_1), div_of(team_id_2))))
    if (length(divs_to_show) == 0)
      return('<p style="color:#888;font-style:italic;">Standings unavailable.</p>')

    .div_block <- function(did) {
      div_teams <- team_standings %>%
        dplyr::filter(division_id == did) %>%
        dplyr::arrange(division_rank)
      if (nrow(div_teams) == 0) return("")

      rows_html <- paste(
        vapply(seq_len(nrow(div_teams)), function(i) {
          r          <- div_teams[i, ]
          highlight  <- r$mlbam_team_id %in% c(team_id_1, team_id_2)
          gb_val     <- suppressWarnings(as.numeric(r$games_back))
          gb_str     <- if (!is.na(gb_val) && gb_val == 0) "—"
                        else if (!is.na(gb_val)) paste0("-", gb_val)
                        else dplyr::coalesce(as.character(r$games_back), "—")
          wl_str     <- paste0(
            dplyr::coalesce(as.character(r$wins), "—"), "-",
            dplyr::coalesce(as.character(r$losses), "—")
          )
          bg <- if (highlight) "background:#eaf2ff;" else ""
          fw <- if (highlight) "font-weight:700;" else ""
          paste0(
            '<tr style="', bg, '">',
            # &nbsp; (not a literal space) after the period — pandoc's
            # native_divs parses raw HTML container contents as markdown,
            # and "1. Team Name" with a real space is indistinguishable
            # from a markdown ordered-list marker, corrupting any
            # surrounding fenced div (only surfaces inside a panel-tabset;
            # this table also renders standalone on Series Preview, where
            # there's no fenced div for it to break).
            '<td style="padding:3px 10px;', fw, '">', r$division_rank, '.&nbsp;', r$team_name, '</td>',
            '<td style="padding:3px 10px;text-align:center;', fw, '">', wl_str, '</td>',
            '<td style="padding:3px 10px;text-align:center;color:#555;">', gb_str, '</td>',
            '</tr>'
          )
        }, character(1)),
        collapse = ""
      )

      paste0(
        '<div style="flex:1;min-width:200px;">',
        '<div style="font-size:11px;text-transform:uppercase;letter-spacing:0.8px;',
        'color:#888;font-weight:600;margin-bottom:6px;">', .div_name(did), '</div>',
        '<table style="width:100%;border-collapse:collapse;font-size:13px;">',
        '<thead><tr style="border-bottom:1px solid #dee2e6;">',
        '<th style="padding:3px 10px;text-align:left;color:#888;font-weight:600;">Team</th>',
        '<th style="padding:3px 10px;text-align:center;color:#888;font-weight:600;">W-L</th>',
        '<th style="padding:3px 10px;text-align:center;color:#888;font-weight:600;">GB</th>',
        '</tr></thead>',
        '<tbody>', rows_html, '</tbody>',
        '</table></div>'
      )
    }

    blocks <- paste(vapply(divs_to_show, .div_block, character(1)), collapse = "")
    paste0('<div style="display:flex;flex-wrap:wrap;gap:2rem;">', blocks, '</div>')

  }, error = function(e) {
    paste0('<p style="color:#888;font-style:italic;">Standings unavailable: ', conditionMessage(e), '</p>')
  })
}

# ------------------------------------------------------------
# Pitch arsenal cards for all probable starters in a series
# Shows top 5 pitches: name, usage bar, velocity, whiff%
# ------------------------------------------------------------

make_series_arsenal_html <- function(next_series_probables) {
  tryCatch({
    prb <- next_series_probables
    if (is.null(prb) || nrow(prb) == 0 ||
        !exists("pitcher_arsenal") || nrow(pitcher_arsenal) == 0)
      return('<p style="color:#888;font-style:italic;">Pitch arsenal data unavailable.</p>')

    # Collect all confirmed probable pitchers with names
    all_pitchers <- dplyr::bind_rows(
      dplyr::transmute(prb,
        mlbam_id = away_pitcher_mlbam_id,
        name     = away_pitcher_name,
        game_date
      ),
      dplyr::transmute(prb,
        mlbam_id = home_pitcher_mlbam_id,
        name     = home_pitcher_name,
        game_date
      )
    ) %>%
      dplyr::filter(!is.na(mlbam_id)) %>%
      dplyr::arrange(game_date, mlbam_id) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

    if (nrow(all_pitchers) == 0)
      return('<p style="color:#888;font-style:italic;">No confirmed probable starters yet.</p>')

    .pitch_color <- function(code) {
      switch(toupper(code),
        "FF" = , "FA" = "#2980b9",
        "SI" = , "FT" = "#1a6696",
        "FC" = "#e67e22",
        "FS" = "#16a085",
        "SL" = "#c0392b",
        "ST" = "#d35400",
        "SV" = "#a04000",
        "CU" = , "KC" = "#8e44ad",
        "CH" = "#27ae60",
        "KN" = "#f39c12",
        "#7f8c8d"
      )
    }

    .pitcher_card <- function(pid, pname) {
      arsenal <- pitcher_arsenal %>%
        dplyr::filter(mlbam_id == pid) %>%
        dplyr::arrange(dplyr::desc(dplyr::coalesce(usage_pct, 0))) %>%
        dplyr::slice_head(n = 5)

      if (nrow(arsenal) == 0)
        return(paste0(
          '<div style="flex:1;min-width:260px;max-width:340px;background:#fafbfc;',
          'border:1px solid #dee2e6;border-radius:8px;padding:14px 16px;">',
          '<div style="font-size:14px;font-weight:700;color:#1a2a4a;margin-bottom:8px;">',
          dplyr::coalesce(pname, paste0("Pitcher ", pid)), '</div>',
          '<div style="color:#888;font-style:italic;font-size:12px;">Arsenal not available</div>',
          '</div>'
        ))

      pitch_rows <- paste(
        vapply(seq_len(nrow(arsenal)), function(i) {
          a         <- arsenal[i, ]
          nm        <- dplyr::coalesce(a$pitch_name, a$pitch_code, "?")
          usage_pct <- dplyr::coalesce(a$usage_pct, 0)
          velo      <- if ("avg_speed"  %in% names(a)) a$avg_speed  else NA_real_
          whiff     <- if ("whiff_pct"  %in% names(a)) a$whiff_pct  else NA_real_

          bar_w   <- round(usage_pct * 100)
          bar_col <- .pitch_color(dplyr::coalesce(a$pitch_code, ""))

          velo_str  <- if (!is.na(velo))  paste0(round(velo, 1), " mph") else "—"
          usage_str <- paste0(round(usage_pct * 100, 0), "%")
          whiff_str <- if (!is.na(whiff)) paste0(round(whiff * 100, 0), "%") else ""

          paste0(
            '<div style="display:flex;align-items:center;gap:6px;',
            'padding:4px 0;border-bottom:1px solid #f0f0f0;">',
            '<div style="width:100px;font-size:12px;font-weight:600;color:#2c3e50;',
            'white-space:nowrap;">', nm, '</div>',
            '<div style="flex:1;height:6px;background:#eee;border-radius:3px;overflow:hidden;">',
            '<div style="width:', bar_w, '%;height:100%;background:', bar_col,
            ';border-radius:3px;"></div></div>',
            '<div style="width:32px;text-align:right;font-size:12px;font-weight:600;">',
            usage_str, '</div>',
            '<div style="width:54px;text-align:right;font-size:12px;color:#555;">',
            velo_str, '</div>',
            if (nchar(whiff_str) > 0)
              paste0('<div style="width:40px;text-align:right;font-size:11px;color:#888;">',
                     whiff_str, '</div>')
            else '',
            '</div>'
          )
        }, character(1)),
        collapse = ""
      )

      paste0(
        '<div style="flex:1;min-width:260px;max-width:360px;background:#fafbfc;',
        'border:1px solid #dee2e6;border-radius:8px;padding:14px 16px;">',
        '<div style="font-size:14px;font-weight:700;color:#1a2a4a;margin-bottom:6px;">',
        dplyr::coalesce(pname, paste0("Pitcher ", pid)), '</div>',
        '<div style="display:flex;font-size:10px;color:#aaa;font-weight:600;',
        'margin-bottom:4px;padding-bottom:3px;border-bottom:1px solid #eee;">',
        '<span style="width:100px;">Pitch</span>',
        '<span style="flex:1;"></span>',
        '<span style="width:32px;text-align:right;">Use%</span>',
        '<span style="width:54px;text-align:right;">Velo</span>',
        '<span style="width:40px;text-align:right;">Wh%</span>',
        '</div>',
        pitch_rows,
        '</div>'
      )
    }

    cards <- paste(
      vapply(seq_len(nrow(all_pitchers)), function(i) {
        .pitcher_card(all_pitchers$mlbam_id[i], all_pitchers$name[i])
      }, character(1)),
      collapse = ""
    )

    paste0('<div style="display:flex;flex-wrap:wrap;gap:1rem;">', cards, '</div>')

  }, error = function(e) {
    paste0('<p style="color:#888;font-style:italic;">Arsenal unavailable: ', conditionMessage(e), '</p>')
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

    # Standout performances — MLB's own pre-computed top-performer summaries
    # (hitter + starter box-score lines), used to ground the narrative in
    # what actually happened rather than generic result-tone filler.
    standouts <- tryCatch({
      box_raw <- httr::GET(
        sprintf("https://statsapi.mlb.com/api/v1/game/%d/boxscore", as.integer(gpk)),
        httr::timeout(15)
      )
      box <- jsonlite::fromJSON(
        httr::content(box_raw, "text", encoding = "UTF-8"),
        simplifyVector = TRUE, simplifyDataFrame = FALSE, flatten = FALSE
      )
      tp <- box$topPerformers
      if (length(tp) == 0) return(list(hitter = NULL, starter = NULL))
      hitter_note  <- NULL
      starter_note <- NULL
      for (p in tp) {
        nm   <- p$player$person$fullName
        if (is.null(nm)) next
        if (is.null(hitter_note) && p$type %in% c("hitter", "batter")) {
          summ <- p$player$stats$batting$summary
          if (!is.null(summ)) hitter_note <- paste0(nm, " (", summ, ")")
        }
        if (is.null(starter_note) && p$type == "starter") {
          summ <- p$player$stats$pitching$summary
          if (!is.null(summ)) starter_note <- paste0(nm, " (", summ, ")")
        }
      }
      list(hitter = hitter_note, starter = starter_note)
    }, error = function(e) list(hitter = NULL, starter = NULL))

    list(
      game_pk   = gpk,
      linescore = linescore,
      standouts = standouts
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

        standout_hitter  <- if (!is.null(gs) && !is.null(gs$standouts$hitter))
          gs$standouts$hitter else NA_character_
        standout_pitcher <- if (!is.null(gs) && !is.null(gs$standouts$starter))
          gs$standouts$starter else NA_character_

        dplyr::tibble(
          Game   = paste0("Game ", i),
          Date   = format(gdate, "%a %b %d"),
          Score  = score_str,
          `Away` = cs$away_team_name,
          `Home` = cs$home_team_name,
          Winner = winner,
          away_r = away_r,
          home_r = home_r,
          standout_hitter  = standout_hitter,
          standout_pitcher = standout_pitcher
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

    # Run differential across series — cs$away_team_id is a single value for
    # the whole series (side doesn't change game-to-game), so this is a
    # scalar branch, not a row-wise one; dplyr::if_else requires matching
    # sizes and errors here since rows$away_r/home_r are per-game vectors.
    braves_r <- if (cs$away_team_id == 144L) rows$away_r else rows$home_r
    opp_r    <- if (cs$away_team_id == 144L) rows$home_r else rows$away_r
    total_rs <- sum(braves_r, na.rm = TRUE)
    total_ra <- sum(opp_r, na.rm = TRUE)

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

    # --- Paragraph 2: offensive highlights — real standout hitter per game
    # (MLB's own top-performer box-score line) rather than generic tone
    # filler ---
    run_avg  <- if (n > 0) round(total_rs / n, 1) else NA
    off_tone <- dplyr::case_when(
      !is.na(run_avg) & run_avg >= 5.5 ~ "productive",
      !is.na(run_avg) & run_avg >= 4.0 ~ "steady",
      !is.na(run_avg) & run_avg >= 2.5 ~ "inconsistent",
      TRUE ~ "quiet"
    )

    hitter_lines <- purrr::map_chr(seq_len(nrow(rows)), function(i) {
      h <- rows$standout_hitter[i]
      if (is.na(h)) return("")
      paste0("Game ", i, "'s top performer at the plate was ", h, ".")
    })
    hitter_lines <- hitter_lines[nchar(hitter_lines) > 0]
    hitter_summary <- if (length(hitter_lines) > 0)
      paste(hitter_lines, collapse = " ")
    else
      "Full batting breakdowns for each game are available in the individual game deep dive pages."

    p2 <- paste0(
      "Offensively, Atlanta was ", off_tone, ", averaging ", run_avg, " runs per game. ",
      hitter_summary
    )

    # --- Paragraph 3: pitching highlights — real standout starter per game,
    # same box-score-line approach as the batting side ---
    best_game_braves_idx <- which.min(opp_r)
    min_ra <- min(opp_r, na.rm = TRUE)

    pitcher_lines <- purrr::map_chr(seq_len(nrow(rows)), function(i) {
      p <- rows$standout_pitcher[i]
      if (is.na(p)) return("")
      paste0("Game ", i, "'s standout arm was ", p, ".")
    })
    pitcher_lines <- pitcher_lines[nchar(pitcher_lines) > 0]
    pitcher_summary <- if (length(pitcher_lines) > 0)
      paste(pitcher_lines, collapse = " ")
    else if (total_ra / n < 3.5)
      "The staff was largely sharp throughout, limiting the opponent's offense."
    else if (total_ra / n < 5)
      "Pitching was a mixed story — solid at times but unable to consistently shut down the opposition."
    else
      "Pitching struggled across the series, giving up too many runs to keep the Braves competitive."

    p3 <- paste0(
      "On the mound, Atlanta's best outing came in Game ", best_game_braves_idx,
      ", holding ", opp_name, " to just ", min_ra, " run",
      if (min_ra != 1) "s" else "", ". ",
      pitcher_summary
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
