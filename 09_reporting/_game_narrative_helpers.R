# ============================================================
# mlb_scouting_report
# GAME NARRATIVE + PREDICTION HELPERS
# SCRIPT: _game_narrative_helpers.R
# ============================================================
# PURPOSE:
#   Generate stat-driven narrative bullets (main page) and
#   a prediction model section (game deep dive).
#
# FUNCTIONS:
#   make_game_bullets_html(gpk)   — <ul> bullets for main page
#   make_game_preview_html(gpk)   — prose paragraph for deep dive top
#   make_prediction_html(gpk)     — projection table + factors
#
# MODEL:
#   Expected runs = LEAGUE_AVG_RUNS
#                   × (lineup avg wRC+ / 100)
#                   × (park_factor)
#                   × (LEAGUE_AVG_FIP / opposing blended pitcher FIP)
#
#   Blended pitcher FIP = SP_frac × SP_xFIP  +  BP_frac × bullpen_ERA
#     SP_frac = min(SP_IP_per_GS / 9, 0.85)
#     BP_frac = 1 - SP_frac
#
#   Home field advantage: +0.02 run residual only (empirical HFA in
#   run differential is ~0 over 2022-2024; 52.5% home win rate comes
#   from last-at-bat tactical edge, not raw run scoring).
#
#   Win probability via Poisson distribution.
#   Constants calibrated to 2022-2024 empirical averages.
# ============================================================

library(dplyr)

# ---- Constants (empirically calibrated 2022-2024, n=7,300 games) ----
LEAGUE_AVG_RUNS  <- 4.43   # empirical MLB avg runs/team/game 2022-2024
LEAGUE_AVG_FIP   <- 4.15   # empirical avg SP xFIP 2022-2024
HOME_FIELD_BONUS <- 0.02   # residual last-at-bat/crowd advantage (run differential ≈0 empirically)
BLEND_GS_FLOOR   <- 15L    # GS needed to fully trust current-season stats; blend with prior below this

# ============================================================
# Internal helpers
# ============================================================

.get_num <- function(row, col, default = NA_real_) {
  if (col %in% names(row) && length(row[[col]]) > 0) {
    v <- suppressWarnings(as.numeric(row[[col]][1]))
    if (is.finite(v)) v else default
  } else default
}

.era_plus_val <- function(row) {
  bbref <- .get_num(row, "bbref_ERA_plus")
  fg_em <- .get_num(row, "fg_ERA_minus")
  val   <- dplyr::coalesce(
    bbref,
    if (!is.na(fg_em) && fg_em > 0) round(10000 / fg_em) else NA_real_
  )
  if (!is.na(val) && is.finite(val)) val else NA_real_
}

.best_fip_val <- function(row) {
  # Prefer xFIP > SIERA > FIP > ERA (best to worst regressor)
  dplyr::coalesce(
    .get_num(row, "fg_xFIP"),
    .get_num(row, "fg_SIERA"),
    .get_num(row, "fg_FIP"),
    .get_num(row, "mlb_era"),
    LEAGUE_AVG_FIP
  )
}

.fip_label_used <- function(row) {
  if (nrow(row) == 0) return("Lg Avg")
  if (!is.na(.get_num(row, "fg_xFIP")))  return("xFIP")
  if (!is.na(.get_num(row, "fg_SIERA"))) return("SIERA")
  if (!is.na(.get_num(row, "fg_FIP")))   return("FIP")
  if (!is.na(.get_num(row, "mlb_era")))  return("ERA")
  "Lg Avg"
}

# Blended FIP: linear weight between prior-season xFIP and current-season FIP.
# At 0 GS → 100% prior; at BLEND_GS_FLOOR GS → 100% current; linear between.
# Falls back to current-only if no prior data exists.
.blended_fip <- function(row) {
  cur_fip   <- .best_fip_val(row)
  cur_gs    <- .get_num(row, "mlb_gs")
  prior_fip <- dplyr::coalesce(.get_num(row, "prior_xfip"), .get_num(row, "prior_era"))

  if (is.na(prior_fip))                          return(cur_fip)
  if (is.na(cur_gs) || cur_gs >= BLEND_GS_FLOOR) return(cur_fip)

  w_cur <- cur_gs / BLEND_GS_FLOOR
  w_cur * cur_fip + (1 - w_cur) * prior_fip
}

# Convert a FIP-like value to an ERA+-like index (100 = league avg, higher = better).
.fip_to_era_plus <- function(fip) {
  if (is.na(fip) || fip <= 0) return(NA_real_)
  round(LEAGUE_AVG_FIP / fip * 100)
}

# ERA+ equivalent using blended stats — used for quality label logic only.
.blended_era_plus <- function(row) {
  .fip_to_era_plus(.blended_fip(row))
}

# Trend note: compares current ERA+ to prior-season ERA+ equivalent.
# Returns "hot start", "slow start", or NULL when sample is sufficient.
# Thresholds: +30 above history = hot start; -25 below history = slow start.
# Only fires when GS < BLEND_GS_FLOOR (otherwise current stats speak for themselves).
.sp_trend_note <- function(row) {
  cur_ep    <- .era_plus_val(row)
  cur_gs    <- .get_num(row, "mlb_gs")
  prior_fip <- dplyr::coalesce(.get_num(row, "prior_xfip"), .get_num(row, "prior_era"))

  if (is.na(cur_ep) || is.na(prior_fip) || is.na(cur_gs)) return(NULL)
  if (cur_gs >= BLEND_GS_FLOOR) return(NULL)

  prior_ep <- .fip_to_era_plus(prior_fip)
  if (is.na(prior_ep)) return(NULL)

  delta <- cur_ep - prior_ep

  if      (delta >=  30) "hot start"
  else if (delta <= -25 && prior_ep >= 110) "slow start \u2014 established track record"
  else if (delta <= -25)                    "slow start"
  else NULL
}

.era_plus_tier <- function(x) {
  if (is.na(x)) return(NULL)
  dplyr::case_when(x >= 150 ~ "elite", x >= 130 ~ "excellent",
                   x >= 110 ~ "above-average", x >= 90 ~ "average",
                   TRUE ~ "below-average")
}

.wrc_tier <- function(x) {
  if (is.na(x) || is.nan(x)) return("average")
  dplyr::case_when(x >= 120 ~ "elite", x >= 110 ~ "above-average",
                   x >= 95  ~ "average", TRUE ~ "below-average")
}

.fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("—")
  paste0(round(x * 100, digits), "%")
}

# Poisson win probability: P(home score > away score)
.poisson_win_prob <- function(lambda_home, lambda_away) {
  r  <- 0:25
  ph <- dpois(r, max(lambda_home, 0.01))
  pa <- dpois(r, max(lambda_away, 0.01))
  p_home_wins <- sum(vapply(seq_along(r), function(i) {
    h <- r[i]
    if (h == 0) return(0)
    ph[i] * sum(pa[seq_len(h)])   # P(H=h) * P(A < h)
  }, numeric(1)))
  p_tie <- sum(ph * pa)
  p_home_wins + 0.5 * p_tie      # ties split 50/50 (extra innings)
}

# Team lineup wRC+ (mean of available values, fallback 100 = league avg)
.team_wrc <- function(lineup_rows) {
  vals <- lineup_rows$fg_wRC_plus[!is.na(lineup_rows$fg_wRC_plus)]
  if (length(vals) < 3) 100 else mean(vals)
}

# ============================================================
# make_game_bullets_html(gpk)
# 3-5 quick-hit bullets for the main page game summary row
# ============================================================

make_game_bullets_html <- function(gpk) {
  game    <- game_context   %>% dplyr::filter(game_pk == gpk)
  sps     <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup  <- lineup_context  %>% dplyr::filter(game_pk == gpk)
  bullpen <- bullpen_grid    %>% dplyr::filter(game_pk == gpk)

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"

  bullets <- character(0)

  # ---- 1. Pitching matchup ----
  away_ep <- if (nrow(away_sp) > 0) .era_plus_val(away_sp) else NA_real_
  home_ep <- if (nrow(home_sp) > 0) .era_plus_val(home_sp) else NA_real_
  away_gs <- if (nrow(away_sp) > 0) .get_num(away_sp, "mlb_gs") else NA_real_
  home_gs <- if (nrow(home_sp) > 0) .get_num(home_sp, "mlb_gs") else NA_real_

  # Blended ERA+ equivalent: weights current season vs prior season by GS.
  # Stabilizes quality label early in the season (e.g. ace off to rough start).
  away_blep <- if (nrow(away_sp) > 0) .blended_era_plus(away_sp) else NA_real_
  home_blep <- if (nrow(home_sp) > 0) .blended_era_plus(home_sp) else NA_real_

  # Display string: show current ERA+ with prior-season context when sample is small.
  # Format: Name (ERA+ X · N GS · YYYY xFIP X.XX — trend note)
  # trend note = "hot start" | "slow start — established track record" | omitted
  .sp_matchup_str <- function(nm, ep, gs, sp_row) {
    parts <- character(0)
    if (!is.na(ep)) parts <- c(parts, paste0("ERA+ ", round(ep)))

    small_sample <- !is.na(gs) && gs < BLEND_GS_FLOOR
    if (small_sample) {
      if (!is.na(gs)) parts <- c(parts, paste0(round(gs), " GS"))
      prior_fip <- if (nrow(sp_row) > 0)
        dplyr::coalesce(.get_num(sp_row, "prior_xfip"), .get_num(sp_row, "prior_era"))
      else NA_real_
      prior_yr  <- if (nrow(sp_row) > 0) .get_num(sp_row, "prior_season") else NA_real_
      if (!is.na(prior_fip)) {
        yr_str <- if (!is.na(prior_yr)) paste0(round(prior_yr), " ") else ""
        parts  <- c(parts, paste0(yr_str, "xFIP ", round(prior_fip, 2)))
      }
    }

    if (length(parts) == 0) return(nm)

    trend <- if (nrow(sp_row) > 0) .sp_trend_note(sp_row) else NULL
    suffix <- if (!is.null(trend)) paste0(" \u2014 ", trend) else ""

    paste0(nm, " (", paste(parts, collapse = " \u00b7 "), suffix, ")")
  }

  away_str <- .sp_matchup_str(away_name, away_ep, away_gs, away_sp)
  home_str <- .sp_matchup_str(home_name, home_ep, home_gs, home_sp)

  # Quality label uses BLENDED ERA+ so a temporarily-struggling ace
  # still earns the right label (and a hot 2-start pitcher doesn't inflate it).
  both_good <- !is.na(away_blep) && !is.na(home_blep) &&
               away_blep >= 110 && home_blep >= 110

  label <- if (both_good && away_blep >= 120 && home_blep >= 120) {
    "Quality pitching matchup"
  } else if (both_good) {
    "Solid pitching matchup"
  } else if (!is.na(away_blep) && away_blep >= 120 && (is.na(home_blep) || home_blep < 100)) {
    paste0("Pitching edge \u2014 ", away_team)
  } else if (!is.na(home_blep) && home_blep >= 120 && (is.na(away_blep) || away_blep < 100)) {
    paste0("Pitching edge \u2014 ", home_team)
  } else {
    "Pitching matchup"
  }

  if (!is.na(away_ep) || !is.na(home_ep) ||
      away_name != "TBD" || home_name != "TBD") {
    bullets <- c(bullets,
      paste0("<strong>", label, "</strong>: ", away_str, " vs ", home_str))
  }

  # ---- 1b. Series context (rubber match / finale / opener — only when notable) ----
  game_in_s   <- if ("game_in_series"   %in% names(game) && !is.na(game$game_in_series[1]))
                   as.integer(game$game_in_series[1])   else NA_integer_
  series_len  <- if ("series_length"    %in% names(game) && !is.na(game$series_length[1]))
                   as.integer(game$series_length[1])    else NA_integer_
  is_rubber   <- isTRUE("is_rubber_match"   %in% names(game) && game$is_rubber_match[1]   == TRUE)
  is_finale   <- isTRUE("is_series_finale"  %in% names(game) && game$is_series_finale[1]  == TRUE)
  is_opener   <- isTRUE("is_series_opener"  %in% names(game) && game$is_series_opener[1]  == TRUE)

  if (is_rubber) {
    bullets <- c(bullets,
      paste0("<strong>Rubber match</strong>: Series tied \u2014 winner takes the series"))
  } else if (is_finale && !is.na(series_len) && series_len >= 4) {
    bullets <- c(bullets,
      paste0("<strong>Series finale</strong>: Game ", game_in_s, " of ", series_len))
  } else if (is_opener && !is.na(series_len)) {
    bullets <- c(bullets,
      paste0("<strong>Series opener</strong>: ", series_len, "-game series at ", home_team))
  }

  # ---- 1c. Park environment (only when notably hitter- or pitcher-friendly) ----
  home_team_id_pf <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
  pf_val <- .park_factor(home_team_id_pf)

  if (pf_val >= 1.05) {
    pf_venue <- dplyr::coalesce(game$venue_name[1], "this park")
    bullets <- c(bullets, paste0(
      "<strong>Hitter-friendly venue</strong>: ", pf_venue,
      " (park factor ", round(pf_val, 3),
      " \u2014 +", round((pf_val - 1) * 100), "% run environment)"
    ))
  } else if (pf_val <= 0.97) {
    pf_venue <- dplyr::coalesce(game$venue_name[1], "this park")
    bullets <- c(bullets, paste0(
      "<strong>Pitcher-friendly venue</strong>: ", pf_venue,
      " (park factor ", round(pf_val, 3),
      " \u2014 ", round((pf_val - 1) * 100), "% run environment)"
    ))
  }

  # ---- 2. Swing-and-miss stuff (highlight any SP with K% ≥ 22%) ----
  k_parts <- character(0)
  for (sp_row in list(away_sp, home_sp)) {
    if (nrow(sp_row) == 0) next
    kpct <- .get_num(sp_row, "fg_K_pct")
    if (is.na(kpct) || kpct < 0.22) next
    kbb  <- .get_num(sp_row, "fg_K_BB_pct")
    nm   <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
    tier <- if (kpct >= 0.27) "elite" else "above-average"
    part <- paste0(nm, ": ", tier, " K rate (", .fmt_pct(kpct), " K%",
                   if (!is.na(kbb)) paste0(", ", .fmt_pct(kbb), " K-BB%") else "", ")")
    k_parts <- c(k_parts, part)
  }
  if (length(k_parts) > 0)
    bullets <- c(bullets,
      paste0("<strong>Strikeout stuff</strong>: ", paste(k_parts, collapse = " · ")))

  # ---- 3. Offense comparison ----
  away_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "away"))
  home_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "home"))

  both_default <- (away_wrc == 100 && home_wrc == 100)
  if (!both_default) {
    if (abs(away_wrc - home_wrc) >= 15) {
      stronger     <- if (away_wrc > home_wrc) away_team else home_team
      stronger_wrc <- max(away_wrc, home_wrc)
      weaker       <- if (away_wrc > home_wrc) home_team else away_team
      weaker_wrc   <- min(away_wrc, home_wrc)
      bullets <- c(bullets, paste0(
        "<strong>Offense edge</strong>: ", stronger, " (", .wrc_tier(stronger_wrc),
        " offense, avg wRC+ ", round(stronger_wrc), ") vs ",
        weaker, " (", round(weaker_wrc), ")"
      ))
    } else {
      bullets <- c(bullets, paste0(
        "<strong>Balanced offenses</strong>: ",
        away_team, " avg wRC+ ", round(away_wrc),
        " · ", home_team, " avg wRC+ ", round(home_wrc)
      ))
    }
  }

  # ---- 4. Weather (only if notable) ----
  wind_mph <- .get_num(game, "wind_speed_mph")
  temp_f   <- .get_num(game, "game_temp_f")
  wind_dir <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
    as.character(game$wind_direction[1]) else NA_character_

  weather_parts <- character(0)
  if (!is.na(wind_mph) && wind_mph >= 10) {
    dir_note <- if (!is.na(wind_dir)) paste0(" (", wind_dir, ")") else ""
    impact   <- dplyr::case_when(
      grepl("out|center|left|right", tolower(dplyr::coalesce(wind_dir, ""))) ~ " — HR-friendly conditions",
      grepl("^in$|home plate|backstop", tolower(dplyr::coalesce(wind_dir, ""))) ~ " — pitcher-friendly conditions",
      TRUE ~ ""
    )
    weather_parts <- c(weather_parts, paste0(round(wind_mph), " mph wind", dir_note, impact))
  }
  if (!is.na(temp_f)) {
    if (temp_f < 42)
      weather_parts <- c(weather_parts, paste0(round(temp_f), "\u00b0F — cold, pitcher-friendly"))
    else if (temp_f > 87)
      weather_parts <- c(weather_parts, paste0(round(temp_f), "\u00b0F — warm, hitter-friendly"))
  }
  if (length(weather_parts) > 0)
    bullets <- c(bullets,
      paste0("<strong>Weather</strong>: ", paste(weather_parts, collapse = " \u00b7 ")))

  # ---- 5. Bullpen unavailability / doubtful ----
  unavail <- bullpen %>%
    dplyr::filter(availability == "unavailable") %>%
    dplyr::arrange(role_sort) %>%
    dplyr::slice_head(n = 3)
  doubtful <- bullpen %>%
    dplyr::filter(availability == "doubtful") %>%
    dplyr::arrange(role_sort) %>%
    dplyr::slice_head(n = 3)

  if (nrow(unavail) > 0 || nrow(doubtful) > 0) {
    parts <- character(0)
    if (nrow(unavail) > 0)
      parts <- c(parts, paste0("Out: ",
        paste(paste0(unavail$player_name, " (", unavail$side, ")"), collapse = ", ")))
    if (nrow(doubtful) > 0)
      parts <- c(parts, paste0("Doubtful: ",
        paste(paste0(doubtful$player_name, " (", doubtful$side, ")"), collapse = ", ")))
    bullets <- c(bullets,
      paste0("<strong>Bullpen watch</strong>: ", paste(parts, collapse = " \u00b7 ")))
  }

  # ---- 6. Matchup & player watchlist ----
  # Pulls lineup_context_splits + starter_splits if available (from 04_matchup_splits.R).
  # Generates up to 3 targeted callouts: batter spotlight, SP regression, power matchup.

  lcs <- if (exists("lineup_context_splits"))
    lineup_context_splits %>% dplyr::filter(game_pk == gpk) else dplyr::tibble()

  streaks <- if (exists("recent_batter_streaks") && nrow(recent_batter_streaks) > 0)
    recent_batter_streaks else dplyr::tibble()

  watch <- character(0)

  # --- 6a. Hot bat / streak callouts ---
  # Hit streak ≥ 5 or OB streak ≥ 7 for any batter in today's lineup (slots 1-9).
  # Also flags last-7 hot bats (.900+ OPS, ≥ 4 games).
  if (nrow(streaks) > 0) {
    streak_players <- lineup %>%
      dplyr::select(side, batting_slot, mlbam_id, player_name) %>%
      dplyr::left_join(streaks, by = "mlbam_id") %>%
      dplyr::filter(!is.na(hit_streak) | !is.na(ob_streak) | !is.na(is_hot))

    for (side_val in c("away", "home")) {
      team_nm <- if (side_val == "away") away_team else home_team

      sp <- streak_players %>%
        dplyr::filter(side == side_val) %>%
        dplyr::arrange(batting_slot)

      # Hit streak ≥ 5
      hit_s <- sp %>% dplyr::filter(!is.na(hit_streak), hit_streak >= 5) %>%
        dplyr::arrange(dplyr::desc(hit_streak)) %>% dplyr::slice_head(n = 2)
      for (i in seq_len(nrow(hit_s))) {
        r <- hit_s[i, ]
        avg_str <- if (!is.na(r$last7_avg))
          paste0(", batting ", sprintf("%.3f", r$last7_avg), " L", r$last7_g) else ""
        watch <- c(watch, paste0(
          "<strong>Hit streak</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          r$hit_streak, "-game hit streak", avg_str
        ))
      }

      # On-base streak ≥ 7 (only if not already covered by hit streak)
      ob_s <- sp %>%
        dplyr::filter(!is.na(ob_streak), ob_streak >= 7,
                      is.na(hit_streak) | hit_streak < 5) %>%
        dplyr::arrange(dplyr::desc(ob_streak)) %>% dplyr::slice_head(n = 1)
      for (i in seq_len(nrow(ob_s))) {
        r <- ob_s[i, ]
        watch <- c(watch, paste0(
          "<strong>On-base streak</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          r$ob_streak, " consecutive games reaching base"
        ))
      }

      # Hot bat last 7 (.900+ OPS, not already called out by streak)
      hot_s <- sp %>%
        dplyr::filter(is_hot == TRUE,
                      is.na(hit_streak) | hit_streak < 5,
                      is.na(ob_streak)  | ob_streak  < 7) %>%
        dplyr::arrange(dplyr::desc(last7_ops)) %>% dplyr::slice_head(n = 1)
      for (i in seq_len(nrow(hot_s))) {
        r <- hot_s[i, ]
        hr_str <- if (!is.na(r$last7_hr) && r$last7_hr >= 2)
          paste0(", ", r$last7_hr, " HR") else ""
        watch <- c(watch, paste0(
          "<strong>Hot bat</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          ".OPS ", sprintf("%.3f", r$last7_ops), hr_str,
          " over last ", r$last7_g, " games"
        ))
      }
    }
  }

  # --- 6b. Batter spotlight: best split OPS vs today's SP handedness ---
  # For each side, find the best bat (slots 2-6, ≥15 PA) facing the opposing SP.
  # Threshold: OPS ≥ .850 to be worth calling out.
  for (sp_side in c("away", "home")) {
    bat_side <- if (sp_side == "away") "home" else "away"
    sp_row   <- sps %>% dplyr::filter(side == sp_side)
    bat_team <- if (bat_side == "away") away_team else home_team
    if (nrow(sp_row) == 0 || nrow(lcs) == 0) next

    sp_nm <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")

    best <- lcs %>%
      dplyr::filter(side == bat_side,
                    !is.na(sp_ops), sp_pa >= 15,
                    batting_slot %in% 2:6) %>%
      dplyr::arrange(dplyr::desc(sp_ops)) %>%
      dplyr::slice_head(n = 1)

    if (nrow(best) == 0 || best$sp_ops[1] < 0.850) next

    hr_note <- if (!is.na(best$sp_hr[1]) && best$sp_hr[1] >= 2)
      paste0(", ", best$sp_hr[1], " HR") else ""

    slot_word <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")
    slot_lbl  <- slot_word[min(best$batting_slot[1], 9L)]

    watch <- c(watch, paste0(
      "<strong>Batter to watch</strong>: ",
      best$player_name[1], " (", bat_team, ", batting ", slot_lbl, ") \u2014 ",
      ".OPS ", sprintf("%.3f", best$sp_ops[1]), hr_note,
      " ", best$split_label[1],
      " vs ", sp_nm,
      " (", best$sp_pa[1], " PA)"
    ))
  }

  # --- 6b. SP regression / luck flag ---
  # ERA vs xFIP gap ≥ 1.2 with at least 3 GS — signals meaningful luck divergence.
  for (sp_row in list(away_sp, home_sp)) {
    if (nrow(sp_row) == 0) next
    era  <- .get_num(sp_row, "mlb_era")
    xfip <- .get_num(sp_row, "fg_xFIP")
    gs   <- .get_num(sp_row, "mlb_gs")
    sp_nm <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
    if (is.na(era) || is.na(xfip) || is.na(gs) || gs < 3) next

    gap <- era - xfip
    if (abs(gap) < 1.2) next

    if (gap > 0) {
      watch <- c(watch, paste0(
        "<strong>Regression candidate</strong>: ", sp_nm,
        " ERA ", round(era, 2), " vs xFIP ", round(xfip, 2),
        " \u2014 bad luck through ", round(gs), " GS, numbers should improve"
      ))
    } else {
      watch <- c(watch, paste0(
        "<strong>Overperforming watch</strong>: ", sp_nm,
        " ERA ", round(era, 2), " vs xFIP ", round(xfip, 2),
        " \u2014 outpacing metrics through ", round(gs), " GS"
      ))
    }
  }

  # --- 6c. Power matchup: ISO threat vs HR-vulnerable SP ---
  # Fires when a key bat (ISO ≥ .220, slots 2-6) faces an SP with HR/FB ≥ 14%.
  if (nrow(lcs) > 0) {
    for (sp_side in c("away", "home")) {
      bat_side <- if (sp_side == "away") "home" else "away"
      sp_row   <- sps %>% dplyr::filter(side == sp_side)
      bat_team <- if (bat_side == "away") away_team else home_team
      if (nrow(sp_row) == 0) next

      hr_fb  <- .get_num(sp_row, "fg_HR.FB")
      if (is.na(hr_fb) || hr_fb < 0.14) next
      sp_nm  <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")

      power_bats <- lineup %>%
        dplyr::filter(side == bat_side, batting_slot %in% 2:6) %>%
        dplyr::filter(!is.na(fg_ISO), fg_ISO >= 0.220) %>%
        dplyr::arrange(dplyr::desc(fg_ISO)) %>%
        dplyr::slice_head(n = 2)

      if (nrow(power_bats) == 0) next

      names_str <- paste(
        paste0(power_bats$player_name, " (ISO ", sprintf("%.3f", power_bats$fg_ISO), ")"),
        collapse = ", "
      )
      watch <- c(watch, paste0(
        "<strong>Power matchup</strong>: ", names_str,
        " vs ", sp_nm,
        " \u2014 ", round(hr_fb * 100, 0), "% HR/FB rate allowed"
      ))
    }
  }

  # --- 6d. Platoon mismatch: 5+ batters with the hand advantage ---
  if (nrow(lcs) > 0) {
    for (side_val in c("away", "home")) {
      team_nm   <- if (side_val == "away") away_team else home_team
      opp_side  <- if (side_val == "away") "home" else "away"
      sp_row    <- sps %>% dplyr::filter(side == opp_side)
      if (nrow(sp_row) == 0) next

      sp_nm      <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
      sp_hand_lbl <- if ("pitch_hand" %in% names(sp_row) && !is.na(sp_row$pitch_hand[1]))
        paste0(sp_row$pitch_hand[1], "HP") else NULL
      if (is.null(sp_hand_lbl)) next

      # Batters who have the platoon edge (vs this hand) and are hitting well
      adv_bats <- lcs %>%
        dplyr::filter(side == side_val, !is.na(sp_ops), sp_pa >= 15,
                      sp_ops >= 0.750) %>%
        nrow()

      if (adv_bats < 5) next

      watch <- c(watch, paste0(
        "<strong>Platoon edge \u2014 ", team_nm, "</strong>: ",
        adv_bats, " batters with \u2265.750 OPS ", lcs$split_label[lcs$side == side_val][1],
        " vs ", sp_nm, " (", sp_hand_lbl, ")"
      ))
    }
  }

  # Append up to 4 watch bullets (streaks are high-value, worth the extra line)
  if (length(watch) > 0)
    bullets <- c(bullets, watch[seq_len(min(4L, length(watch)))])

  if (length(bullets) == 0) return("")

  paste0(
    '<ul style="margin:4px 0 0 0; padding-left:18px; font-size:12px; ',
    'color:#444; line-height:1.75;">',
    paste0("<li>", bullets, "</li>", collapse = ""),
    "</ul>"
  )
}

# ============================================================
# make_game_preview_html(gpk)
# One-paragraph prose summary for the deep dive page header
# ============================================================

make_game_preview_html <- function(gpk) {
  game   <- game_context    %>% dplyr::filter(game_pk == gpk)
  sps    <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context  %>% dplyr::filter(game_pk == gpk)

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"
  venue     <- dplyr::coalesce(game$venue_name[1], "the ballpark")

  # SP descriptor
  .sp_desc <- function(sp_row, sp_nm) {
    if (nrow(sp_row) == 0) return(sp_nm)
    ep   <- .era_plus_val(sp_row)
    war  <- .get_num(sp_row, "fg_WAR")
    fip  <- dplyr::coalesce(.get_num(sp_row, "fg_xFIP"), .get_num(sp_row, "fg_FIP"))
    kpct <- .get_num(sp_row, "fg_K_pct")

    parts <- character(0)
    if (!is.na(ep))   parts <- c(parts, paste0("ERA+ ", round(ep)))
    if (!is.na(fip))  parts <- c(parts, paste0(.fip_label_used(sp_row), " ", round(fip, 2)))
    if (!is.na(kpct)) parts <- c(parts, paste0(.fmt_pct(kpct), " K%"))
    if (!is.na(war))  parts <- c(parts, paste0(round(war, 1), " fWAR"))

    if (length(parts) == 0) return(sp_nm)
    paste0("<strong>", sp_nm, "</strong> (", paste(parts, collapse = ", "), ")")
  }

  away_desc <- .sp_desc(away_sp, away_name)
  home_desc <- .sp_desc(home_sp, home_name)

  # Pitching tone
  away_ep  <- if (nrow(away_sp) > 0) .era_plus_val(away_sp) else NA_real_
  home_ep  <- if (nrow(home_sp) > 0) .era_plus_val(home_sp) else NA_real_
  avg_ep   <- mean(c(away_ep, home_ep), na.rm = TRUE)
  pitch_tone <- dplyr::case_when(
    !is.na(avg_ep) && avg_ep >= 120 ~ "a marquee pitching matchup",
    !is.na(avg_ep) && avg_ep >= 105 ~ "a solid pitching matchup",
    !is.na(avg_ep) && avg_ep < 95   ~ "an offense-friendly environment",
    TRUE ~ "today's matchup"
  )

  # Offense summary
  away_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "away"))
  home_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "home"))
  off_note <- if (away_wrc == 100 && home_wrc == 100) {
    ""  # no data, skip
  } else {
    stronger <- if (away_wrc >= home_wrc) away_team else home_team
    stronger_wrc <- max(away_wrc, home_wrc)
    weaker   <- if (away_wrc >= home_wrc) home_team else away_team
    weaker_wrc   <- min(away_wrc, home_wrc)
    if (abs(away_wrc - home_wrc) >= 12) {
      paste0(" Offensively, ", stronger, " carries the stronger lineup (avg wRC+ ",
             round(stronger_wrc), " vs ", round(weaker_wrc), " for ", weaker, ").")
    } else {
      paste0(" Both offenses are comparable (", away_team, " avg wRC+ ", round(away_wrc),
             ", ", home_team, " ", round(home_wrc), ").")
    }
  }

  # Weather note
  wind_mph <- .get_num(game, "wind_speed_mph")
  temp_f   <- .get_num(game, "game_temp_f")
  wind_dir <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
    as.character(game$wind_direction[1]) else NA_character_

  weather_note <- ""
  if (!is.na(temp_f) || (!is.na(wind_mph) && wind_mph >= 8)) {
    parts <- character(0)
    if (!is.na(temp_f)) parts <- c(parts, paste0(round(temp_f), "\u00b0F"))
    if (!is.na(wind_mph) && wind_mph >= 8) {
      dir_str <- if (!is.na(wind_dir)) paste0(" from the ", wind_dir) else ""
      parts <- c(parts, paste0(round(wind_mph), " mph wind", dir_str))
    }
    weather_note <- paste0(" Conditions at ", venue, ": ", paste(parts, collapse = ", "), ".")
  }

  paste0(
    '<div style="background:#f8f9fa; border-left:4px solid #1a73e8; ',
    'padding:12px 16px; border-radius:0 6px 6px 0; ',
    'font-size:13px; color:#333; line-height:1.75; margin-bottom:1rem;">',
    away_desc, " heads to ", venue, " to face ", home_desc,
    " in what shapes up as ", pitch_tone, ".",
    off_note,
    weather_note,
    "</div>"
  )
}

# ============================================================
# make_prediction_html(gpk)
# Projection table + key factors for the deep dive
# ============================================================

# Internal: IP-weighted bullpen ERA for available/fresh arms (excludes LR, injured, unavailable)
.bullpen_era <- function(bp_rows) {
  rel <- bp_rows %>%
    dplyr::filter(
      availability %in% c("fresh", "available", "limited", "doubtful"),
      !fg_role %in% c("LR"),
      !is.na(mlb_ip), mlb_ip > 0
    )
  era_col <- dplyr::coalesce(
    if ("bbref_ERA" %in% names(rel)) rel$bbref_ERA else rep(NA_real_, nrow(rel)),
    rel$mlb_era
  )
  if (length(era_col) == 0 || all(is.na(era_col))) return(LEAGUE_AVG_FIP)
  valid <- !is.na(era_col) & is.finite(era_col) & era_col < 15
  if (!any(valid)) return(LEAGUE_AVG_FIP)
  weighted.mean(era_col[valid], rel$mlb_ip[valid], na.rm = TRUE)
}

# Internal: expected SP IP per start from starter_matchup row
.sp_ip_per_gs <- function(sp_row) {
  ip <- .get_num(sp_row, "mlb_ip")
  gs <- .get_num(sp_row, "mlb_gs")
  if (!is.na(ip) && !is.na(gs) && gs > 0) min(ip / gs, 7.5) else 5.5
}

# Internal: lookup park factor for home team (1.0 = neutral fallback)
.park_factor <- function(home_team_id) {
  if (!exists("park_factors") || nrow(park_factors) == 0) return(1.0)
  row <- park_factors %>%
    dplyr::filter(mlbam_team_id == as.integer(home_team_id))
  if (nrow(row) == 0) return(1.0)
  row$pf_runs[1]
}

# Team recent form multiplier: if a team's lineup shows a meaningful collective
# hot/cold trend over the last 7 games, apply a small adjustment to expected runs.
# Uses recent_batter_streaks (from 03_recent_batting_logs.R) when available.
# Cap: ±8% max adjustment. Only applies when ≥ 5 batters have recent data.
# At season start (no data): returns 1.0 (no adjustment).
.team_form_mult <- function(lineup_rows) {
  if (!exists("recent_batter_streaks") || nrow(recent_batter_streaks) == 0) return(1.0)

  ids <- lineup_rows$mlbam_id[!is.na(lineup_rows$mlbam_id)]
  if (length(ids) == 0) return(1.0)

  form <- recent_batter_streaks %>%
    dplyr::filter(mlbam_id %in% ids, !is.na(last7_ops), last7_g >= 3)

  if (nrow(form) < 5) return(1.0)

  # Compute team average last-7 OPS vs a neutral baseline (.750 = league avg OPS)
  LEAGUE_AVG_OPS <- 0.750
  team_l7_ops    <- mean(form$last7_ops, na.rm = TRUE)
  raw_mult       <- team_l7_ops / LEAGUE_AVG_OPS

  # Regress adjustment toward 1.0: weight is proportional to bats with data
  # Full coverage (9 bats) = 30% adjustment, partial = less
  coverage_weight <- min(nrow(form) / 9, 1.0) * 0.30
  mult <- 1.0 + (raw_mult - 1.0) * coverage_weight

  # Hard cap: ±8%
  max(0.92, min(1.08, mult))
}

make_prediction_html <- function(gpk) {
  game   <- game_context    %>% dplyr::filter(game_pk == gpk)
  sps    <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context  %>% dplyr::filter(game_pk == gpk)
  bullpen <- if (exists("bullpen_grid")) bullpen_grid %>% dplyr::filter(game_pk == gpk) else
             dplyr::tibble()

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"

  # --- SP quality (xFIP → SIERA → FIP → ERA → Lg avg) ---
  away_sp_fip       <- if (nrow(away_sp) > 0) .best_fip_val(away_sp) else LEAGUE_AVG_FIP
  home_sp_fip       <- if (nrow(home_sp) > 0) .best_fip_val(home_sp) else LEAGUE_AVG_FIP
  away_fip_label    <- if (nrow(away_sp) > 0) .fip_label_used(away_sp) else "Lg Avg"
  home_fip_label    <- if (nrow(home_sp) > 0) .fip_label_used(home_sp) else "Lg Avg"

  # --- SP IP fraction ---
  away_sp_ipgs  <- if (nrow(away_sp) > 0) .sp_ip_per_gs(away_sp) else 5.5
  home_sp_ipgs  <- if (nrow(home_sp) > 0) .sp_ip_per_gs(home_sp) else 5.5
  away_sp_frac  <- min(away_sp_ipgs / 9, 0.85)
  home_sp_frac  <- min(home_sp_ipgs / 9, 0.85)

  # --- Bullpen quality (IP-weighted ERA of available/fresh arms, excl LR) ---
  away_bp_era  <- if (nrow(bullpen) > 0)
    .bullpen_era(bullpen %>% dplyr::filter(side == "away")) else LEAGUE_AVG_FIP
  home_bp_era  <- if (nrow(bullpen) > 0)
    .bullpen_era(bullpen %>% dplyr::filter(side == "home")) else LEAGUE_AVG_FIP

  # --- Blended pitcher quality: SP covers sp_frac, bullpen covers rest ---
  away_blended_fip <- away_sp_frac * away_sp_fip + (1 - away_sp_frac) * away_bp_era
  home_blended_fip <- home_sp_frac * home_sp_fip + (1 - home_sp_frac) * home_bp_era

  # --- Lineup offense ---
  away_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "away"))
  home_wrc <- .team_wrc(lineup %>% dplyr::filter(side == "home"))
  away_wrc_flag <- (away_wrc == 100 && sum(!is.na((lineup %>% dplyr::filter(side=="away"))$fg_wRC_plus)) < 3)
  home_wrc_flag <- (home_wrc == 100 && sum(!is.na((lineup %>% dplyr::filter(side=="home"))$fg_wRC_plus)) < 3)

  # Recent form multiplier (regressed toward 1.0 — small adjustment, not override)
  away_form_mult <- .team_form_mult(lineup %>% dplyr::filter(side == "away"))
  home_form_mult <- .team_form_mult(lineup %>% dplyr::filter(side == "home"))

  # --- Park factor (home park affects both offenses equally) ---
  home_team_id <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
  pf           <- .park_factor(home_team_id)
  pf_note      <- if (pf != 1.0) {
    venue <- dplyr::coalesce(game$venue_name[1], "this park")
    paste0(venue, " park factor = ", round(pf, 3),
           " (", if (pf > 1) "+" else "", round((pf - 1) * 100, 1), "% run environment)")
  } else NULL

  # --- Expected runs ---
  # Formula: lg_avg × (wRC+/100) × park_factor × (opp_blended_FIP / lg_avg_FIP)
  # Both teams' offenses are boosted/suppressed equally by the park.
  away_exp_r <- LEAGUE_AVG_RUNS * (away_wrc / 100) * pf *
                (home_blended_fip / LEAGUE_AVG_FIP) * away_form_mult
  home_exp_r <- LEAGUE_AVG_RUNS * (home_wrc / 100) * pf *
                (away_blended_fip / LEAGUE_AVG_FIP) * home_form_mult +
                HOME_FIELD_BONUS
  away_exp_r <- max(1.5, min(away_exp_r, 9.5))
  home_exp_r <- max(1.5, min(home_exp_r, 9.5))
  total_exp  <- away_exp_r + home_exp_r

  # --- Win probability ---
  p_home   <- .poisson_win_prob(home_exp_r, away_exp_r)
  p_away   <- 1 - p_home
  fav_team <- if (p_home >= p_away) home_team else away_team
  fav_pct  <- max(p_home, p_away)

  # --- Factor list ---
  factors <- character(0)

  away_fip_d <- round((away_blended_fip - LEAGUE_AVG_FIP) / LEAGUE_AVG_FIP * 100)
  home_fip_d <- round((home_blended_fip - LEAGUE_AVG_FIP) / LEAGUE_AVG_FIP * 100)
  bp_frac_away <- 1 - away_sp_frac
  bp_frac_home <- 1 - home_sp_frac

  factors <- c(factors,
    paste0(away_name, " ", away_fip_label, " = ", round(away_sp_fip, 2),
           " \u00b7 bullpen ERA = ", round(away_bp_era, 2),
           " \u2192 blended = ", round(away_blended_fip, 2),
           " (", round(away_sp_frac * 100), "% SP / ",
           round(bp_frac_away * 100), "% BP innings)"),
    paste0(home_name, " ", home_fip_label, " = ", round(home_sp_fip, 2),
           " \u00b7 bullpen ERA = ", round(home_bp_era, 2),
           " \u2192 blended = ", round(home_blended_fip, 2),
           " (", round(home_sp_frac * 100), "% SP / ",
           round(bp_frac_home * 100), "% BP innings)"),
    paste0(away_team, " lineup avg wRC+ = ", round(away_wrc),
           if (away_wrc_flag) " <em>(insufficient data \u2014 using lg avg)</em>" else ""),
    paste0(home_team, " lineup avg wRC+ = ", round(home_wrc),
           if (home_wrc_flag) " <em>(insufficient data \u2014 using lg avg)</em>" else "")
  )
  if (!is.null(pf_note)) factors <- c(factors, pf_note)
  away_form_note <- if (away_form_mult != 1.0)
    paste0(away_team, " recent form adj: \u00d7", round(away_form_mult, 3),
           " (L7 team OPS data)")
  else NULL
  home_form_note <- if (home_form_mult != 1.0)
    paste0(home_team, " recent form adj: \u00d7", round(home_form_mult, 3),
           " (L7 team OPS data)")
  else NULL

  factors <- c(factors,
    paste0("Home field residual: +", HOME_FIELD_BONUS,
           " R/game (last-at-bat/crowd; empirical run diff \u22480 over 2022\u20132024)")
  )
  if (!is.null(away_form_note)) factors <- c(factors, away_form_note)
  if (!is.null(home_form_note)) factors <- c(factors, home_form_note)

  # --- HTML rows helper ---
  tr <- function(label, away_val, home_val, style = "") {
    paste0(
      '<tr style="', style, '">',
      '<td style="padding:6px 14px; border-bottom:1px solid #eee;">', label, '</td>',
      '<td style="padding:6px 16px; text-align:center; border-bottom:1px solid #eee;">', away_val, '</td>',
      '<td style="padding:6px 16px; text-align:center; border-bottom:1px solid #eee;">', home_val, '</td>',
      '</tr>'
    )
  }

  away_win_bg <- if (p_away > p_home) "background:#e8f5e9;" else ""
  home_win_bg <- if (p_home > p_away) "background:#e8f5e9;" else ""

  # Blended FIP label shows SP quality label / bullpen
  away_blend_label <- paste0("Blended (", away_fip_label, " + BP)")
  home_blend_label <- paste0("Blended (", home_fip_label, " + BP)")
  blend_label      <- paste0("Pitcher Quality (", away_fip_label, "+BP / ", home_fip_label, "+BP)")

  pf_display <- if (pf != 1.0) paste0("Park Factor (", round(pf, 3), "×)") else NULL

  table_html <- paste0(
    '<table style="border-collapse:collapse; font-size:13px; margin-bottom:0.8rem;">',
    '<thead><tr style="background:#f1f3f4;">',
    '<th style="padding:8px 14px; text-align:left; border-bottom:2px solid #ddd; min-width:200px;"></th>',
    '<th style="padding:8px 16px; text-align:center; border-bottom:2px solid #ddd; min-width:140px;">',
    away_team, ' (Away)</th>',
    '<th style="padding:8px 16px; text-align:center; border-bottom:2px solid #ddd; min-width:140px;">',
    home_team, ' (Home)</th>',
    '</tr></thead><tbody>',
    tr("Starting Pitcher", away_name, home_name),
    tr(paste0("SP Quality (", away_fip_label, " / ", home_fip_label, ")"),
       round(away_sp_fip, 2), round(home_sp_fip, 2)),
    tr("Bullpen ERA (avail/fresh arms)",
       round(away_bp_era, 2), round(home_bp_era, 2)),
    tr(blend_label,
       paste0("<strong>", round(away_blended_fip, 2), "</strong>"),
       paste0("<strong>", round(home_blended_fip, 2), "</strong>")),
    tr("Lineup avg wRC+",
       paste0(round(away_wrc), if (away_wrc_flag) "*" else ""),
       paste0(round(home_wrc), if (home_wrc_flag) "*" else "")),
    if (!is.null(pf_display)) tr(pf_display, round(pf, 3), round(pf, 3)) else "",
    tr("<strong>Projected Runs</strong>",
       paste0("<strong>", round(away_exp_r, 1), "</strong>"),
       paste0("<strong>", round(home_exp_r, 1), "</strong>")),
    tr("<strong>Win Probability</strong>",
       paste0('<span style="', away_win_bg, 'padding:2px 10px; border-radius:4px; font-weight:bold;">',
              round(p_away * 100), "%</span>"),
       paste0('<span style="', home_win_bg, 'padding:2px 10px; border-radius:4px; font-weight:bold;">',
              round(p_home * 100), "%</span>")),
    '</tbody></table>',
    '<p style="font-size:12px; color:#444; margin:0 0 0.8rem 0;">',
    '<strong>Projected total: ', round(total_exp, 1), ' runs</strong>',
    ' \u00b7 Projected winner: <strong>', fav_team, '</strong> (',
    round(fav_pct * 100), '% win probability)</p>'
  )

  factors_html <- paste0(
    '<p style="font-size:12px; font-weight:bold; color:#444; margin:0.8rem 0 4px 0;">',
    'Model inputs:</p>',
    '<ul style="font-size:12px; color:#555; margin:0 0 0.8rem 0; ',
    'padding-left:18px; line-height:1.8;">',
    paste0("<li>", factors, "</li>", collapse = ""),
    "</ul>"
  )

  method_html <- paste0(
    '<p style="font-size:11px; color:#888; padding:8px 10px; background:#f8f9fa; ',
    'border-radius:4px; border-left:3px solid #dee2e6; margin:0;">',
    '<strong>Model:</strong> E[runs] = ', LEAGUE_AVG_RUNS,
    ' \u00d7 (wRC+/100) \u00d7 park_factor \u00d7 (blended_FIP/', LEAGUE_AVG_FIP, ')',
    ' \u00d7 recent_form_adj. ',
    'Blended FIP = SP_frac \u00d7 SP_xFIP + BP_frac \u00d7 bullpen_ERA. ',
    'Recent form adj (±8% max) from last-7-game team OPS; no adjustment when <5 batters have data. ',
    'Win probability via Poisson distribution. ',
    'Constants calibrated to 2022\u20132024 (n=7,300 games). ',
    'Accuracy improves as the season progresses and sample sizes grow.',
    '</p>'
  )

  paste0(table_html, factors_html, method_html)
}
