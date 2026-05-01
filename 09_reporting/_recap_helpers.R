# ============================================================
# _recap_helpers.R
# PURPOSE:
#   Pull and render yesterday's MLB game results.
#   Self-contained — no pipeline cache dependency.
# ============================================================

# ------------------------------------------------------------
# Pull yesterday's completed games (hydrated with decisions,
# linescore, and pitcher/batter highlights)
# ------------------------------------------------------------

pull_yesterday_results <- function(recap_date = Sys.Date() - 1) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/schedule",
    "?sportId=1",
    "&date=", format(recap_date, "%Y-%m-%d"),
    "&gameType=R",
    "&hydrate=decisions,linescore,boxscore,probablePitcher"
  )

  resp <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

  raw <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw) || length(raw$dates) == 0) return(NULL)

  raw$dates[[1]]$games
}

# ------------------------------------------------------------
# Fetch boxscore for a game (separate endpoint — schedule doesn't embed it)
# Returns list with $teams (away/home players+pitchers) and $topPerformers
# ------------------------------------------------------------

.fetch_boxscore <- function(game_pk) {
  tryCatch({
    url  <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk, "/boxscore")
    resp <- httr::GET(url, httr::timeout(15))
    if (httr::status_code(resp) != 200) return(NULL)
    jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )
  }, error = function(e) NULL)
}

# ------------------------------------------------------------
# Extract the line for a starting pitcher from a boxscore object
# ------------------------------------------------------------

.sp_line <- function(bs, side) {
  tryCatch({
    pitchers <- bs$teams[[side]]$pitchers
    if (length(pitchers) == 0) return(NULL)
    sp_id <- pitchers[[1]]
    stats <- bs$teams[[side]]$players[[paste0("ID", sp_id)]]$stats$pitching
    info  <- bs$teams[[side]]$players[[paste0("ID", sp_id)]]$person
    list(
      name = info$fullName,
      ip   = stats$inningsPitched,
      h    = stats$hits,
      er   = stats$earnedRuns,
      bb   = stats$baseOnBalls,
      k    = stats$strikeOuts,
      pc   = stats$pitchesThrown
    )
  }, error = function(e) NULL)
}

# ------------------------------------------------------------
# Fetch Statcast pitch-level data for a game
# Returns a data frame or NULL
# ------------------------------------------------------------

.fetch_statcast_game <- function(game_pk) {
  tryCatch({
    url  <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv",
      "?all=true&game_pk=", game_pk,
      "&type=details&player_type=pitcher"
    )
    resp <- httr::GET(url, httr::timeout(25))
    if (httr::status_code(resp) != 200) return(NULL)
    df   <- readr::read_csv(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      show_col_types = FALSE, progress = FALSE
    )
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) NULL)
}

# ------------------------------------------------------------
# Pitch mix mini-table for a single starter
# starter_id: MLB MLBAM id; sc: Statcast data frame
# ------------------------------------------------------------

.pitch_mix_html <- function(sc, starter_id, starter_name) {
  tryCatch({
    p <- sc %>%
      dplyr::filter(pitcher == starter_id, !is.na(pitch_name)) %>%
      dplyr::group_by(pitch_name) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        velo     = round(mean(release_speed,      na.rm = TRUE), 1),
        spin     = round(mean(release_spin_rate,  na.rm = TRUE)),
        whiff    = round(mean(description == "swinging_strike", na.rm = TRUE) * 100, 1),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::slice_head(n = 4)          # top 4 pitches only

    if (nrow(p) == 0) return("")

    best_whiff_idx <- which.max(p$whiff)

    rows <- vapply(seq_len(nrow(p)), function(i) {
      is_best <- i == best_whiff_idx && p$whiff[i] > 0
      txt <- sprintf(
        "%s &mdash; %s%% &nbsp;|&nbsp; %.1f mph &nbsp;|&nbsp; %d rpm &nbsp;|&nbsp; %.1f%% whiff%s",
        p$pitch_name[i], p$pct[i], p$velo[i], p$spin[i], p$whiff[i],
        if (is_best) " \u2605" else ""
      )
      style <- if (is_best) ' style="color:#1a73e8;font-weight:600;"' else ""
      sprintf('<div class="pm-row"%s>%s</div>', style, txt)
    }, character(1))

    paste0(
      '<div class="pitch-mix">',
      sprintf('<div class="pm-header">\u26be %s — pitch mix</div>', starter_name),
      paste(rows, collapse = ""),
      '</div>'
    )
  }, error = function(e) "")
}

# ------------------------------------------------------------
# Fetch play-by-play for a game
# Returns a list: $scoring_plays, $top_play (highest captivatingIndex)
# ------------------------------------------------------------

.fetch_play_by_play <- function(game_pk) {
  tryCatch({
    url  <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk, "/playByPlay")
    resp <- httr::GET(url, httr::timeout(20))
    if (httr::status_code(resp) != 200) return(NULL)
    raw  <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )
    plays <- raw$allPlays
    if (length(plays) == 0) return(NULL)

    # Scoring plays with descriptions
    scoring <- Filter(function(p) {
      isTRUE(p$about$isScoringPlay)
    }, plays)

    scoring_df <- do.call(rbind, lapply(scoring, function(p) {
      data.frame(
        inning      = as.integer(p$about$inning),
        half        = as.character(p$about$halfInning),
        event       = as.character(p$result$event),
        description = as.character(p$result$description),
        rbi         = as.integer(dplyr::coalesce(p$result$rbi, 0L)),
        captivating = as.integer(dplyr::coalesce(p$about$captivatingIndex, 0L)),
        pitcher_id  = as.integer(dplyr::coalesce(p$matchup$pitcher$id, NA_integer_)),
        stringsAsFactors = FALSE
      )
    }))

    # Most dramatic play (any play — not just scoring)
    top_play <- tryCatch({
      all_cap <- do.call(rbind, lapply(plays, function(p) {
        ci <- p$about$captivatingIndex
        if (is.null(ci) || is.na(ci) || as.integer(ci) < 70) return(NULL)
        data.frame(
          inning      = as.integer(p$about$inning),
          half        = as.character(p$about$halfInning),
          event       = as.character(p$result$event),
          description = as.character(p$result$description),
          captivating = as.integer(ci),
          stringsAsFactors = FALSE
        )
      }))
      if (is.null(all_cap) || nrow(all_cap) == 0) NULL
      else all_cap[which.max(all_cap$captivating), ]
    }, error = function(e) NULL)

    list(scoring_plays = scoring_df, top_play = top_play)
  }, error = function(e) NULL)
}

# ------------------------------------------------------------
# Spectacular plays: high-xBA balls that became outs
# ------------------------------------------------------------

.spectacular_plays_html <- function(sc, threshold = 0.55) {
  tryCatch({
    # Hardest catches: fly balls / line drives with high xBA that became outs
    hard_catches <- sc %>%
      dplyr::filter(
        !is.na(estimated_ba_using_speedangle),
        estimated_ba_using_speedangle >= threshold,
        bb_type %in% c("fly_ball", "line_drive"),
        events %in% c("field_out", "force_out", "sac_fly",
                       "double_play", "grounded_into_double_play")
      ) %>%
      dplyr::arrange(dplyr::desc(estimated_ba_using_speedangle)) %>%
      dplyr::slice_head(n = 3)

    if (nrow(hard_catches) == 0) return("")

    lines <- vapply(seq_len(nrow(hard_catches)), function(i) {
      row      <- hard_catches[i, ]
      catch_p  <- round((1 - row$estimated_ba_using_speedangle) * 100)
      ev       <- if (!is.na(row$launch_speed))    sprintf("%.0f mph EV", row$launch_speed)    else NULL
      dist     <- if (!is.na(row$hit_distance_sc)) sprintf("%.0f ft",    row$hit_distance_sc) else NULL
      catch_lbl <- sprintf('<strong style="color:#c0392b;">%d%% catch prob</strong>', catch_p)
      stats    <- paste(c(catch_lbl, ev, dist), collapse = " &middot; ")
      inn      <- sprintf("Inn. %s %s", row$inning,
                          if (!is.na(row$inning_topbot)) paste0("(", row$inning_topbot, ")") else "")
      sprintf('<div class="pm-row"><strong>%s</strong> &mdash; %s<br><em style="color:#555;">%s</em></div>',
              inn, stats, row$des)
    }, character(1))

    paste0(
      '<div class="pitch-mix">',
      '<div class="pm-header">\U0001f9e4 Hardest catches</div>',
      paste(lines, collapse = ""),
      '</div>'
    )
  }, error = function(e) "")
}

# ------------------------------------------------------------
# Game narrative — 2-4 sentence recap of how the game unfolded
# ------------------------------------------------------------

.ordinal <- function(n) {
  switch(as.character(n),
    "1"="1st","2"="2nd","3"="3rd","4"="4th","5"="5th",
    "6"="6th","7"="7th","8"="8th","9"="9th",
    paste0(n, "th")
  )
}

make_game_narrative_html <- function(game, bs, sc) {
  tryCatch({
    away_name  <- game$teams$away$team$name
    home_name  <- game$teams$home$team$name
    away_score <- as.integer(game$teams$away$score)
    home_score <- as.integer(game$teams$home$score)
    if (is.null(away_score) || is.null(home_score)) return("")

    game_pk  <- game$gamePk
    away_win <- away_score > home_score
    winner   <- if (away_win) away_name else home_name
    loser    <- if (away_win) home_name else away_name
    margin   <- abs(away_score - home_score)

    .abbr <- function(nm) tail(strsplit(nm, " ")[[1]], 1)

    away_sp <- .sp_line(bs, "away")
    home_sp <- .sp_line(bs, "home")
    win_sp  <- if (away_win) away_sp else home_sp
    los_sp  <- if (away_win) home_sp else away_sp

    # Fetch play-by-play (scoring plays + top dramatic moment)
    pbp <- .fetch_play_by_play(game_pk)

    parts <- c()

    # ── 1. Result + winner's starter quality + best pitch ──────
    result_verb <- dplyr::case_when(
      margin == 1 ~ "edged",
      margin >= 7 ~ "rolled past",
      margin >= 4 ~ "handled",
      TRUE        ~ "defeated"
    )

    if (!is.null(win_sp)) {
      ip_val <- suppressWarnings(as.numeric(win_sp$ip))
      er_val <- suppressWarnings(as.integer(win_sp$er))
      k_val  <- suppressWarnings(as.integer(win_sp$k))

      quality <- if (!is.na(ip_val) && !is.na(er_val)) {
        if      (ip_val >= 7 && er_val <= 1) "dominant"
        else if (ip_val >= 6 && er_val <= 2) "sharp"
        else if (ip_val >= 6 && er_val <= 3) "solid"
        else if (ip_val >= 5)                "serviceable"
        else                                 "limited"
      } else "solid"

      # Best whiff pitch from Statcast
      best_pitch_note <- tryCatch({
        if (is.null(sc)) return("")
        win_sp_id <- if (away_win) bs$teams$away$pitchers[[1]] else bs$teams$home$pitchers[[1]]
        pm <- sc %>%
          dplyr::filter(pitcher == win_sp_id, !is.na(pitch_name)) %>%
          dplyr::group_by(pitch_name) %>%
          dplyr::summarise(
            n     = dplyr::n(),
            whiff = mean(description == "swinging_strike", na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::filter(n >= 5) %>%
          dplyr::arrange(dplyr::desc(whiff)) %>%
          dplyr::slice_head(n = 1)
        if (nrow(pm) == 0 || pm$whiff[1] < 0.20) return("")
        sprintf(" His %s was filthy, generating %.0f%% whiff.",
                pm$pitch_name[1], pm$whiff[1] * 100)
      }, error = function(e) "")

      k_note <- if (!is.na(k_val) && k_val >= 8) paste0(", fanning ", k_val) else
                if (!is.na(k_val) && k_val >= 6) paste0(", with ", k_val, " strikeouts") else ""

      parts <- c(parts, sprintf(
        "The %s %s the %s %d\u2013%d behind %s, who delivered a %s outing (%s IP, %sER%s).%s",
        winner, result_verb, loser,
        max(away_score, home_score), min(away_score, home_score),
        win_sp$name, quality, win_sp$ip, win_sp$er, k_note,
        best_pitch_note
      ))
    } else {
      parts <- c(parts, sprintf(
        "The %s %s the %s %d\u2013%d.",
        winner, result_verb, loser,
        max(away_score, home_score), min(away_score, home_score)
      ))
    }

    # ── 2. Key batter from top performers ──────────────────────
    top_batter_note <- tryCatch({
      if (is.null(bs) || length(bs$topPerformers) == 0) return("")
      for (tp in bs$topPerformers) {
        if (!tp$type %in% c("hitter", "batter")) next
        nm   <- tp$player$person$fullName
        summ <- tp$player$stats$batting$summary
        if (!is.null(summ) && nchar(summ) > 0)
          return(sprintf("%s paced the offense, going %s.", nm, summ))
      }
      ""
    }, error = function(e) "")
    if (nchar(top_batter_note) > 0) parts <- c(parts, top_batter_note)

    # ── 3. Score flow / turning point from play-by-play ────────
    if (!is.null(pbp)) {

      # Most dramatic scoring play (captivatingIndex)
      if (!is.null(pbp$scoring_plays) && nrow(pbp$scoring_plays) > 0) {
        sp <- pbp$scoring_plays
        # Find the highest-RBI or highest captivatingIndex scoring play
        key_play <- sp[which.max(sp$captivating + sp$rbi * 10), ]
        if (!is.null(key_play) && nrow(key_play) > 0 && nchar(key_play$description[1]) > 0) {
          # Trim to first sentence of the description
          desc <- gsub("\\. .*$", ".", key_play$description[1])
          desc <- gsub("scores\\.", "scores", desc)
          inn_lbl <- sprintf("In the %s", .ordinal(key_play$inning[1]))
          parts <- c(parts, paste0(inn_lbl, ": ", desc))
        }
      }

      # Most dramatic moment overall (captivatingIndex >= 80, not already captured)
      if (!is.null(pbp$top_play) && nrow(pbp$top_play) > 0) {
        tp <- pbp$top_play
        if (!tp$event[1] %in% c("Home Run", "Single", "Double", "Triple") ||
            tp$captivating[1] >= 90) {
          desc <- gsub("\\. .*$", ".", tp$description[1])
          parts <- c(parts, sprintf(
            "Play of the game (captivating index: %d): %s",
            tp$captivating[1], desc
          ))
        }
      }
    }

    # ── 4. Loser's starter if they got knocked around ──────────
    if (!is.null(los_sp)) {
      er_val <- suppressWarnings(as.integer(los_sp$er))
      ip_val <- suppressWarnings(as.numeric(los_sp$ip))
      if (!is.na(er_val) && !is.na(ip_val) && er_val >= 4) {
        parts <- c(parts, sprintf(
          "%s struggled for the %s, taking the hook after %s IP and %s earned runs.",
          los_sp$name, .abbr(loser), los_sp$ip, los_sp$er
        ))
      }
    }

    # ── 5. Key reliever moments ─────────────────────────────────
    dec <- game$decisions
    if (!is.null(dec) && !is.null(bs)) {

      # Save — give more detail
      if (!is.null(dec$save)) {
        sv_line <- tryCatch({
          sv_id <- dec$save$id
          for (side in c("away", "home")) {
            p <- bs$teams[[side]]$players[[paste0("ID", sv_id)]]
            if (!is.null(p)) {
              s <- p$stats$pitching
              return(list(ip = s$inningsPitched, er = s$earnedRuns, k = s$strikeOuts))
            }
          }
          NULL
        }, error = function(e) NULL)

        if (!is.null(sv_line) && !is.null(sv_line$ip)) {
          er_part <- if (!is.null(sv_line$er) && as.integer(sv_line$er) > 0)
            paste0(", ", sv_line$er, "ER") else ""
          k_part  <- if (!is.null(sv_line$k)  && as.integer(sv_line$k)  > 0)
            paste0(", ", sv_line$k, "K") else ""
          closer_verb <- if (margin == 1) "held on for" else "locked it down for"
          parts <- c(parts, sprintf(
            "%s %s the save (%s IP%s%s).",
            dec$save$fullName, closer_verb, sv_line$ip, er_part, k_part
          ))
        }
      }

      # Loss: richer narrative when a reliever blew it
      if (!is.null(dec$loser)) {
        los_side     <- if (away_win) "home" else "away"
        los_pitchers <- bs$teams[[los_side]]$pitchers
        loser_id     <- as.integer(dec$loser$id)
        is_rp        <- length(los_pitchers) > 1 && loser_id != as.integer(los_pitchers[[1]])

        rp_line <- tryCatch({
          p <- bs$teams[[los_side]]$players[[paste0("ID", loser_id)]]
          if (!is.null(p)) list(ip = p$stats$pitching$inningsPitched,
                                er = p$stats$pitching$earnedRuns) else NULL
        }, error = function(e) NULL)

        if (is_rp) {
          # Find the inning they entered (first play with their pitcher_id in pbp)
          entry_inn <- tryCatch({
            if (is.null(pbp)) return(NULL)
            all_plays_raw <- .fetch_play_by_play(game_pk)
            # Re-use pbp$scoring_plays which already has pitcher_id
            sp <- pbp$scoring_plays
            if (is.null(sp) || nrow(sp) == 0) return(NULL)
            rp_plays <- sp[!is.na(sp$pitcher_id) & sp$pitcher_id == loser_id, ]
            if (nrow(rp_plays) == 0) return(NULL)
            rp_plays[order(rp_plays$inning), ][1, ]
          }, error = function(e) NULL)

          if (!is.null(entry_inn)) {
            # Trim play description to first sentence
            desc <- gsub("\\.\\.+", ".", entry_inn$description[1])
            desc <- regmatches(desc, regexpr("^[^.!?]+[.!?]", desc))
            if (length(desc) == 0 || nchar(desc) == 0)
              desc <- entry_inn$description[1]

            line_str <- if (!is.null(rp_line) && !is.null(rp_line$ip))
              sprintf(" (%s IP, %sER)", rp_line$ip, rp_line$er) else ""

            parts <- c(parts, sprintf(
              "%s came in for the %s and things unraveled \u2014 %s %s was charged with the loss%s.",
              dec$loser$fullName, .ordinal(entry_inn$inning[1]),
              desc, dec$loser$fullName, line_str
            ))
          } else {
            # Fallback: no matching scoring play found
            line_str <- if (!is.null(rp_line) && !is.null(rp_line$ip))
              sprintf(" (%s IP, %sER)", rp_line$ip, rp_line$er) else ""
            parts <- c(parts, sprintf(
              "%s was charged with the loss out of the bullpen%s.",
              dec$loser$fullName, line_str
            ))
          }
        }
      }
    }

    # ── 6. Spectacular defensive play from Statcast ─────────────
    if (!is.null(sc)) {
      spec <- tryCatch({
        sc %>%
          dplyr::filter(
            !is.na(estimated_ba_using_speedangle),
            estimated_ba_using_speedangle >= 0.60,
            bb_type %in% c("fly_ball", "line_drive"),
            events %in% c("field_out", "force_out", "sac_fly",
                           "double_play", "grounded_into_double_play")
          ) %>%
          dplyr::arrange(dplyr::desc(estimated_ba_using_speedangle)) %>%
          dplyr::slice_head(n = 1)
      }, error = function(e) data.frame())

      if (nrow(spec) > 0) {
        xba_pct <- round(spec$estimated_ba_using_speedangle[1] * 100)
        ev_str  <- if (!is.na(spec$launch_speed[1]))
          sprintf(" on a %.0f mph shot", spec$launch_speed[1]) else ""
        dist_str <- if (!is.na(spec$hit_distance_sc[1]) && spec$hit_distance_sc[1] > 0)
          sprintf(" (%.0f ft)", spec$hit_distance_sc[1]) else ""
        parts <- c(parts, sprintf(
          "\U0001f9e4 Defensive gem: %d%% xBA ball%s%s turned into an out — check the highlights.",
          xba_pct, ev_str, dist_str
        ))
      }
    }

    if (length(parts) == 0) return("")
    paste0('<div class="game-narrative">', paste(parts, collapse = " "), '</div>')

  }, error = function(e) "")
}

# ------------------------------------------------------------
# Build HTML card for one game
# ------------------------------------------------------------

make_game_recap_card <- function(game) {
  tryCatch({
    status <- game$status$detailedState
    is_final <- grepl("Final|Completed", status, ignore.case = TRUE)

    away_name  <- game$teams$away$team$name
    home_name  <- game$teams$home$team$name
    away_score <- game$teams$away$score
    home_score <- game$teams$home$score
    game_pk    <- game$gamePk

    # Abbreviated team names (last word)
    abbr <- function(nm) {
      parts <- strsplit(nm, " ")[[1]]
      tail(parts, 1)
    }

    # Fetch boxscore (SP lines + top performers) and Statcast pitch data
    bs <- if (is_final) .fetch_boxscore(game_pk) else NULL
    sc <- if (is_final) .fetch_statcast_game(game_pk) else NULL

    # Linescore innings
    innings_html <- tryCatch({
      inn <- game$linescore$innings
      if (length(inn) == 0) {
        ""
      } else {
        away_runs <- vapply(inn, function(i) if (!is.null(i$away$runs)) as.character(i$away$runs) else ".", character(1))
        home_runs <- vapply(inn, function(i) if (!is.null(i$home$runs)) as.character(i$home$runs) else ".", character(1))
        inn_nums  <- seq_along(inn)

        cells_hdr  <- paste(sprintf('<td>%s</td>', inn_nums), collapse = "")
        cells_away <- paste(sprintf('<td>%s</td>', away_runs), collapse = "")
        cells_home <- paste(sprintf('<td>%s</td>', home_runs), collapse = "")

        away_h <- if (!is.null(bs)) bs$teams$away$teamStats$batting$hits else game$teams$away$hits
        home_h <- if (!is.null(bs)) bs$teams$home$teamStats$batting$hits else game$teams$home$hits
        .v <- function(x, d = "") if (!is.null(x) && length(x) > 0) x else d
        paste0(
          '<table class="linescore-tbl">',
          '<tr><th></th>', cells_hdr, '<th>R</th><th>H</th><th>E</th></tr>',
          '<tr><td class="ls-team">', abbr(away_name), '</td>', cells_away,
            '<td class="ls-r">', .v(away_score, 0), '</td>',
            '<td class="ls-r">', .v(away_h, ""), '</td>',
            '<td class="ls-r">', .v(game$teams$away$errors, 0), '</td>',
          '</tr>',
          '<tr><td class="ls-team">', abbr(home_name), '</td>', cells_home,
            '<td class="ls-r">', .v(home_score, 0), '</td>',
            '<td class="ls-r">', .v(home_h, ""), '</td>',
            '<td class="ls-r">', .v(game$teams$home$errors, 0), '</td>',
          '</tr>',
          '</table>'
        )
      }
    }, error = function(e) "")

    # Decisions + key reliever stat lines (W, L, SV from boxscore)
    decisions_html <- tryCatch({
      dec <- game$decisions
      if (is.null(dec)) return("")

      .rp_line <- function(person_id, bs) {
        tryCatch({
          for (side in c("away", "home")) {
            key <- paste0("ID", person_id)
            p   <- bs$teams[[side]]$players[[key]]
            if (!is.null(p)) {
              s <- p$stats$pitching
              ip  <- s$inningsPitched
              h   <- s$hits
              er  <- s$earnedRuns
              bb  <- s$baseOnBalls
              k   <- s$strikeOuts
              if (!is.null(ip)) {
                parts <- c(paste0(ip, " IP"))
                if (!is.null(h))  parts <- c(parts, paste0(h, "H"))
                if (!is.null(er)) parts <- c(parts, paste0(er, "ER"))
                if (!is.null(bb) && bb > 0) parts <- c(parts, paste0(bb, "BB"))
                if (!is.null(k)  && k  > 0) parts <- c(parts, paste0(k, "K"))
                return(paste(parts, collapse = " "))
              }
            }
          }
          ""
        }, error = function(e) "")
      }

      lines <- c()
      for (role in list(
        list(lbl = "W",  person = dec$winner),
        list(lbl = "L",  person = dec$loser),
        list(lbl = "SV", person = dec$save)
      )) {
        if (!is.null(role$person)) {
          stat <- if (!is.null(bs)) .rp_line(role$person$id, bs) else ""
          txt  <- paste0(role$lbl, ": ", role$person$fullName)
          if (nchar(stat) > 0) txt <- paste0(txt, " (", stat, ")")
          lines <- c(lines, txt)
        }
      }
      if (length(lines) == 0) "" else
        paste0('<div class="decisions">', paste(lines, collapse = " &nbsp;&middot;&nbsp; "), '</div>')
    }, error = function(e) "")

    # Pitch mix per starter (from Statcast)
    pitch_mix_html <- tryCatch({
      if (is.null(sc) || is.null(bs)) {
        ""
      } else {
        away_sp_id <- tryCatch(bs$teams$away$pitchers[[1]], error = function(e) NULL)
        home_sp_id <- tryCatch(bs$teams$home$pitchers[[1]], error = function(e) NULL)
        away_sp    <- if (!is.null(bs)) .sp_line(bs, "away") else NULL
        home_sp    <- if (!is.null(bs)) .sp_line(bs, "home") else NULL
        away_html  <- if (!is.null(away_sp_id) && !is.null(away_sp))
          .pitch_mix_html(sc, away_sp_id, away_sp$name) else ""
        home_html  <- if (!is.null(home_sp_id) && !is.null(home_sp))
          .pitch_mix_html(sc, home_sp_id, home_sp$name) else ""
        paste0(away_html, home_html)
      }
    }, error = function(e) "")

    # Game narrative blurb
    narrative_html <- if (is_final) make_game_narrative_html(game, bs, sc) else ""

    # Spectacular plays (from Statcast)
    spectacular_html <- if (!is.null(sc)) .spectacular_plays_html(sc) else ""

    # Starting pitcher lines (from boxscore)
    sp_html <- tryCatch({
      if (is.null(bs)) {
        ""
      } else {
        fmt_sp <- function(sp, team_abbr) {
          if (is.null(sp)) return("")
          pc_str <- if (!is.null(sp$pc) && !is.na(sp$pc)) paste0(", ", sp$pc, "p") else ""
          .v2 <- function(x) if (!is.null(x) && length(x) > 0) x else "?"
          sprintf(
            '<span class="sp-team">%s</span> <span class="sp-name">%s</span> &mdash; %s IP, %sH %sER %sBB %sK%s',
            team_abbr, sp$name,
            .v2(sp$ip), .v2(sp$h), .v2(sp$er), .v2(sp$bb), .v2(sp$k), pc_str
          )
        }
        lines <- c(
          fmt_sp(.sp_line(bs, "away"), abbr(away_name)),
          fmt_sp(.sp_line(bs, "home"), abbr(home_name))
        )
        lines <- lines[nchar(lines) > 0]
        if (length(lines) == 0) "" else
          paste0('<div class="sp-lines">', paste(lines, collapse = "<br>"), '</div>')
      }
    }, error = function(e) "")

    # HR list (from boxscore player stats)
    hr_html <- tryCatch({
      if (is.null(bs)) {
        ""
      } else {
        hr_lines <- c()
        for (side in c("away", "home")) {
          team_lbl <- abbr(if (side == "away") away_name else home_name)
          for (pid in names(bs$teams[[side]]$players)) {
            p   <- bs$teams[[side]]$players[[pid]]
            hrs <- p$stats$batting$homeRuns
            if (!is.null(hrs) && !is.na(hrs) && hrs > 0) {
              rbi <- dplyr::coalesce(p$stats$batting$rbi, 0L)
              hr_lines <- c(hr_lines,
                paste0(p$person$fullName, " (", team_lbl, ")",
                       if (hrs > 1) paste0(" \u00d7", hrs) else "",
                       if (rbi > 0) paste0(", ", rbi, " RBI") else ""))
            }
          }
        }
        if (length(hr_lines) == 0) "" else
          paste0('<div class="hr-list">\u26be&nbsp;HR: ', paste(hr_lines, collapse = " \u00b7 "), '</div>')
      }
    }, error = function(e) "")

    # Top performers (MLB's pre-built list — hitters only here; starter shown in SP line)
    performers_html <- tryCatch({
      if (is.null(bs) || length(bs$topPerformers) == 0) {
        ""
      } else {
        lines <- vapply(bs$topPerformers, function(tp) {
          if (!tp$type %in% c("hitter", "batter")) return("")
          nm   <- tp$player$person$fullName
          summ <- tp$player$stats$batting$summary
          if (is.null(summ) || length(summ) == 0) return("")
          sprintf('<span>\u2b50 %s: %s</span>', nm, summ)
        }, character(1))
        lines <- lines[nchar(lines) > 0]
        if (length(lines) == 0) "" else
          paste0('<div class="hr-list">', paste(lines, collapse = " &nbsp;&middot;&nbsp; "), '</div>')
      }
    }, error = function(e) "")

    # Score header + special game badges
    away_win  <- is_final && !is.null(away_score) && !is.null(home_score) && away_score > home_score
    home_win  <- is_final && !is.null(away_score) && !is.null(home_score) && home_score > away_score
    n_innings <- tryCatch(length(game$linescore$innings), error = function(e) 9L)
    is_extra  <- isTRUE(n_innings > 9)
    is_shutout <- is_final && (
      (isTRUE(away_win) && isTRUE(dplyr::coalesce(home_score, 1L) == 0L)) ||
      (isTRUE(home_win) && isTRUE(dplyr::coalesce(away_score, 1L) == 0L))
    )
    margin <- if (is_final && !is.null(away_score) && !is.null(home_score))
      abs(away_score - home_score) else NA_integer_
    is_walkoff <- isTRUE(home_win) && isTRUE(n_innings >= 9)

    score_html <- sprintf(
      '<div class="rc-matchup">%s <span class="rc-score %s">%s</span> @ <span class="rc-score %s">%s</span> %s</div>',
      away_name,
      if (isTRUE(away_win)) "rc-winner" else "",
      if (is_final) as.character(dplyr::coalesce(away_score, 0)) else "\u2013",
      if (isTRUE(home_win)) "rc-winner" else "",
      if (is_final) as.character(dplyr::coalesce(home_score, 0)) else "\u2013",
      home_name
    )

    badges <- c()
    if (!is_final)              badges <- c(badges, paste0('<span class="rc-status">', status, '</span>'))
    if (is_extra)               badges <- c(badges, paste0('<span class="rc-status" style="background:#d4edda;color:#155724;">F/', n_innings, '</span>'))
    if (is_shutout)             badges <- c(badges, '<span class="rc-status" style="background:#cce5ff;color:#004085;">SHO</span>')
    if (!is.na(margin) && margin == 1) badges <- c(badges, '<span class="rc-status" style="background:#fff3cd;color:#856404;">1-run</span>')
    badge_html <- paste(badges, collapse = " ")

    paste0(
      '<div class="recap-card">',
      score_html, badge_html,
      innings_html,
      sp_html,
      pitch_mix_html,
      hr_html,
      performers_html,
      narrative_html,
      spectacular_html,
      decisions_html,
      '</div>'
    )
  }, error = function(e) {
    paste0('<div class="recap-card"><p style="color:#888;">Game data unavailable.</p></div>')
  })
}

# ------------------------------------------------------------
# Build the full recap HTML for all games on a date
# ------------------------------------------------------------

make_recap_html <- function(recap_date = Sys.Date() - 1) {
  games <- pull_yesterday_results(recap_date)

  if (is.null(games) || length(games) == 0) {
    return(paste0(
      '<p style="color:#888; padding:20px 0;">',
      'No completed games found for ', format(recap_date, "%B %d, %Y"), '.',
      '</p>'
    ))
  }

  cards <- vapply(games, make_game_recap_card, character(1))
  paste0('<div class="recap-grid">', paste(cards, collapse = "\n"), '</div>')
}
