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
# Extract the line for a starting pitcher from boxscore
# ------------------------------------------------------------

.sp_line <- function(game, side) {
  tryCatch({
    pitchers <- game$boxscore$teams[[side]]$pitchers
    if (length(pitchers) == 0) return(NULL)
    sp_id  <- pitchers[[1]]
    stats  <- game$boxscore$teams[[side]]$players[[paste0("ID", sp_id)]]$stats$pitching
    info   <- game$boxscore$teams[[side]]$players[[paste0("ID", sp_id)]]$person

    list(
      name  = info$fullName,
      ip    = stats$inningsPitched,
      h     = stats$hits,
      er    = stats$earnedRuns,
      bb    = stats$baseOnBalls,
      k     = stats$strikeOuts,
      pc    = stats$pitchesThrown
    )
  }, error = function(e) NULL)
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

    # Abbreviated team names (last word or abbreviation)
    abbr <- function(nm) {
      parts <- strsplit(nm, " ")[[1]]
      tail(parts, 1)
    }

    # Linescore innings
    innings_html <- tryCatch({
      inn <- game$linescore$innings
      if (length(inn) == 0) return("")
      away_runs <- vapply(inn, function(i) as.character(dplyr::coalesce(i$away$runs, ".")), character(1))
      home_runs <- vapply(inn, function(i) as.character(dplyr::coalesce(i$home$runs, ".")), character(1))
      inn_nums  <- seq_along(inn)

      cells_hdr  <- paste(sprintf('<td>%s</td>', inn_nums), collapse = "")
      cells_away <- paste(sprintf('<td>%s</td>', away_runs), collapse = "")
      cells_home <- paste(sprintf('<td>%s</td>', home_runs), collapse = "")

      paste0(
        '<table class="linescore-tbl">',
        '<tr><th></th>', cells_hdr, '<th>R</th><th>H</th><th>E</th></tr>',
        '<tr><td class="ls-team">', abbr(away_name), '</td>', cells_away,
          '<td class="ls-r">', dplyr::coalesce(away_score, 0), '</td>',
          '<td class="ls-r">', dplyr::coalesce(game$teams$away$hits, ""), '</td>',
          '<td class="ls-r">', dplyr::coalesce(game$teams$away$errors, 0), '</td>',
        '</tr>',
        '<tr><td class="ls-team">', abbr(home_name), '</td>', cells_home,
          '<td class="ls-r">', dplyr::coalesce(home_score, 0), '</td>',
          '<td class="ls-r">', dplyr::coalesce(game$teams$home$hits, ""), '</td>',
          '<td class="ls-r">', dplyr::coalesce(game$teams$home$errors, 0), '</td>',
        '</tr>',
        '</table>'
      )
    }, error = function(e) "")

    # Decisions
    decisions_html <- tryCatch({
      dec <- game$decisions
      lines <- c()
      if (!is.null(dec$winner))
        lines <- c(lines, paste0("W: ", dec$winner$fullName))
      if (!is.null(dec$loser))
        lines <- c(lines, paste0("L: ", dec$loser$fullName))
      if (!is.null(dec$save))
        lines <- c(lines, paste0("SV: ", dec$save$fullName))
      if (length(lines) == 0) return("")
      paste0('<div class="decisions">', paste(lines, collapse = " &nbsp;&middot;&nbsp; "), '</div>')
    }, error = function(e) "")

    # Starting pitcher lines
    sp_html <- tryCatch({
      away_sp <- .sp_line(game, "away")
      home_sp <- .sp_line(game, "home")

      fmt_sp <- function(sp, team_abbr) {
        if (is.null(sp)) return("")
        pc_str <- if (!is.null(sp$pc) && !is.na(sp$pc)) paste0(", ", sp$pc, "p") else ""
        sprintf(
          '<span class="sp-team">%s</span> <span class="sp-name">%s</span> &mdash; %s IP, %sH %sER %sBB %sK%s',
          team_abbr, sp$name,
          dplyr::coalesce(sp$ip, "?"),
          dplyr::coalesce(sp$h,  "?"),
          dplyr::coalesce(sp$er, "?"),
          dplyr::coalesce(sp$bb, "?"),
          dplyr::coalesce(sp$k,  "?"),
          pc_str
        )
      }
      lines <- c(
        fmt_sp(away_sp, abbr(away_name)),
        fmt_sp(home_sp, abbr(home_name))
      )
      lines <- lines[nchar(lines) > 0]
      if (length(lines) == 0) return("")
      paste0('<div class="sp-lines">', paste(lines, collapse = "<br>"), '</div>')
    }, error = function(e) "")

    # HR list
    hr_html <- tryCatch({
      hr_players <- c()
      for (side in c("away", "home")) {
        players <- game$boxscore$teams[[side]]$players
        for (pid in names(players)) {
          p     <- players[[pid]]
          hrs   <- p$stats$batting$homeRuns
          if (!is.null(hrs) && !is.na(hrs) && hrs > 0) {
            hr_players <- c(hr_players,
              paste0(p$person$fullName, " (", if (side == "away") abbr(away_name) else abbr(home_name),
                     if (hrs > 1) paste0(", ", hrs) else "", ")"))
          }
        }
      }
      if (length(hr_players) == 0) return("")
      paste0('<div class="hr-list">\u26be HR: ', paste(hr_players, collapse = ", "), '</div>')
    }, error = function(e) "")

    # Score header
    away_win <- is_final && !is.null(away_score) && !is.null(home_score) && away_score > home_score
    home_win <- is_final && !is.null(away_score) && !is.null(home_score) && home_score > away_score

    score_html <- sprintf(
      '<div class="rc-matchup">%s <span class="rc-score %s">%s</span> @ <span class="rc-score %s">%s</span> %s</div>',
      away_name,
      if (isTRUE(away_win)) "rc-winner" else "",
      if (is_final) as.character(dplyr::coalesce(away_score, 0)) else "–",
      if (isTRUE(home_win)) "rc-winner" else "",
      if (is_final) as.character(dplyr::coalesce(home_score, 0)) else "–",
      home_name
    )

    status_badge <- if (!is_final) {
      paste0('<span class="rc-status">', status, '</span>')
    } else ""

    paste0(
      '<div class="recap-card">',
      score_html, status_badge,
      innings_html,
      sp_html,
      hr_html,
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
