# ============================================================
# mlb_scouting_report
# SCRIPT: generate_index.R
# ============================================================
# PURPOSE:
#   Build the index.html landing page deployed to GitHub Pages.
#   Lists today's games with links to all report types,
#   plus a "Past Games" archive section.
# ============================================================

if (!dir.exists("reports")) dir.create("reports")

today_str <- as.character(Sys.Date())

# ------------------------------------------------------------
# Scan reports/ for all HTML files
# ------------------------------------------------------------

all_files <- list.files("reports", pattern = "\\.html$", full.names = FALSE)

# Parse filenames into structured data
parse_report_file <- function(f) {
  # Expected patterns:
  #   scouting_YYYY-MM-DD.html
  #   deepdive_YYYY-MM-DD_AWAY_HOME.html
  #   print_YYYY-MM-DD_AWAY_HOME.html
  #   team_YYYY-MM-DD_ABBR.html
  #   matchup_YYYY-MM-DD_AWAY_HOME.html
  #   prediction_YYYY-MM-DD_AWAY_HOME.html
  #   hitting_YYYY-MM-DD_AWAY_HOME.html
  #   stat_reference.html

  if (f == "stat_reference.html") {
    return(data.frame(type="reference", date=NA_character_,
                      away=NA_character_, home=NA_character_,
                      file=f, stringsAsFactors=FALSE))
  }

  if (grepl("^recap_\\d{4}-\\d{2}-\\d{2}\\.html$", f)) {
    dm <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
    return(data.frame(type="recap", date=dm,
                      away=NA_character_, home=NA_character_,
                      file=f, stringsAsFactors=FALSE))
  }

  if (grepl("^series_preview_", f)) {
    dm <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
    parts <- strsplit(gsub("\\.html$", "", f), "_")[[1]]
    n <- length(parts)
    return(data.frame(type="series_preview", date=dm,
                      away=if(n>=5) parts[n-1] else NA_character_,
                      home=if(n>=5) parts[n]   else NA_character_,
                      file=f, stringsAsFactors=FALSE))
  }

  if (grepl("^series_recap_", f)) {
    dm <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
    parts <- strsplit(gsub("\\.html$", "", f), "_")[[1]]
    n <- length(parts)
    return(data.frame(type="series_recap", date=dm,
                      away=if(n>=5) parts[n-1] else NA_character_,
                      home=if(n>=5) parts[n]   else NA_character_,
                      file=f, stringsAsFactors=FALSE))
  }

  m <- regmatches(f, regexec(
    "^(scouting|deepdive|print|team|matchup|prediction|hitting|result)_(\\d{4}-\\d{2}-\\d{2})(?:_([A-Z0-9]+)(?:_([A-Z0-9]+))?)?\\.html$",
    f
  ))[[1]]

  if (length(m) == 0) return(NULL)

  data.frame(
    type = m[2],
    date = m[3],
    away = if (nchar(m[4]) > 0) m[4] else NA_character_,
    home = if (nchar(m[5]) > 0) m[5] else NA_character_,
    file = f,
    stringsAsFactors = FALSE
  )
}

file_df <- do.call(rbind, Filter(Negate(is.null), lapply(all_files, parse_report_file)))

if (is.null(file_df) || nrow(file_df) == 0) {
  file_df <- data.frame(type=character(), date=character(),
                        away=character(), home=character(),
                        file=character(), stringsAsFactors=FALSE)
}

# ------------------------------------------------------------
# Build game cards for a given date
# ------------------------------------------------------------

build_game_card <- function(date_str, away, home, files_for_game) {
  link_btn <- function(label, file, color = "#1a73e8") {
    if (is.na(file) || !file %in% all_files) return("")
    sprintf(
      '<a href="%s" class="btn" style="background:%s;">%s</a>',
      file, color, label
    )
  }

  deepdive_f   <- files_for_game$file[files_for_game$type == "deepdive"][1]
  print_f      <- files_for_game$file[files_for_game$type == "print"][1]
  matchup_f    <- files_for_game$file[files_for_game$type == "matchup"][1]
  prediction_f <- files_for_game$file[files_for_game$type == "prediction"][1]
  hitting_f    <- files_for_game$file[files_for_game$type == "hitting"][1]

  btns <- paste(
    link_btn("Deep Dive",   deepdive_f,   "#1a73e8"),
    link_btn("Matchup",     matchup_f,    "#0f9d58"),
    link_btn("Prediction",  prediction_f, "#f4511e"),
    link_btn("Hitting",     hitting_f,    "#9334e6"),
    link_btn("Print View",  print_f,      "#5f6368"),
    sep = "\n    "
  )

  title <- if (!is.na(away) && !is.na(home)) {
    sprintf("%s <span style='color:#999;font-weight:400'>@</span> %s", away, home)
  } else {
    "Full Slate"
  }

  sprintf('
  <div class="game-card">
    <div class="matchup">%s</div>
    <div class="btns">%s</div>
  </div>', title, btns)
}

# ------------------------------------------------------------
# Collect today's games
# ------------------------------------------------------------

today_games <- file_df[!is.na(file_df$date) & file_df$date == today_str &
                          file_df$type %in% c("deepdive", "print", "matchup",
                                               "prediction", "hitting"), ]

# Get unique matchups for today
today_matchups <- unique(today_games[, c("away", "home")])
today_matchups <- today_matchups[!is.na(today_matchups$away), ]

today_cards <- ""
if (nrow(today_matchups) > 0) {
  for (i in seq_len(nrow(today_matchups))) {
    aw <- today_matchups$away[i]
    hm <- today_matchups$home[i]
    files_m <- today_games[
      (is.na(today_games$away) | today_games$away == aw) &
      (is.na(today_games$home) | today_games$home == hm), ]
    today_cards <- paste0(today_cards, build_game_card(today_str, aw, hm, files_m))
  }
} else {
  today_cards <- '<p style="color:#888;padding:16px;">No game reports found for today yet. Check back after the 10 AM run.</p>'
}

# Also add scouting overview for today if it exists
scouting_today <- file_df$file[file_df$type == "scouting" & !is.na(file_df$date) & file_df$date == today_str]
scouting_link <- if (length(scouting_today) > 0) {
  sprintf('<a href="%s" class="overview-link">View Full Scouting Overview &rarr;</a>', scouting_today[1])
} else ""

# ------------------------------------------------------------
# Build past dates archive
# ------------------------------------------------------------

all_dates <- sort(unique(file_df$date[!is.na(file_df$date)]), decreasing = TRUE)
past_dates <- all_dates[all_dates < today_str]

past_rows <- ""
for (d in past_dates) {
  scouting_f <- file_df$file[file_df$type == "scouting" & !is.na(file_df$date) & file_df$date == d]
  games_on_date <- unique(file_df[
    !is.na(file_df$date) & file_df$date == d &
    file_df$type == "deepdive", c("away", "home")])
  games_on_date <- games_on_date[!is.na(games_on_date$away), ]

  game_links <- ""
  for (j in seq_len(nrow(games_on_date))) {
    aw <- games_on_date$away[j]
    hm <- games_on_date$home[j]

    dd_file <- file_df$file[
      file_df$type == "deepdive" &
      !is.na(file_df$date) & file_df$date == d &
      !is.na(file_df$away) & file_df$away == aw &
      !is.na(file_df$home) & file_df$home == hm]
    res_file <- file_df$file[
      file_df$type == "result" &
      !is.na(file_df$date) & file_df$date == d &
      !is.na(file_df$away) & file_df$away == aw &
      !is.na(file_df$home) & file_df$home == hm]

    # Prefer the post-game result page (final score, prediction accuracy,
    # box score highlights) over the pre-game deep dive once the game is
    # actually over — that's what a past game should link to.
    link_file <- if (length(res_file) > 0) res_file[1] else if (length(dd_file) > 0) dd_file[1] else NA_character_

    if (!is.na(link_file)) {
      game_links <- paste0(game_links, sprintf(
        '<a href="%s" class="past-game-link">%s @ %s</a>',
        link_file, aw, hm
      ))
    }
  }

  scouting_btn <- if (length(scouting_f) > 0) {
    sprintf('<a href="%s" class="past-overview-btn">Overview</a>', scouting_f[1])
  } else ""

  recap_f  <- paste0("recap_", d, ".html")
  results_btn <- if (recap_f %in% all_files) {
    sprintf('<a href="%s" class="past-overview-btn" style="background:#1a3c6e;border-color:#58a6ff;">Results</a>', recap_f)
  } else ""

  past_rows <- paste0(past_rows, sprintf('
    <div class="past-row">
      <span class="past-date">%s</span>
      %s
      %s
      <div class="past-game-links">%s</div>
    </div>', d, results_btn, scouting_btn, game_links))
}

if (nchar(past_rows) == 0) {
  past_rows <- '<p style="color:#888;padding:8px 0;">No past reports yet.</p>'
}

# ------------------------------------------------------------
# Series preview / recap cards
# ------------------------------------------------------------

series_files <- file_df[file_df$type %in% c("series_preview", "series_recap"), ]
series_files <- series_files[order(series_files$date, decreasing = TRUE), ]

.series_card <- function(sf) {
  label <- if (sf$type == "series_preview") "Series Preview" else "Series Recap"
  matchup <- if (!is.na(sf$away) && !is.na(sf$home)) {
    paste0(sf$away, " @ ", sf$home)
  } else sf$file
  color <- if (sf$type == "series_preview") "#1a73e8" else "#0f9d58"
  sprintf(
    '<a href="%s" class="series-card" style="border-left:4px solid %s;">
      <span class="series-label" style="color:%s;">%s</span>
      <span class="series-matchup">%s</span>
      <span class="series-date">%s</span>
    </a>',
    sf$file, color, color, label, matchup, sf$date
  )
}

# Only the most recent preview and most recent recap are promoted to the
# homepage — a series from weeks ago shouldn't sit front and center.
# Everything else (still fully rendered and linkable) moves into the
# collapsed Series Archive below, same idea as Past Games.
current_series <- series_files[0, ]
if (nrow(series_files) > 0) {
  latest_preview <- series_files[series_files$type == "series_preview", ][1, ]
  latest_recap   <- series_files[series_files$type == "series_recap", ][1, ]
  current_series <- rbind(
    series_files[0, ],
    latest_preview[!is.na(latest_preview$file), ],
    latest_recap[!is.na(latest_recap$file), ]
  )
}
archived_series <- if (nrow(current_series) > 0) {
  series_files[!series_files$file %in% current_series$file, ]
} else {
  series_files
}

series_cards_html <- ""
if (nrow(current_series) > 0) {
  for (i in seq_len(nrow(current_series))) {
    series_cards_html <- paste0(series_cards_html, .series_card(current_series[i, ]))
  }
}

series_section <- if (nchar(series_cards_html) > 0) {
  paste0('<div class="section-title">Braves Series</div>',
         '<div class="series-grid">', series_cards_html, '</div>')
} else ""

archived_series_html <- ""
if (nrow(archived_series) > 0) {
  for (i in seq_len(nrow(archived_series))) {
    archived_series_html <- paste0(archived_series_html, .series_card(archived_series[i, ]))
  }
}

series_archive_section <- if (nchar(archived_series_html) > 0) {
  paste0(
    '<details class="past-section">',
    '<summary class="section-title" style="cursor:pointer; list-style:none; display:flex; align-items:center; gap:8px;">',
    'Series Archive <span style="font-size:0.7rem; color:#58a6ff; font-weight:400;">click to expand</span>',
    '</summary>',
    '<div class="series-grid" style="margin-top:10px;">', archived_series_html, '</div>',
    '</details>'
  )
} else ""

# Stat reference link
ref_link <- if ("stat_reference.html" %in% all_files) {
  '<a href="stat_reference.html" class="ref-link">Stat Reference &amp; Grade Ranges</a>'
} else ""

# Accuracy tracker link
accuracy_link <- if ("accuracy.html" %in% all_files) {
  '<a href="accuracy.html" class="ref-link" style="background:#0d1117;border-color:#58a6ff;">&#127919; Prediction Accuracy</a>'
} else ""

# Yesterday's recap link
yesterday_str   <- as.character(Sys.Date() - 1)
recap_file      <- paste0("recap_", yesterday_str, ".html")
recap_link <- if (recap_file %in% all_files) {
  sprintf('<a href="%s" class="ref-link" style="background:#0d1117; border-color:#58a6ff;">&#x23F0; Yesterday\'s Results</a>',
          recap_file)
} else ""

# ------------------------------------------------------------
# Assemble HTML
# ------------------------------------------------------------

html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>MLB Scouting Report — %s</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      background: #0d1117;
      color: #e6edf3;
      min-height: 100vh;
    }
    header {
      background: #161b22;
      border-bottom: 1px solid #30363d;
      padding: 18px 32px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-wrap: wrap;
      gap: 12px;
    }
    header h1 {
      font-size: 1.4rem;
      font-weight: 700;
      letter-spacing: -0.3px;
    }
    header h1 span { color: #58a6ff; }
    .header-links { display: flex; gap: 14px; align-items: center; }
    .ref-link {
      color: #58a6ff;
      text-decoration: none;
      font-size: 0.85rem;
      border: 1px solid #30363d;
      padding: 5px 12px;
      border-radius: 6px;
    }
    .ref-link:hover { background: #161b22; }
    main { max-width: 960px; margin: 0 auto; padding: 28px 20px 60px; }
    .section-title {
      font-size: 0.78rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 1px;
      color: #8b949e;
      margin-bottom: 14px;
      padding-bottom: 8px;
      border-bottom: 1px solid #21262d;
    }
    .today-date {
      font-size: 1.6rem;
      font-weight: 700;
      margin-bottom: 6px;
      color: #e6edf3;
    }
    .overview-link {
      display: inline-block;
      color: #58a6ff;
      text-decoration: none;
      font-size: 0.88rem;
      margin-bottom: 20px;
    }
    .overview-link:hover { text-decoration: underline; }
    .games-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
      gap: 14px;
      margin-bottom: 40px;
    }
    .game-card {
      background: #161b22;
      border: 1px solid #30363d;
      border-radius: 10px;
      padding: 16px;
    }
    .matchup {
      font-size: 1rem;
      font-weight: 700;
      margin-bottom: 12px;
      color: #e6edf3;
    }
    .btns {
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
    }
    .btn {
      display: inline-block;
      padding: 5px 11px;
      border-radius: 6px;
      font-size: 0.78rem;
      font-weight: 600;
      color: white;
      text-decoration: none;
      transition: opacity 0.15s;
    }
    .btn:hover { opacity: 0.85; }
    /* Past games */
    .past-section { margin-top: 8px; }
    .past-row {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      gap: 8px;
      padding: 10px 0;
      border-bottom: 1px solid #21262d;
    }
    .past-date {
      font-size: 0.85rem;
      font-weight: 600;
      color: #8b949e;
      min-width: 100px;
    }
    .past-overview-btn {
      background: #21262d;
      color: #58a6ff;
      text-decoration: none;
      font-size: 0.78rem;
      padding: 3px 9px;
      border-radius: 5px;
      font-weight: 600;
    }
    .past-game-links { display: flex; flex-wrap: wrap; gap: 6px; }
    .past-game-link {
      background: #0d1117;
      border: 1px solid #30363d;
      color: #8b949e;
      text-decoration: none;
      font-size: 0.78rem;
      padding: 3px 9px;
      border-radius: 5px;
    }
    .past-game-link:hover { color: #e6edf3; border-color: #58a6ff; }
    .series-grid {
      display: flex;
      flex-wrap: wrap;
      gap: 12px;
      margin-bottom: 32px;
    }
    .series-card {
      background: #161b22;
      border: 1px solid #30363d;
      border-radius: 10px;
      padding: 14px 18px;
      text-decoration: none;
      display: flex;
      flex-direction: column;
      gap: 4px;
      min-width: 220px;
    }
    .series-card:hover { border-color: #58a6ff; }
    .series-label { font-size: 0.75rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; }
    .series-matchup { font-size: 1rem; font-weight: 700; color: #e6edf3; }
    .series-date { font-size: 0.78rem; color: #8b949e; }
    .footer {
      text-align: center;
      font-size: 0.75rem;
      color: #484f58;
      padding: 24px 0 0;
    }
  </style>
</head>
<body>
  <header>
    <h1>⚾ MLB <span>Scouting Report</span></h1>
    <div class="header-links">%s%s%s</div>
  </header>
  <main>
    <div class="today-date">%s</div>
    %s
    %s
    <div class="section-title">Today\'s Games</div>
    <div class="games-grid">%s</div>

    <details class="past-section">
      <summary class="section-title" style="cursor:pointer; list-style:none; display:flex; align-items:center; gap:8px;">
        Past Games <span style="font-size:0.7rem; color:#58a6ff; font-weight:400;">click to expand</span>
      </summary>
      <div style="margin-top:10px;">%s</div>
    </details>
    %s
  </main>
  <div class="footer">
    Updated %s ET &nbsp;·&nbsp; Reports auto-refresh at 7:30 AM, 4:30 PM, 3 AM ET
  </div>
</body>
</html>',
  today_str,            # title date
  recap_link,           # header recap link
  ref_link,             # header ref link
  accuracy_link,        # header accuracy link
  today_str,            # h2 date
  scouting_link,        # overview link
  series_section,       # series preview / recap
  today_cards,          # game cards
  past_rows,            # past games
  series_archive_section, # older series preview/recap archive
  format(Sys.time(), "%Y-%m-%d %I:%M %p", tz = "America/New_York")
)

writeLines(html, "reports/index.html")
message("index.html generated: reports/index.html")
