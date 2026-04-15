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

  m <- regmatches(f, regexec(
    "^(scouting|deepdive|print|team|matchup|prediction|hitting)_(\\d{4}-\\d{2}-\\d{2})(?:_([A-Z0-9]+)(?:_([A-Z0-9]+))?)?\\.html$",
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
    dd_file <- file_df$file[
      file_df$type == "deepdive" &
      !is.na(file_df$date) & file_df$date == d &
      !is.na(file_df$away) & file_df$away == games_on_date$away[j] &
      !is.na(file_df$home) & file_df$home == games_on_date$home[j]]
    if (length(dd_file) > 0) {
      game_links <- paste0(game_links, sprintf(
        '<a href="%s" class="past-game-link">%s @ %s</a>',
        dd_file[1], games_on_date$away[j], games_on_date$home[j]
      ))
    }
  }

  scouting_btn <- if (length(scouting_f) > 0) {
    sprintf('<a href="%s" class="past-overview-btn">Overview</a>', scouting_f[1])
  } else ""

  past_rows <- paste0(past_rows, sprintf('
    <div class="past-row">
      <span class="past-date">%s</span>
      %s
      <div class="past-game-links">%s</div>
    </div>', d, scouting_btn, game_links))
}

if (nchar(past_rows) == 0) {
  past_rows <- '<p style="color:#888;padding:8px 0;">No past reports yet.</p>'
}

# Stat reference link
ref_link <- if ("stat_reference.html" %in% all_files) {
  '<a href="stat_reference.html" class="ref-link">Stat Reference &amp; Grade Ranges</a>'
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
    <div class="header-links">%s%s</div>
  </header>
  <main>
    <div class="today-date">%s</div>
    %s
    <div class="section-title">Today\'s Games</div>
    <div class="games-grid">%s</div>

    <details class="past-section">
      <summary class="section-title" style="cursor:pointer; list-style:none; display:flex; align-items:center; gap:8px;">
        Past Games <span style="font-size:0.7rem; color:#58a6ff; font-weight:400;">click to expand</span>
      </summary>
      <div style="margin-top:10px;">%s</div>
    </details>
  </main>
  <div class="footer">
    Updated %s ET &nbsp;·&nbsp; Reports auto-refresh at 10 AM, 4 PM, 6:30 PM ET
  </div>
</body>
</html>',
  today_str,            # title date
  recap_link,           # header recap link
  ref_link,             # header ref link
  today_str,            # h2 date
  scouting_link,        # overview link
  today_cards,          # game cards
  past_rows,            # past games
  format(Sys.time(), "%Y-%m-%d %I:%M %p", tz = "America/New_York")
)

writeLines(html, "reports/index.html")
message("index.html generated: reports/index.html")
