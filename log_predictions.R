# ============================================================
# mlb_scouting_report
# SCRIPT: log_predictions.R
# ============================================================
# PURPOSE:
#   1. Log today's game predictions to data/predictions_log.csv
#      (skips games already logged by game_pk)
#   2. Fill in actual results for any past games without actuals
#      by calling the MLB Stats API
#
# CALLED FROM: run_pipeline_phase01.R (end of daily run)
# REQUIRES:    game_context, starter_matchup, lineup_context,
#              bullpen_grid in the environment
# ============================================================

LOG_PATH <- "data/predictions_log.csv"

if (!dir.exists("data")) dir.create("data")

# Source narrative helpers (needed for make_prediction_data)
if (!exists("make_prediction_data")) {
  suppressMessages({
    source("09_reporting/_report_helpers.R")
    source("09_reporting/_game_narrative_helpers.R")
  })
}

if (!exists("game_context") || nrow(game_context) == 0) {
  message("log_predictions: no game_context — skipping")
} else {

# ------------------------------------------------------------
# Schema
# ------------------------------------------------------------
log_schema <- dplyr::tibble(
  game_pk          = integer(),
  game_date        = character(),
  away_team        = character(),
  home_team        = character(),
  pred_away_runs   = numeric(),
  pred_home_runs   = numeric(),
  away_win_pct     = numeric(),
  home_win_pct     = numeric(),
  predicted_winner = character(),
  actual_away_runs = integer(),
  actual_home_runs = integer(),
  actual_winner    = character(),
  correct_winner   = logical(),
  # "" for a normal single game, "_G1"/"_G2" for a doubleheader —
  # see game_context$file_suffix (99_game_context.R). Captured here at
  # log time since game_context only covers the current run's target
  # date, but this log accumulates rows across many past days.
  file_suffix      = character()
)

# ------------------------------------------------------------
# Read existing log
# ------------------------------------------------------------
log_df <- if (file.exists(LOG_PATH)) {
  tryCatch(
    readr::read_csv(LOG_PATH, col_types = readr::cols(
      game_pk          = readr::col_integer(),
      game_date        = readr::col_character(),
      away_team        = readr::col_character(),
      home_team        = readr::col_character(),
      pred_away_runs   = readr::col_double(),
      pred_home_runs   = readr::col_double(),
      away_win_pct     = readr::col_double(),
      home_win_pct     = readr::col_double(),
      predicted_winner = readr::col_character(),
      actual_away_runs = readr::col_integer(),
      actual_home_runs = readr::col_integer(),
      actual_winner    = readr::col_character(),
      correct_winner   = readr::col_logical(),
      file_suffix      = readr::col_character()
    )),
    error = function(e) { message("  log read failed: ", e$message); log_schema }
  )
} else log_schema

# Rows written before file_suffix existed have no such column — add it.
if (!"file_suffix" %in% names(log_df)) log_df$file_suffix <- NA_character_
log_df$file_suffix <- dplyr::coalesce(log_df$file_suffix, "")

# ------------------------------------------------------------
# Step 1: Log predictions for today's new games
# ------------------------------------------------------------
today_gpks <- unique(game_context$game_pk)
new_gpks   <- setdiff(today_gpks, log_df$game_pk)

if (length(new_gpks) > 0) {
  new_rows <- dplyr::bind_rows(lapply(new_gpks, function(gpk) {
    tryCatch({
      pred <- make_prediction_data(gpk)
      game <- game_context %>% dplyr::filter(game_pk == gpk)
      dplyr::tibble(
        game_pk          = as.integer(gpk),
        game_date        = dplyr::coalesce(
                             as.character(game$game_date[1]), as.character(Sys.Date())),
        away_team        = pred$away_team,
        home_team        = pred$home_team,
        pred_away_runs   = round(pred$away_runs,    2),
        pred_home_runs   = round(pred$home_runs,    2),
        away_win_pct     = round(pred$away_win_pct, 3),
        home_win_pct     = round(pred$home_win_pct, 3),
        predicted_winner = if (pred$home_win_pct >= pred$away_win_pct)
                             pred$home_team else pred$away_team,
        actual_away_runs = NA_integer_,
        actual_home_runs = NA_integer_,
        actual_winner    = NA_character_,
        correct_winner   = NA,
        file_suffix      = dplyr::coalesce(game$file_suffix[1], "")
      )
    }, error = function(e) {
      message("  Prediction log failed for gpk ", gpk, ": ", e$message)
      NULL
    })
  }))

  if (nrow(new_rows) > 0) {
    log_df <- dplyr::bind_rows(log_df, new_rows)
    message("log_predictions: +", nrow(new_rows), " games logged")
  }
}

# ------------------------------------------------------------
# Step 2: Fill in actual results for past games
# ------------------------------------------------------------
pending <- log_df %>%
  dplyr::filter(is.na(actual_winner), as.Date(game_date) < Sys.Date())

if (nrow(pending) > 0) {
  for (d in unique(pending$game_date)) {
    scores <- tryCatch({
      url <- paste0(
        "https://statsapi.mlb.com/api/v1/schedule",
        "?sportId=1&date=", d, "&gameType=R&hydrate=linescore"
      )
      resp <- httr::GET(url, httr::timeout(30))
      if (httr::status_code(resp) != 200) stop("HTTP ", httr::status_code(resp))
      raw <- jsonlite::fromJSON(
        httr::content(resp, as = "text", encoding = "UTF-8"),
        simplifyDataFrame = FALSE
      )
      dates_list <- raw[["dates"]]
      if (length(dates_list) == 0) return(NULL)

      dplyr::bind_rows(lapply(dates_list[[1]][["games"]], function(g) {
        status <- dplyr::coalesce(g[["status"]][["detailedState"]], "")
        if (!grepl("Final|Completed", status, ignore.case = TRUE)) return(NULL)
        ls <- g[["linescore"]]
        if (is.null(ls)) return(NULL)
        away_r <- tryCatch(as.integer(ls[["teams"]][["away"]][["runs"]]), error = function(e) NA_integer_)
        home_r <- tryCatch(as.integer(ls[["teams"]][["home"]][["runs"]]), error = function(e) NA_integer_)
        if (is.na(away_r) || is.na(home_r)) return(NULL)
        dplyr::tibble(game_pk = as.integer(g[["gamePk"]]),
                      actual_away_runs = away_r,
                      actual_home_runs = home_r)
      }))
    }, error = function(e) { message("  Results fetch failed for ", d, ": ", e$message); NULL })

    if (is.null(scores) || nrow(scores) == 0) next

    for (i in seq_len(nrow(scores))) {
      gpk  <- scores$game_pk[i]
      ar   <- scores$actual_away_runs[i]
      hr   <- scores$actual_home_runs[i]
      idx  <- which(log_df$game_pk == gpk)
      if (length(idx) == 0) next

      row      <- log_df[idx[1], ]
      winner   <- if (hr > ar) row$home_team else if (ar > hr) row$away_team else NA_character_
      correct  <- if (!is.na(winner)) isTRUE(winner == row$predicted_winner) else NA

      log_df$actual_away_runs[idx[1]] <- ar
      log_df$actual_home_runs[idx[1]] <- hr
      log_df$actual_winner[idx[1]]    <- winner
      log_df$correct_winner[idx[1]]   <- correct
    }
    message("log_predictions: filled results for ", d)
  }
}

# ------------------------------------------------------------
# Write
# ------------------------------------------------------------
readr::write_csv(log_df, LOG_PATH)
message("log_predictions: ", nrow(log_df), " total rows saved to ", LOG_PATH)

}  # end if game_context exists
