# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 07_team_standings.R
# ============================================================
# PURPOSE:
#   Pull current regular-season standings from the MLB Stats API.
#   Provides W-L record, RS, RA, run differential, and division
#   rank for use in series overview narratives.
#
# OUTPUT:
#   team_standings
#   Columns: mlbam_team_id, team_name, wins, losses, pct,
#            games_played, runs_scored, runs_allowed, run_diff,
#            games_back, division_rank, league_rank, wc_rank,
#            wc_games_back, division_id
# ============================================================

message("07_team_standings: pulling current standings...")

team_standings <- tryCatch({
  resp <- httr::GET(
    "https://statsapi.mlb.com/api/v1/standings",
    query = list(
      leagueId       = "103,104",
      season         = target_season,
      standingsTypes = "regularSeason"
    ),
    httr::timeout(30)
  )
  if (httr::http_error(resp)) stop("HTTP error")
  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE
  )
  raw_list <- parsed$records$teamRecords
  div_ids  <- parsed$records$division.id

  dplyr::bind_rows(lapply(seq_along(raw_list), function(i) {
    tr <- raw_list[[i]]
    if (!is.data.frame(tr) || nrow(tr) == 0) return(NULL)
    dplyr::tibble(
      mlbam_team_id = suppressWarnings(as.integer(tr$team.id)),
      team_name     = as.character(tr$team.name),
      wins          = suppressWarnings(as.integer(tr$wins)),
      losses        = suppressWarnings(as.integer(tr$losses)),
      pct           = suppressWarnings(as.numeric(tr$winningPercentage)),
      games_played  = suppressWarnings(as.integer(tr$gamesPlayed)),
      runs_scored   = suppressWarnings(as.integer(tr$runsScored)),
      runs_allowed  = suppressWarnings(as.integer(tr$runsAllowed)),
      run_diff      = suppressWarnings(as.integer(tr$runDifferential)),
      games_back    = as.character(tr$gamesBack),
      division_rank = suppressWarnings(as.integer(tr$divisionRank)),
      league_rank   = suppressWarnings(as.integer(tr$leagueRank)),
      wc_rank       = suppressWarnings(as.integer(tr$wildCardRank)),
      wc_games_back = as.character(tr$wildCardGamesBack),
      division_id   = suppressWarnings(as.integer(div_ids[i]))
    )
  })) %>%
  dplyr::filter(!is.na(mlbam_team_id)) %>%
  dplyr::distinct(mlbam_team_id, .keep_all = TRUE)

}, error = function(e) {
  message("07_team_standings: failed — ", e$message)
  dplyr::tibble(
    mlbam_team_id = integer(), team_name = character(),
    wins = integer(), losses = integer(), pct = numeric(),
    games_played = integer(), runs_scored = integer(),
    runs_allowed = integer(), run_diff = integer(),
    games_back = character(), division_rank = integer(),
    league_rank = integer(), wc_rank = integer(),
    wc_games_back = character(), division_id = integer()
  )
})

message("07_team_standings complete: ", nrow(team_standings), " teams")
