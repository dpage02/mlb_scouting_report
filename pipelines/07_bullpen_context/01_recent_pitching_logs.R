# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 07_bullpen_context
# SCRIPT: 01_recent_pitching_logs.R
# ============================================================
# PURPOSE:
#   Pull pitcher appearance data for the last 7 days.
#   Scoped to games involving today's teams only.
#
# METHOD:
#   1. Get completed game_pks for each day in the lookback window
#      where today's teams played (mlb_game_pks per date)
#   2. Pull /api/v1/game/{gamePk}/boxscore for each game
#      — tiny JSON payload vs full PBP (~10x faster)
#   3. Parse pitcher pitches + IP directly from boxscore
#
# KEY COLUMNS:
#   mlbam_id         — pitcher ID (direct from boxscore)
#   game_date        — date of appearance
#   pitches_thrown   — total pitches in game
#   innings_pitched  — innings pitched
#   days_ago         — days before target_date
#
# OUTPUT:
#   recent_pitching_logs
# ============================================================

lookback_start <- target_date - 7
lookback_end   <- target_date - 1

message("Pulling pitching logs from ", lookback_start, " to ", lookback_end)

# ------------------------------------------------------------
# Get today's team IDs
# ------------------------------------------------------------

todays_team_ids <- c(
  schedule_context$home_team_id,
  schedule_context$away_team_id
) %>%
  as.integer() %>%
  unique()

message("Scoping to ", length(todays_team_ids), " team IDs playing today")

# ------------------------------------------------------------
# Get completed game_pks for each day in lookback window
# ------------------------------------------------------------

get_team_game_pks <- function(date) {
  pks <- tryCatch(
    baseballr::mlb_game_pks(
      date      = as.character(date),
      level_ids = 1
    ),
    error = function(e) NULL
  )

  if (is.null(pks) || nrow(pks) == 0) return(NULL)

  pks %>%
    dplyr::filter(
      status.abstractGameState == "Final",
      teams.home.team.id %in% todays_team_ids |
        teams.away.team.id %in% todays_team_ids
    ) %>%
    dplyr::transmute(
      game_pk   = as.integer(game_pk),
      game_date = as.Date(officialDate)
    )
}

lookback_dates <- seq(lookback_start, lookback_end, by = "day")

game_schedule <- purrr::map_dfr(lookback_dates, get_team_game_pks) %>%
  dplyr::distinct()

message("Found ", nrow(game_schedule), " completed games in lookback window")

# ------------------------------------------------------------
# Empty output scaffold
# ------------------------------------------------------------

empty_log <- dplyr::tibble(
  mlbam_id        = integer(),
  game_pk         = integer(),
  game_date       = as.Date(character()),
  pitches_thrown  = integer(),
  innings_pitched = numeric(),
  days_ago        = integer()
)

if (nrow(game_schedule) == 0) {

  message("No completed games found — all pitchers will be marked fresh")
  recent_pitching_logs <- empty_log

} else {

  # ------------------------------------------------------------
  # Pull boxscore per game (fast — small JSON payload)
  # Endpoint: statsapi.mlb.com/api/v1/game/{gamePk}/boxscore
  # ------------------------------------------------------------

  parse_team_pitchers <- function(team) {
    pitcher_ids <- team$pitchers
    if (is.null(pitcher_ids) || length(pitcher_ids) == 0) return(NULL)

    purrr::map_dfr(pitcher_ids, function(pid) {
      player <- team$players[[paste0("ID", pid)]]
      if (is.null(player)) return(NULL)

      np <- player$stats$pitching$numberOfPitches
      ip <- player$stats$pitching$inningsPitched

      dplyr::tibble(
        mlbam_id        = as.integer(pid),
        pitches_thrown  = as.integer(if (!is.null(np)) np else NA_integer_),
        innings_pitched = suppressWarnings(
          as.numeric(if (!is.null(ip)) ip else NA_character_)
        )
      )
    })
  }

  pull_boxscore_pitchers <- function(game_pk, game_date) {
    url  <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk, "/boxscore")

    resp <- tryCatch(
      httr::GET(url, httr::timeout(30)),
      error = function(e) NULL
    )

    if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

    box <- tryCatch(
      jsonlite::fromJSON(
        httr::content(resp, as = "text", encoding = "UTF-8"),
        simplifyVector   = TRUE,
        simplifyDataFrame = FALSE,
        flatten          = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(box)) return(NULL)

    result <- dplyr::bind_rows(
      parse_team_pitchers(box$teams$home),
      parse_team_pitchers(box$teams$away)
    )

    if (is.null(result) || nrow(result) == 0) return(NULL)

    result %>%
      dplyr::mutate(
        game_pk   = as.integer(game_pk),
        game_date = as.Date(game_date)
      )
  }

  message("Pulling boxscores for ", nrow(game_schedule), " games...")

  raw_logs <- purrr::map2_dfr(
    game_schedule$game_pk,
    game_schedule$game_date,
    pull_boxscore_pitchers
  )

  message("Raw boxscore pitcher rows: ", nrow(raw_logs))

  if (nrow(raw_logs) == 0) {

    message("No pitcher data extracted from boxscores — creating empty log")
    recent_pitching_logs <- empty_log

  } else {

    recent_pitching_logs <- raw_logs %>%
      dplyr::filter(!is.na(mlbam_id),
                    !is.na(pitches_thrown),
                    pitches_thrown > 0) %>%
      dplyr::mutate(days_ago = as.integer(target_date - game_date)) %>%
      dplyr::select(mlbam_id, game_pk, game_date, pitches_thrown,
                    innings_pitched, days_ago) %>%
      dplyr::distinct()

  }

}

message("01_recent_pitching_logs complete: ",
        nrow(recent_pitching_logs), " pitcher-game rows | ",
        dplyr::n_distinct(recent_pitching_logs$mlbam_id), " pitchers | ",
        "last 7 days")
