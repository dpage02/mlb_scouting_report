# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 08_braves_series.R
# ============================================================
# PURPOSE:
#   Detect Braves series boundaries and build context for the
#   series preview and series recap pages.
#
#   Triggers when today is the FINALE of a Braves series:
#     - Identifies the current series game_pks   → recap
#     - Identifies the next upcoming series      → preview
#     - Fetches Tier-1 probables (mlb_probables) for next series
#
# OUTPUT:
#   braves_series_context (list)
#     $is_finale_today        — logical
#     $current_series         — list (game_pks, dates, opponent, venue)
#     $next_series            — list (game_pks, dates, opponent, venue)
#     $next_series_probables  — tibble (one row per next-series game)
# ============================================================

library(dplyr)
library(baseballr)
library(purrr)

message("Running 08_braves_series.R")

BRAVES_ID      <- 144L
current_season <- as.integer(format(target_date, "%Y"))

# Safe empty context returned when nothing triggers
.empty_probables <- dplyr::tibble(
  game_pk               = integer(),
  game_date             = as.Date(character()),
  home_team_id          = integer(),
  away_team_id          = integer(),
  home_pitcher_name     = character(),
  home_pitcher_mlbam_id = integer(),
  away_pitcher_name     = character(),
  away_pitcher_mlbam_id = integer()
)

braves_series_context <- list(
  is_finale_today       = FALSE,
  current_series        = NULL,
  next_series           = NULL,
  next_series_probables = .empty_probables
)

# ----------------------------------------------------------------
# 1. Pull full-season Braves schedule
#    Reuse raw_schedule_full from 01_schedule.R if available
#    (both scripts run in the same environment), otherwise fetch.
# ----------------------------------------------------------------

braves_sched_raw <- tryCatch({
  if (exists("raw_schedule_full") && nrow(raw_schedule_full) > 0) {
    raw_schedule_full %>%
      dplyr::filter(
        teams_home_team_id == BRAVES_ID | teams_away_team_id == BRAVES_ID
      )
  } else {
    baseballr::mlb_schedule(
      season    = current_season,
      team_ids  = BRAVES_ID,
      sport_ids = 1
    )
  }
}, error = function(e) {
  message("08_braves_series: schedule fetch failed — ", e$message)
  NULL
})

if (is.null(braves_sched_raw) || nrow(braves_sched_raw) == 0) {
  message("08_braves_series: no schedule data. Skipping.")
} else {

  braves_sched <- braves_sched_raw %>%
    dplyr::filter(
      game_type == "R",
      !is.na(official_date)
    ) %>%
    dplyr::mutate(
      game_date       = as.Date(official_date),
      home_team_id    = as.integer(teams_home_team_id),
      away_team_id    = as.integer(teams_away_team_id),
      home_team_name  = as.character(teams_home_team_name),
      away_team_name  = as.character(teams_away_team_name),
      series_game_num = as.integer(series_game_number),
      games_in_series = as.integer(games_in_series),
      opponent_id     = dplyr::if_else(
        home_team_id == BRAVES_ID, away_team_id, home_team_id
      ),
      opponent_name   = dplyr::if_else(
        home_team_id == BRAVES_ID, away_team_name, home_team_name
      ),
      braves_are_home = home_team_id == BRAVES_ID
    ) %>%
    dplyr::arrange(game_date)

  # ----------------------------------------------------------------
  # 2. Detect: is today (or yesterday) a Braves series finale?
  #    Check yesterday too so nightly runs (3 AM ET = next calendar
  #    day) still catch finales from games that finished that evening.
  # ----------------------------------------------------------------

  check_dates <- unique(c(target_date, target_date - 1))

  finale_today <- braves_sched %>%
    dplyr::filter(
      game_date %in% check_dates,
      !is.na(series_game_num), !is.na(games_in_series),
      series_game_num == games_in_series
    ) %>%
    dplyr::arrange(dplyr::desc(game_date)) %>%  # prefer most recent
    dplyr::slice(1)

  braves_series_context$is_finale_today <- nrow(finale_today) > 0

  if (braves_series_context$is_finale_today) {

    cur          <- finale_today[1, ]
    finale_date  <- cur$game_date   # may be yesterday if nightly run

    # ----------------------------------------------------------------
    # 3. Current series game_pks (for recap)
    #    Walk back to the most recent series opener with same matchup.
    # ----------------------------------------------------------------

    opener_row <- braves_sched %>%
      dplyr::filter(
        home_team_id    == cur$home_team_id,
        away_team_id    == cur$away_team_id,
        series_game_num == 1L,
        game_date       <= finale_date
      ) %>%
      dplyr::arrange(dplyr::desc(game_date)) %>%
      dplyr::slice(1)

    opener_date <- if (nrow(opener_row) > 0) opener_row$game_date[1] else target_date

    current_pks <- braves_sched %>%
      dplyr::filter(
        home_team_id == cur$home_team_id,
        away_team_id == cur$away_team_id,
        game_date >= opener_date,
        game_date <= finale_date
      ) %>%
      dplyr::pull(game_pk)

    current_dates <- braves_sched %>%
      dplyr::filter(game_pk %in% current_pks) %>%
      dplyr::pull(game_date)

    braves_series_context$current_series <- list(
      game_pks       = current_pks,
      game_dates     = current_dates,
      n_games        = length(current_pks),
      opponent_id    = cur$opponent_id,
      opponent_name  = cur$opponent_name,
      home_team_id   = cur$home_team_id,
      away_team_id   = cur$away_team_id,
      home_team_name = cur$home_team_name,
      away_team_name = cur$away_team_name,
      braves_are_home = cur$braves_are_home,
      venue_name     = dplyr::coalesce(cur$venue_name, "Unknown"),
      end_date       = finale_date,
      start_date     = opener_date
    )

    message(sprintf(
      "08_braves_series: current series — %s vs %s, %d games ending today",
      cur$away_team_name, cur$home_team_name, length(current_pks)
    ))

    # ----------------------------------------------------------------
    # 4. Next series: first Braves series opener after today
    # ----------------------------------------------------------------

    upcoming <- braves_sched %>%
      dplyr::filter(game_date > finale_date) %>%
      dplyr::arrange(game_date)

    if (nrow(upcoming) > 0) {

      # Prefer an explicit series opener (game_num == 1); fall back to first game
      next_opener <- upcoming %>%
        dplyr::filter(!is.na(series_game_num), series_game_num == 1L) %>%
        dplyr::slice(1)

      if (nrow(next_opener) == 0) next_opener <- upcoming[1, ]

      n_next <- as.integer(dplyr::coalesce(next_opener$games_in_series, 3L))

      next_series_games <- upcoming %>%
        dplyr::filter(
          home_team_id == next_opener$home_team_id,
          away_team_id == next_opener$away_team_id,
          game_date    >= next_opener$game_date
        ) %>%
        dplyr::slice_head(n = n_next)

      next_pks   <- next_series_games$game_pk
      next_dates <- next_series_games$game_date

      braves_series_context$next_series <- list(
        game_pks        = next_pks,
        game_dates      = next_dates,
        n_games         = length(next_pks),
        opponent_id     = next_opener$opponent_id,
        opponent_name   = next_opener$opponent_name,
        home_team_id    = next_opener$home_team_id,
        away_team_id    = next_opener$away_team_id,
        home_team_name  = next_opener$home_team_name,
        away_team_name  = next_opener$away_team_name,
        braves_are_home = next_opener$braves_are_home,
        venue_name      = dplyr::coalesce(next_opener$venue_name, "Unknown"),
        start_date      = min(next_dates)
      )

      # ----------------------------------------------------------------
      # 5. Tier-1 probables for next series via mlb_probables()
      #    Same call pattern as 03_probable_pitchers.R
      # ----------------------------------------------------------------

      next_series_probables <- purrr::map_dfr(
        seq_along(next_pks),
        function(i) {
          gpk    <- next_pks[i]
          gdate  <- next_dates[i]
          hid    <- next_series_games$home_team_id[i]
          aid    <- next_series_games$away_team_id[i]

          prob <- tryCatch(
            baseballr::mlb_probables(gpk),
            error = function(e) NULL
          )

          .get_sp <- function(prob_df, tid) {
            if (is.null(prob_df) || nrow(prob_df) == 0)
              return(list(name = NA_character_, id = NA_integer_))
            r <- prob_df %>% dplyr::filter(team_id == tid)
            if (nrow(r) == 0)
              return(list(name = NA_character_, id = NA_integer_))
            list(name = as.character(r$fullName[1]),
                 id   = suppressWarnings(as.integer(r$id[1])))
          }

          hp <- .get_sp(prob, hid)
          ap <- .get_sp(prob, aid)

          dplyr::tibble(
            game_pk               = as.integer(gpk),
            game_date             = gdate,
            home_team_id          = as.integer(hid),
            away_team_id          = as.integer(aid),
            home_pitcher_name     = hp$name,
            home_pitcher_mlbam_id = hp$id,
            away_pitcher_name     = ap$name,
            away_pitcher_mlbam_id = ap$id
          )
        }
      )

      braves_series_context$next_series_probables <- next_series_probables

      n_confirmed <- sum(
        !is.na(next_series_probables$home_pitcher_name) |
        !is.na(next_series_probables$away_pitcher_name)
      )

      message(sprintf(
        "08_braves_series: next series vs %s — %d games, %d/%d probables confirmed",
        next_opener$opponent_name, length(next_pks),
        n_confirmed, length(next_pks)
      ))

    } else {
      message("08_braves_series: no upcoming Braves games found.")
    }

  } else {
    message("08_braves_series: today is not a Braves series finale — no series pages triggered.")
  }
}

message("08_braves_series.R complete")
