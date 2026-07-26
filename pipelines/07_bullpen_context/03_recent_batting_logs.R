# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 07_bullpen_context
# SCRIPT: 03_recent_batting_logs.R
# ============================================================
# PURPOSE:
#   Pull recent game-by-game batting data for players in
#   today's lineups. Used to compute hit streaks, on-base
#   streaks, and last-7-day performance.
#
# METHOD:
#   Reuses game_schedule from 01_recent_pitching_logs.R
#   (same boxscore endpoint, parses batters instead of pitchers).
#   Extends lookback to 15 days to capture meaningful streaks.
#
# KEY COLUMNS:
#   mlbam_id    — batter ID
#   game_date   — game date
#   hits        — hits in game
#   ab          — at-bats
#   bb          — walks
#   hbp         — hit by pitch
#   sf          — sac flies
#   hr          — home runs
#   days_ago    — days before target_date
#
# OUTPUT:
#   recent_batting_logs
#   recent_batter_streaks   — one row per batter with streak/hot-cold flags
# ============================================================

lookback_days <- 15L   # enough for a meaningful streak window

# ------------------------------------------------------------
# Re-derive game schedule for a 15-day window
# (01_recent_pitching_logs uses 7 days; we need more for streaks)
# ------------------------------------------------------------

lookback_start_bat <- target_date - lookback_days
lookback_end_bat   <- target_date - 1L

# Reuse todays_team_ids if already in env (set by 01_recent_pitching_logs)
if (!exists("todays_team_ids")) {
  todays_team_ids <- c(
    schedule_context$home_team_id,
    schedule_context$away_team_id
  ) %>% as.integer() %>% unique()
}

get_team_game_pks_bat <- function(date) {
  pks <- tryCatch(
    baseballr::mlb_game_pks(date = as.character(date), level_ids = 1),
    error = function(e) NULL
  )
  # mlb_game_pks() returns a plain list() (not a data.frame) on dates with
  # no scheduled games (e.g. All-Star break) — guard against that shape
  # before checking nrow(), or the || short-circuit evaluates to NA.
  if (is.null(pks) || !is.data.frame(pks) || nrow(pks) == 0) return(NULL)
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

bat_dates    <- seq(lookback_start_bat, lookback_end_bat, by = "day")
bat_schedule <- purrr::map_dfr(bat_dates, get_team_game_pks_bat) %>%
  dplyr::distinct()

message("Batter log lookback: ", nrow(bat_schedule), " games over last ",
        lookback_days, " days")

# ------------------------------------------------------------
# Empty scaffold
# ------------------------------------------------------------

empty_bat_log <- dplyr::tibble(
  mlbam_id  = integer(),
  game_pk   = integer(),
  game_date = as.Date(character()),
  hits      = integer(),
  ab        = integer(),
  bb        = integer(),
  hbp       = integer(),
  sf        = integer(),
  hr        = integer(),
  days_ago  = integer()
)

if (nrow(bat_schedule) == 0) {

  message("No completed games in batter lookback window")
  recent_batting_logs <- empty_bat_log

} else {

  # ----------------------------------------------------------
  # Parse batters from boxscore
  # Endpoint: /api/v1/game/{gamePk}/boxscore
  # ----------------------------------------------------------

  parse_team_batters <- function(team) {
    batter_ids <- team$batters
    if (is.null(batter_ids) || length(batter_ids) == 0) return(NULL)

    purrr::map_dfr(batter_ids, function(bid) {
      player <- team$players[[paste0("ID", bid)]]
      if (is.null(player)) return(NULL)
      b <- player$stats$batting
      if (is.null(b)) return(NULL)

      dplyr::tibble(
        mlbam_id = as.integer(bid),
        hits     = as.integer(dplyr::coalesce(b$hits,        0L)),
        ab       = as.integer(dplyr::coalesce(b$atBats,      0L)),
        bb       = as.integer(dplyr::coalesce(b$baseOnBalls, 0L)),
        hbp      = as.integer(dplyr::coalesce(b$hitByPitch,  0L)),
        sf       = as.integer(dplyr::coalesce(b$sacFlies,    0L)),
        hr       = as.integer(dplyr::coalesce(b$homeRuns,    0L))
      )
    })
  }

  pull_boxscore_batters <- function(game_pk, game_date) {
    url  <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk, "/boxscore")
    resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

    box <- tryCatch(
      jsonlite::fromJSON(
        httr::content(resp, as = "text", encoding = "UTF-8"),
        simplifyVector    = TRUE,
        simplifyDataFrame = FALSE,
        flatten           = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(box)) return(NULL)

    result <- dplyr::bind_rows(
      parse_team_batters(box$teams$home),
      parse_team_batters(box$teams$away)
    )
    if (is.null(result) || nrow(result) == 0) return(NULL)

    result %>% dplyr::mutate(
      game_pk   = as.integer(game_pk),
      game_date = as.Date(game_date)
    )
  }

  message("Pulling batter boxscores for ", nrow(bat_schedule), " games...")

  raw_bat <- purrr::map2_dfr(
    bat_schedule$game_pk,
    bat_schedule$game_date,
    pull_boxscore_batters
  )

  if (nrow(raw_bat) == 0) {
    message("No batter data extracted from boxscores")
    recent_batting_logs <- empty_bat_log
  } else {
    recent_batting_logs <- raw_bat %>%
      dplyr::filter(!is.na(mlbam_id), ab > 0) %>%
      dplyr::mutate(days_ago = as.integer(target_date - game_date)) %>%
      dplyr::select(mlbam_id, game_pk, game_date, hits, ab, bb, hbp, sf, hr, days_ago) %>%
      dplyr::distinct()
  }

}

# ------------------------------------------------------------
# Compute per-batter streak and hot/cold flags
#
# For each batter (scoped to today's lineup players):
#   hit_streak    — consecutive games with ≥1 hit entering today
#   ob_streak     — consecutive games reaching base (H, BB, or HBP)
#   last7_avg     — BA over last 7 games
#   last7_ops     — OPS over last 7 games (OBP + SLG)
#   last7_hr      — HR over last 7 games
#   is_hot        — last7_ops ≥ .900 with ≥ 4 games
#   is_cold       — last7_ops ≤ .550 with ≥ 4 games
# ------------------------------------------------------------

lineup_ids <- if (exists("lineup_context")) {
  unique(as.integer(lineup_context$mlbam_id[!is.na(lineup_context$mlbam_id)]))
} else {
  integer(0)
}

compute_streaks <- function(batter_logs) {
  # Sort oldest → newest
  logs <- batter_logs %>% dplyr::arrange(game_date)
  n    <- nrow(logs)
  if (n == 0) return(NULL)

  # Hit streak: count backwards from most recent game
  hit_streak <- 0L
  for (i in rev(seq_len(n))) {
    if (logs$hits[i] > 0) hit_streak <- hit_streak + 1L else break
  }

  # On-base streak
  ob_streak <- 0L
  for (i in rev(seq_len(n))) {
    on_base <- logs$hits[i] > 0 | logs$bb[i] > 0 | logs$hbp[i] > 0
    if (on_base) ob_streak <- ob_streak + 1L else break
  }

  # Last 7 games
  last7 <- logs %>% dplyr::slice_tail(n = 7)
  l7_ab  <- sum(last7$ab,  na.rm = TRUE)
  l7_h   <- sum(last7$hits, na.rm = TRUE)
  l7_bb  <- sum(last7$bb,  na.rm = TRUE)
  l7_hbp <- sum(last7$hbp, na.rm = TRUE)
  l7_sf  <- sum(last7$sf,  na.rm = TRUE)
  l7_hr  <- sum(last7$hr,  na.rm = TRUE)
  l7_tb  <- l7_h + l7_hr * 3  # rough TB (HR=4 bases, but singles/xbh unknown — approximation)
  l7_g   <- nrow(last7)

  l7_avg <- if (l7_ab > 0) l7_h / l7_ab else NA_real_
  l7_obp <- if ((l7_ab + l7_bb + l7_hbp + l7_sf) > 0)
    (l7_h + l7_bb + l7_hbp) / (l7_ab + l7_bb + l7_hbp + l7_sf) else NA_real_
  l7_slg <- if (l7_ab > 0) l7_tb / l7_ab else NA_real_
  l7_ops <- if (!is.na(l7_obp) && !is.na(l7_slg)) l7_obp + l7_slg else NA_real_

  dplyr::tibble(
    hit_streak = hit_streak,
    ob_streak  = ob_streak,
    last7_g    = l7_g,
    last7_avg  = l7_avg,
    last7_ops  = l7_ops,
    last7_hr   = l7_hr,
    is_hot     = !is.na(l7_ops) && l7_ops >= 0.900 && l7_g >= 4,
    is_cold    = !is.na(l7_ops) && l7_ops <= 0.550 && l7_g >= 4
  )
}

if (nrow(recent_batting_logs) > 0 && length(lineup_ids) > 0) {

  streak_list <- purrr::map_dfr(lineup_ids, function(id) {
    logs <- recent_batting_logs %>% dplyr::filter(mlbam_id == id)
    if (nrow(logs) == 0) return(NULL)
    result <- compute_streaks(logs)
    if (is.null(result)) return(NULL)
    dplyr::bind_cols(dplyr::tibble(mlbam_id = id), result)
  })

  recent_batter_streaks <- streak_list

} else {
  recent_batter_streaks <- dplyr::tibble(
    mlbam_id   = integer(),
    hit_streak = integer(),
    ob_streak  = integer(),
    last7_g    = integer(),
    last7_avg  = numeric(),
    last7_ops  = numeric(),
    last7_hr   = integer(),
    is_hot     = logical(),
    is_cold    = logical()
  )
}

message("03_recent_batting_logs complete: ",
        nrow(recent_batting_logs), " batter-game rows | ",
        dplyr::n_distinct(recent_batting_logs$mlbam_id), " batters | ",
        "streaks computed for ", nrow(recent_batter_streaks), " players | ",
        "hit streaks ≥5: ",  sum(recent_batter_streaks$hit_streak >= 5,  na.rm = TRUE),
        " | OB streaks ≥7: ", sum(recent_batter_streaks$ob_streak  >= 7,  na.rm = TRUE),
        " | hot bats: ",      sum(recent_batter_streaks$is_hot,           na.rm = TRUE))
