# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 02_lineup_context.R
# ============================================================
# PURPOSE:
#   Build the expected lineup for each team in today's games.
#
# LINEUP SOURCE PRIORITY (3-tier):
#   1. MLB Stats API confirmed lineup — posted ~60-90 min pre-game
#   2. Roster Resource projected lineup — FanGraphs day-of projection
#      accounting for injuries/rest/manager tendencies
#   3. FanGraphs depth chart batting slots — static fallback
#
# GRAIN:
#   One row per batting slot per side per game_pk
#   = up to 18 rows per game (9 per side)
#
# INPUT:
#   game_context          — game_pk, home/away team IDs + names
#   depth_charts          — fg_role 1-9 = batting slot, team_abbr
#   offense_master_season — season stats keyed on mlbam_id
#   team_ids              — mlbam_team_id <-> team_abbr bridge
#
# OUTPUT:
#   lineup_context        — with lineup_source col ("confirmed" /
#                           "roster_resource" / "depth_chart")
# ============================================================

required_objects <- c("game_context", "depth_charts",
                      "offense_master_season", "team_ids")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Offense season stats (shared across all tiers)
# ------------------------------------------------------------

off_stats <- offense_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id,
    dplyr::any_of(c(
      "mlb_pa", "mlb_avg", "mlb_obp", "mlb_slg", "mlb_ops",
      "mlb_hr", "mlb_rbi", "mlb_sb", "mlb_bb", "mlb_so",
      "fg_wRC_plus", "fg_OBP", "fg_SLG", "fg_ISO",
      "fg_BB_pct", "fg_K_pct", "fg_BABIP", "fg_WAR",
      "bbref_OPS", "bbref_PA"
    ))
  )

# ------------------------------------------------------------
# Team <-> game bridge
# ------------------------------------------------------------

home_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "home",
    team_name = home_team_name,
    team_id   = as.integer(home_team_id)
  )

away_games <- game_context %>%
  dplyr::transmute(
    game_pk   = game_pk,
    game_date = game_date,
    side      = "away",
    team_name = away_team_name,
    team_id   = as.integer(away_team_id)
  )

team_game_bridge <- dplyr::bind_rows(home_games, away_games) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, team_abbr),
    by = c("team_id" = "mlbam_team_id")
  )

# ============================================================
# TIER 1 — MLB Stats API confirmed lineups
# Endpoint: /api/v1/game/{game_pk}/lineups
# Returns the official submitted batting order once posted.
# ============================================================

.pull_confirmed_lineup <- function(gpk) {
  tryCatch({
    url  <- paste0("https://statsapi.mlb.com/api/v1/game/", gpk, "/lineups")
    resp <- httr::GET(url, httr::timeout(15))
    if (httr::http_error(resp)) return(NULL)

    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )

    # API returns homePlayers / awayPlayers lists
    sides <- list(
      home = parsed[["homePlayers"]],
      away = parsed[["awayPlayers"]]
    )

    rows <- purrr::imap_dfr(sides, function(players, side_lbl) {
      if (is.null(players) || length(players) == 0) return(NULL)
      df <- dplyr::as_tibble(players)
      if (!"id" %in% names(df)) return(NULL)
      df %>%
        dplyr::transmute(
          side         = side_lbl,
          batting_slot = dplyr::row_number(),
          mlbam_id     = as.integer(id),
          player_name  = dplyr::coalesce(
            if ("fullName"    %in% names(.)) fullName    else NA_character_,
            if ("boxscoreName" %in% names(.)) boxscoreName else NA_character_
          ),
          fg_position  = dplyr::coalesce(
            if ("primaryPosition.abbreviation" %in% names(.))
              .data[["primaryPosition.abbreviation"]] else NA_character_,
            NA_character_
          ),
          lineup_source = "confirmed"
        )
    })

    if (nrow(rows) == 0) return(NULL)
    rows$game_pk <- gpk
    rows

  }, error = function(e) NULL)
}

message("Fetching confirmed lineups from MLB Stats API...")
confirmed_raw <- purrr::map_dfr(game_context$game_pk, .pull_confirmed_lineup)

n_confirmed <- if (nrow(confirmed_raw) > 0) {
  dplyr::n_distinct(confirmed_raw$game_pk[confirmed_raw$lineup_source == "confirmed"])
} else 0L
message("  Confirmed lineups: ", n_confirmed, " of ",
        nrow(game_context), " games")

# ============================================================
# TIER 2 — Roster Resource projected lineups
# FanGraphs owns Roster Resource; their scores page uses it.
# Endpoint: /api/roster-resource/depth-charts/data?statType=2
# Returns projected batting order (roles 1-9) per team.
# ============================================================

.pull_roster_resource <- function() {
  tryCatch({
    resp <- httr::GET(
      "https://www.fangraphs.com/api/roster-resource/depth-charts/data",
      query = list(statType = "2"),
      httr::add_headers(
        `User-Agent` = paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                              "AppleWebKit/537.36 (KHTML, like Gecko) ",
                              "Chrome/124.0.0.0 Safari/537.36"),
        `Referer`    = "https://www.fangraphs.com/roster-resource/depth-charts"
      ),
      httr::timeout(30)
    )
    if (httr::http_error(resp)) return(NULL)

    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )

    # Response structure: list with team entries, each containing players
    # Batting slots are numeric roles 1-9
    if (is.data.frame(parsed)) {
      df <- dplyr::as_tibble(parsed)
    } else if ("data" %in% names(parsed)) {
      df <- dplyr::as_tibble(parsed$data)
    } else if (is.list(parsed) && length(parsed) > 0) {
      df <- tryCatch(dplyr::bind_rows(parsed), error = function(e) NULL)
      if (is.null(df)) return(NULL)
    } else {
      return(NULL)
    }

    if (nrow(df) == 0) return(NULL)

    # Identify key columns flexibly
    id_col   <- intersect(c("mlbamid", "xMLBAMID", "mlbam_id", "MLBAMID"), names(df))[1]
    role_col <- intersect(c("role", "battingOrder", "battingSlot", "slot"), names(df))[1]
    name_col <- intersect(c("PlayerName", "player", "Name", "playerName"), names(df))[1]
    pos_col  <- intersect(c("position", "Position", "fg_position"), names(df))[1]
    team_col <- intersect(c("teamid", "TeamId", "teamId", "fg_team_id"), names(df))[1]
    abbr_col <- intersect(c("AbbName", "teamAbbr", "fg_team_abbr", "abbrev"), names(df))[1]

    if (is.na(id_col) || is.na(role_col)) return(NULL)

    df %>%
      dplyr::filter(!is.na(.data[[id_col]]),
                    suppressWarnings(as.integer(.data[[role_col]])) %in% 1:9) %>%
      dplyr::transmute(
        mlbam_id     = as.integer(.data[[id_col]]),
        batting_slot = as.integer(.data[[role_col]]),
        player_name  = if (!is.na(name_col)) as.character(.data[[name_col]]) else NA_character_,
        fg_position  = if (!is.na(pos_col))  as.character(.data[[pos_col]])  else NA_character_,
        rr_team_abbr = if (!is.na(abbr_col)) as.character(.data[[abbr_col]]) else NA_character_,
        lineup_source = "roster_resource"
      ) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE)

  }, error = function(e) {
    message("  Roster Resource API error: ", e$message)
    NULL
  })
}

message("Fetching Roster Resource projected lineups...")
rr_raw <- .pull_roster_resource()

if (!is.null(rr_raw) && nrow(rr_raw) > 0) {
  message("  Roster Resource: ", nrow(rr_raw), " player-slots across ",
          dplyr::n_distinct(rr_raw$rr_team_abbr, na.rm = TRUE), " teams")
} else {
  message("  Roster Resource unavailable — will fall back to depth charts")
  rr_raw <- NULL
}

# ============================================================
# TIER 3 — FanGraphs depth charts (static fallback)
# ============================================================

dc_batting <- depth_charts %>%
  dplyr::filter(
    fg_role %in% as.character(1:9),
    roster_type %in% c("mlb-sp", "mlb-bp", "mlb-sl")
  ) %>%
  dplyr::transmute(
    team_abbr    = team_abbr,
    batting_slot = as.integer(fg_role),
    mlbam_id     = mlbam_id,
    player_name  = player_name,
    fg_position  = fg_position,
    lineup_source = "depth_chart"
  )

# ============================================================
# ASSEMBLE — apply tier priority per game/team
# ============================================================

lineup_rows <- purrr::pmap_dfr(
  list(team_game_bridge$game_pk,
       team_game_bridge$side,
       team_game_bridge$team_abbr,
       team_game_bridge$team_id,
       team_game_bridge$team_name,
       team_game_bridge$game_date),

  function(gpk, side_val, t_abbr, t_id, t_name, g_date) {

    # --- Tier 1: confirmed ---
    if (nrow(confirmed_raw) > 0) {
      conf <- confirmed_raw %>%
        dplyr::filter(game_pk == gpk, side == side_val)
      if (nrow(conf) >= 8) {
        return(conf %>%
          dplyr::mutate(
            game_date = g_date,
            team_name = t_name,
            team_abbr = t_abbr
          ) %>%
          dplyr::select(game_pk, game_date, side, team_name, team_abbr,
                        batting_slot, mlbam_id, player_name,
                        fg_position, lineup_source))
      }
    }

    # --- Tier 2: Roster Resource ---
    if (!is.null(rr_raw) && nrow(rr_raw) > 0) {
      # Match by team_abbr or by mlbam_id intersection with team roster
      team_player_ids <- depth_charts %>%
        dplyr::filter(team_abbr == t_abbr) %>%
        dplyr::pull(mlbam_id)

      rr_team <- rr_raw %>%
        dplyr::filter(mlbam_id %in% team_player_ids)

      # Fallback: try to match via rr_team_abbr if available
      if (nrow(rr_team) < 5 && !is.na(rr_raw$rr_team_abbr[1])) {
        rr_team <- rr_raw %>%
          dplyr::filter(toupper(rr_team_abbr) == toupper(t_abbr))
      }

      if (nrow(rr_team) >= 7) {
        return(rr_team %>%
          dplyr::arrange(batting_slot) %>%
          dplyr::slice_head(n = 9) %>%
          dplyr::mutate(
            game_pk   = gpk,
            game_date = g_date,
            side      = side_val,
            team_name = t_name,
            team_abbr = t_abbr
          ) %>%
          dplyr::select(game_pk, game_date, side, team_name, team_abbr,
                        batting_slot, mlbam_id, player_name,
                        fg_position, lineup_source))
      }
    }

    # --- Tier 3: FanGraphs depth charts ---
    dc_team <- dc_batting %>%
      dplyr::filter(team_abbr == t_abbr) %>%
      dplyr::arrange(batting_slot)

    if (nrow(dc_team) == 0) return(NULL)

    dc_team %>%
      dplyr::mutate(
        game_pk   = gpk,
        game_date = g_date,
        side      = side_val,
        team_name = t_name
      ) %>%
      dplyr::select(game_pk, game_date, side, team_name, team_abbr,
                    batting_slot, mlbam_id, player_name,
                    fg_position, lineup_source)
  }
)

# ------------------------------------------------------------
# Join offense stats
# ------------------------------------------------------------

lineup_context <- lineup_rows %>%
  dplyr::left_join(off_stats, by = "mlbam_id") %>%
  dplyr::arrange(game_pk, side, batting_slot)

# ------------------------------------------------------------
# Join baserunning + defense stats (one row per mlbam_id — highest
# games/innings stint for players traded mid-season). Only net-new
# signal columns are pulled in; anything already covered by off_stats
# (mlb_sb, fg_SB/CS, etc.) is left alone to avoid duplicate columns.
# ------------------------------------------------------------

if (exists("baserunning_master_season") && nrow(baserunning_master_season) > 0) {
  br_stats <- baserunning_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_sb, 0L) + dplyr::coalesce(mlb_cs, 0L))) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(
      mlbam_id,
      br_sb_pct   = mlb_sb_pct,
      br_wBsR     = fg_wBsR,
      br_UBR      = fg_UBR,
      br_Spd      = fg_Spd,
      br_sprint_speed = sc_sprint_speed
    )
  lineup_context <- lineup_context %>%
    dplyr::left_join(br_stats, by = "mlbam_id")
}

if (exists("defense_master_season") && nrow(defense_master_season) > 0) {
  def_stats <- defense_master_season %>%
    dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_innings_fielding, 0))) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(
      mlbam_id,
      def_primary_position = primary_position,
      def_position_group    = position_group,
      def_OAA               = fg_OAA,
      def_DRS                = fg_DRS,
      def_Defense            = fg_Defense,
      def_fielding_pct       = mlb_fielding_pct,
      # Catcher-specific (NA for non-catchers)
      def_c_framing          = fg_CFraming,
      def_c_FRP              = fg_FRP,
      def_c_rCERA            = fg_rCERA,
      def_c_PB                = fg_PB,
      def_c_WP                = fg_WP,
      def_c_rSB               = fg_rSB
    )
  lineup_context <- lineup_context %>%
    dplyr::left_join(def_stats, by = "mlbam_id")
}

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_games    <- dplyr::n_distinct(lineup_context$game_pk)
avg_lineup <- nrow(lineup_context) / max(n_games * 2, 1)

source_summary <- lineup_context %>%
  dplyr::count(lineup_source) %>%
  dplyr::mutate(label = paste0(n, " ", lineup_source)) %>%
  dplyr::pull(label) %>%
  paste(collapse = " | ")

if (avg_lineup < 7) {
  warning("Average lineup size is ", round(avg_lineup, 1),
          " — expected ~9. Lineup sources may be incomplete.")
}

message("02_lineup_context complete: ",
        nrow(lineup_context), " rows | ",
        n_games, " games | ",
        round(avg_lineup, 1), " avg batters per side | ",
        source_summary)
