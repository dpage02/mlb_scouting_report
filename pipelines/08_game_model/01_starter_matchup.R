# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 01_starter_matchup.R
# ============================================================
# PURPOSE:
#   Build the starter matchup table — one row per side per game.
#   Joins probable pitchers from game_context with their
#   season stats from pitching_master_season.
#
# GRAIN:
#   One row per game_pk per side (home / away)
#   = 2 rows per game
#
# INPUT:
#   game_context          — game_pk, home/away pitcher IDs + names
#   pitching_master_season — season stats keyed on mlbam_id
#
# OUTPUT:
#   starter_matchup
# ============================================================

required_objects <- c("game_context", "pitching_master_season", "player_career_pitching")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

sp_stat_cols <- c(
  # MLB Stats API
  "mlb_g", "mlb_gs", "mlb_ip", "mlb_era", "mlb_whip",
  "mlb_so", "mlb_bb", "mlb_hr", "mlb_sv",
  # FanGraphs advanced
  "fg_era", "fg_whip", "fg_FIP", "fg_xFIP", "fg_xERA", "fg_SIERA",
  "fg_BABIP", "fg_LOB_pct",
  "fg_K_9", "fg_BB_9", "fg_H_9", "fg_HR_9",
  "fg_K_pct", "fg_BB_pct", "fg_K_BB_pct",
  "fg_WAR", "fg_ERA_minus",
  # FanGraphs batted ball (from type 2 extra pull)
  "fg_GB.", "fg_LD.", "fg_FB.", "fg_IFFB.", "fg_HR.FB",
  "fg_Hard.", "fg_Med.", "fg_Soft.",
  # FanGraphs plate discipline (from type 5 extra pull)
  "fg_O.Swing.", "fg_Z.Swing.", "fg_Zone.", "fg_F.Strike.",
  "fg_SwStr.", "fg_CSW.",
  # Statcast pitcher-against (from player_season_statcast_pitching)
  "sc_avg_ev_allowed", "sc_ev95percent_allowed", "sc_barrel_pct_allowed",
  "sc_xba_allowed", "sc_xslg_allowed", "sc_xwoba_allowed", "sc_xera",
  "sc_woba_allowed",
  # BBRef advanced
  "bbref_ERA_plus", "bbref_WAR",
  "bbref_H9", "bbref_HR9", "bbref_BB9", "bbref_SO9", "bbref_SO_W"
)

# ------------------------------------------------------------
# Pitcher season stats spine (deduplicated to one row per pitcher)
# Prefer highest-IP row (handles multi-team players).
# ------------------------------------------------------------

sp_stats <- pitching_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(mlbam_id, dplyr::any_of(sp_stat_cols))

# ------------------------------------------------------------
# Career fallback: for starters not found in pitching_master_season
# (e.g. injury returns, debut players) pull their most recent
# active season from player_career_pitching and map to mlb_ cols.
# ------------------------------------------------------------

starters_needed <- c(
  game_context$home_pitcher_mlbam_id,
  game_context$away_pitcher_mlbam_id
)
starters_needed <- unique(as.integer(starters_needed[!is.na(starters_needed)]))
missing_from_season <- setdiff(starters_needed, sp_stats$mlbam_id)

if (length(missing_from_season) > 0) {
  message("Falling back to career data for ", length(missing_from_season),
          " starter(s) not in current pitching_master_season: ",
          paste(missing_from_season, collapse = ", "))

  career_fallback <- player_career_pitching %>%
    dplyr::filter(mlbam_id %in% missing_from_season, !is.na(hist_era)) %>%
    dplyr::arrange(mlbam_id, dplyr::desc(season)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::transmute(
      mlbam_id,
      mlb_g    = hist_g,
      mlb_gs   = hist_gs,
      mlb_ip   = hist_ip,
      mlb_era  = hist_era,
      mlb_whip = hist_whip,
      mlb_so   = hist_so,
      mlb_bb   = hist_bb,
      mlb_hr   = hist_hr,
      mlb_sv   = hist_sv
    )

  # Supplement career fallback with best-available advanced stats
  # (FG/Statcast/BBRef may be from prior season — join by mlbam_id only)
  adv_cols <- setdiff(sp_stat_cols, names(career_fallback))

  if (exists("player_season_fg_pitching") && nrow(player_season_fg_pitching) > 0) {
    .fg_ip_vec <- if ("fg_ip" %in% names(player_season_fg_pitching)) {
      dplyr::coalesce(player_season_fg_pitching$fg_ip, 0)
    } else {
      rep(0, nrow(player_season_fg_pitching))
    }
    fg_adv <- player_season_fg_pitching %>%
      dplyr::mutate(.fg_ip_sort = .fg_ip_vec) %>%
      dplyr::filter(mlbam_id %in% missing_from_season) %>%
      dplyr::arrange(mlbam_id, dplyr::desc(.fg_ip_sort)) %>%
      dplyr::select(-.fg_ip_sort) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(adv_cols))
    career_fallback <- dplyr::left_join(career_fallback, fg_adv, by = "mlbam_id")
    adv_cols <- setdiff(adv_cols, names(fg_adv))
  }

  if (exists("player_season_statcast_pitching") && nrow(player_season_statcast_pitching) > 0) {
    sc_adv <- player_season_statcast_pitching %>%
      dplyr::filter(mlbam_id %in% missing_from_season) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(adv_cols))
    career_fallback <- dplyr::left_join(career_fallback, sc_adv, by = "mlbam_id")
    adv_cols <- setdiff(adv_cols, names(sc_adv))
  }

  if (exists("player_season_bbref_pitching_advanced") &&
      nrow(player_season_bbref_pitching_advanced) > 0) {
    bbref_adv <- player_season_bbref_pitching_advanced %>%
      dplyr::filter(mlbam_id %in% missing_from_season) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(adv_cols))
    career_fallback <- dplyr::left_join(career_fallback, bbref_adv, by = "mlbam_id")
  }

  sp_stats <- dplyr::bind_rows(sp_stats, career_fallback)
}

# ------------------------------------------------------------
# Pivot game_context to long (one row per side per game)
# ------------------------------------------------------------

home_starters <- game_context %>%
  dplyr::filter(!is.na(home_pitcher_mlbam_id)) %>%
  dplyr::transmute(
    game_pk      = game_pk,
    game_date    = game_date,
    side         = "home",
    team_name    = home_team_name,
    mlbam_id     = as.integer(home_pitcher_mlbam_id),
    pitcher_name = home_pitcher_name
  )

away_starters <- game_context %>%
  dplyr::filter(!is.na(away_pitcher_mlbam_id)) %>%
  dplyr::transmute(
    game_pk      = game_pk,
    game_date    = game_date,
    side         = "away",
    team_name    = away_team_name,
    mlbam_id     = as.integer(away_pitcher_mlbam_id),
    pitcher_name = away_pitcher_name
  )

# ------------------------------------------------------------
# Stack and join season stats
# ------------------------------------------------------------

starter_matchup <- dplyr::bind_rows(home_starters, away_starters) %>%
  dplyr::left_join(sp_stats, by = "mlbam_id") %>%
  dplyr::arrange(game_pk, side)

# ------------------------------------------------------------
# Prior-season context for early-season blending
# Pull the most recent completed season from player_career_pitching
# and attach as prior_gs / prior_era / prior_xfip / prior_war.
# Used by display helpers to blend stats when current GS is small.
# ------------------------------------------------------------

prior_yr_val <- as.integer(format(min(game_context$game_date, na.rm = TRUE), "%Y")) - 1L

prior_stats_raw <- player_career_pitching %>%
  dplyr::filter(season == prior_yr_val) %>%
  dplyr::select(mlbam_id, hist_gs, hist_ip, hist_era,
                dplyr::any_of(c("fg_xFIP", "fg_WAR")))

if (!"fg_xFIP" %in% names(prior_stats_raw)) prior_stats_raw$fg_xFIP <- NA_real_
if (!"fg_WAR"  %in% names(prior_stats_raw)) prior_stats_raw$fg_WAR  <- NA_real_

prior_stats <- prior_stats_raw %>%
  dplyr::rename(
    prior_gs   = hist_gs,
    prior_ip   = hist_ip,
    prior_era  = hist_era,
    prior_xfip = fg_xFIP,
    prior_war  = fg_WAR
  ) %>%
  dplyr::mutate(prior_season = prior_yr_val)

starter_matchup <- starter_matchup %>%
  dplyr::left_join(prior_stats, by = "mlbam_id")

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_games   <- dplyr::n_distinct(starter_matchup$game_pk)
n_missing <- sum(is.na(starter_matchup$mlb_era), na.rm = FALSE)

if (n_missing > 0) {
  message("NOTE: ", n_missing, " starters missing season stats ",
          "(Spring Training / new callup — expected)")
}

message("01_starter_matchup complete: ",
        nrow(starter_matchup), " rows | ",
        n_games, " games | ",
        sum(!is.na(starter_matchup$mlbam_id)), " starters identified")
