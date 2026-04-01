# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 06_player_career_pitching.R
# ============================================================
# PURPOSE:
#   Build a multi-year pitching history table for the deep dive.
#
#   Sources:
#     - Lahman::Pitching       → historical seasons
#     - pitching_master_season → current season
#
# OUTPUT:
#   player_career_pitching
#
# GRAIN:
#   One row per mlbam_id per season (total across teams)
# ============================================================

library(Lahman)

n_seasons    <- 5
current_year <- unique(player_season_mlb_pitching$season)[1]
hist_start   <- current_year - n_seasons

# ------------------------------------------------------------
# Historical seasons from Lahman
# ------------------------------------------------------------

lahman_career_p <- Lahman::Pitching %>%
  dplyr::filter(yearID >= hist_start, yearID < current_year) %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    G   = sum(G,   na.rm = TRUE),
    GS  = sum(GS,  na.rm = TRUE),
    IPouts = sum(IPouts, na.rm = TRUE),
    H   = sum(H,   na.rm = TRUE),
    ER  = sum(ER,  na.rm = TRUE),
    HR  = sum(HR,  na.rm = TRUE),
    BB  = sum(BB,  na.rm = TRUE),
    SO  = sum(SO,  na.rm = TRUE),
    SV  = sum(SV,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(HLD = NA_integer_) %>%
  dplyr::mutate(
    IP   = IPouts / 3,
    ERA  = dplyr::if_else(IP > 0, (ER * 9) / IP, NA_real_),
    WHIP = dplyr::if_else(IP > 0, (BB + H) / IP, NA_real_),
    K9   = dplyr::if_else(IP > 0, (SO * 9) / IP, NA_real_),
    BB9  = dplyr::if_else(IP > 0, (BB * 9) / IP, NA_real_),
    K_BB = dplyr::if_else(BB > 0, SO / BB, NA_real_)
  ) %>%
  dplyr::rename(lahman_id = playerID, season = yearID) %>%
  dplyr::left_join(
    player_master_ids %>% dplyr::select(lahman_id, mlbam_id),
    by = "lahman_id"
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::select(mlbam_id, season, G, GS, IP, H, HR, BB, SO,
                SV, HLD, ERA, WHIP, K9, BB9, K_BB) %>%
  dplyr::rename(
    hist_g = G, hist_gs = GS, hist_ip = IP,
    hist_h = H, hist_hr = HR, hist_bb = BB, hist_so = SO,
    hist_sv = SV, hist_hld = HLD,
    hist_era = ERA, hist_whip = WHIP,
    hist_k9 = K9, hist_bb9 = BB9, hist_k_bb = K_BB
  )

# ------------------------------------------------------------
# Current season from pitching_master_season
# ------------------------------------------------------------

current_season_p <- pitching_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_ip, 0))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    hist_g    = mlb_g,
    hist_gs   = mlb_gs,
    hist_ip   = mlb_ip,
    hist_h    = mlb_h,
    hist_hr   = mlb_hr,
    hist_bb   = mlb_bb,
    hist_so   = mlb_so,
    hist_sv   = mlb_sv,
    hist_hld  = mlb_hld,
    hist_era  = mlb_era,
    hist_whip = mlb_whip,
    hist_k9   = dplyr::if_else(
      !is.na(mlb_ip) & mlb_ip > 0, (mlb_so * 9) / mlb_ip, NA_real_),
    hist_bb9  = dplyr::if_else(
      !is.na(mlb_ip) & mlb_ip > 0, (mlb_bb * 9) / mlb_ip, NA_real_),
    hist_k_bb = dplyr::if_else(
      !is.na(mlb_bb) & mlb_bb > 0, mlb_so / mlb_bb, NA_real_)
  ) %>%
  dplyr::select(
    mlbam_id, season,
    hist_g, hist_gs, hist_ip, hist_h, hist_hr, hist_bb, hist_so,
    hist_sv, hist_hld, hist_era, hist_whip, hist_k9, hist_bb9, hist_k_bb,
    dplyr::any_of(c("fg_FIP", "fg_xFIP", "fg_WAR"))
  )

# ------------------------------------------------------------
# Gap fill: Lahman lags ~1 year; pull any missing seasons from MLB API
# ------------------------------------------------------------

latest_lahman_p <- if (nrow(lahman_career_p) > 0)
  max(lahman_career_p$season, na.rm = TRUE) else hist_start - 1L

gap_start_p <- latest_lahman_p + 1L
gap_end_p   <- current_year - 1L
gap_years_p <- if (gap_start_p <= gap_end_p) seq(gap_start_p, gap_end_p) else integer(0)

gap_seasons_p_list <- lapply(gap_years_p, function(yr) {
  raw <- tryCatch(
    baseballr::mlb_stats(
      stat_type   = "season",
      stat_group  = "pitching",
      player_pool = "all",
      season      = yr,
      limit       = 3000
    ),
    error = function(e) NULL
  )
  if (is.null(raw) || !"player_id" %in% names(raw) || nrow(raw) == 0)
    return(NULL)

  raw %>%
    dplyr::transmute(
      mlbam_id = as.integer(player_id),
      season   = as.integer(yr),
      hist_g   = as.integer(games_played),
      hist_gs  = as.integer(games_started),
      ip_raw   = as.numeric(innings_pitched),
      hist_h   = as.integer(hits),
      hist_hr  = as.integer(home_runs),
      hist_bb  = as.integer(base_on_balls),
      hist_so  = as.integer(strike_outs),
      hist_sv  = as.integer(saves),
      hist_hld = as.integer(holds),
      er_raw   = as.integer(earned_runs)
    ) %>%
    dplyr::group_by(mlbam_id, season) %>%
    dplyr::summarise(
      hist_g   = sum(hist_g,  na.rm = TRUE),
      hist_gs  = sum(hist_gs, na.rm = TRUE),
      hist_ip  = sum(ip_raw,  na.rm = TRUE),
      hist_h   = sum(hist_h,  na.rm = TRUE),
      hist_hr  = sum(hist_hr, na.rm = TRUE),
      hist_bb  = sum(hist_bb, na.rm = TRUE),
      hist_so  = sum(hist_so, na.rm = TRUE),
      hist_sv  = sum(hist_sv, na.rm = TRUE),
      hist_hld = sum(hist_hld,na.rm = TRUE),
      er_tot   = sum(er_raw,  na.rm = TRUE),
      hist_era  = dplyr::if_else(sum(ip_raw, na.rm=TRUE) > 0,
                    (sum(er_raw, na.rm=TRUE) * 9) / sum(ip_raw, na.rm=TRUE), NA_real_),
      hist_whip = dplyr::if_else(sum(ip_raw, na.rm=TRUE) > 0,
                    (sum(hist_bb, na.rm=TRUE) + sum(hist_h, na.rm=TRUE)) /
                    sum(ip_raw, na.rm=TRUE), NA_real_),
      hist_k9   = dplyr::if_else(sum(ip_raw, na.rm=TRUE) > 0,
                    (sum(hist_so, na.rm=TRUE) * 9) / sum(ip_raw, na.rm=TRUE), NA_real_),
      hist_bb9  = dplyr::if_else(sum(ip_raw, na.rm=TRUE) > 0,
                    (sum(hist_bb, na.rm=TRUE) * 9) / sum(ip_raw, na.rm=TRUE), NA_real_),
      hist_k_bb = dplyr::if_else(sum(hist_bb, na.rm=TRUE) > 0,
                    sum(hist_so, na.rm=TRUE) / sum(hist_bb, na.rm=TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::select(-er_tot)
})

gap_seasons_p <- dplyr::bind_rows(Filter(Negate(is.null), gap_seasons_p_list))

if (nrow(gap_seasons_p) > 0) {
  message("Gap fill: added ", nrow(gap_seasons_p), " pitcher-season rows for seasons ",
          paste(gap_years_p, collapse = ", "))
}

# ------------------------------------------------------------
# FanGraphs historical pull — adds FIP, xFIP, WAR, BABIP,
# LOB%, K%, BB% for each historical pitcher season
# (current season already carries these from pitching_master_season)
# ------------------------------------------------------------

fg_hist_p_list <- lapply(seq(hist_start, current_year - 1L), function(yr) {
  fg <- tryCatch(
    baseballr::fg_pitcher_leaders(
      qual        = "0",
      startseason = as.character(yr),
      endseason   = as.character(yr),
      ind         = "0",
      pageitems   = "10000"
    ),
    error = function(e) {
      message("FG pitcher career pull failed for ", yr, ": ", e$message)
      NULL
    }
  )
  if (is.null(fg) || nrow(fg) == 0) return(NULL)

  # Prefer xMLBAMID for direct mlbam lookup; fall back to playerid → fg_id join
  id_vec <- if ("xMLBAMID" %in% names(fg)) {
    suppressWarnings(as.integer(fg$xMLBAMID))
  } else if ("playerid" %in% names(fg) && exists("player_master_ids")) {
    id_map <- player_master_ids %>%
      dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
      dplyr::distinct(fg_id, .keep_all = TRUE) %>%
      dplyr::select(fg_id, mlbam_id)
    merged <- dplyr::left_join(
      dplyr::tibble(playerid = as.character(fg$playerid)),
      id_map, by = c("playerid" = "fg_id")
    )
    suppressWarnings(as.integer(merged$mlbam_id))
  } else {
    return(NULL)
  }

  out <- dplyr::tibble(mlbam_id = id_vec, season = as.integer(yr))

  safe_num <- function(col) suppressWarnings(as.numeric(fg[[col]]))

  if ("FIP"   %in% names(fg)) out$fg_FIP    <- safe_num("FIP")
  if ("xFIP"  %in% names(fg)) out$fg_xFIP   <- safe_num("xFIP")
  if ("WAR"   %in% names(fg)) out$fg_WAR    <- safe_num("WAR")
  if ("BABIP" %in% names(fg)) out$fg_BABIP  <- safe_num("BABIP")

  lob_col <- intersect(c("LOB%", "LOB."), names(fg))[1]
  if (!is.na(lob_col)) out$fg_LOB_pct <- safe_num(lob_col)

  k_col  <- intersect(c("K%",  "K."),  names(fg))[1]
  bb_col <- intersect(c("BB%", "BB."), names(fg))[1]
  if (!is.na(k_col))  out$fg_k_pct  <- safe_num(k_col)
  if (!is.na(bb_col)) out$fg_bb_pct <- safe_num(bb_col)

  out %>% dplyr::filter(!is.na(mlbam_id))
})

fg_hist_p <- dplyr::bind_rows(Filter(Negate(is.null), fg_hist_p_list))

message("FG pitcher historical pull: ", nrow(fg_hist_p), " player-season rows | ",
        "seasons ", hist_start, "-", current_year - 1L)

join_fg_hist_p <- function(df) {
  if (nrow(fg_hist_p) == 0 || nrow(df) == 0 || !"mlbam_id" %in% names(df)) return(df)
  df %>%
    dplyr::left_join(
      fg_hist_p %>% dplyr::select(mlbam_id, season,
                                   dplyr::any_of(c("fg_FIP", "fg_xFIP", "fg_WAR",
                                                    "fg_BABIP", "fg_LOB_pct",
                                                    "fg_k_pct", "fg_bb_pct"))),
      by = c("mlbam_id", "season")
    )
}

# ------------------------------------------------------------
# Stack historical + gap fill + current
# ------------------------------------------------------------

player_career_pitching <- dplyr::bind_rows(
  lahman_career_p  %>% dplyr::mutate(season = as.integer(season)) %>% join_fg_hist_p(),
  gap_seasons_p    %>% dplyr::mutate(season = as.integer(season)) %>% join_fg_hist_p(),
  current_season_p %>% dplyr::mutate(season = as.integer(season))
) %>%
  dplyr::arrange(mlbam_id, season)

message("06_player_career_pitching complete: ",
        nrow(player_career_pitching), " player-season rows | ",
        dplyr::n_distinct(player_career_pitching$mlbam_id), " pitchers | ",
        "seasons ", hist_start, "-", current_year)
