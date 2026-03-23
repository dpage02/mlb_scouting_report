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
# Stack
# ------------------------------------------------------------

player_career_pitching <- dplyr::bind_rows(
  lahman_career_p %>% dplyr::mutate(season = as.integer(season)),
  current_season_p %>% dplyr::mutate(season = as.integer(season))
) %>%
  dplyr::arrange(mlbam_id, season)

message("06_player_career_pitching complete: ",
        nrow(player_career_pitching), " player-season rows | ",
        dplyr::n_distinct(player_career_pitching$mlbam_id), " pitchers | ",
        "seasons ", hist_start, "-", current_year)
