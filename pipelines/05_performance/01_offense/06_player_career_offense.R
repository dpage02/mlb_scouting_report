# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 06_player_career_offense.R
# ============================================================
# PURPOSE:
#   Build a multi-year batting history table for use in the
#   player deep dive section of the game report.
#
#   Sources:
#     - Lahman::Batting  → historical seasons (through last year)
#     - player_season_mlb_offense → current season
#     - offense_master_season     → current season advanced stats
#
# OUTPUT:
#   player_career_offense
#
# GRAIN:
#   One row per mlbam_id per season (total — not team splits)
#   Covers last 5 seasons + current
# ============================================================

library(Lahman)

# ------------------------------------------------------------
# Config: how many historical seasons to include
# ------------------------------------------------------------

n_seasons    <- 5
current_year <- unique(player_season_mlb_offense$season)[1]
hist_start   <- current_year - n_seasons

# ------------------------------------------------------------
# Historical seasons from Lahman (aggregated across teams = TOT)
# ------------------------------------------------------------

lahman_career <- Lahman::Batting %>%
  dplyr::filter(yearID >= hist_start, yearID < current_year) %>%
  dplyr::group_by(playerID, yearID) %>%
  dplyr::summarise(
    G    = sum(G,    na.rm = TRUE),
    PA   = sum(AB + BB + HBP + SF + SH, na.rm = TRUE),
    AB   = sum(AB,   na.rm = TRUE),
    H    = sum(H,    na.rm = TRUE),
    X2B  = sum(X2B,  na.rm = TRUE),
    X3B  = sum(X3B,  na.rm = TRUE),
    HR   = sum(HR,   na.rm = TRUE),
    R    = sum(R,    na.rm = TRUE),
    RBI  = sum(RBI,  na.rm = TRUE),
    BB   = sum(BB,   na.rm = TRUE),
    SO   = sum(SO,   na.rm = TRUE),
    SB   = sum(SB,   na.rm = TRUE),
    CS   = sum(CS,   na.rm = TRUE),
    HBP  = sum(HBP,  na.rm = TRUE),
    SF   = sum(SF,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    AVG  = dplyr::if_else(AB  > 0, H / AB, NA_real_),
    OBP  = dplyr::if_else(PA  > 0,
             (H + BB + HBP) / (AB + BB + HBP + SF), NA_real_),
    SLG  = dplyr::if_else(AB  > 0,
             (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR) / AB, NA_real_),
    OPS  = OBP + SLG,
    ISO  = SLG - AVG,
    BB_pct = dplyr::if_else(PA > 0, BB / PA, NA_real_),
    K_pct  = dplyr::if_else(PA > 0, SO / PA, NA_real_)
  ) %>%
  dplyr::rename(lahman_id = playerID, season = yearID) %>%
  dplyr::left_join(
    player_master_ids %>% dplyr::select(lahman_id, mlbam_id),
    by = "lahman_id"
  ) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::select(mlbam_id, season, G, PA, AB, H, X2B, X3B, HR,
                R, RBI, BB, SO, SB, AVG, OBP, SLG, OPS, ISO,
                BB_pct, K_pct) %>%
  dplyr::rename(
    hist_g = G, hist_pa = PA, hist_ab = AB, hist_h = H,
    hist_2b = X2B, hist_3b = X3B, hist_hr = HR,
    hist_r = R, hist_rbi = RBI, hist_bb = BB, hist_so = SO,
    hist_sb = SB, hist_avg = AVG, hist_obp = OBP,
    hist_slg = SLG, hist_ops = OPS, hist_iso = ISO,
    hist_bb_pct = BB_pct, hist_k_pct = K_pct
  )

# ------------------------------------------------------------
# Current season from offense_master_season
# One row per player (highest PA), with derived rates
# ------------------------------------------------------------

current_season <- offense_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    hist_g      = NA_integer_,
    hist_pa     = mlb_pa,
    hist_ab     = mlb_ab,
    hist_h      = mlb_h,
    hist_2b     = mlb_2b,
    hist_3b     = mlb_3b,
    hist_hr     = mlb_hr,
    hist_r      = mlb_r,
    hist_rbi    = mlb_rbi,
    hist_bb     = mlb_bb,
    hist_so     = mlb_so,
    hist_sb     = mlb_sb,
    hist_avg    = mlb_avg,
    hist_obp    = mlb_obp,
    hist_slg    = mlb_slg,
    hist_ops    = mlb_ops,
    hist_iso    = mlb_slg - mlb_avg,
    hist_bb_pct = dplyr::case_when(
      !is.na(mlb_pa) & mlb_pa > 0 ~ mlb_bb / mlb_pa,
      TRUE ~ NA_real_),
    hist_k_pct  = dplyr::case_when(
      !is.na(mlb_pa) & mlb_pa > 0 ~ mlb_so / mlb_pa,
      TRUE ~ NA_real_)
  ) %>%
  dplyr::select(
    mlbam_id, season,
    hist_g, hist_pa, hist_ab, hist_h, hist_2b, hist_3b, hist_hr,
    hist_r, hist_rbi, hist_bb, hist_so, hist_sb,
    hist_avg, hist_obp, hist_slg, hist_ops, hist_iso,
    hist_bb_pct, hist_k_pct,
    # Advanced — include whatever is available in offense_master_season
    dplyr::any_of(c(
      "fg_wRC_plus", "fg_wOBA", "fg_WAR",
      "sc_avg_hit_speed", "sc_brl_percent", "sc_ev95percent",
      "sc_est_ba", "sc_est_slg", "sc_est_woba", "sc_woba"
    ))
  )

# ------------------------------------------------------------
# Stack historical + current
# ------------------------------------------------------------

player_career_offense <- dplyr::bind_rows(
  lahman_career %>% dplyr::mutate(season = as.integer(season)),
  current_season %>% dplyr::mutate(season = as.integer(season))
) %>%
  dplyr::arrange(mlbam_id, season)

message("06_player_career_offense complete: ",
        nrow(player_career_offense), " player-season rows | ",
        dplyr::n_distinct(player_career_offense$mlbam_id), " players | ",
        "seasons ", hist_start, "-", current_year)
