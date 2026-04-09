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
      "fg_wRC_plus", "fg_wOBA", "fg_WAR", "fg_Age",
      "sc_avg_hit_speed", "sc_brl_percent", "sc_ev95percent",
      "sc_est_ba", "sc_est_slg", "sc_est_woba", "sc_woba"
    ))
  )

# ------------------------------------------------------------
# Gap fill: Lahman lags ~1 year; pull any missing seasons from MLB API
# ------------------------------------------------------------

latest_lahman <- if (nrow(lahman_career) > 0)
  max(lahman_career$season, na.rm = TRUE) else hist_start - 1L

# Guard against seq() returning a decreasing sequence when there is no gap
gap_start <- latest_lahman + 1L
gap_end   <- current_year - 1L
gap_years <- if (gap_start <= gap_end) seq(gap_start, gap_end) else integer(0)

gap_seasons_list <- lapply(gap_years, function(yr) {
  raw <- tryCatch(
    baseballr::mlb_stats(
      stat_type   = "season",
      stat_group  = "hitting",
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
      mlbam_id    = as.integer(player_id),
      season      = as.integer(yr),
      hist_g      = NA_integer_,
      hist_pa     = as.integer(plate_appearances),
      hist_ab     = as.integer(at_bats),
      hist_h      = as.integer(hits),
      hist_2b     = as.integer(doubles),
      hist_3b     = as.integer(triples),
      hist_hr     = as.integer(home_runs),
      hist_r      = as.integer(runs),
      hist_rbi    = as.integer(rbi),
      hist_bb     = as.integer(base_on_balls),
      hist_so     = as.integer(strike_outs),
      hist_sb     = as.integer(stolen_bases),
      hist_avg    = as.numeric(avg),
      hist_obp    = as.numeric(obp),
      hist_slg    = as.numeric(slg),
      hist_ops    = as.numeric(ops),
      hist_iso    = as.numeric(slg) - as.numeric(avg),
      hist_bb_pct = dplyr::if_else(
        !is.na(plate_appearances) & as.integer(plate_appearances) > 0,
        as.integer(base_on_balls) / as.integer(plate_appearances), NA_real_),
      hist_k_pct  = dplyr::if_else(
        !is.na(plate_appearances) & as.integer(plate_appearances) > 0,
        as.integer(strike_outs) / as.integer(plate_appearances), NA_real_)
    ) %>%
    # Aggregate across teams (same player, same season)
    dplyr::group_by(mlbam_id, season) %>%
    dplyr::summarise(
      hist_g      = NA_integer_,
      hist_ab     = sum(hist_ab,  na.rm = TRUE),
      hist_h      = sum(hist_h,   na.rm = TRUE),
      hist_2b     = sum(hist_2b,  na.rm = TRUE),
      hist_3b     = sum(hist_3b,  na.rm = TRUE),
      hist_hr     = sum(hist_hr,  na.rm = TRUE),
      hist_r      = sum(hist_r,   na.rm = TRUE),
      hist_rbi    = sum(hist_rbi, na.rm = TRUE),
      hist_bb     = sum(hist_bb,  na.rm = TRUE),
      hist_so     = sum(hist_so,  na.rm = TRUE),
      hist_sb     = sum(hist_sb,  na.rm = TRUE),
      hist_pa     = sum(hist_pa,  na.rm = TRUE),
      hist_avg    = dplyr::if_else(sum(hist_ab,  na.rm=TRUE) > 0,
                      sum(hist_h,  na.rm=TRUE) / sum(hist_ab,  na.rm=TRUE), NA_real_),
      hist_obp    = dplyr::if_else(sum(hist_pa,  na.rm=TRUE) > 0,
                      (sum(hist_h, na.rm=TRUE) + sum(hist_bb, na.rm=TRUE)) /
                      sum(hist_pa, na.rm=TRUE), NA_real_),
      hist_slg    = dplyr::if_else(sum(hist_ab,  na.rm=TRUE) > 0,
                      (sum(hist_h, na.rm=TRUE) + sum(hist_2b, na.rm=TRUE) +
                       2*sum(hist_3b, na.rm=TRUE) + 3*sum(hist_hr, na.rm=TRUE)) /
                      sum(hist_ab, na.rm=TRUE), NA_real_),
      hist_ops    = hist_obp + hist_slg,
      hist_iso    = hist_slg - hist_avg,
      hist_bb_pct = dplyr::if_else(sum(hist_pa, na.rm=TRUE) > 0,
                      sum(hist_bb, na.rm=TRUE) / sum(hist_pa, na.rm=TRUE), NA_real_),
      hist_k_pct  = dplyr::if_else(sum(hist_pa, na.rm=TRUE) > 0,
                      sum(hist_so, na.rm=TRUE) / sum(hist_pa, na.rm=TRUE), NA_real_),
      .groups = "drop"
    )
})

gap_seasons <- dplyr::bind_rows(Filter(Negate(is.null), gap_seasons_list))

if (nrow(gap_seasons) > 0) {
  message("Gap fill: added ", nrow(gap_seasons), " player-season rows for seasons ",
          paste(gap_years, collapse = ", "))
}

# ------------------------------------------------------------
# FanGraphs pull for historical years — adds wRC+ and wOBA
# One call per year from hist_start to current_year - 1
# (current_year already has these from offense_master_season)
# ------------------------------------------------------------

# Use direct FanGraphs API — bypasses baseballr::fg_batter_leaders() bug
# pull_fg_batting_api() is defined in 02_fangraphs_offense_season.R (already sourced)
.fg_career_pull <- function(yr) {
  raw <- if (exists("pull_fg_batting_api", mode = "function")) {
    pull_fg_batting_api(yr, type_num = 8)
  } else {
    tryCatch({
      resp <- httr::GET(
        "https://www.fangraphs.com/api/leaders/major-league/data",
        query = list(
          age = "", pos = "all", stats = "bat", lg = "all",
          season = yr, season1 = yr, ind = "0", qual = "0",
          type = "8", pageitems = "2000000", pagenum = "1", rost = "0"
        ),
        httr::timeout(60)
      )
      if (is.null(resp) || httr::http_error(resp)) return(NULL)
      parsed <- tryCatch(
        jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE),
        error = function(e) NULL
      )
      if (is.null(parsed) || !"data" %in% names(parsed)) return(NULL)
      tryCatch(dplyr::as_tibble(parsed$data), error = function(e) NULL)
    }, error = function(e) {
      message("FG career pull failed for ", yr, ": ", e$message)
      NULL
    })
  }

  if (is.null(raw) || nrow(raw) < 10) {
    message("FG career pull: no data for ", yr)
    return(NULL)
  }

  # Normalize wRC+ column name (FanGraphs uses several variants)
  names(raw) <- dplyr::case_when(
    names(raw) %in% c("wRC+", "wRC.", "wRC_plus") ~ "wRC_plus",
    TRUE ~ names(raw)
  )

  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id"), names(raw))[1]
  if (is.na(mlbam_col)) {
    message("FG career pull: no mlbam_id column found for ", yr)
    return(NULL)
  }

  out <- dplyr::tibble(
    mlbam_id = suppressWarnings(as.integer(raw[[mlbam_col]])),
    season   = as.integer(yr)
  )
  if ("wRC_plus" %in% names(raw))
    out$fg_wRC_plus <- suppressWarnings(as.numeric(raw$wRC_plus))
  if ("wOBA" %in% names(raw))
    out$fg_wOBA <- suppressWarnings(as.numeric(raw$wOBA))
  # Carry age for use in stabilized regression fallback
  age_col <- intersect(c("Age", "fg_Age"), names(raw))[1]
  if (!is.na(age_col))
    out$fg_Age <- suppressWarnings(as.numeric(raw[[age_col]]))
  out %>% dplyr::filter(!is.na(mlbam_id))
}

fg_hist_list <- lapply(seq(hist_start, current_year - 1L), .fg_career_pull)

fg_hist <- dplyr::bind_rows(Filter(Negate(is.null), fg_hist_list))

message("FG historical pull: ", nrow(fg_hist), " player-season rows | ",
        "seasons ", hist_start, "-", current_year - 1L)

# Join wRC+/wOBA onto Lahman and gap rows
join_fg_hist <- function(df) {
  if (nrow(fg_hist) == 0 || nrow(df) == 0 || !"mlbam_id" %in% names(df)) return(df)
  df %>%
    dplyr::left_join(
      fg_hist %>% dplyr::select(mlbam_id, season,
                                dplyr::any_of(c("fg_wRC_plus", "fg_wOBA"))),
      by = c("mlbam_id", "season")
    )
}

# ------------------------------------------------------------
# Stack historical + gap fill + current
# ------------------------------------------------------------

player_career_offense <- dplyr::bind_rows(
  lahman_career  %>% dplyr::mutate(season = as.integer(season)) %>% join_fg_hist(),
  gap_seasons    %>% dplyr::mutate(season = as.integer(season)) %>% join_fg_hist(),
  current_season %>% dplyr::mutate(season = as.integer(season))
) %>%
  dplyr::arrange(mlbam_id, season) %>%
  dplyr::distinct(mlbam_id, season, .keep_all = TRUE)  # safety dedup

message("06_player_career_offense complete: ",
        nrow(player_career_offense), " player-season rows | ",
        dplyr::n_distinct(player_career_offense$mlbam_id), " players | ",
        "seasons ", hist_start, "-", current_year)
