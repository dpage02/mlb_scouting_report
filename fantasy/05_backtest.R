# ============================================================
# FANTASY BASEBALL — Backtesting & Weight Optimization
# ============================================================
# Tests whether our Statcast signals add predictive value over
# naive regression-to-mean projections, and finds optimal
# blend weights by minimizing RMSE against actual fantasy
# point production across multiple historical seasons.
#
# APPROACH:
#   For each season pair (N → N+1):
#     1. "Naive projection" = Year N actuals × 0.85 regression
#     2. "Statcast adjusted" = naive + Statcast signals from Year N
#     3. "Blended" = w * naive + (1-w) * Statcast-adjusted
#     4. Measure RMSE vs Year N+1 actuals
#   Grid search over blend weights and signal coefficients.
#
# SEASONS TESTED:  2022→2023, 2023→2024
# MIN THRESHOLDS:  100 PA (batters), 40 IP (pitchers)
#
# OUTPUT:
#   backtest_bat     — per-player error by season/weight combo
#   weight_results   — RMSE summary by weight combination
#   optimal_bat_wts  — recommended batter blend weights
#   optimal_pit_wts  — recommended pitcher blend weights
# ============================================================

source("fantasy/00_fantasy_config.R")

BACKTEST_SEASONS <- c(2022, 2023, 2024)
MIN_PA  <- 100
MIN_IP  <- 40

# ── Fantasy point formulas (mirrors 03_big_board.R) ──────────

fpts_bat <- function(h, d2b, d3b, hr, r, rbi, sb, cs, bb, hbp) {
  # pts = H + 2B + 2*3B + 3*HR + R + RBI + 2*SB - CS + BB + HBP
  coalesce(h,0) + coalesce(d2b,0) + 2*coalesce(d3b,0) +
    3*coalesce(hr,0) + coalesce(r,0) + coalesce(rbi,0) +
    2*coalesce(sb,0) - coalesce(cs,0) + coalesce(bb,0) + coalesce(hbp,0)
}

fpts_pit <- function(ip, k, w, sv, er, qs) {
  # pts = IP*0.99 + K + 4*W + 2*SV - ER + 2*QS
  coalesce(ip,0)*0.99 + coalesce(k,0) + 4*coalesce(w,0) +
    2*coalesce(sv,0) - coalesce(er,0) + 2*coalesce(qs,0)
}

# ── Pull actual batting stats for one season ─────────────────
fetch_actual_bat <- function(season) {
  message("  Fetching actual batting stats: ", season)
  tryCatch({
    raw <- baseballr::fg_batter_leaders(
      startseason = season, endseason = season,
      qual = "0", type = 8, ind = 0
    )
    if (is.null(raw) || nrow(raw) == 0) return(NULL)

    # Normalize column names — FG uses mixed case
    names(raw) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(raw)))

    id_col  <- intersect(c("playerid","xfip","mlbamid"), names(raw))[1]
    pa_col  <- intersect(c("pa","pa_"),     names(raw))[1]
    ab_col  <- intersect(c("ab","ab_"),     names(raw))[1]
    h_col   <- intersect(c("h","h_"),       names(raw))[1]
    d2b_col <- intersect(c("x2b","_2b","2b_"), names(raw))[1]
    d3b_col <- intersect(c("x3b","_3b","3b_"), names(raw))[1]
    hr_col  <- intersect(c("hr","hr_"),     names(raw))[1]
    r_col   <- intersect(c("r","r_"),       names(raw))[1]
    rbi_col <- intersect(c("rbi","rbi_"),   names(raw))[1]
    sb_col  <- intersect(c("sb","sb_"),     names(raw))[1]
    cs_col  <- intersect(c("cs","cs_"),     names(raw))[1]
    bb_col  <- intersect(c("bb","bb_"),     names(raw))[1]
    hbp_col <- intersect(c("hbp","hbp_"),   names(raw))[1]

    safe_n <- function(col) {
      if (is.na(col) || !col %in% names(raw)) return(rep(NA_real_, nrow(raw)))
      suppressWarnings(as.numeric(raw[[col]]))
    }

    dplyr::tibble(
      fg_id   = as.character(raw[[id_col]]),
      season  = season,
      pa      = safe_n(pa_col),
      ab      = safe_n(ab_col),
      h       = safe_n(h_col),
      d2b     = safe_n(d2b_col),
      d3b     = safe_n(d3b_col),
      hr      = safe_n(hr_col),
      r       = safe_n(r_col),
      rbi     = safe_n(rbi_col),
      sb      = safe_n(sb_col),
      cs      = safe_n(cs_col),
      bb      = safe_n(bb_col),
      hbp     = safe_n(hbp_col)
    ) %>%
      dplyr::filter(!is.na(fg_id), !is.na(pa), pa >= MIN_PA) %>%
      dplyr::mutate(
        actual_fpts = fpts_bat(h, d2b, d3b, hr, r, rbi, sb, cs, bb, hbp)
      )
  }, error = function(e) {
    message("  ERROR: ", e$message); NULL
  })
}

# ── Pull actual pitching stats for one season ─────────────────
fetch_actual_pit <- function(season) {
  message("  Fetching actual pitching stats: ", season)
  tryCatch({
    # fg_pitcher_leaders is broken in some baseballr versions — use direct API
    url <- paste0(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      "?pos=all&stats=pit&lg=all&qual=0&type=1",
      "&season=", season, "&season1=", season,
      "&ind=0&team=0&rost=0&age=0&filter=&players=0&pageitems=2000000"
    )
    resp <- httr::GET(url, httr::add_headers(`User-Agent`="Mozilla/5.0"), httr::timeout(30))
    if (httr::status_code(resp) != 200) return(NULL)
    parsed <- jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"), flatten=TRUE)
    raw <- if (is.data.frame(parsed)) parsed else parsed$data
    if (is.null(raw) || nrow(raw) == 0) return(NULL)

    names(raw) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(raw)))

    id_col  <- intersect(c("playerid","xfip","mlbamid"), names(raw))[1]
    ip_col  <- intersect(c("ip","ip_"),   names(raw))[1]
    w_col   <- intersect(c("w","w_"),     names(raw))[1]
    sv_col  <- intersect(c("sv","sv_"),   names(raw))[1]
    k_col   <- intersect(c("so","k","k_"), names(raw))[1]
    er_col  <- intersect(c("er","er_"),   names(raw))[1]
    qs_col  <- intersect(c("qs","qs_"),   names(raw))[1]
    gs_col  <- intersect(c("gs","gs_"),   names(raw))[1]
    era_col <- intersect(c("era","era_"), names(raw))[1]

    safe_n <- function(col) {
      if (is.na(col) || !col %in% names(raw)) return(rep(NA_real_, nrow(raw)))
      suppressWarnings(as.numeric(raw[[col]]))
    }

    df <- dplyr::tibble(
      fg_id   = as.character(raw[[id_col]]),
      season  = season,
      ip      = safe_n(ip_col),
      gs      = safe_n(gs_col),
      w       = safe_n(w_col),
      sv      = safe_n(sv_col),
      k       = safe_n(k_col),
      er      = safe_n(er_col),
      era     = safe_n(era_col),
      qs      = safe_n(qs_col)
    ) %>%
      dplyr::filter(!is.na(fg_id), !is.na(ip), ip >= MIN_IP) %>%
      dplyr::mutate(
        # Estimate QS if not available: ERA-based rate * GS
        qs_est = dplyr::case_when(
          !is.na(qs) ~ qs,
          !is.na(gs) & !is.na(era) ~ gs * dplyr::case_when(
            era < 3.00 ~ 0.65, era < 3.50 ~ 0.58, era < 4.00 ~ 0.50,
            era < 4.50 ~ 0.42, era < 5.00 ~ 0.33, TRUE ~ 0.18
          ),
          TRUE ~ 0
        ),
        actual_fpts = fpts_pit(ip, k, w, sv, er, qs_est)
      )
  }, error = function(e) {
    message("  ERROR: ", e$message); NULL
  })
}

# ── Pull Statcast batting metrics for one season ──────────────
fetch_sc_bat <- function(season) {
  message("  Fetching Statcast batting: ", season)
  tryCatch({
    # Barrel % and EV metrics
    brl <- baseballr::statcast_leaderboards(
      leaderboard = "exit_velocity_barrels",
      year = season, min_pa = 50, player_type = "batter"
    )
    # Expected stats (xBA, xwOBA)
    exp <- baseballr::statcast_leaderboards(
      leaderboard = "expected_statistics",
      year = season, min_pa = 50, player_type = "batter"
    )
    # Sprint speed
    spd <- baseballr::statcast_leaderboards(
      leaderboard = "sprint_speed",
      year = season, min_pa = 0
    )

    # Helper: safely extract a column by trying multiple name candidates
    safe_col <- function(df, candidates) {
      col <- intersect(candidates, names(df))[1]
      if (is.na(col)) rep(NA_real_, nrow(df))
      else suppressWarnings(as.numeric(df[[col]]))
    }

    # Normalize each
    clean_brl <- if (!is.null(brl) && nrow(brl) > 0) {
      names(brl) <- tolower(names(brl))
      message("    brl cols: ", paste(names(brl), collapse=", "))
      id_col  <- intersect(c("player_id","mlb_id","batter"), names(brl))[1]
      if (!is.na(id_col))
        dplyr::tibble(
          mlbam_id   = as.integer(brl[[id_col]]),
          sc_brl_pct = safe_col(brl, c("brl_percent","barrel_batted_rate","brl_pa","barrel_percent"))
        )
      else NULL
    } else NULL

    clean_exp <- if (!is.null(exp) && nrow(exp) > 0) {
      names(exp) <- tolower(names(exp))
      id_col <- intersect(c("player_id","mlb_id","batter"), names(exp))[1]
      if (!is.na(id_col))
        dplyr::tibble(
          mlbam_id = as.integer(exp[[id_col]]),
          sc_xba   = safe_col(exp,  c("est_ba","xba","x_ba","est_ba_")),
          sc_xwoba = safe_col(exp,  c("est_woba","xwoba","x_woba","est_woba_"))
        )
      else NULL
    } else NULL

    clean_spd <- if (!is.null(spd) && nrow(spd) > 0) {
      names(spd) <- tolower(names(spd))
      id_col  <- intersect(c("player_id","mlb_id","batter","id"), names(spd))[1]
      if (!is.na(id_col))
        dplyr::tibble(
          mlbam_id        = as.integer(spd[[id_col]]),
          sc_sprint_speed = safe_col(spd, c("sprint_speed","r_sprint_speed","speed"))
        )
      else NULL
    } else NULL

    # Merge all Statcast sources
    sc <- dplyr::tibble(mlbam_id = integer(0))
    if (!is.null(clean_brl)) sc <- dplyr::full_join(sc, clean_brl, by = "mlbam_id")
    if (!is.null(clean_exp)) sc <- dplyr::full_join(sc, clean_exp, by = "mlbam_id")
    if (!is.null(clean_spd)) sc <- dplyr::full_join(sc, clean_spd, by = "mlbam_id")

    sc %>%
      dplyr::filter(!is.na(mlbam_id)) %>%
      dplyr::mutate(season = season)
  }, error = function(e) {
    message("  SC ERROR: ", e$message); NULL
  })
}

# ── Build player ID bridge (fg_id ↔ mlbam_id) ────────────────
id_bridge <- player_master_ids %>%
  dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
  dplyr::distinct(fg_id, .keep_all = TRUE) %>%
  dplyr::transmute(fg_id = as.character(fg_id), mlbam_id = as.integer(mlbam_id))

# ── Fetch all data ────────────────────────────────────────────
message("=== Backtesting: fetching historical data ===")

actual_bat_list <- lapply(BACKTEST_SEASONS, fetch_actual_bat)
actual_pit_list <- lapply(BACKTEST_SEASONS, fetch_actual_pit)
sc_bat_list     <- lapply(BACKTEST_SEASONS, fetch_sc_bat)

names(actual_bat_list) <- names(actual_pit_list) <- names(sc_bat_list) <- BACKTEST_SEASONS

actual_bat <- dplyr::bind_rows(Filter(Negate(is.null), actual_bat_list))
actual_pit <- dplyr::bind_rows(Filter(Negate(is.null), actual_pit_list))

sc_bat_raw <- dplyr::bind_rows(Filter(Negate(is.null), sc_bat_list))
# Ensure Statcast table always has required columns even if all fetches failed
sc_bat_all <- sc_bat_raw %>%
  { if (!"mlbam_id"        %in% names(.)) dplyr::mutate(., mlbam_id=NA_integer_)        else . } %>%
  { if (!"sc_brl_pct"      %in% names(.)) dplyr::mutate(., sc_brl_pct=NA_real_)         else . } %>%
  { if (!"sc_xba"          %in% names(.)) dplyr::mutate(., sc_xba=NA_real_)             else . } %>%
  { if (!"sc_xwoba"        %in% names(.)) dplyr::mutate(., sc_xwoba=NA_real_)           else . } %>%
  { if (!"sc_sprint_speed" %in% names(.)) dplyr::mutate(., sc_sprint_speed=NA_real_)    else . } %>%
  { if (!"season"          %in% names(.)) dplyr::mutate(., season=NA_integer_)          else . }

message("Actual batting rows: ", nrow(actual_bat))
message("Actual pitching rows: ", nrow(actual_pit))
message("Statcast batting rows: ", nrow(sc_bat_all))

# ── Build batter prediction dataset ──────────────────────────
# For each season pair (N → N+1):
#   prior = Year N actuals (the "projection" input)
#   next  = Year N+1 actuals (the target)
#   sc    = Year N Statcast signals

season_pairs <- list(
  list(prior = 2022, next_ = 2023),
  list(prior = 2023, next_ = 2024)
)

build_bat_pairs <- function(pair) {
  prior_yr <- pair$prior
  next_yr  <- pair$next_

  prior <- actual_bat %>% dplyr::filter(season == prior_yr) %>%
    dplyr::left_join(id_bridge, by = "fg_id")

  next_ <- actual_bat %>% dplyr::filter(season == next_yr) %>%
    dplyr::select(fg_id, actual_fpts_next = actual_fpts,
                  pa_next = pa, hr_next = hr, sb_next = sb)

  sc <- sc_bat_all %>% dplyr::filter(season == prior_yr)

  prior %>%
    dplyr::left_join(next_, by = "fg_id") %>%
    dplyr::left_join(sc,    by = "mlbam_id") %>%
    dplyr::filter(!is.na(actual_fpts_next), pa_next >= MIN_PA) %>%
    dplyr::mutate(pair_label = paste0(prior_yr, "->", next_yr))
}

bat_pairs <- dplyr::bind_rows(lapply(season_pairs, build_bat_pairs))
message("Batter prediction pairs: ", nrow(bat_pairs),
        " (", paste(unique(bat_pairs$pair_label), collapse=", "), ")")

# ── Batter weight grid search ─────────────────────────────────
# For each weight combo: build projected fpts, compute RMSE vs actual_fpts_next

BLEND_GRID    <- seq(0.50, 1.00, by = 0.05)   # steamer weight (Statcast = 1 - blend)
BRL_COEF_GRID <- c(0.006, 0.009, 0.012, 0.015, 0.018)
XBA_GRID      <- c(0.20, 0.30, 0.35, 0.40, 0.50)
SPD_GRID      <- c(0.03, 0.05, 0.06, 0.07, 0.09)

LEAGUE_AVG_BRL <- 8.0

message("Running batter grid search: ",
        length(BLEND_GRID)*length(BRL_COEF_GRID)*length(XBA_GRID)*length(SPD_GRID),
        " combinations...")

bat_grid_results <- dplyr::bind_rows(lapply(BLEND_GRID, function(sw) {
  dplyr::bind_rows(lapply(BRL_COEF_GRID, function(bc) {
    dplyr::bind_rows(lapply(XBA_GRID, function(xb) {
      dplyr::bind_rows(lapply(SPD_GRID, function(sc_spd) {
        statcast_wt <- 1 - sw

        proj <- bat_pairs %>%
          dplyr::mutate(
            REGRESS = 0.85,

            # Naive projection: prior year * regression
            naive_hr  = hr  * REGRESS,
            naive_sb  = sb  * REGRESS,
            naive_h   = h   * REGRESS,
            naive_d2b = d2b * REGRESS,
            naive_d3b = d3b * REGRESS,
            naive_r   = r   * REGRESS,
            naive_rbi = rbi * REGRESS,
            naive_bb  = bb  * REGRESS,
            naive_hbp = hbp * REGRESS,
            naive_cs  = cs  * REGRESS,

            # Statcast-adjusted HR
            brl_factor = dplyr::case_when(
              !is.na(sc_brl_pct) & !is.na(naive_hr) & naive_hr > 0 ~
                pmax(0.75, pmin(1.35, 1 + (sc_brl_pct - LEAGUE_AVG_BRL) * bc)),
              TRUE ~ 1.0
            ),
            adj_hr = naive_hr * brl_factor,

            # Statcast-adjusted AVG (xBA signal)
            naive_avg = dplyr::if_else(dplyr::coalesce(ab,0) > 0, h/ab, 0.250),
            avg_adj   = naive_avg + dplyr::coalesce(sc_xba - naive_avg, 0) * xb,
            avg_adj   = pmax(0.150, pmin(0.380, avg_adj)),
            adj_h     = dplyr::coalesce(ab, 0) * avg_adj * REGRESS,

            # Statcast-adjusted SB (sprint speed)
            spd_factor = dplyr::case_when(
              !is.na(sc_sprint_speed) & !is.na(naive_sb) & naive_sb > 0 ~
                pmax(0.70, pmin(1.50, 1 + (sc_sprint_speed - 27.0) * sc_spd)),
              TRUE ~ 1.0
            ),
            adj_sb = naive_sb * spd_factor,

            # HR delta propagation to R/RBI
            hr_delta = adj_hr - naive_hr,
            adj_r    = naive_r   + hr_delta * 0.85,
            adj_rbi  = naive_rbi + hr_delta * 0.90,

            # Blended projection
            proj_hr  = sw * naive_hr  + statcast_wt * adj_hr,
            proj_h   = sw * naive_h   + statcast_wt * adj_h,
            proj_sb  = sw * naive_sb  + statcast_wt * adj_sb,
            proj_r   = sw * naive_r   + statcast_wt * adj_r,
            proj_rbi = sw * naive_rbi + statcast_wt * adj_rbi,

            proj_fpts = fpts_bat(proj_h, naive_d2b, naive_d3b, proj_hr,
                                 proj_r, proj_rbi, proj_sb, naive_cs,
                                 naive_bb, naive_hbp),
            resid = proj_fpts - actual_fpts_next
          )

        # RMSE and correlation
        rmse   <- sqrt(mean(proj$resid^2, na.rm = TRUE))
        mae    <- mean(abs(proj$resid), na.rm = TRUE)
        corr   <- suppressWarnings(cor(proj$proj_fpts, proj$actual_fpts_next,
                                       use = "complete.obs"))
        n      <- sum(!is.na(proj$resid))

        dplyr::tibble(
          steamer_wt = sw, statcast_wt = statcast_wt,
          brl_coef = bc, xba_blend = xb, spd_coef = sc_spd,
          rmse = rmse, mae = mae, corr = corr, n = n
        )
      }))
    }))
  }))
}))

message("Grid search complete. Rows: ", nrow(bat_grid_results))

# ── Find optimal batter weights ───────────────────────────────
optimal_bat_wts <- bat_grid_results %>%
  dplyr::arrange(rmse) %>%
  dplyr::slice(1)

message("\n=== OPTIMAL BATTER WEIGHTS ===")
message("  Steamer weight:    ", optimal_bat_wts$steamer_wt,
        "  (current: ", BLEND_STEAMER_WT, ")")
message("  Statcast weight:   ", optimal_bat_wts$statcast_wt,
        "  (current: ", BLEND_STATCAST_WT, ")")
message("  Barrel coef:       ", optimal_bat_wts$brl_coef,
        "  (current: 0.012)")
message("  xBA blend:         ", optimal_bat_wts$xba_blend,
        "  (current: 0.35)")
message("  Sprint speed coef: ", optimal_bat_wts$spd_coef,
        "  (current: 0.06)")
message("  RMSE:  ", round(optimal_bat_wts$rmse, 2))
message("  MAE:   ", round(optimal_bat_wts$mae,  2))
message("  Corr:  ", round(optimal_bat_wts$corr, 3))

# ── Per-signal correlation analysis (shows which signals matter most) ──────
message("\n=== SIGNAL CONTRIBUTION ANALYSIS ===")

signal_corr <- bat_pairs %>%
  dplyr::left_join(id_bridge, by = "fg_id") %>%
  dplyr::summarise(
    corr_brl_vs_hr_next    = suppressWarnings(cor(sc_brl_pct,      hr_next,   use="complete.obs")),
    corr_xba_vs_fpts_next  = suppressWarnings(cor(sc_xba,          actual_fpts_next, use="complete.obs")),
    corr_xwoba_vs_fpts     = suppressWarnings(cor(sc_xwoba,        actual_fpts_next, use="complete.obs")),
    corr_spd_vs_sb_next    = suppressWarnings(cor(sc_sprint_speed,  sb_next,   use="complete.obs")),
    corr_prior_fpts_vs_next= suppressWarnings(cor(actual_fpts,     actual_fpts_next, use="complete.obs"))
  )

message("  Prior-year fpts → next-year fpts:  ", round(signal_corr$corr_prior_fpts_vs_next, 3),
        "  (baseline — naive regression)")
message("  Barrel% → next-year HR:            ", round(signal_corr$corr_brl_vs_hr_next, 3))
message("  xBA → next-year fpts:              ", round(signal_corr$corr_xba_vs_fpts_next, 3))
message("  xwOBA → next-year fpts:            ", round(signal_corr$corr_xwoba_vs_fpts, 3))
message("  Sprint speed → next-year SB:       ", round(signal_corr$corr_spd_vs_sb_next, 3))

# ── Pitcher ERA signal correlation ────────────────────────────
message("\n=== PITCHER SIGNAL ANALYSIS ===")

if (nrow(actual_pit) == 0) {
  message("  Pitcher actuals unavailable — skipping pitcher analysis")
} else {

# Build pitcher pairs for ERA signal evaluation
pit_pairs <- dplyr::bind_rows(lapply(season_pairs, function(pair) {
  prior <- actual_pit %>% dplyr::filter(season == pair$prior) %>%
    dplyr::left_join(id_bridge, by = "fg_id")

  # Pull FIP/xFIP for prior season from pitching_master_season if available
  fip_data <- if (exists("pitching_master_season")) {
    pitching_master_season %>%
      dplyr::filter(season == pair$prior, !is.na(mlbam_id)) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(c("fg_FIP","fg_xFIP","fg_SIERA","fg_K_9")))
  } else NULL

  next_ <- actual_pit %>% dplyr::filter(season == pair$next_) %>%
    dplyr::select(fg_id, actual_fpts_next = actual_fpts, era_next = era, k_next = k)

  df <- prior %>%
    dplyr::left_join(next_, by = "fg_id") %>%
    dplyr::filter(!is.na(actual_fpts_next))

  if (!is.null(fip_data))
    df <- df %>% dplyr::left_join(fip_data, by = "mlbam_id")

  df %>% dplyr::mutate(pair_label = paste0(pair$prior, "->", pair$next_))
}))

if (nrow(pit_pairs) > 0) {
  pit_corr <- pit_pairs %>%
    dplyr::summarise(
      corr_era_vs_next_era   = suppressWarnings(cor(era,    era_next,          use="complete.obs")),
      corr_fip_vs_next_era   = suppressWarnings(cor(dplyr::coalesce(
                                 if ("fg_FIP" %in% names(.)) fg_FIP else NA_real_, era),
                                 era_next, use="complete.obs")),
      corr_xfip_vs_next_era  = suppressWarnings(cor(dplyr::coalesce(
                                 if ("fg_xFIP" %in% names(.)) fg_xFIP else NA_real_, era),
                                 era_next, use="complete.obs")),
      corr_k9_vs_next_k      = suppressWarnings(cor(
                                 if ("fg_K_9" %in% names(.)) fg_K_9 else k/pmax(ip,1)*9,
                                 k_next, use="complete.obs")),
      corr_prior_fpts_vs_next= suppressWarnings(cor(actual_fpts, actual_fpts_next, use="complete.obs"))
    )

  message("  Prior-year ERA → next-year ERA:  ", round(pit_corr$corr_era_vs_next_era, 3))
  message("  FIP → next-year ERA:             ", round(pit_corr$corr_fip_vs_next_era, 3))
  message("  xFIP → next-year ERA:            ", round(pit_corr$corr_xfip_vs_next_era, 3))
  message("  K/9 → next-year K:               ", round(pit_corr$corr_k9_vs_next_k, 3))
  message("  Prior-year fpts → next-year fpts:", round(pit_corr$corr_prior_fpts_vs_next, 3))
}

# ── Top and bottom of our RMSE grid (show range) ─────────────
message("\n=== GRID SEARCH SUMMARY ===")
message("Best 5 weight combinations:")
print(bat_grid_results %>% dplyr::arrange(rmse) %>% dplyr::slice(1:5))
message("\nWorst 5 weight combinations:")
print(bat_grid_results %>% dplyr::arrange(dplyr::desc(rmse)) %>% dplyr::slice(1:5))

message("\n=== CURRENT WEIGHTS vs OPTIMAL ===")
current_rmse <- bat_grid_results %>%
  dplyr::filter(
    abs(steamer_wt - BLEND_STEAMER_WT) < 0.01,
    abs(brl_coef   - 0.012)  < 0.001,
    abs(xba_blend  - 0.35)   < 0.01,
    abs(spd_coef   - 0.06)   < 0.001
  ) %>%
  dplyr::pull(rmse)

if (length(current_rmse) > 0)
  message("  Current weights RMSE: ", round(current_rmse[1], 2))
message("  Optimal weights RMSE: ", round(optimal_bat_wts$rmse, 2))
if (length(current_rmse) > 0)
  message("  Improvement: ", round(current_rmse[1] - optimal_bat_wts$rmse, 2), " pts RMSE")

} # end pitcher analysis block

message("\n05_backtest.R complete.")
