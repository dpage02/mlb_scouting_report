# ============================================================
# mlb_scouting_report
# GAME NARRATIVE + PREDICTION HELPERS
# SCRIPT: _game_narrative_helpers.R
# ============================================================
# PURPOSE:
#   Generate stat-driven narrative bullets (main page) and
#   a prediction model section (game deep dive).
#
# FUNCTIONS:
#   make_game_bullets_html(gpk)   — <ul> bullets for main page
#   make_game_preview_html(gpk)   — prose paragraph for deep dive top
#   make_prediction_html(gpk)     — projection table + factors
#
# MODEL:
#   Expected runs = LEAGUE_AVG_RUNS
#                   × (lineup avg wRC+ / 100)
#                   × (park_factor)
#                   × (LEAGUE_AVG_FIP / opposing blended pitcher FIP)
#
#   Blended pitcher FIP = SP_frac × SP_xFIP  +  BP_frac × bullpen_ERA
#     SP_frac = min(SP_IP_per_GS / 9, 0.85)
#     BP_frac = 1 - SP_frac
#
#   Home field advantage: +0.02 run residual only (empirical HFA in
#   run differential is ~0 over 2022-2024; 52.5% home win rate comes
#   from last-at-bat tactical edge, not raw run scoring).
#
#   Win probability via Poisson distribution.
#   Constants calibrated to 2022-2024 empirical averages.
# ============================================================

library(dplyr)

# ---- Constants (empirically calibrated 2022-2024, n=7,300 games) ----
LEAGUE_AVG_RUNS  <- 4.43   # empirical MLB avg runs/team/game 2022-2024
LEAGUE_AVG_FIP   <- 4.15   # empirical avg SP xFIP 2022-2024
HOME_FIELD_BONUS <- 0.02   # residual last-at-bat/crowd advantage (run differential ≈0 empirically)
BLEND_GS_FLOOR   <- 15L    # GS needed to fully trust current-season stats; blend with prior below this

# Expected PA per batting slot relative to lineup average (empirical MLB 2022-2024)
# Slot 1 bats ~4.3×/game vs slot 9 ~3.5×/game; normalized so mean slot = 1.0
# Used to weight each batter's wRC+ by how many times they'll actually bat
SLOT_PA_WEIGHT <- c(
  `1` = 1.103, `2` = 1.077, `3` = 1.051, `4` = 1.026, `5` = 1.000,
  `6` = 0.974, `7` = 0.949, `8` = 0.923, `9` = 0.897
)

# Stabilization constant for wRC+ (empirical: ~550 PA = 50/50 signal vs noise)
# Source: Baseball Prospectus / FanGraphs stabilization research
WRC_STAB_PA <- 550L

# ============================================================
# Internal helpers
# ============================================================

.get_num <- function(row, col, default = NA_real_) {
  if (col %in% names(row) && length(row[[col]]) > 0) {
    v <- suppressWarnings(as.numeric(row[[col]][1]))
    if (is.finite(v)) v else default
  } else default
}

.era_plus_val <- function(row) {
  bbref <- .get_num(row, "bbref_ERA_plus")
  fg_em <- .get_num(row, "fg_ERA_minus")
  val   <- dplyr::coalesce(
    bbref,
    if (!is.na(fg_em) && fg_em > 0) round(10000 / fg_em) else NA_real_
  )
  if (!is.na(val) && is.finite(val)) val else NA_real_
}

.best_fip_val <- function(row) {
  # Prefer xFIP > SIERA > FIP > ERA (best to worst regressor)
  dplyr::coalesce(
    .get_num(row, "fg_xFIP"),
    .get_num(row, "fg_SIERA"),
    .get_num(row, "fg_FIP"),
    .get_num(row, "mlb_era"),
    LEAGUE_AVG_FIP
  )
}

.fip_label_used <- function(row) {
  if (nrow(row) == 0) return("Lg Avg")
  if (!is.na(.get_num(row, "fg_xFIP")))  return("xFIP")
  if (!is.na(.get_num(row, "fg_SIERA"))) return("SIERA")
  if (!is.na(.get_num(row, "fg_FIP")))   return("FIP")
  if (!is.na(.get_num(row, "mlb_era")))  return("ERA")
  "Lg Avg"
}

# Blended FIP: linear weight between prior-season xFIP and current-season FIP.
# At 0 GS → 100% prior; at BLEND_GS_FLOOR GS → 100% current; linear between.
# Falls back to current-only if no prior data exists.
.blended_fip <- function(row) {
  cur_fip   <- .best_fip_val(row)
  cur_gs    <- .get_num(row, "mlb_gs")
  prior_fip <- dplyr::coalesce(.get_num(row, "prior_xfip"), .get_num(row, "prior_era"))

  if (is.na(prior_fip))                          return(cur_fip)
  if (is.na(cur_gs) || cur_gs >= BLEND_GS_FLOOR) return(cur_fip)

  w_cur <- cur_gs / BLEND_GS_FLOOR
  w_cur * cur_fip + (1 - w_cur) * prior_fip
}

# Convert a FIP-like value to an ERA+-like index (100 = league avg, higher = better).
.fip_to_era_plus <- function(fip) {
  if (is.na(fip) || fip <= 0) return(NA_real_)
  round(LEAGUE_AVG_FIP / fip * 100)
}

# ERA+ equivalent using blended stats — used for quality label logic only.
.blended_era_plus <- function(row) {
  .fip_to_era_plus(.blended_fip(row))
}

# Trend note: compares current ERA+ to prior-season ERA+ equivalent.
# Returns "hot start", "slow start", or NULL when sample is sufficient.
# Thresholds: +30 above history = hot start; -25 below history = slow start.
# Only fires when GS < BLEND_GS_FLOOR (otherwise current stats speak for themselves).
.sp_trend_note <- function(row) {
  cur_ep    <- .era_plus_val(row)
  cur_gs    <- .get_num(row, "mlb_gs")
  prior_fip <- dplyr::coalesce(.get_num(row, "prior_xfip"), .get_num(row, "prior_era"))

  if (is.na(cur_ep) || is.na(prior_fip) || is.na(cur_gs)) return(NULL)
  if (cur_gs >= BLEND_GS_FLOOR) return(NULL)

  prior_ep <- .fip_to_era_plus(prior_fip)
  if (is.na(prior_ep)) return(NULL)

  delta <- cur_ep - prior_ep

  if      (delta >=  30) "hot start"
  else if (delta <= -25 && prior_ep >= 110) "slow start \u2014 established track record"
  else if (delta <= -25)                    "slow start"
  else NULL
}

# Rotation slot from depth_charts (SP1=1, SP2=2, ... SP5=5, NA if unavailable).
.sp_rotation_slot <- function(sp_row) {
  if (nrow(sp_row) == 0) return(NA_integer_)
  if (!exists("depth_charts") || nrow(depth_charts) == 0) return(NA_integer_)
  mid <- dplyr::coalesce(sp_row$mlbam_id[1], NA_integer_)
  if (is.na(mid)) return(NA_integer_)
  dc <- depth_charts %>% dplyr::filter(mlbam_id == mid) %>% dplyr::slice_head(n = 1)
  if (nrow(dc) == 0 || !"fg_role" %in% names(dc)) return(NA_integer_)
  suppressWarnings(as.integer(stringr::str_extract(dplyr::coalesce(dc$fg_role[1], ""), "\\d+")))
}

# TRUE when a SP is genuinely top-of-rotation caliber, not just "good numbers this week".
# Requires slot 1-2 in the depth chart, OR (no slot data AND ERA+ >= 140 — very high bar).
.sp_is_ace_caliber <- function(sp_row) {
  blep <- tryCatch(.blended_era_plus(sp_row), error = function(e) NA_real_)
  if (is.na(blep) || blep < 115) return(FALSE)
  slot <- .sp_rotation_slot(sp_row)
  if (is.na(slot)) return(blep >= 140)
  slot <= 2
}

.era_plus_tier <- function(x) {
  if (is.na(x)) return(NULL)
  dplyr::case_when(x >= 150 ~ "elite", x >= 130 ~ "excellent",
                   x >= 110 ~ "above-average", x >= 90 ~ "average",
                   TRUE ~ "below-average")
}

.wrc_tier <- function(x) {
  if (is.na(x) || is.nan(x)) return("average")
  dplyr::case_when(x >= 120 ~ "elite", x >= 110 ~ "above-average",
                   x >= 95  ~ "average", TRUE ~ "below-average")
}

.fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("—")
  paste0(round(x * 100, digits), "%")
}

# Poisson win probability: P(home score > away score)
.poisson_win_prob <- function(lambda_home, lambda_away) {
  r  <- 0:25
  ph <- dpois(r, max(lambda_home, 0.01))
  pa <- dpois(r, max(lambda_away, 0.01))
  p_home_wins <- sum(vapply(seq_along(r), function(i) {
    h <- r[i]
    if (h == 0) return(0)
    ph[i] * sum(pa[seq_len(h)])   # P(H=h) * P(A < h)
  }, numeric(1)))
  p_tie <- sum(ph * pa)
  p_home_wins + 0.5 * p_tie      # ties split 50/50 (extra innings)
}

# Age adjustment for wRC+ — empirical MLB career arc
# Peak ~27-28; improvement in mid-20s; accelerating decline after 32
# Returns a delta to add to the regressed estimate
.age_adj_wrc <- function(age) {
  if (is.na(age) || !is.finite(age) || age <= 0) return(0)
  dplyr::case_when(
    age <= 22 ~ -3L,   # raw, inconsistent production
    age <= 24 ~ -1L,   # developing, below projection
    age <= 29 ~ +1L,   # peak window
    age <= 31 ~  0L,   # post-peak, holding
    age <= 33 ~ -2L,   # noticeable decline
    age <= 35 ~ -4L,   # steep decline
    TRUE      ~ -6L    # late career
  )
}

# Stabilization-based regression fallback — used when Steamer projection unavailable.
#
# How this works (and why it's better than Marcel's 3-2-1):
#   Marcel applies fixed weights (1.0 / 0.6 / 0.3) regardless of sample size,
#   then regresses by a fixed amount regardless of how much data exists.
#
#   Here we do it correctly:
#     1. Weight each season's PA by recency (recent seasons more predictive)
#     2. Compute the PA-weighted observed wRC+ across those seasons
#     3. Regress toward 100 in proportion to sample size:
#          regressed = (obs × total_eff_PA + 100 × STAB_PA) / (total_eff_PA + STAB_PA)
#        At 0 PA  → fully regress to 100 (no information)
#        At 550 PA → 50% observed / 50% mean  (stabilization point)
#        At 1100 PA → 67% observed / 33% mean
#        At 2200 PA → 80% observed / 20% mean  (established veteran)
#     4. Apply age curve: +1 in peak years (26-29), -2 to -6 in decline
#
.stabilized_wrc <- function(pid) {
  # ── Primary path: offense_master_season (rebuilt every daily run) ──────────
  # Uses FG wRC+ + FG PA directly — no base rebuild required.
  # fg_wRC_plus comes from 02_fangraphs_offense_season.R type-8 pull.
  # fg_PA is the full prior-season (or YTD) PA behind that wRC+ estimate.
  if (exists("offense_master_season") &&
      "fg_wRC_plus" %in% names(offense_master_season)) {
    oms_rows <- offense_master_season[
      offense_master_season$mlbam_id == pid, , drop = FALSE]
    if (nrow(oms_rows) > 0) {
      # For traded players there may be multiple stint rows; pick highest FG PA
      if ("fg_PA" %in% names(oms_rows)) {
        best_idx <- which.max(suppressWarnings(as.numeric(oms_rows$fg_PA)))
        best     <- oms_rows[best_idx, , drop = FALSE]
      } else {
        best <- oms_rows[1, , drop = FALSE]
      }
      obs_wrc <- suppressWarnings(as.numeric(best$fg_wRC_plus[1]))
      if (!is.na(obs_wrc) && is.finite(obs_wrc)) {
        pa <- if ("fg_PA" %in% names(best))
                suppressWarnings(as.numeric(best$fg_PA[1])) else NA_real_
        # mlb_pa as PA fallback (may be tiny early-season YTD)
        if (is.na(pa) || pa < 1) {
          pa <- if ("mlb_pa" %in% names(best))
                  suppressWarnings(as.numeric(best$mlb_pa[1])) else NA_real_
        }
        if (is.na(pa) || pa < 1) pa <- 200  # default: treat as ~half-season data

        # Stabilization regression: (obs × PA + 100 × STAB) / (PA + STAB)
        regressed <- (obs_wrc * pa + 100 * WRC_STAB_PA) / (pa + WRC_STAB_PA)

        # Age adjustment
        age_val <- if ("fg_Age" %in% names(best))
                     suppressWarnings(as.numeric(best$fg_Age[1])) else NA_real_

        return(max(50, min(185, regressed + .age_adj_wrc(age_val))))
      }
    }
  }

  # ── Fallback: multi-year regression from player_career_offense ──────────────
  # Used when offense_master_season is unavailable or has no wRC+ for this player.
  # Requires base pipeline rebuild to have populated fg_wRC_plus in career table.
  if (!exists("player_career_offense") || nrow(player_career_offense) == 0 ||
      !"fg_wRC_plus" %in% names(player_career_offense)) return(NA_real_)

  cur_yr  <- max(player_career_offense$season, na.rm = TRUE)
  yr_map  <- c(cur_yr, cur_yr - 1L, cur_yr - 2L)
  wt_map  <- c(1.00, 0.65, 0.45)

  rows <- player_career_offense[
    player_career_offense$mlbam_id == pid &
    player_career_offense$season %in% yr_map &
    !is.na(player_career_offense$fg_wRC_plus), , drop = FALSE]
  if (nrow(rows) == 0) return(NA_real_)

  pa_col <- intersect(c("hist_pa", "mlb_pa", "PA", "pa"), names(rows))[1]
  pa_vec <- if (!is.na(pa_col)) suppressWarnings(as.numeric(rows[[pa_col]]))
            else rep(200, nrow(rows))
  pa_vec[is.na(pa_vec) | pa_vec < 0] <- 0

  yr_idx   <- match(rows$season, yr_map)
  eff_pa   <- pa_vec * wt_map[yr_idx]
  total_pa <- sum(eff_pa, na.rm = TRUE)
  if (total_pa < 1) return(NA_real_)

  obs_wrc   <- sum(rows$fg_wRC_plus * eff_pa, na.rm = TRUE) / total_pa
  regressed <- (obs_wrc * total_pa + 100 * WRC_STAB_PA) / (total_pa + WRC_STAB_PA)

  age_val <- NA_real_
  if ("fg_Age" %in% names(rows))
    age_val <- suppressWarnings(as.numeric(rows$fg_Age[which.max(rows$season)]))
  if ((is.na(age_val) || !is.finite(age_val)) &&
      exists("offense_master_season") &&
      "fg_Age" %in% names(offense_master_season)) {
    age_row <- offense_master_season[offense_master_season$mlbam_id == pid, , drop = FALSE]
    if (nrow(age_row) > 0)
      age_val <- suppressWarnings(as.numeric(age_row$fg_Age[1]))
  }

  max(50, min(185, regressed + .age_adj_wrc(age_val)))
}

# Display version: PA-weighted mean of actual season fg_wRC_plus.
# Matches what the overview cards and lineup tables show.
# Falls back to .team_wrc() (blended) if no actual data is available.
.team_wrc_display <- function(lineup_rows) {
  if (nrow(lineup_rows) == 0) return(100)

  full_stats <- if (exists("offense_master_season") && nrow(offense_master_season) > 0) {
    offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id,
                    dplyr::any_of(c("mlb_pa", "fg_wRC_plus")))
  } else NULL

  if (is.null(full_stats)) return(.team_wrc(lineup_rows))

  raw <- lineup_rows %>%
    dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
    dplyr::select(-dplyr::ends_with("_dup"))

  wrc_vals <- if ("fg_wRC_plus" %in% names(raw)) raw$fg_wRC_plus else rep(NA_real_, nrow(raw))
  pa_vals  <- dplyr::coalesce(
    if ("mlb_pa" %in% names(raw)) as.numeric(raw$mlb_pa) else NULL,
    rep(1, nrow(raw))
  )
  pa_vals <- pmax(pa_vals, 1)

  ok <- !is.na(wrc_vals) & is.finite(wrc_vals)
  if (sum(ok) == 0) return(.team_wrc(lineup_rows))
  round(weighted.mean(wrc_vals[ok], pa_vals[ok]))
}

# Team lineup wRC+ — best-available estimate per batter, adjusted for:
#   1. True talent: Steamer projected wRC+ > Marcel blend (PA-weighted 3-yr)
#   2. Handedness: OPS ratio vs opposing SP's arm side (min 50 PA; cap ±25%)
#   3. Batting order: PA-weighted by slot (slot 1 bats ~22% more than slot 9)
# Used internally for the run model only — use .team_wrc_display() for text/tables.
.team_wrc <- function(lineup_rows) {
  if (nrow(lineup_rows) == 0) return(100)

  steamer_ok <- exists("steamer_projections") &&
    is.data.frame(steamer_projections) &&
    nrow(steamer_projections) > 0 &&
    "steamer_wrc_plus" %in% names(steamer_projections)

  # Locate this lineup's applicable split data (from 04_matchup_splits.R)
  # lineup_context_splits has sp_ops (split OPS) and mlb_ops (overall OPS)
  game_pk_val <- if ("game_pk" %in% names(lineup_rows)) lineup_rows$game_pk[1] else NA_integer_
  side_val    <- if ("side"    %in% names(lineup_rows)) lineup_rows$side[1]    else NA_character_

  applicable_splits <- NULL
  if (!is.na(game_pk_val) && !is.na(side_val) &&
      exists("lineup_context_splits") &&
      is.data.frame(lineup_context_splits) && nrow(lineup_context_splits) > 0 &&
      all(c("sp_ops", "sp_pa", "applicable_split") %in% names(lineup_context_splits))) {
    applicable_splits <- lineup_context_splits %>%
      dplyr::filter(game_pk == game_pk_val, side == side_val) %>%
      dplyr::select(mlbam_id, sp_ops, sp_pa, applicable_split)
  }

  has_splits <- !is.null(applicable_splits) && nrow(applicable_splits) > 0 &&
    any(!is.na(applicable_splits$applicable_split))

  # Per-player adjusted wRC+ with slot weight
  results <- lapply(seq_len(nrow(lineup_rows)), function(i) {
    pid  <- lineup_rows$mlbam_id[i]
    slot <- as.character(
      if ("batting_slot" %in% names(lineup_rows)) lineup_rows$batting_slot[i] else NA_integer_
    )

    # 1. True-talent wRC+
    # Blend actual season wRC+ with Steamer/Marcel based on PA:
    #   <30 PA  → preseason projection only
    #   30 PA   → 20% actual, 80% projection
    #   100 PA  → 55% actual, 45% projection
    #   200 PA  → 75% actual, 25% projection
    #   400+ PA → 90% actual, 10% projection
    actual_wrc <- if ("fg_wRC_plus" %in% names(lineup_rows) &&
                      !is.na(lineup_rows$fg_wRC_plus[i]))
      as.numeric(lineup_rows$fg_wRC_plus[i]) else NA_real_
    actual_pa  <- if ("mlb_pa" %in% names(lineup_rows) &&
                      !is.na(lineup_rows$mlb_pa[i]))
      as.numeric(lineup_rows$mlb_pa[i]) else 0

    proj_wrc <- NA_real_
    if (steamer_ok) {
      st_row <- steamer_projections[steamer_projections$mlbam_id == pid, ]
      if (nrow(st_row) > 0 && !is.na(st_row$steamer_wrc_plus[1]))
        proj_wrc <- as.numeric(st_row$steamer_wrc_plus[1])
    }
    if (is.na(proj_wrc)) proj_wrc <- .stabilized_wrc(pid)

    base_wrc <- if (!is.na(actual_wrc) && actual_pa >= 30 && !is.na(proj_wrc)) {
      actual_w <- min(0.90, max(0.20, (actual_pa - 30) / 400))
      actual_wrc * actual_w + proj_wrc * (1 - actual_w)
    } else if (!is.na(actual_wrc) && actual_pa >= 30) {
      actual_wrc
    } else {
      proj_wrc
    }
    if (is.na(base_wrc)) return(NULL)

    # 2. Handedness adjustment: apply split OPS ratio to true-talent estimate
    # Logic: if a batter hits .820 OPS overall but .680 vs RHP (vs .750 avg),
    # their effective wRC+ against today's RH starter is ~(680/820) × base_wrc
    # Minimum 50 PA in the split to apply; cap multiplier at ±25%
    if (has_splits) {
      sp_row <- applicable_splits[applicable_splits$mlbam_id == pid, ]
      if (nrow(sp_row) > 0 && !is.na(sp_row$applicable_split[1])) {
        sp_ops_val  <- sp_row$sp_ops[1]
        sp_pa_val   <- if ("sp_pa" %in% names(sp_row)) sp_row$sp_pa[1] else NA_integer_
        overall_ops <- if ("mlb_ops" %in% names(lineup_rows)) lineup_rows$mlb_ops[i] else NA_real_

        if (!is.na(sp_ops_val) && !is.na(overall_ops) && overall_ops > 0.100 &&
            !is.na(sp_pa_val) && sp_pa_val >= 50L) {
          hand_mult <- max(0.75, min(1.25, sp_ops_val / overall_ops))
          base_wrc  <- base_wrc * hand_mult
        }
      }
    }

    # 3. Batting order PA weight
    slot_wt <- if (!is.na(slot) && slot %in% names(SLOT_PA_WEIGHT))
      SLOT_PA_WEIGHT[[slot]] else 1.0

    list(wrc = base_wrc, wt = slot_wt)
  })

  # Slot-PA-weighted mean
  results  <- Filter(Negate(is.null), results)
  wrc_vals <- sapply(results, `[[`, "wrc")
  wt_vals  <- sapply(results, `[[`, "wt")
  ok <- !is.na(wrc_vals)
  if (sum(ok) < 3) return(100)
  sum(wrc_vals[ok] * wt_vals[ok]) / sum(wt_vals[ok])
}

# ============================================================
# make_game_bullets_html(gpk)
# 3-5 quick-hit bullets for the main page game summary row
# ============================================================

make_game_bullets_html <- function(gpk) {
  game    <- game_context   %>% dplyr::filter(game_pk == gpk)
  sps     <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup  <- lineup_context  %>% dplyr::filter(game_pk == gpk)
  bullpen <- bullpen_grid    %>% dplyr::filter(game_pk == gpk)

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"

  bullets <- character(0)

  # ---- 1. Pitching matchup ----
  away_ep <- if (nrow(away_sp) > 0) .era_plus_val(away_sp) else NA_real_
  home_ep <- if (nrow(home_sp) > 0) .era_plus_val(home_sp) else NA_real_
  away_gs <- if (nrow(away_sp) > 0) .get_num(away_sp, "mlb_gs") else NA_real_
  home_gs <- if (nrow(home_sp) > 0) .get_num(home_sp, "mlb_gs") else NA_real_

  # Blended ERA+ equivalent: weights current season vs prior season by GS.
  # Stabilizes quality label early in the season (e.g. ace off to rough start).
  away_blep <- if (nrow(away_sp) > 0) .blended_era_plus(away_sp) else NA_real_
  home_blep <- if (nrow(home_sp) > 0) .blended_era_plus(home_sp) else NA_real_

  # Display string: show current ERA+ with prior-season context when sample is small.
  # Format: Name (ERA+ X · N GS · YYYY xFIP X.XX — trend note)
  # trend note = "hot start" | "slow start — established track record" | omitted
  .sp_matchup_str <- function(nm, ep, gs, sp_row) {
    parts <- character(0)
    if (!is.na(ep)) parts <- c(parts, paste0("ERA+ ", round(ep)))

    small_sample <- !is.na(gs) && gs < BLEND_GS_FLOOR
    if (small_sample) {
      if (!is.na(gs)) parts <- c(parts, paste0(round(gs), " GS"))
      prior_fip <- if (nrow(sp_row) > 0)
        dplyr::coalesce(.get_num(sp_row, "prior_xfip"), .get_num(sp_row, "prior_era"))
      else NA_real_
      prior_yr  <- if (nrow(sp_row) > 0) .get_num(sp_row, "prior_season") else NA_real_
      if (!is.na(prior_fip)) {
        yr_str <- if (!is.na(prior_yr)) paste0(round(prior_yr), " ") else ""
        parts  <- c(parts, paste0(yr_str, "xFIP ", round(prior_fip, 2)))
      }
    }

    if (length(parts) == 0) return(nm)

    trend <- if (nrow(sp_row) > 0) .sp_trend_note(sp_row) else NULL
    suffix <- if (!is.null(trend)) paste0(" \u2014 ", trend) else ""

    paste0(nm, " (", paste(parts, collapse = " \u00b7 "), suffix, ")")
  }

  away_str <- .sp_matchup_str(away_name, away_ep, away_gs, away_sp)
  home_str <- .sp_matchup_str(home_name, home_ep, home_gs, home_sp)

  # Quality label uses BLENDED ERA+ so a temporarily-struggling ace
  # still earns the right label (and a hot 2-start pitcher doesn't inflate it).
  # Rotation slot gates "marquee" language — a #4 with great numbers is interesting,
  # but not the same as two genuine front-line starters going head-to-head.
  both_good      <- !is.na(away_blep) && !is.na(home_blep) &&
                    away_blep >= 110 && home_blep >= 110
  away_ace_cal   <- .sp_is_ace_caliber(away_sp)
  home_ace_cal   <- .sp_is_ace_caliber(home_sp)

  label <- if (both_good && away_ace_cal && home_ace_cal) {
    "Quality pitching matchup"
  } else if (both_good) {
    "Solid pitching matchup"
  } else if (!is.na(away_blep) && away_blep >= 120 && (is.na(home_blep) || home_blep < 100)) {
    paste0("Pitching edge \u2014 ", away_team)
  } else if (!is.na(home_blep) && home_blep >= 120 && (is.na(away_blep) || away_blep < 100)) {
    paste0("Pitching edge \u2014 ", home_team)
  } else {
    "Pitching matchup"
  }

  if (!is.na(away_ep) || !is.na(home_ep) ||
      away_name != "TBD" || home_name != "TBD") {
    bullets <- c(bullets,
      paste0("<strong>", label, "</strong>: ", away_str, " vs ", home_str))
  }

  # ---- 1b. Series context (rubber match / finale / opener — only when notable) ----
  game_in_s   <- if ("game_in_series"   %in% names(game) && !is.na(game$game_in_series[1]))
                   as.integer(game$game_in_series[1])   else NA_integer_
  series_len  <- if ("series_length"    %in% names(game) && !is.na(game$series_length[1]))
                   as.integer(game$series_length[1])    else NA_integer_
  is_rubber   <- isTRUE("is_rubber_match"   %in% names(game) && game$is_rubber_match[1]   == TRUE)
  is_finale   <- isTRUE("is_series_finale"  %in% names(game) && game$is_series_finale[1]  == TRUE)
  is_opener   <- isTRUE("is_series_opener"  %in% names(game) && game$is_series_opener[1]  == TRUE)

  if (is_rubber) {
    bullets <- c(bullets,
      paste0("<strong>Rubber match</strong>: Series tied \u2014 winner takes the series"))
  } else if (is_finale && !is.na(series_len) && series_len >= 4) {
    bullets <- c(bullets,
      paste0("<strong>Series finale</strong>: Game ", game_in_s, " of ", series_len))
  } else if (is_opener && !is.na(series_len)) {
    bullets <- c(bullets,
      paste0("<strong>Series opener</strong>: ", series_len, "-game series at ", home_team))
  }

  # ---- 1c. Park environment (only when notably hitter- or pitcher-friendly) ----
  home_team_id_pf <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
  pf_val <- .park_factor(home_team_id_pf)

  if (pf_val >= 1.05) {
    pf_venue <- dplyr::coalesce(game$venue_name[1], "this park")
    bullets <- c(bullets, paste0(
      "<strong>Hitter-friendly venue</strong>: ", pf_venue,
      " (park factor ", round(pf_val, 3),
      " \u2014 +", round((pf_val - 1) * 100), "% run environment)"
    ))
  } else if (pf_val <= 0.97) {
    pf_venue <- dplyr::coalesce(game$venue_name[1], "this park")
    bullets <- c(bullets, paste0(
      "<strong>Pitcher-friendly venue</strong>: ", pf_venue,
      " (park factor ", round(pf_val, 3),
      " \u2014 ", round((pf_val - 1) * 100), "% run environment)"
    ))
  }

  # ---- 2. Swing-and-miss stuff (highlight any SP with K% ≥ 22%) ----
  k_parts <- character(0)
  for (sp_row in list(away_sp, home_sp)) {
    if (nrow(sp_row) == 0) next
    kpct <- .get_num(sp_row, "fg_K_pct")
    if (is.na(kpct) || kpct < 0.22) next
    kbb  <- .get_num(sp_row, "fg_K_BB_pct")
    nm   <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
    tier <- if (kpct >= 0.27) "elite" else "above-average"
    part <- paste0(nm, ": ", tier, " K rate (", .fmt_pct(kpct), " K%",
                   if (!is.na(kbb)) paste0(", ", .fmt_pct(kbb), " K-BB%") else "", ")")
    k_parts <- c(k_parts, part)
  }
  if (length(k_parts) > 0)
    bullets <- c(bullets,
      paste0("<strong>Strikeout stuff</strong>: ", paste(k_parts, collapse = " · ")))

  # ---- 3. Offense comparison ----
  away_wrc <- .team_wrc_display(lineup %>% dplyr::filter(side == "away"))
  home_wrc <- .team_wrc_display(lineup %>% dplyr::filter(side == "home"))

  both_default <- (away_wrc == 100 && home_wrc == 100)
  if (!both_default) {
    if (abs(away_wrc - home_wrc) >= 15) {
      stronger     <- if (away_wrc > home_wrc) away_team else home_team
      stronger_wrc <- max(away_wrc, home_wrc)
      weaker       <- if (away_wrc > home_wrc) home_team else away_team
      weaker_wrc   <- min(away_wrc, home_wrc)
      bullets <- c(bullets, paste0(
        "<strong>Offense edge \u2014 ", stronger, "</strong>: ",
        round(stronger_wrc), " wRC+ vs ", round(weaker_wrc), " for ", weaker
      ))
    } else {
      bullets <- c(bullets, paste0(
        "<strong>Even offenses</strong>: ",
        away_team, " (", round(away_wrc), " wRC+)",
        " \u00b7 ", home_team, " (", round(home_wrc), " wRC+)"
      ))
    }
  }

  # ---- 4. Weather (only if notable) ----
  wind_mph <- .get_num(game, "wind_speed_mph")
  temp_f   <- .get_num(game, "game_temp_f")
  wind_dir <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
    as.character(game$wind_direction[1]) else NA_character_

  weather_parts <- character(0)
  if (!is.na(wind_mph) && wind_mph >= 10) {
    dir_note <- if (!is.na(wind_dir)) paste0(" (", wind_dir, ")") else ""
    impact   <- dplyr::case_when(
      grepl("out|center|left|right", tolower(dplyr::coalesce(wind_dir, ""))) ~ " — HR-friendly conditions",
      grepl("^in$|home plate|backstop", tolower(dplyr::coalesce(wind_dir, ""))) ~ " — pitcher-friendly conditions",
      TRUE ~ ""
    )
    weather_parts <- c(weather_parts, paste0(round(wind_mph), " mph wind", dir_note, impact))
  }
  if (!is.na(temp_f)) {
    if (temp_f < 42)
      weather_parts <- c(weather_parts, paste0(round(temp_f), "\u00b0F — cold, pitcher-friendly"))
    else if (temp_f > 87)
      weather_parts <- c(weather_parts, paste0(round(temp_f), "\u00b0F — warm, hitter-friendly"))
  }
  if (length(weather_parts) > 0)
    bullets <- c(bullets,
      paste0("<strong>Weather</strong>: ", paste(weather_parts, collapse = " \u00b7 ")))

  # ---- 5. Bullpen unavailability / doubtful ----
  unavail <- bullpen %>%
    dplyr::filter(availability == "unavailable") %>%
    dplyr::arrange(role_sort) %>%
    dplyr::slice_head(n = 3)
  doubtful <- bullpen %>%
    dplyr::filter(availability == "doubtful") %>%
    dplyr::arrange(role_sort) %>%
    dplyr::slice_head(n = 3)

  if (nrow(unavail) > 0 || nrow(doubtful) > 0) {
    parts <- character(0)
    if (nrow(unavail) > 0)
      parts <- c(parts, paste0("Out: ",
        paste(paste0(unavail$player_name, " (", unavail$side, ")"), collapse = ", ")))
    if (nrow(doubtful) > 0)
      parts <- c(parts, paste0("Doubtful: ",
        paste(paste0(doubtful$player_name, " (", doubtful$side, ")"), collapse = ", ")))
    bullets <- c(bullets,
      paste0("<strong>Bullpen watch</strong>: ", paste(parts, collapse = " \u00b7 ")))
  }

  # ---- 6. Matchup & player watchlist ----
  # Pulls lineup_context_splits + starter_splits if available (from 04_matchup_splits.R).
  # Generates up to 3 targeted callouts: batter spotlight, SP regression, power matchup.

  lcs <- if (exists("lineup_context_splits"))
    lineup_context_splits %>% dplyr::filter(game_pk == gpk) else dplyr::tibble()

  streaks <- if (exists("recent_batter_streaks") && nrow(recent_batter_streaks) > 0)
    recent_batter_streaks else dplyr::tibble()

  watch <- character(0)

  # --- 6a. Hot bat / streak callouts ---
  # Hit streak ≥ 5 or OB streak ≥ 7 for any batter in today's lineup (slots 1-9).
  # Also flags last-7 hot bats (.900+ OPS, ≥ 4 games).
  if (nrow(streaks) > 0) {
    streak_players <- lineup %>%
      dplyr::select(side, batting_slot, mlbam_id, player_name) %>%
      dplyr::left_join(streaks, by = "mlbam_id") %>%
      dplyr::filter(!is.na(hit_streak) | !is.na(ob_streak) | !is.na(is_hot))

    for (side_val in c("away", "home")) {
      team_nm <- if (side_val == "away") away_team else home_team

      sp <- streak_players %>%
        dplyr::filter(side == side_val) %>%
        dplyr::arrange(batting_slot)

      # Hit streak ≥ 5
      hit_s <- sp %>% dplyr::filter(!is.na(hit_streak), hit_streak >= 5) %>%
        dplyr::arrange(dplyr::desc(hit_streak)) %>% dplyr::slice_head(n = 2)
      for (i in seq_len(nrow(hit_s))) {
        r <- hit_s[i, ]
        avg_str <- if (!is.na(r$last7_avg))
          paste0(", batting ", sprintf("%.3f", r$last7_avg), " L", r$last7_g) else ""
        watch <- c(watch, paste0(
          "<strong>Hit streak</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          r$hit_streak, "-game hit streak", avg_str
        ))
      }

      # On-base streak ≥ 7 (only if not already covered by hit streak)
      ob_s <- sp %>%
        dplyr::filter(!is.na(ob_streak), ob_streak >= 7,
                      is.na(hit_streak) | hit_streak < 5) %>%
        dplyr::arrange(dplyr::desc(ob_streak)) %>% dplyr::slice_head(n = 1)
      for (i in seq_len(nrow(ob_s))) {
        r <- ob_s[i, ]
        watch <- c(watch, paste0(
          "<strong>On-base streak</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          r$ob_streak, " consecutive games reaching base"
        ))
      }

      # Hot bat last 7 (.900+ OPS, not already called out by streak)
      hot_s <- sp %>%
        dplyr::filter(is_hot == TRUE,
                      is.na(hit_streak) | hit_streak < 5,
                      is.na(ob_streak)  | ob_streak  < 7) %>%
        dplyr::arrange(dplyr::desc(last7_ops)) %>% dplyr::slice_head(n = 1)
      for (i in seq_len(nrow(hot_s))) {
        r <- hot_s[i, ]
        hr_str <- if (!is.na(r$last7_hr) && r$last7_hr >= 2)
          paste0(", ", r$last7_hr, " HR") else ""
        watch <- c(watch, paste0(
          "<strong>Hot bat</strong>: ", r$player_name, " (", team_nm, ") \u2014 ",
          ".OPS ", sprintf("%.3f", r$last7_ops), hr_str,
          " over last ", r$last7_g, " games"
        ))
      }
    }
  }

  # --- 6b. Batter spotlight: best split OPS vs today's SP handedness ---
  # For each side, find the best bat (slots 2-6, ≥15 PA) facing the opposing SP.
  # Threshold: OPS ≥ .850 to be worth calling out.
  for (sp_side in c("away", "home")) {
    bat_side <- if (sp_side == "away") "home" else "away"
    sp_row   <- sps %>% dplyr::filter(side == sp_side)
    bat_team <- if (bat_side == "away") away_team else home_team
    if (nrow(sp_row) == 0 || nrow(lcs) == 0) next

    sp_nm <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")

    best <- lcs %>%
      dplyr::filter(side == bat_side,
                    !is.na(sp_ops), sp_pa >= 15,
                    batting_slot %in% 2:6) %>%
      dplyr::arrange(dplyr::desc(sp_ops)) %>%
      dplyr::slice_head(n = 1)

    if (nrow(best) == 0 || best$sp_ops[1] < 0.850) next

    hr_note <- if (!is.na(best$sp_hr[1]) && best$sp_hr[1] >= 2)
      paste0(", ", best$sp_hr[1], " HR") else ""

    slot_word <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")
    slot_lbl  <- slot_word[min(best$batting_slot[1], 9L)]

    watch <- c(watch, paste0(
      "<strong>Batter to watch</strong>: ",
      best$player_name[1], " (", bat_team, ", batting ", slot_lbl, ") \u2014 ",
      ".OPS ", sprintf("%.3f", best$sp_ops[1]), hr_note,
      " ", best$split_label[1],
      " vs ", sp_nm,
      " (", best$sp_pa[1], " PA)"
    ))
  }

  # --- 6b. SP regression / luck flag ---
  # ERA vs xFIP gap ≥ 1.2 with at least 3 GS — signals meaningful luck divergence.
  for (sp_row in list(away_sp, home_sp)) {
    if (nrow(sp_row) == 0) next
    era  <- .get_num(sp_row, "mlb_era")
    xfip <- .get_num(sp_row, "fg_xFIP")
    gs   <- .get_num(sp_row, "mlb_gs")
    sp_nm <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
    if (is.na(era) || is.na(xfip) || is.na(gs) || gs < 3) next

    gap <- era - xfip
    if (abs(gap) < 1.2) next

    if (gap > 0) {
      watch <- c(watch, paste0(
        "<strong>Regression candidate</strong>: ", sp_nm,
        " ERA ", round(era, 2), " vs xFIP ", round(xfip, 2),
        " \u2014 bad luck through ", round(gs), " GS, numbers should improve"
      ))
    } else {
      watch <- c(watch, paste0(
        "<strong>Overperforming watch</strong>: ", sp_nm,
        " ERA ", round(era, 2), " vs xFIP ", round(xfip, 2),
        " \u2014 outpacing metrics through ", round(gs), " GS"
      ))
    }
  }

  # --- 6c. Power matchup: ISO threat vs HR-vulnerable SP ---
  # Fires when a key bat (ISO ≥ .220, slots 2-6) faces an SP with HR/FB ≥ 14%.
  if (nrow(lcs) > 0) {
    for (sp_side in c("away", "home")) {
      bat_side <- if (sp_side == "away") "home" else "away"
      sp_row   <- sps %>% dplyr::filter(side == sp_side)
      bat_team <- if (bat_side == "away") away_team else home_team
      if (nrow(sp_row) == 0) next

      hr_fb  <- .get_num(sp_row, "fg_HR.FB")
      if (is.na(hr_fb) || hr_fb < 0.14) next
      sp_nm  <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")

      power_bats <- lineup %>%
        dplyr::filter(side == bat_side, batting_slot %in% 2:6) %>%
        dplyr::filter(!is.na(fg_ISO), fg_ISO >= 0.220) %>%
        dplyr::arrange(dplyr::desc(fg_ISO)) %>%
        dplyr::slice_head(n = 2)

      if (nrow(power_bats) == 0) next

      names_str <- paste(
        paste0(power_bats$player_name, " (ISO ", sprintf("%.3f", power_bats$fg_ISO), ")"),
        collapse = ", "
      )
      watch <- c(watch, paste0(
        "<strong>Power matchup</strong>: ", names_str,
        " vs ", sp_nm,
        " \u2014 ", round(hr_fb * 100, 0), "% HR/FB rate allowed"
      ))
    }
  }

  # --- 6d. Platoon mismatch: 5+ batters with the hand advantage ---
  if (nrow(lcs) > 0) {
    for (side_val in c("away", "home")) {
      team_nm   <- if (side_val == "away") away_team else home_team
      opp_side  <- if (side_val == "away") "home" else "away"
      sp_row    <- sps %>% dplyr::filter(side == opp_side)
      if (nrow(sp_row) == 0) next

      sp_nm      <- dplyr::coalesce(sp_row$pitcher_name[1], "SP")
      sp_hand_lbl <- if ("pitch_hand" %in% names(sp_row) && !is.na(sp_row$pitch_hand[1]))
        paste0(sp_row$pitch_hand[1], "HP") else NULL
      if (is.null(sp_hand_lbl)) next

      # Batters who have the platoon edge (vs this hand) and are hitting well
      adv_bats <- lcs %>%
        dplyr::filter(side == side_val, !is.na(sp_ops), sp_pa >= 15,
                      sp_ops >= 0.750) %>%
        nrow()

      if (adv_bats < 5) next

      watch <- c(watch, paste0(
        "<strong>Platoon edge \u2014 ", team_nm, "</strong>: ",
        adv_bats, " batters with \u2265.750 OPS ", lcs$split_label[lcs$side == side_val][1],
        " vs ", sp_nm, " (", sp_hand_lbl, ")"
      ))
    }
  }

  # Append up to 4 watch bullets (streaks are high-value, worth the extra line)
  if (length(watch) > 0)
    bullets <- c(bullets, watch[seq_len(min(4L, length(watch)))])

  if (length(bullets) == 0) return("")

  paste0(
    '<ul style="margin:4px 0 0 0; padding-left:18px; font-size:12px; ',
    'color:#444; line-height:1.75;">',
    paste0("<li>", bullets, "</li>", collapse = ""),
    "</ul>"
  )
}

# ============================================================
# make_game_preview_html(gpk)
# One-paragraph prose summary for the deep dive page header
# ============================================================

make_game_preview_html <- function(gpk) {
  game   <- game_context    %>% dplyr::filter(game_pk == gpk)
  sps    <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context  %>% dplyr::filter(game_pk == gpk)

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"
  venue     <- dplyr::coalesce(game$venue_name[1], "the ballpark")

  # SP descriptor
  .sp_desc <- function(sp_row, sp_nm) {
    if (nrow(sp_row) == 0) return(sp_nm)
    ep   <- .era_plus_val(sp_row)
    war  <- .get_num(sp_row, "fg_WAR")
    fip  <- dplyr::coalesce(.get_num(sp_row, "fg_xFIP"), .get_num(sp_row, "fg_FIP"))
    kpct <- .get_num(sp_row, "fg_K_pct")

    parts <- character(0)
    if (!is.na(ep))   parts <- c(parts, paste0("ERA+ ", round(ep)))
    if (!is.na(fip))  parts <- c(parts, paste0(.fip_label_used(sp_row), " ", round(fip, 2)))
    if (!is.na(kpct)) parts <- c(parts, paste0(.fmt_pct(kpct), " K%"))
    if (!is.na(war))  parts <- c(parts, paste0(round(war, 1), " fWAR"))

    if (length(parts) == 0) return(sp_nm)
    paste0("<strong>", sp_nm, "</strong> (", paste(parts, collapse = ", "), ")")
  }

  away_desc <- .sp_desc(away_sp, away_name)
  home_desc <- .sp_desc(home_sp, home_name)

  # Pitching tone
  away_ep  <- if (nrow(away_sp) > 0) .era_plus_val(away_sp) else NA_real_
  home_ep  <- if (nrow(home_sp) > 0) .era_plus_val(home_sp) else NA_real_
  avg_ep   <- mean(c(away_ep, home_ep), na.rm = TRUE)
  pitch_tone <- dplyr::case_when(
    !is.na(avg_ep) && avg_ep >= 120 ~ "a marquee pitching matchup",
    !is.na(avg_ep) && avg_ep >= 105 ~ "a solid pitching matchup",
    !is.na(avg_ep) && avg_ep < 95   ~ "an offense-friendly environment",
    TRUE ~ "today's matchup"
  )

  # Offense summary
  away_wrc <- .team_wrc_display(lineup %>% dplyr::filter(side == "away"))
  home_wrc <- .team_wrc_display(lineup %>% dplyr::filter(side == "home"))
  off_note <- if (away_wrc == 100 && home_wrc == 100) {
    ""  # no data, skip
  } else {
    stronger <- if (away_wrc >= home_wrc) away_team else home_team
    stronger_wrc <- max(away_wrc, home_wrc)
    weaker   <- if (away_wrc >= home_wrc) home_team else away_team
    weaker_wrc   <- min(away_wrc, home_wrc)
    if (abs(away_wrc - home_wrc) >= 12) {
      paste0(" Offensively, ", stronger, " carries the stronger lineup (avg wRC+ ",
             round(stronger_wrc), " vs ", round(weaker_wrc), " for ", weaker, ").")
    } else {
      paste0(" Both offenses are comparable (", away_team, " avg wRC+ ", round(away_wrc),
             ", ", home_team, " ", round(home_wrc), ").")
    }
  }

  # Weather note
  wind_mph <- .get_num(game, "wind_speed_mph")
  temp_f   <- .get_num(game, "game_temp_f")
  wind_dir <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
    as.character(game$wind_direction[1]) else NA_character_

  weather_note <- ""
  if (!is.na(temp_f) || (!is.na(wind_mph) && wind_mph >= 8)) {
    parts <- character(0)
    if (!is.na(temp_f)) parts <- c(parts, paste0(round(temp_f), "\u00b0F"))
    if (!is.na(wind_mph) && wind_mph >= 8) {
      dir_str <- if (!is.na(wind_dir)) paste0(" from the ", wind_dir) else ""
      parts <- c(parts, paste0(round(wind_mph), " mph wind", dir_str))
    }
    weather_note <- paste0(" Conditions at ", venue, ": ", paste(parts, collapse = ", "), ".")
  }

  paste0(
    '<div style="background:#f8f9fa; border-left:4px solid #1a73e8; ',
    'padding:12px 16px; border-radius:0 6px 6px 0; ',
    'font-size:13px; color:#333; line-height:1.75; margin-bottom:1rem;">',
    away_desc, " heads to ", venue, " to face ", home_desc,
    " in what shapes up as ", pitch_tone, ".",
    off_note,
    weather_note,
    "</div>"
  )
}

# ============================================================
# make_series_overview_html(gpk)
# Series preview section — only rendered for series openers
# ============================================================

make_series_overview_html <- function(gpk) {
  game <- tryCatch(game_context %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
  if (nrow(game) == 0) return("")

  # Only render for series openers
  is_opener <- isTRUE("is_series_opener" %in% names(game) && game$is_series_opener[1] == TRUE)
  if (!is_opener) return("")

  away_team    <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team    <- dplyr::coalesce(game$home_team_name[1], "Home")
  venue        <- dplyr::coalesce(game$venue_name[1], "the ballpark")
  series_len   <- if ("series_length" %in% names(game) && !is.na(game$series_length[1]))
                    as.integer(game$series_length[1]) else NA_integer_
  series_str   <- if (!is.na(series_len)) as.character(series_len) else "?"
  away_team_id <- dplyr::coalesce(game$away_team_id[1], NA_integer_)
  home_team_id <- dplyr::coalesce(game$home_team_id[1], NA_integer_)

  sps    <- tryCatch(starter_matchup %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
  lineup <- tryCatch(lineup_context  %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())

  away_sp <- if (nrow(sps) > 0) sps %>% dplyr::filter(side == "away") else dplyr::tibble()
  home_sp <- if (nrow(sps) > 0) sps %>% dplyr::filter(side == "home") else dplyr::tibble()
  away_lu <- if (nrow(lineup) > 0) lineup %>% dplyr::filter(side == "away") else dplyr::tibble()
  home_lu <- if (nrow(lineup) > 0) lineup %>% dplyr::filter(side == "home") else dplyr::tibble()

  # ---- Helper: get team record from standings ----
  .team_record <- function(team_id) {
    if (!exists("team_standings") || nrow(team_standings) == 0) return(NULL)
    r <- team_standings %>% dplyr::filter(mlbam_team_id == as.integer(team_id))
    if (nrow(r) == 0) return(NULL)
    r[1, ]
  }

  # ---- Helper: team season offense (PA-weighted avg wRC+) ----
  .team_season_offense <- function(team_id) {
    if (!exists("offense_master_season") || nrow(offense_master_season) == 0)
      return(list(wrc = NA_real_, n = 0L))
    abbr <- if (exists("team_ids")) {
      team_ids %>% dplyr::filter(mlbam_team_id == as.integer(team_id)) %>%
        dplyr::pull(team_abbr) %>% dplyr::first()
    } else NA_character_
    if (is.na(abbr) || length(abbr) == 0) return(list(wrc = NA_real_, n = 0L))
    team_rows <- offense_master_season %>%
      dplyr::filter(team_abbr == abbr, !is.na(mlb_pa), mlb_pa >= 15,
                    !is.na(fg_wRC_plus))
    if (nrow(team_rows) == 0) return(list(wrc = NA_real_, n = 0L))
    list(
      wrc = weighted.mean(team_rows$fg_wRC_plus, team_rows$mlb_pa, na.rm = TRUE),
      n   = nrow(team_rows)
    )
  }

  # ---- Helper: team rotation ERA (SPs only, IP-weighted) ----
  .team_season_rotation_era <- function(team_id) {
    if (!exists("pitching_master_season") || nrow(pitching_master_season) == 0)
      return(NA_real_)
    abbr <- if (exists("team_ids")) {
      team_ids %>% dplyr::filter(mlbam_team_id == as.integer(team_id)) %>%
        dplyr::pull(team_abbr) %>% dplyr::first()
    } else NA_character_
    if (is.na(abbr) || length(abbr) == 0) return(NA_real_)
    sp_rows <- pitching_master_season %>%
      dplyr::filter(team_abbr == abbr, !is.na(mlb_gs), mlb_gs >= 1,
                    !is.na(mlb_ip), mlb_ip > 0, !is.na(mlb_era), mlb_era < 20)
    if (nrow(sp_rows) == 0) return(NA_real_)
    weighted.mean(sp_rows$mlb_era, sp_rows$mlb_ip, na.rm = TRUE)
  }

  # ---- Tier label helpers ----
  .record_tier <- function(pct) {
    if (is.na(pct)) return("in action")
    dplyr::case_when(
      pct >= 0.680 ~ "on a torrid pace",
      pct >= 0.580 ~ "playing strong ball",
      pct >= 0.520 ~ "above .500",
      pct >= 0.480 ~ "hovering around .500",
      pct >= 0.400 ~ "struggling",
      TRUE         ~ "mired in a difficult stretch"
    )
  }

  .offense_tier_label <- function(wrc) {
    if (is.na(wrc)) return("offense")
    dplyr::case_when(
      wrc >= 115 ~ "elite offense",
      wrc >= 105 ~ "above-average offense",
      wrc >= 95  ~ "average offense",
      wrc >= 85  ~ "below-average offense",
      TRUE       ~ "struggling offense"
    )
  }

  .era_tier_label <- function(era) {
    if (is.na(era)) return("rotation")
    dplyr::case_when(
      era <= 3.00 ~ "elite rotation",
      era <= 3.50 ~ "strong rotation",
      era <= 4.00 ~ "solid rotation",
      era <= 4.50 ~ "average rotation",
      TRUE        ~ "shaky rotation"
    )
  }

  # ---- Build per-team card content ----
  .build_team_card <- function(team_id, team_nm, sp_row, lu_rows, side_lbl) {
    rec <- .team_record(team_id)

    # Record line
    if (!is.null(rec)) {
      w   <- dplyr::coalesce(rec$wins[1],   NA_integer_)
      l   <- dplyr::coalesce(rec$losses[1], NA_integer_)
      pct <- dplyr::coalesce(rec$pct[1],    NA_real_)
      dr  <- dplyr::coalesce(rec$division_rank[1], NA_integer_)

      pct_str  <- if (!is.na(pct)) sprintf("%.3f", pct) else "—"
      wl_str   <- if (!is.na(w) && !is.na(l)) paste0(w, "-", l, " (", pct_str, ")") else "—"

      # Division rank label — try to derive division name from division_id
      div_id  <- dplyr::coalesce(rec$division_id[1], NA_integer_)
      div_label <- tryCatch({
        div_map <- c(
          `200` = "AL West", `201` = "AL East", `202` = "AL Central",
          `203` = "NL West", `204` = "NL East", `205` = "NL Central"
        )
        if (!is.na(div_id)) div_map[as.character(div_id)] else NA_character_
      }, error = function(e) NA_character_)
      rank_str <- if (!is.na(dr) && !is.na(div_label) && !is.na(div_label))
        paste0(dr, ifelse(dr == 1, "st", ifelse(dr == 2, "nd", ifelse(dr == 3, "rd", "th"))),
               " ", div_label)
      else if (!is.na(dr))
        paste0("Rank ", dr)
      else ""

      record_line <- paste0(team_nm, " \u2014 ", wl_str,
                            if (nchar(rank_str) > 0) paste0(" \u00b7 ", rank_str) else "")

      # Run environment
      gp <- dplyr::coalesce(rec$games_played[1], NA_integer_)
      rs <- dplyr::coalesce(rec$runs_scored[1],  NA_integer_)
      ra <- dplyr::coalesce(rec$runs_allowed[1], NA_integer_)
      rd <- dplyr::coalesce(rec$run_diff[1],     NA_integer_)
      rs_pg  <- if (!is.na(rs) && !is.na(gp) && gp > 0) round(rs / gp, 1) else NA_real_
      ra_pg  <- if (!is.na(ra) && !is.na(gp) && gp > 0) round(ra / gp, 1) else NA_real_
      rd_str <- if (!is.na(rd)) ifelse(rd >= 0, paste0("+", rd), as.character(rd)) else "—"
      run_env_line <- paste0(
        "RS/G: ", if (!is.na(rs_pg)) rs_pg else "\u2014",
        " \u00b7 RA/G: ", if (!is.na(ra_pg)) ra_pg else "\u2014",
        " \u00b7 Diff: ", rd_str
      )
    } else {
      record_line  <- team_nm
      run_env_line <- NULL
    }

    # Offense line
    off      <- .team_season_offense(team_id)
    off_wrc  <- off$wrc
    off_line <- if (!is.na(off_wrc)) {
      paste0("Offense: ", .offense_tier_label(off_wrc), " (avg wRC+ ", round(off_wrc), ")")
    } else "Offense: data pending"

    # Rotation line
    rot_era  <- .team_season_rotation_era(team_id)
    rot_line <- if (!is.na(rot_era)) {
      paste0("Rotation: ", .era_tier_label(rot_era), " (", round(rot_era, 2), " ERA)")
    } else "Rotation: data pending"

    # Recent form — show actual last-7 team OPS when data is available
    form_mult <- tryCatch(.team_form_mult(lu_rows), error = function(e) 1.0)
    form_note <- tryCatch({
      if (exists("recent_batter_streaks") && nrow(recent_batter_streaks) > 0 &&
          nrow(lu_rows) > 0) {
        ids <- lu_rows$mlbam_id[!is.na(lu_rows$mlbam_id)]
        form_rows <- recent_batter_streaks %>%
          dplyr::filter(mlbam_id %in% ids, !is.na(last7_ops), last7_g >= 3)
        if (nrow(form_rows) >= 5) {
          avg_ops <- round(mean(form_rows$last7_ops, na.rm = TRUE), 3)
          n_g     <- round(mean(form_rows$last7_g,  na.rm = TRUE))
          label   <- if (form_mult > 1.02) "Trending up"
                     else if (form_mult < 0.98) "Trending down"
                     else "Steady"
          paste0("Recent form: ", label, " — team avg .OPS ", sprintf("%.3f", avg_ops),
                 " last ", n_g, "G")
        } else {
          if (form_mult > 1.02) "Recent form: trending up (last 7G)"
          else if (form_mult < 0.98) "Recent form: trending down (last 7G)"
          else "Recent form: steady"
        }
      } else "Recent form: insufficient data"
    }, error = function(e) "Recent form: data unavailable")

    # Key bat: top batter in lineup by Steamer wRC+ or current wRC+
    key_bat_line <- tryCatch({
      if (nrow(lu_rows) == 0) return(NULL)
      steamer_ok <- exists("steamer_projections") && is.data.frame(steamer_projections) &&
        nrow(steamer_projections) > 0 && "steamer_wrc_plus" %in% names(steamer_projections)

      best_bat <- NULL
      best_wrc <- NA_real_

      for (i in seq_len(nrow(lu_rows))) {
        pid  <- lu_rows$mlbam_id[i]
        nm_b <- if ("player_name" %in% names(lu_rows)) lu_rows$player_name[i] else NA_character_
        wrc_v <- NA_real_
        if (steamer_ok) {
          st <- steamer_projections[steamer_projections$mlbam_id == pid, , drop = FALSE]
          if (nrow(st) > 0 && !is.na(st$steamer_wrc_plus[1]))
            wrc_v <- as.numeric(st$steamer_wrc_plus[1])
        }
        if (is.na(wrc_v)) {
          if (exists("offense_master_season")) {
            om <- offense_master_season[offense_master_season$mlbam_id == pid, , drop = FALSE]
            if (nrow(om) > 0 && "fg_wRC_plus" %in% names(om) && !is.na(om$fg_wRC_plus[1]))
              wrc_v <- as.numeric(om$fg_wRC_plus[1])
          }
        }
        if (!is.na(wrc_v) && (is.na(best_wrc) || wrc_v > best_wrc)) {
          best_wrc <- wrc_v
          best_bat <- nm_b
        }
      }
      if (!is.null(best_bat) && !is.na(best_wrc))
        paste0("Watch: ", best_bat, " (wRC+ ", round(best_wrc), ")")
      else NULL
    }, error = function(e) NULL)

    # Today's SP
    sp_line <- tryCatch({
      if (nrow(sp_row) == 0) return(NULL)
      sp_nm   <- dplyr::coalesce(sp_row$pitcher_name[1], "TBD")
      ep      <- .era_plus_val(sp_row)
      xfip    <- .get_num(sp_row, "fg_xFIP")
      ep_str  <- if (!is.na(ep))   paste0("ERA+ ", round(ep))   else NULL
      fip_str <- if (!is.na(xfip)) paste0("xFIP ", round(xfip, 2)) else NULL
      stats   <- paste(Filter(Negate(is.null), list(ep_str, fip_str)), collapse = ", ")
      if (nchar(stats) > 0)
        paste0("Starting: ", sp_nm, " (", stats, ")")
      else
        paste0("Starting: ", sp_nm)
    }, error = function(e) NULL)

    # Assemble all lines
    lines <- c(
      if (!is.null(run_env_line)) run_env_line else NULL,
      off_line,
      rot_line,
      form_note,
      key_bat_line,
      sp_line
    )

    list(
      record_line = record_line,
      detail_html = paste(lines, collapse = "<br>"),
      off_wrc     = off_wrc
    )
  }

  away_card <- tryCatch(
    .build_team_card(away_team_id, away_team, away_sp, away_lu, "away"),
    error = function(e) list(record_line = away_team, detail_html = "", off_wrc = NA_real_)
  )
  home_card <- tryCatch(
    .build_team_card(home_team_id, home_team, home_sp, home_lu, "home"),
    error = function(e) list(record_line = home_team, detail_html = "", off_wrc = NA_real_)
  )

  # ---- Series outlook paragraph ----
  away_blep <- tryCatch(if (nrow(away_sp) > 0) .blended_era_plus(away_sp) else NA_real_, error = function(e) NA_real_)
  home_blep <- tryCatch(if (nrow(home_sp) > 0) .blended_era_plus(home_sp) else NA_real_, error = function(e) NA_real_)
  away_sp_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "the away starter") else "the away starter"
  home_sp_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "the home starter") else "the home starter"
  away_off_wrc <- away_card$off_wrc
  home_off_wrc <- home_card$off_wrc

  outlook_text <- tryCatch({
    # Pitching edge sentence
    pitch_sentence <- if (!is.na(away_blep) && !is.na(home_blep)) {
      if (away_blep >= home_blep + 15) {
        away_xfip <- .get_num(away_sp, "fg_xFIP")
        metric_str <- if (!is.na(away_xfip)) paste0(away_sp_name, "'s xFIP advantage (", round(away_xfip, 2), ")")
                      else paste0(away_sp_name, "'s edge (ERA+ ", round(away_blep), ")")
        paste0(away_team, " holds the pitching edge with ", metric_str, ".")
      } else if (home_blep >= away_blep + 15) {
        home_xfip <- .get_num(home_sp, "fg_xFIP")
        metric_str <- if (!is.na(home_xfip)) paste0(home_sp_name, "'s xFIP advantage (", round(home_xfip, 2), ")")
                      else paste0(home_sp_name, "'s edge (ERA+ ", round(home_blep), ")")
        paste0(home_team, " holds the pitching edge with ", metric_str, ".")
      } else {
        "Both starters are evenly matched on the mound."
      }
    } else ""

    # Offense edge sentence
    off_sentence <- if (!is.na(away_off_wrc) && !is.na(home_off_wrc)) {
      if (away_off_wrc >= home_off_wrc + 10) {
        paste0(away_team, "'s lineup projects as the stronger offensive unit (wRC+ ",
               round(away_off_wrc), " vs ", round(home_off_wrc), ").")
      } else if (home_off_wrc >= away_off_wrc + 10) {
        paste0(home_team, "'s lineup projects as the stronger offensive unit (wRC+ ",
               round(home_off_wrc), " vs ", round(away_off_wrc), ").")
      } else {
        paste0("Both offenses are similarly projected (wRC+ ", round(away_off_wrc),
               " vs ", round(home_off_wrc), ").")
      }
    } else ""

    # Closing sentence
    best_ep  <- max(c(away_blep, home_blep), na.rm = TRUE)
    best_sp  <- if (!is.na(away_blep) && !is.na(home_blep) && away_blep >= home_blep) away_sp_name else home_sp_name
    both_off_strong <- !is.na(away_off_wrc) && !is.na(home_off_wrc) &&
                       away_off_wrc >= 108 && home_off_wrc >= 108
    closing <- if (is.finite(best_ep) && best_ep >= 130) {
      paste0("Expect a pitcher's duel if ", best_sp, " is on.")
    } else if (both_off_strong) {
      "This series figures to produce plenty of runs."
    } else {
      "A balanced series where small edges may decide it."
    }

    sentences <- Filter(nchar, c(pitch_sentence, off_sentence, closing))
    paste(sentences, collapse = " ")
  }, error = function(e) "Series outlook unavailable.")

  # ---- HTML output ----
  card_style <- paste0(
    'style="flex:1; min-width:240px; background:white; border-radius:6px; ',
    'padding:12px 14px; border:1px solid #dee2e6;"'
  )

  paste0(
    '<div style="background:#f0f4ff; border:1px solid #c5d4f5; border-radius:8px; ',
    'padding:16px 20px; margin-bottom:1.5rem;">',

    '<h3 style="margin:0 0 4px; color:#1a3c6e; font-size:16px;">',
    '\u26be Series Preview: ', away_team, ' @ ', home_team,
    '</h3>',

    '<p style="margin:0 0 14px; color:#555; font-size:13px; font-style:italic;">',
    series_str, '-game series \u00b7 ', venue, ' \u00b7 Game 1 of ', series_str,
    '</p>',

    '<div style="display:flex; gap:2rem; flex-wrap:wrap; margin-bottom:14px;">',

    # Away card
    '<div ', card_style, '>',
    '<div style="font-weight:700; font-size:14px; color:#2c3e50; margin-bottom:6px;">',
    away_card$record_line,
    '</div>',
    '<div style="font-size:12px; color:#555; line-height:1.9;">',
    away_card$detail_html,
    '</div>',
    '</div>',

    # Home card
    '<div ', card_style, '>',
    '<div style="font-weight:700; font-size:14px; color:#2c3e50; margin-bottom:6px;">',
    home_card$record_line,
    '</div>',
    '<div style="font-size:12px; color:#555; line-height:1.9;">',
    home_card$detail_html,
    '</div>',
    '</div>',

    '</div>',  # end flex row

    '<p style="margin:0; font-size:13px; color:#333; line-height:1.75; ',
    'border-top:1px solid #dee2e6; padding-top:10px;">',
    '<strong>Series outlook:</strong> ', outlook_text,
    '</p>',

    '</div>'
  )
}

# ============================================================
# make_game_narrative_html(gpk)
# Richer multi-paragraph prose for the deep dive page header
# ============================================================

make_game_narrative_html <- function(gpk) {
  game    <- tryCatch(game_context    %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
  sps     <- tryCatch(starter_matchup %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
  lineup  <- tryCatch(lineup_context  %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
  bullpen <- tryCatch(bullpen_grid    %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())

  if (nrow(game) == 0) return("")

  away_sp <- if (nrow(sps) > 0) sps %>% dplyr::filter(side == "away") else dplyr::tibble()
  home_sp <- if (nrow(sps) > 0) sps %>% dplyr::filter(side == "home") else dplyr::tibble()
  away_lu <- if (nrow(lineup) > 0) lineup %>% dplyr::filter(side == "away") else dplyr::tibble()
  home_lu <- if (nrow(lineup) > 0) lineup %>% dplyr::filter(side == "home") else dplyr::tibble()

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"
  venue     <- dplyr::coalesce(game$venue_name[1], "the ballpark")

  # ---- Paragraph 1: Pitching matchup ----
  .sp_desc_full <- function(sp_row, sp_nm) {
    if (nrow(sp_row) == 0) return(paste0("<strong>", sp_nm, "</strong>"))
    ep   <- .era_plus_val(sp_row)
    fip  <- dplyr::coalesce(.get_num(sp_row, "fg_xFIP"), .get_num(sp_row, "fg_FIP"))
    kpct <- .get_num(sp_row, "fg_K_pct")
    war  <- .get_num(sp_row, "fg_WAR")
    parts <- character(0)
    if (!is.na(ep))   parts <- c(parts, paste0("ERA+ ", round(ep)))
    if (!is.na(fip))  parts <- c(parts, paste0(.fip_label_used(sp_row), " ", round(fip, 2)))
    if (!is.na(kpct)) parts <- c(parts, paste0(.fmt_pct(kpct), " K%"))
    if (!is.na(war))  parts <- c(parts, paste0(round(war, 1), " fWAR"))
    if (length(parts) == 0) return(paste0("<strong>", sp_nm, "</strong>"))
    paste0("<strong>", sp_nm, "</strong> (", paste(parts, collapse = ", "), ")")
  }

  away_desc <- .sp_desc_full(away_sp, away_name)
  home_desc <- .sp_desc_full(home_sp, home_name)

  away_ep  <- if (nrow(away_sp) > 0) .era_plus_val(away_sp) else NA_real_
  home_ep  <- if (nrow(home_sp) > 0) .era_plus_val(home_sp) else NA_real_
  avg_ep   <- mean(c(away_ep, home_ep), na.rm = TRUE)

  pitch_tone <- dplyr::case_when(
    !is.na(avg_ep) && avg_ep >= 120 ~ "a marquee pitching matchup",
    !is.na(avg_ep) && avg_ep >= 105 ~ "a solid pitching matchup",
    !is.na(avg_ep) && avg_ep < 95   ~ "an offense-friendly environment",
    TRUE ~ "today's matchup"
  )

  # SP quality sentence — highlight the best arm if notably dominant
  sp_quality_sentence <- tryCatch({
    sentences <- character(0)
    for (sp_info in list(list(sp_row = away_sp, nm = away_name, team = away_team),
                         list(sp_row = home_sp, nm = home_name, team = home_team))) {
      sp_row <- sp_info$sp_row
      if (nrow(sp_row) == 0) next
      ep   <- .era_plus_val(sp_row)
      xfip <- .get_num(sp_row, "fg_xFIP")
      kpct <- .get_num(sp_row, "fg_K_pct")
      gs   <- .get_num(sp_row, "mlb_gs")
      slot <- .sp_rotation_slot(sp_row)
      if (is.na(ep) || ep < 120) next
      parts <- character(0)
      if (!is.na(xfip)) parts <- c(parts, paste0(round(xfip, 2), " xFIP"))
      if (!is.na(kpct) && kpct >= 0.22) parts <- c(parts, paste0(.fmt_pct(kpct), " K rate"))
      if (length(parts) > 0) {
        tier_adj  <- if (ep >= 150) "been dominant" else if (ep >= 130) "been excellent" else "pitched well"
        above_slot <- !is.na(slot) && slot >= 3
        closer_str <- if (above_slot) {
          paste0(". Listed as a #", slot, " starter, he's been pitching well above that this season")
        } else if (!is.na(gs) && gs >= 5) {
          ". He's one of the nastier draws on today's card"
        } else ""
        sentences <- c(sentences, paste0(
          sp_info$nm, " has ", tier_adj, " this season",
          if (length(parts) > 0) paste0(" \u2014 ", paste(parts, collapse = ", ")) else "",
          closer_str, "."
        ))
      }
    }
    if (length(sentences) > 0) paste(sentences, collapse = " ") else NULL
  }, error = function(e) NULL)

  para1 <- {
    away_ace_p1 <- .sp_is_ace_caliber(away_sp)
    home_ace_p1 <- .sp_is_ace_caliber(home_sp)
    both_ace  <- away_ace_p1 && home_ace_p1 && !is.na(away_ep) && away_ep >= 120 && !is.na(home_ep) && home_ep >= 120
    away_dom  <- !is.na(away_ep) && away_ep >= 130
    home_dom  <- !is.na(home_ep) && home_ep >= 130
    away_soft <- !is.na(away_ep) && away_ep < 95
    home_soft <- !is.na(home_ep) && home_ep < 95

    opening <- if (both_ace) {
      paste0("A legitimate ace duel at ", venue, " \u2014 ", away_desc, " facing off against ", home_desc, ".")
    } else if (away_dom && home_soft) {
      paste0(away_desc, " is the pitching headliner today, making the trip to ", venue,
             " against a ", home_team, " offense that'll need to earn their runs.")
    } else if (home_dom && away_soft) {
      paste0(away_team, " visits ", venue, " in a tough draw: ", home_desc,
             " is the kind of arm that can shut down a road offense.")
    } else if (!is.na(avg_ep) && avg_ep < 95) {
      paste0(away_desc, " and ", home_desc, " square off at ", venue,
             " \u2014 both starters carry some vulnerability, so expect the offenses to have their say.")
    } else {
      paste0(away_desc, " heads to ", venue, " to face ", home_desc, " in ", pitch_tone, ".")
    }

    paste0(opening, if (!is.null(sp_quality_sentence)) paste0(" ", sp_quality_sentence) else "")
  }

  # ---- Paragraph 2: Lineup & matchup edge ----
  para2 <- tryCatch({
    away_wrc <- .team_wrc_display(away_lu)
    home_wrc <- .team_wrc_display(home_lu)

    # Offense comparison opener
    off_intro <- if (away_wrc == 100 && home_wrc == 100) {
      ""
    } else if (abs(away_wrc - home_wrc) >= 12) {
      stronger     <- if (away_wrc >= home_wrc) away_team else home_team
      stronger_wrc <- max(away_wrc, home_wrc)
      weaker       <- if (away_wrc >= home_wrc) home_team else away_team
      weaker_wrc   <- min(away_wrc, home_wrc)
      gap_note <- if (abs(away_wrc - home_wrc) >= 25) "a significant edge on paper" else "a meaningful advantage in the lineup"
      paste0(stronger, " brings the bigger offensive threat here \u2014 ", round(stronger_wrc), " wRC+ to ",
             weaker, "'s ", round(weaker_wrc), ", ", gap_note, ".")
    } else {
      paste0("The lineups are fairly matched \u2014 ", away_team, " at ", round(away_wrc), " wRC+, ",
             home_team, " at ", round(home_wrc), ". Execution will matter more than raw talent gap here.")
    }

    # Best split batter
    lcs <- if (exists("lineup_context_splits"))
      tryCatch(lineup_context_splits %>% dplyr::filter(game_pk == gpk), error = function(e) dplyr::tibble())
    else dplyr::tibble()

    split_note <- ""
    if (nrow(lcs) > 0) {
      best_split <- lcs %>%
        dplyr::filter(!is.na(sp_ops), sp_pa >= 25, batting_slot %in% 1:6) %>%
        dplyr::arrange(dplyr::desc(sp_ops)) %>%
        dplyr::slice_head(n = 1)

      if (nrow(best_split) > 0 && best_split$sp_ops[1] >= 0.820) {
        bat_team_lbl <- if (best_split$side[1] == "away") away_team else home_team
        split_note <- paste0(
          " ", best_split$player_name[1], " leads the ", bat_team_lbl,
          " lineup with a ", sprintf("%.3f", best_split$sp_ops[1]), " OPS ",
          best_split$split_label[1], " over ", best_split$sp_pa[1], " PA."
        )
      }

      # Platoon mismatch note
      for (side_val in c("away", "home")) {
        opp_side <- if (side_val == "away") "home" else "away"
        sp_r     <- sps %>% dplyr::filter(side == opp_side)
        if (nrow(sp_r) == 0) next
        adv_n <- lcs %>%
          dplyr::filter(side == side_val, !is.na(sp_ops), sp_pa >= 15, sp_ops >= 0.750) %>%
          nrow()
        if (adv_n >= 5) {
          team_nm  <- if (side_val == "away") away_team else home_team
          sp_nm    <- dplyr::coalesce(sp_r$pitcher_name[1], "the starter")
          split_note <- paste0(split_note, " ", adv_n, " of ", team_nm,
                               "'s batters hold the platoon edge vs ", sp_nm, ".")
          break
        }
      }
    }

    # ---- Matchup insights block ----
    matchup_insights <- tryCatch({
      insights <- character(0)

      for (sp_side in c("away", "home")) {
        bat_side <- if (sp_side == "away") "home" else "away"
        sp_row   <- sps %>% dplyr::filter(side == sp_side)
        bat_lu   <- if (bat_side == "away") away_lu else home_lu
        bat_team <- if (bat_side == "away") away_team else home_team
        if (nrow(sp_row) == 0 || nrow(bat_lu) == 0) next

        sp_name  <- dplyr::coalesce(sp_row$pitcher_name[1], "The starter")

        # Get lineup stats joined from offense_master_season
        lu_stats <- tryCatch({
          if (!exists("offense_master_season") || nrow(offense_master_season) == 0)
            return(dplyr::tibble())
          full_stats <- offense_master_season %>%
            dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
            dplyr::distinct(mlbam_id, .keep_all = TRUE)
          bat_lu %>%
            dplyr::left_join(full_stats, by = "mlbam_id", suffix = c("", "_dup")) %>%
            dplyr::select(-dplyr::ends_with("_dup")) %>%
            dplyr::filter(dplyr::coalesce(mlb_pa, 0L) >= 10)
        }, error = function(e) dplyr::tibble())

        # 1. K pitcher vs K lineup
        tryCatch({
          sp_kpct <- .get_num(sp_row, "fg_K_pct")
          if (!is.na(sp_kpct) && sp_kpct >= 0.24 && nrow(lu_stats) >= 3) {
            lu_kpct_vals <- lu_stats[["fg_K_pct"]]
            if (!is.null(lu_kpct_vals)) {
              lu_kpct <- mean(lu_kpct_vals, na.rm = TRUE)
              if (!is.na(lu_kpct)) {
                if (lu_kpct >= 0.22) {
                  insights <- c(insights, paste0(
                    sp_name, " is a punch-out arm (", .fmt_pct(sp_kpct), "% K) going against a ",
                    bat_team, " lineup that whiffs frequently (", .fmt_pct(lu_kpct),
                    "% K rate) \u2014 this could be a strikeout-heavy afternoon."
                  ))
                } else if (lu_kpct < 0.18) {
                  insights <- c(insights, paste0(
                    sp_name, "'s strikeout stuff (", .fmt_pct(sp_kpct), "% K) faces a contact-oriented ",
                    bat_team, " lineup (", .fmt_pct(lu_kpct),
                    "% K rate) \u2014 a tough test for his put-away ability."
                  ))
                }
              }
            }
          }
        }, error = function(e) invisible(NULL))

        # 2. GB pitcher vs power lineup
        tryCatch({
          sp_gbpct <- dplyr::coalesce(.get_num(sp_row, "fg_GB_pct"), .get_num(sp_row, "fg_GB."))
          if (!is.na(sp_gbpct) && sp_gbpct >= 0.50 && nrow(lu_stats) >= 3) {
            lu_iso_vals <- lu_stats[["fg_ISO"]]
            if (!is.null(lu_iso_vals)) {
              lu_iso <- mean(lu_iso_vals, na.rm = TRUE)
              if (!is.na(lu_iso) && lu_iso >= 0.175) {
                insights <- c(insights, paste0(
                  sp_name, " generates groundballs at a high rate (", .fmt_pct(sp_gbpct), "% GB), ",
                  "but faces a ", bat_team, " lineup with real pop (avg ISO ",
                  sprintf("%.3f", lu_iso), ") \u2014 keeping the ball down will be critical."
                ))
              }
            }
          }
        }, error = function(e) invisible(NULL))

        # 3. Wild SP vs patient lineup
        tryCatch({
          sp_bbpct <- .get_num(sp_row, "fg_BB_pct")
          if (!is.na(sp_bbpct) && sp_bbpct >= 0.10 && nrow(lu_stats) >= 3) {
            lu_bbpct_vals <- lu_stats[["fg_BB_pct"]]
            if (!is.null(lu_bbpct_vals)) {
              lu_bbpct <- mean(lu_bbpct_vals, na.rm = TRUE)
              if (!is.na(lu_bbpct) && lu_bbpct >= 0.095) {
                insights <- c(insights, paste0(
                  sp_name, "'s command has been shaky this season (", .fmt_pct(sp_bbpct), "% BB), ",
                  "and ", bat_team, "'s patient approach (", .fmt_pct(lu_bbpct),
                  "% BB) could inflate pitch counts early."
                ))
              }
            }
          }
        }, error = function(e) invisible(NULL))

        # 4. Fly-ball pitcher in hitter-friendly/wind-out conditions
        tryCatch({
          sp_fbpct <- dplyr::coalesce(.get_num(sp_row, "fg_FB_pct"), .get_num(sp_row, "fg_FB."))
          if (!is.na(sp_fbpct) && sp_fbpct >= 0.38) {
            wind_mult_val <- tryCatch(
              .wind_mult(game$wind_speed_mph[1], game$wind_direction[1]),
              error = function(e) 0
            )
            home_team_id_mi <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
            pf_val_mi <- tryCatch(.park_factor(home_team_id_mi), error = function(e) 1.0)
            if (!is.na(wind_mult_val) && !is.na(pf_val_mi) &&
                (wind_mult_val >= 0.05 || pf_val_mi >= 1.05)) {
              cond_note <- if (!is.na(wind_mult_val) && wind_mult_val >= 0.05) {
                paste0(round(game$wind_speed_mph[1]), " mph wind blowing out")
              } else {
                paste0("a hitter-friendly park (PF ", round(pf_val_mi * 100), ")")
              }
              insights <- c(insights, paste0(
                sp_name, " is a fly-ball pitcher (", .fmt_pct(sp_fbpct), "% FB) \u2014 ",
                cond_note, " could turn routine fly balls into extra bases."
              ))
            }
          }
        }, error = function(e) invisible(NULL))
      }
      insights
    }, error = function(e) character(0))

    insight_text <- if (length(matchup_insights) > 0)
      paste(" ", paste(matchup_insights, collapse = " ")) else ""

    if (nchar(off_intro) == 0 && nchar(split_note) == 0 && nchar(insight_text) == 0) ""
    else paste0(off_intro, split_note, insight_text)
  }, error = function(e) "")

  # ---- Paragraph 3: Bullpen, weather, watch-for ----
  para3 <- tryCatch({
    parts <- character(0)

    # Bullpen note
    if (nrow(bullpen) > 0) {
      unavail <- bullpen %>%
        dplyr::filter(availability == "unavailable") %>%
        dplyr::arrange(role_sort) %>%
        dplyr::slice_head(n = 2)
      if (nrow(unavail) > 0) {
        parts <- c(parts, paste0(
          "Bullpen watch: ", paste(unavail$player_name, collapse = ", "),
          " (", paste(unavail$side, collapse = "/"), ") unavailable."
        ))
      } else {
        away_bp <- bullpen %>% dplyr::filter(side == "away")
        home_bp <- bullpen %>% dplyr::filter(side == "home")
        away_bp_era <- tryCatch(.bullpen_era(away_bp), error = function(e) LEAGUE_AVG_FIP)
        home_bp_era <- tryCatch(.bullpen_era(home_bp), error = function(e) LEAGUE_AVG_FIP)
        if (!is.na(away_bp_era) && !is.na(home_bp_era) && abs(away_bp_era - home_bp_era) >= 0.50) {
          edge_team <- if (away_bp_era < home_bp_era) away_team else home_team
          edge_era  <- min(away_bp_era, home_bp_era)
          parts <- c(parts, paste0(edge_team, " owns the bullpen edge (", round(edge_era, 2), " ERA equivalent)."))
        }
      }
    }

    # Weather + park note
    wind_mph <- .get_num(game, "wind_speed_mph")
    temp_f   <- .get_num(game, "game_temp_f")
    wind_dir <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
      as.character(game$wind_direction[1]) else NA_character_
    home_team_id_pf <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
    pf_val  <- tryCatch(.park_factor(home_team_id_pf), error = function(e) 1.0)

    env_parts <- character(0)
    if (!is.na(temp_f) && (temp_f < 42 || temp_f > 87))
      env_parts <- c(env_parts, paste0(round(temp_f), "\u00b0F"))
    if (!is.na(wind_mph) && wind_mph >= 10) {
      dir_str <- if (!is.na(wind_dir)) paste0(" from the ", wind_dir) else ""
      env_parts <- c(env_parts, paste0(round(wind_mph), " mph wind", dir_str))
    }
    if (pf_val >= 1.05)
      env_parts <- c(env_parts, paste0(venue, " plays as a hitter-friendly park (PF ", round(pf_val, 3), ")"))
    else if (pf_val <= 0.97)
      env_parts <- c(env_parts, paste0(venue, " plays as a pitcher-friendly park (PF ", round(pf_val, 3), ")"))

    if (length(env_parts) > 0)
      parts <- c(parts, paste0("Conditions: ", paste(env_parts, collapse = "; "), "."))

    # Watch-for close: most interesting angle
    # NOTE: no return() inside tryCatch — return() exits the outer function in R
    watch_close <- tryCatch({
      result <- NULL
      # Regression candidate?
      for (sp_r in list(away_sp, home_sp)) {
        if (nrow(sp_r) == 0 || !is.null(result)) next
        era  <- .get_num(sp_r, "mlb_era")
        xfip <- .get_num(sp_r, "fg_xFIP")
        gs   <- .get_num(sp_r, "mlb_gs")
        if (!is.na(era) && !is.na(xfip) && !is.na(gs) && gs >= 3 && (era - xfip) >= 1.2) {
          nm_r   <- dplyr::coalesce(sp_r$pitcher_name[1], "the starter")
          result <- paste0("Watch for ", nm_r, " \u2014 ERA (", round(era, 2),
                           ") is running well above xFIP (", round(xfip, 2),
                           "), suggesting positive regression ahead.")
        }
      }
      # Hot bat from streaks?
      if (is.null(result) &&
          exists("recent_batter_streaks") && nrow(recent_batter_streaks) > 0 &&
          nrow(lineup) > 0) {
        ids <- lineup$mlbam_id[!is.na(lineup$mlbam_id)]
        hot <- recent_batter_streaks %>%
          dplyr::filter(mlbam_id %in% ids, !is.na(hit_streak), hit_streak >= 6) %>%
          dplyr::arrange(dplyr::desc(hit_streak)) %>%
          dplyr::slice_head(n = 1)
        if (nrow(hot) > 0) {
          bat_nm <- dplyr::left_join(hot, lineup %>% dplyr::select(mlbam_id, player_name, side),
                                     by = "mlbam_id") %>%
            dplyr::pull(player_name) %>% dplyr::first()
          team_nm <- dplyr::left_join(hot, lineup %>% dplyr::select(mlbam_id, side),
                                      by = "mlbam_id") %>%
            dplyr::pull(side) %>% dplyr::first()
          team_lbl <- if (!is.na(team_nm) && team_nm == "away") away_team else home_team
          if (!is.na(bat_nm))
            result <- paste0("Watch ", bat_nm, " (", team_lbl, ") \u2014 active ",
                             hot$hit_streak[1], "-game hit streak.")
        }
      }
      # Series context note
      if (is.null(result)) {
        is_opener <- isTRUE("is_series_opener" %in% names(game) &&
                             game$is_series_opener[1] == TRUE)
        if (is_opener) {
          series_len <- if ("series_length" %in% names(game) &&
                            !is.na(game$series_length[1]))
                          as.integer(game$series_length[1]) else NA_integer_
          if (!is.na(series_len))
            result <- paste0("Series opener: tone-setting game 1 of ", series_len,
                             " at ", venue, ".")
        }
      }
      result
    }, error = function(e) NULL)

    if (!is.null(watch_close)) parts <- c(parts, watch_close)

    paste(parts, collapse = " ")
  }, error = function(e) "")

  # ---- Assemble paragraphs ----
  paras <- Filter(function(x) nchar(trimws(x)) > 0, list(para1, para2, para3))

  if (length(paras) == 0) return("")

  paste0(
    '<div style="background:#f8f9fa; border-left:4px solid #1a73e8; ',
    'padding:12px 16px; border-radius:0 6px 6px 0; ',
    'font-size:13px; color:#333; line-height:1.75; margin-bottom:1rem;">',
    paste(
      sapply(paras, function(p) paste0('<p style="margin:0 0 0.6em 0;">', p, "</p>")),
      collapse = "\n"
    ),
    "</div>"
  )
}

# ============================================================
# make_prediction_html(gpk)
# Projection table + key factors for the deep dive
# ============================================================

# Stabilization constant for bullpen ERA regression.
# With ~120 total IP the team bullpen is at 50/50 signal vs noise.
# April bullpens often have fluky 0.00 ERAs over 10-20 IP — must regress heavily.
BP_STAB_IP <- 120

# Internal: IP-weighted bullpen quality for available/fresh arms (excludes LR, injured, unavailable)
# Regresses toward LEAGUE_AVG_FIP based on total IP to prevent early-season flukes.
# Prefers xFIP (most stable for relievers) > ERA-based alternatives.
.bullpen_era <- function(bp_rows) {
  rel <- bp_rows %>%
    dplyr::filter(
      availability %in% c("fresh", "available", "limited", "doubtful"),
      !fg_role %in% c("LR"),
      !is.na(mlb_ip), mlb_ip > 0
    )
  # xFIP > bbref_ERA > mlb_ERA (xFIP normalizes HR rate — most predictive for relievers)
  fip_col <- dplyr::coalesce(
    if ("fg_xFIP"   %in% names(rel)) rel$fg_xFIP   else rep(NA_real_, nrow(rel)),
    if ("bbref_ERA" %in% names(rel)) rel$bbref_ERA  else rep(NA_real_, nrow(rel)),
    rel$mlb_era
  )
  if (length(fip_col) == 0 || all(is.na(fip_col))) return(LEAGUE_AVG_FIP)
  valid <- !is.na(fip_col) & is.finite(fip_col) & fip_col < 15
  if (!any(valid)) return(LEAGUE_AVG_FIP)
  raw_era  <- weighted.mean(fip_col[valid], rel$mlb_ip[valid], na.rm = TRUE)
  total_ip <- sum(rel$mlb_ip[valid], na.rm = TRUE)
  # Regression: (observed × IP + league_avg × STAB) / (IP + STAB)
  # At 40 IP → 75% regression; at 120 IP → 50%; at 240 IP → 33%
  (raw_era * total_ip + LEAGUE_AVG_FIP * BP_STAB_IP) / (total_ip + BP_STAB_IP)
}

# Internal: expected SP IP per start — blends current and prior season when sample is small.
# At 0 GS → 100% prior; at BLEND_GS_FLOOR GS → 100% current; linear between.
.sp_ip_per_gs <- function(sp_row) {
  ip <- .get_num(sp_row, "mlb_ip")
  gs <- .get_num(sp_row, "mlb_gs")
  cur_ipgs <- if (!is.na(ip) && !is.na(gs) && gs > 0) min(ip / gs, 7.5) else NA_real_

  # Blend with prior season when current-season GS count is small
  if (is.na(gs) || gs < BLEND_GS_FLOOR) {
    prior_ip <- .get_num(sp_row, "prior_ip")
    prior_gs <- .get_num(sp_row, "prior_gs")
    prior_ipgs <- if (!is.na(prior_ip) && !is.na(prior_gs) && prior_gs > 0)
      min(prior_ip / prior_gs, 7.5) else NA_real_

    if (!is.na(prior_ipgs)) {
      if (is.na(cur_ipgs) || is.na(gs)) return(prior_ipgs)
      w_cur <- gs / BLEND_GS_FLOOR
      return(w_cur * cur_ipgs + (1 - w_cur) * prior_ipgs)
    }
  }

  dplyr::coalesce(cur_ipgs, 5.5)
}

# Internal: lookup park factor for home team (1.0 = neutral fallback)
.park_factor <- function(home_team_id) {
  if (!exists("park_factors") || nrow(park_factors) == 0) return(1.0)
  row <- park_factors %>%
    dplyr::filter(mlbam_team_id == as.integer(home_team_id))
  if (nrow(row) == 0) return(1.0)
  row$pf_runs[1]
}

# Team recent form multiplier: if a team's lineup shows a meaningful collective
# hot/cold trend over the last 7 games, apply a small adjustment to expected runs.
# Uses recent_batter_streaks (from 03_recent_batting_logs.R) when available.
# Cap: ±8% max adjustment. Only applies when ≥ 5 batters have recent data.
# At season start (no data): returns 1.0 (no adjustment).
.team_form_mult <- function(lineup_rows) {
  if (!exists("recent_batter_streaks") || nrow(recent_batter_streaks) == 0) return(1.0)

  ids <- lineup_rows$mlbam_id[!is.na(lineup_rows$mlbam_id)]
  if (length(ids) == 0) return(1.0)

  form <- recent_batter_streaks %>%
    dplyr::filter(mlbam_id %in% ids, !is.na(last7_ops), last7_g >= 3)

  if (nrow(form) < 5) return(1.0)

  # Compute team average last-7 OPS vs a neutral baseline (.750 = league avg OPS)
  LEAGUE_AVG_OPS <- 0.750
  team_l7_ops    <- mean(form$last7_ops, na.rm = TRUE)
  raw_mult       <- team_l7_ops / LEAGUE_AVG_OPS

  # Regress adjustment toward 1.0: weight is proportional to bats with data
  # Full coverage (9 bats) = 30% adjustment, partial = less
  coverage_weight <- min(nrow(form) / 9, 1.0) * 0.30
  mult <- 1.0 + (raw_mult - 1.0) * coverage_weight

  # Hard cap: ±8%
  max(0.92, min(1.08, mult))
}

# Internal: wind run multiplier
# Applies to both teams (affects run environment symmetrically for most parks)
.wind_mult <- function(wind_speed, wind_dir) {
  if (is.na(wind_speed) || wind_speed < 8) return(1.0)
  dir <- tolower(trimws(dplyr::coalesce(wind_dir, "")))
  base_effect <- (min(wind_speed, 25) - 7) / 18  # 0 at 7mph, 1.0 at 25mph
  mult <- if (grepl("out", dir) && !grepl("cloudy|overcast", dir)) {
    1.0 + 0.10 * base_effect   # max +10% at 25mph
  } else if (grepl("^in$|^in ", dir) || grepl("in$", dir)) {
    1.0 - 0.10 * base_effect   # max -10% at 25mph
  } else {
    1.0 + 0.02 * base_effect   # crosswind/unknown: tiny effect
  }
  max(0.90, min(1.10, mult))
}

# Internal: team defense run-suppression factor
# Returns multiplier applied to OPPONENT expected runs.
# Good defense -> < 1.0 (fewer runs allowed); bad defense -> > 1.0.
.team_defense_factor <- function(gpk, fielding_side) {
  if (!exists("defense_master_season") || nrow(defense_master_season) == 0) return(1.0)
  if (!exists("lineup_context") || nrow(lineup_context) == 0) return(1.0)

  fielding_players <- lineup_context %>%
    dplyr::filter(game_pk == gpk, side == fielding_side) %>%
    dplyr::pull(mlbam_id)
  fielding_players <- unique(as.integer(fielding_players[!is.na(fielding_players)]))
  if (length(fielding_players) < 4) return(1.0)

  def_rows <- defense_master_season %>%
    dplyr::filter(mlbam_id %in% fielding_players) %>%
    dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(
      if ("mlb_inn" %in% names(.)) .data$mlb_inn else 0,
      0
    ))) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE)

  # Prefer OAA > DRS > Defense (most park-neutral first)
  metric_col <- intersect(c("sc_oaa", "fg_DRS", "fg_Defense"), names(def_rows))[1]
  if (is.na(metric_col)) return(1.0)

  vals <- suppressWarnings(as.numeric(def_rows[[metric_col]]))
  valid <- is.finite(vals)
  if (sum(valid) < 4) return(1.0)

  team_metric <- sum(vals[valid], na.rm = TRUE)
  n_found     <- sum(valid)

  # Scale to full 8-man lineup equivalent, then to runs/game
  # Each OAA/DRS unit approx 0.75 runs saved over the full season (162 games)
  full_team_metric <- team_metric * (8.0 / n_found)
  runs_per_game    <- full_team_metric * 0.75 / 162

  # Raw multiplier (on opponent expected runs): better defense -> lower multiplier
  raw_mult <- 1.0 - (runs_per_game / LEAGUE_AVG_RUNS)

  # Regress heavily toward 1.0 (defense metrics are noisy, esp. early season)
  # Only apply 30% of signal
  mult <- 1.0 + (raw_mult - 1.0) * 0.30
  max(0.95, min(1.05, mult))
}

# ============================================================
# make_lineup_projection_html(gpk)
# Per-player Steamer projection vs YTD stats for both lineups
# ============================================================

make_lineup_projection_html <- function(gpk) {
  game   <- game_context   %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context %>% dplyr::filter(game_pk == gpk)

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")

  steamer_ok <- exists("steamer_projections") &&
    is.data.frame(steamer_projections) && nrow(steamer_projections) > 0

  source_note <- if (steamer_ok) "(Steamer)" else "(*=stabilized regression)"

  # wRC+ color coding helper
  .wrc_color <- function(v) {
    if (is.na(v) || !is.finite(v)) return("")
    bg <- if      (v >= 130) "#27ae60"
          else if (v >= 115) "#a9dfbf"
          else if (v >= 100) "#ffffff"
          else if (v >=  85) "#fef9c3"
          else               "#fde8e8"
    fg <- if (v >= 130) "white" else "#333"
    paste0('background:', bg, '; color:', fg, '; font-weight:bold; border-radius:3px; padding:1px 5px;')
  }

  # Build one team's table HTML
  .team_table <- function(side, team_label) {
    rows <- lineup %>%
      dplyr::filter(side == !!side) %>%
      dplyr::arrange(batting_slot)

    if (nrow(rows) == 0) return(paste0('<p style="color:#888;font-style:italic;">No lineup data for ', team_label, '.</p>'))

    # Join Steamer projections
    if (steamer_ok) {
      rows <- rows %>%
        dplyr::left_join(
          steamer_projections %>%
            dplyr::select(mlbam_id,
                          dplyr::any_of(c("steamer_wrc_plus", "steamer_pa",
                                          "steamer_hr", "steamer_avg",
                                          "steamer_obp", "steamer_slg"))),
          by = "mlbam_id"
        )
    } else {
      rows$steamer_wrc_plus <- NA_real_
      rows$steamer_pa       <- NA_integer_
      rows$steamer_hr       <- NA_real_
      rows$steamer_avg      <- NA_real_
      rows$steamer_obp      <- NA_real_
      rows$steamer_slg      <- NA_real_
    }

    # Pull YTD stats from offense_master_season when available
    if (exists("offense_master_season") && nrow(offense_master_season) > 0) {
      ytd <- offense_master_season %>%
        dplyr::select(mlbam_id,
                      dplyr::any_of(c("fg_wRC_plus", "mlb_pa", "mlb_hr",
                                      "mlb_avg", "mlb_obp", "mlb_slg"))) %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE)
      rows <- rows %>% dplyr::left_join(ytd, by = "mlbam_id", suffix = c("", ".ytd"))
    }

    # Determine YTD wRC+ column
    ytd_wrc_col <- intersect(c("fg_wRC_plus"), names(rows))[1]
    ytd_pa_col  <- intersect(c("mlb_pa"), names(rows))[1]
    ytd_hr_col  <- intersect(c("mlb_hr"), names(rows))[1]
    ytd_avg_col <- intersect(c("mlb_avg"), names(rows))[1]
    ytd_obp_col <- intersect(c("mlb_obp"), names(rows))[1]
    ytd_slg_col <- intersect(c("mlb_slg"), names(rows))[1]

    header_html <- paste0(
      '<table style="border-collapse:collapse; font-size:12px; width:100%; min-width:560px;">',
      '<thead><tr style="background:#f1f3f4; font-size:11px; color:#555;">',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">#</th>',
      '<th style="padding:5px 8px; text-align:left; border-bottom:2px solid #ddd;">Name</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">Pos</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd; border-left:2px solid #1a73e8;">',
        'Steamer wRC+ ', source_note, '</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">YTD wRC+</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">AVG</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">OBP</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">SLG</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">HR Proj</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">HR YTD</th>',
      '<th style="padding:5px 8px; text-align:center; border-bottom:2px solid #ddd;">PA</th>',
      '</tr></thead><tbody>'
    )

    row_html <- vapply(seq_len(nrow(rows)), function(j) {
      r <- rows[j, ]
      slot       <- dplyr::coalesce(r$batting_slot, j)
      name       <- dplyr::coalesce(r$player_name, "Unknown")
      pos        <- dplyr::coalesce(r$fg_position, "—")

      s_wrc <- if ("steamer_wrc_plus" %in% names(r) && !is.na(r$steamer_wrc_plus))
        round(r$steamer_wrc_plus) else NA_integer_
      y_wrc <- if (!is.na(ytd_wrc_col) && ytd_wrc_col %in% names(r) && !is.na(r[[ytd_wrc_col]]))
        round(r[[ytd_wrc_col]]) else NA_integer_
      ytd_pa <- if (!is.na(ytd_pa_col) && ytd_pa_col %in% names(r) && !is.na(r[[ytd_pa_col]]))
        as.integer(r[[ytd_pa_col]]) else NA_integer_
      ytd_hr <- if (!is.na(ytd_hr_col) && ytd_hr_col %in% names(r) && !is.na(r[[ytd_hr_col]]))
        as.integer(r[[ytd_hr_col]]) else NA_integer_
      s_hr   <- if ("steamer_hr" %in% names(r) && !is.na(r$steamer_hr))
        round(r$steamer_hr, 0) else NA_integer_

      # Prefer Steamer avg/obp/slg, fall back to YTD
      s_avg <- if ("steamer_avg" %in% names(r) && !is.na(r$steamer_avg)) r$steamer_avg else
               if (!is.na(ytd_avg_col) && ytd_avg_col %in% names(r)) r[[ytd_avg_col]] else NA_real_
      s_obp <- if ("steamer_obp" %in% names(r) && !is.na(r$steamer_obp)) r$steamer_obp else
               if (!is.na(ytd_obp_col) && ytd_obp_col %in% names(r)) r[[ytd_obp_col]] else NA_real_
      s_slg <- if ("steamer_slg" %in% names(r) && !is.na(r$steamer_slg)) r$steamer_slg else
               if (!is.na(ytd_slg_col) && ytd_slg_col %in% names(r)) r[[ytd_slg_col]] else NA_real_

      wrc_style <- .wrc_color(s_wrc)
      row_bg    <- if (j %% 2 == 0) "background:#fafafa;" else ""

      paste0(
        '<tr style="', row_bg, '">',
        '<td style="padding:4px 8px; text-align:center; color:#888;">', slot, '</td>',
        '<td style="padding:4px 8px; font-weight:500;">', name, '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">', pos, '</td>',
        '<td style="padding:4px 8px; text-align:center; border-left:2px solid #e8f0fe;">',
          if (!is.na(s_wrc)) paste0('<span style="', wrc_style, '">', s_wrc, '</span>') else '<span style="color:#bbb;">—</span>',
        '</td>',
        '<td style="padding:4px 8px; text-align:center;">',
          if (!is.na(y_wrc)) {
            ys <- .wrc_color(y_wrc)
            paste0('<span style="', ys, '">', y_wrc, '</span>')
          } else '<span style="color:#bbb;">—</span>',
        '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(s_avg) && is.finite(s_avg)) sprintf("%.3f", s_avg) else "—", '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(s_obp) && is.finite(s_obp)) sprintf("%.3f", s_obp) else "—", '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(s_slg) && is.finite(s_slg)) sprintf("%.3f", s_slg) else "—", '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(s_hr)) s_hr else "—", '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(ytd_hr)) ytd_hr else "—", '</td>',
        '<td style="padding:4px 8px; text-align:center; color:#555;">',
          if (!is.na(ytd_pa)) ytd_pa else "—", '</td>',
        '</tr>'
      )
    }, character(1))

    paste0(
      '<div style="overflow-x:auto; margin-bottom:1.5rem;">',
      '<p style="font-size:13px; font-weight:600; margin-bottom:4px; color:#1a3c6e;">',
        team_label, '</p>',
      header_html,
      paste0(row_html, collapse = ""),
      '</tbody></table></div>'
    )
  }

  away_html <- tryCatch(.team_table("away", away_team), error = function(e)
    paste0('<p style="color:#c00;">Error building ', away_team, ' table: ', e$message, '</p>'))
  home_html <- tryCatch(.team_table("home", home_team), error = function(e)
    paste0('<p style="color:#c00;">Error building ', home_team, ' table: ', e$message, '</p>'))

  paste0(
    '<p style="font-size:12px; color:#666; margin-bottom:0.8rem;">',
    'wRC+ color scale: ',
    '<span style="background:#27ae60;color:white;padding:1px 6px;border-radius:3px;font-size:11px;">130+</span> ',
    '<span style="background:#a9dfbf;color:#333;padding:1px 6px;border-radius:3px;font-size:11px;">115\u2013129</span> ',
    '<span style="background:#fff;border:1px solid #ddd;padding:1px 6px;border-radius:3px;font-size:11px;">100\u2013114</span> ',
    '<span style="background:#fef9c3;color:#333;padding:1px 6px;border-radius:3px;font-size:11px;">85\u201399</span> ',
    '<span style="background:#fde8e8;color:#333;padding:1px 6px;border-radius:3px;font-size:11px;">&lt;85</span>',
    '</p>',
    away_html,
    home_html
  )
}

# make_prediction_data(gpk)
# Returns the raw numeric projection used by both make_prediction_html
# and mlb_print.qmd — single source of truth for the model.
make_prediction_data <- function(gpk) {
  game   <- game_context    %>% dplyr::filter(game_pk == gpk)
  sps    <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context  %>% dplyr::filter(game_pk == gpk)
  bullpen <- if (exists("bullpen_grid")) bullpen_grid %>% dplyr::filter(game_pk == gpk) else
             dplyr::tibble()

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")

  away_sp_fip  <- if (nrow(away_sp) > 0) .best_fip_val(away_sp) else LEAGUE_AVG_FIP
  home_sp_fip  <- if (nrow(home_sp) > 0) .best_fip_val(home_sp) else LEAGUE_AVG_FIP

  away_sp_ipgs <- if (nrow(away_sp) > 0) .sp_ip_per_gs(away_sp) else 5.5
  home_sp_ipgs <- if (nrow(home_sp) > 0) .sp_ip_per_gs(home_sp) else 5.5
  away_sp_frac <- min(away_sp_ipgs / 9, 0.85)
  home_sp_frac <- min(home_sp_ipgs / 9, 0.85)

  away_bp_era  <- if (nrow(bullpen) > 0) .bullpen_era(bullpen %>% dplyr::filter(side == "away")) else LEAGUE_AVG_FIP
  home_bp_era  <- if (nrow(bullpen) > 0) .bullpen_era(bullpen %>% dplyr::filter(side == "home")) else LEAGUE_AVG_FIP

  away_blended <- away_sp_frac * away_sp_fip + (1 - away_sp_frac) * away_bp_era
  home_blended <- home_sp_frac * home_sp_fip + (1 - home_sp_frac) * home_bp_era

  away_wrc <- tryCatch(.team_wrc(lineup %>% dplyr::filter(side == "away")), error = function(e) 100)
  home_wrc <- tryCatch(.team_wrc(lineup %>% dplyr::filter(side == "home")), error = function(e) 100)

  pf            <- .park_factor(dplyr::coalesce(game$home_team_id[1], NA_integer_))
  temp_f        <- .get_num(game, "game_temp_f")
  weather_mult  <- if (!is.na(temp_f)) dplyr::case_when(
    temp_f < 40 ~ 0.93, temp_f < 50 ~ 0.96, temp_f < 60 ~ 0.98,
    temp_f > 95 ~ 1.05, temp_f > 85 ~ 1.03, temp_f > 75 ~ 1.01, TRUE ~ 1.00
  ) else 1.0
  wind_mult_val <- .wind_mult(.get_num(game, "wind_speed_mph"),
                              if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
                                as.character(game$wind_direction[1]) else NA_character_)

  away_def_mult <- .team_defense_factor(gpk, "home")
  home_def_mult <- .team_defense_factor(gpk, "away")
  away_form     <- .team_form_mult(lineup %>% dplyr::filter(side == "away"))
  home_form     <- .team_form_mult(lineup %>% dplyr::filter(side == "home"))

  away_exp_r <- LEAGUE_AVG_RUNS * (away_wrc / 100) * pf * weather_mult * wind_mult_val *
                (home_blended / LEAGUE_AVG_FIP) * away_form * away_def_mult
  home_exp_r <- LEAGUE_AVG_RUNS * (home_wrc / 100) * pf * weather_mult * wind_mult_val *
                (away_blended / LEAGUE_AVG_FIP) * home_form * home_def_mult + HOME_FIELD_BONUS
  away_exp_r <- max(1.5, min(away_exp_r, 9.5))
  home_exp_r <- max(1.5, min(home_exp_r, 9.5))

  p_home   <- .poisson_win_prob(home_exp_r, away_exp_r)
  p_away   <- 1 - p_home

  list(
    away_runs     = away_exp_r,
    home_runs     = home_exp_r,
    away_win_pct  = p_away,
    home_win_pct  = p_home,
    away_team     = away_team,
    home_team     = home_team
  )
}

make_prediction_html <- function(gpk) {
  game   <- game_context    %>% dplyr::filter(game_pk == gpk)
  sps    <- starter_matchup %>% dplyr::filter(game_pk == gpk)
  lineup <- lineup_context  %>% dplyr::filter(game_pk == gpk)
  bullpen <- if (exists("bullpen_grid")) bullpen_grid %>% dplyr::filter(game_pk == gpk) else
             dplyr::tibble()

  away_sp <- sps %>% dplyr::filter(side == "away")
  home_sp <- sps %>% dplyr::filter(side == "home")

  away_team <- dplyr::coalesce(game$away_team_name[1], "Away")
  home_team <- dplyr::coalesce(game$home_team_name[1], "Home")
  away_name <- if (nrow(away_sp) > 0) dplyr::coalesce(away_sp$pitcher_name[1], "TBD") else "TBD"
  home_name <- if (nrow(home_sp) > 0) dplyr::coalesce(home_sp$pitcher_name[1], "TBD") else "TBD"

  # --- SP quality (xFIP → SIERA → FIP → ERA → Lg avg) ---
  away_sp_fip       <- if (nrow(away_sp) > 0) .best_fip_val(away_sp) else LEAGUE_AVG_FIP
  home_sp_fip       <- if (nrow(home_sp) > 0) .best_fip_val(home_sp) else LEAGUE_AVG_FIP
  away_fip_label    <- if (nrow(away_sp) > 0) .fip_label_used(away_sp) else "Lg Avg"
  home_fip_label    <- if (nrow(home_sp) > 0) .fip_label_used(home_sp) else "Lg Avg"

  # --- SP IP fraction ---
  away_sp_ipgs  <- if (nrow(away_sp) > 0) .sp_ip_per_gs(away_sp) else 5.5
  home_sp_ipgs  <- if (nrow(home_sp) > 0) .sp_ip_per_gs(home_sp) else 5.5
  away_sp_frac  <- min(away_sp_ipgs / 9, 0.85)
  home_sp_frac  <- min(home_sp_ipgs / 9, 0.85)

  # --- Bullpen quality (IP-weighted ERA of available/fresh arms, excl LR) ---
  away_bp_era  <- if (nrow(bullpen) > 0)
    .bullpen_era(bullpen %>% dplyr::filter(side == "away")) else LEAGUE_AVG_FIP
  home_bp_era  <- if (nrow(bullpen) > 0)
    .bullpen_era(bullpen %>% dplyr::filter(side == "home")) else LEAGUE_AVG_FIP

  # --- Blended pitcher quality: SP covers sp_frac, bullpen covers rest ---
  away_blended_fip <- away_sp_frac * away_sp_fip + (1 - away_sp_frac) * away_bp_era
  home_blended_fip <- home_sp_frac * home_sp_fip + (1 - home_sp_frac) * home_bp_era

  # --- Lineup offense ---
  away_lineup_rows <- lineup %>% dplyr::filter(side == "away")
  home_lineup_rows <- lineup %>% dplyr::filter(side == "home")
  away_wrc <- .team_wrc(away_lineup_rows)          # blended — used for run model
  home_wrc <- .team_wrc(home_lineup_rows)
  away_wrc_disp <- .team_wrc_display(away_lineup_rows)  # actual — shown in table/text
  home_wrc_disp <- .team_wrc_display(home_lineup_rows)

  # (flag logic kept for compatibility but no longer shown)
  steamer_ok <- exists("steamer_projections") &&
    is.data.frame(steamer_projections) && nrow(steamer_projections) > 0

  .wrc_flag_label <- function(lr) {
    if (steamer_ok) {
      ids_in_steamer <- sum(lr$mlbam_id %in% steamer_projections$mlbam_id, na.rm = TRUE)
      if (ids_in_steamer >= 5) return(NULL)
      return("marcel")
    }
    "marcel"  # no Steamer at all
  }
  away_wrc_note <- .wrc_flag_label(away_lineup_rows)
  home_wrc_note <- .wrc_flag_label(home_lineup_rows)
  away_wrc_flag <- !is.null(away_wrc_note)
  home_wrc_flag <- !is.null(home_wrc_note)

  # Recent form multiplier (regressed toward 1.0 — small adjustment, not override)
  away_form_mult <- .team_form_mult(lineup %>% dplyr::filter(side == "away"))
  home_form_mult <- .team_form_mult(lineup %>% dplyr::filter(side == "home"))

  # --- Park factor (home park affects both offenses equally) ---
  home_team_id <- dplyr::coalesce(game$home_team_id[1], NA_integer_)
  pf           <- .park_factor(home_team_id)
  pf_note      <- if (pf != 1.0) {
    venue <- dplyr::coalesce(game$venue_name[1], "this park")
    paste0(venue, " park factor = ", round(pf, 3),
           " (", if (pf > 1) "+" else "", round((pf - 1) * 100, 1), "% run environment)")
  } else NULL

  # --- Temperature adjustment (affects both teams equally — same environment) ---
  # Empirical run suppression/boost by temperature (calibrated to 2022-2024 data)
  temp_f       <- .get_num(game, "game_temp_f")
  weather_mult <- if (!is.na(temp_f)) {
    dplyr::case_when(
      temp_f < 40 ~ 0.93,  # very cold: -7%
      temp_f < 50 ~ 0.96,  # cold:      -4%
      temp_f < 60 ~ 0.98,  # cool:      -2%
      temp_f > 95 ~ 1.05,  # very hot:  +5%
      temp_f > 85 ~ 1.03,  # hot:       +3%
      temp_f > 75 ~ 1.01,  # warm:      +1%
      TRUE        ~ 1.00
    )
  } else 1.0
  weather_note_factor <- if (!is.na(temp_f) && weather_mult != 1.0) {
    paste0(round(temp_f), "\u00b0F",
           " (", if (weather_mult > 1) "+" else "",
           round((weather_mult - 1) * 100), "% run environment)")
  } else NULL

  # --- Wind adjustment ---
  wind_mph  <- .get_num(game, "wind_speed_mph")
  wind_dir  <- if ("wind_direction" %in% names(game) && !is.na(game$wind_direction[1]))
    as.character(game$wind_direction[1]) else NA_character_
  wind_mult_val <- .wind_mult(wind_mph, wind_dir)
  wind_note_factor <- if (!is.na(wind_mph) && wind_mult_val != 1.0) {
    paste0(round(wind_mph), " mph (", dplyr::coalesce(wind_dir, "unknown"), ")",
           " (", if (wind_mult_val > 1) "+" else "",
           round((wind_mult_val - 1) * 100, 1), "% run environment)")
  } else NULL

  # --- Defense adjustments ---
  away_def_mult <- .team_defense_factor(gpk, "home")  # home team fields vs away batters
  home_def_mult <- .team_defense_factor(gpk, "away")  # away team fields vs home batters

  # --- Expected runs ---
  # Formula: lg_avg x (wRC+/100) x park_factor x temp_mult x wind_mult x (opp_blended_FIP / lg_avg_FIP)
  #          x recent_form_adj x defense_factor
  away_exp_r <- LEAGUE_AVG_RUNS * (away_wrc / 100) * pf * weather_mult * wind_mult_val *
                (home_blended_fip / LEAGUE_AVG_FIP) * away_form_mult * away_def_mult
  home_exp_r <- LEAGUE_AVG_RUNS * (home_wrc / 100) * pf * weather_mult * wind_mult_val *
                (away_blended_fip / LEAGUE_AVG_FIP) * home_form_mult * home_def_mult +
                HOME_FIELD_BONUS
  away_exp_r <- max(1.5, min(away_exp_r, 9.5))
  home_exp_r <- max(1.5, min(home_exp_r, 9.5))
  total_exp  <- away_exp_r + home_exp_r

  # --- Win probability ---
  p_home   <- .poisson_win_prob(home_exp_r, away_exp_r)
  p_away   <- 1 - p_home
  fav_team <- if (p_home >= p_away) home_team else away_team
  fav_pct  <- max(p_home, p_away)

  # --- Factor list ---
  factors <- character(0)

  away_fip_d <- round((away_blended_fip - LEAGUE_AVG_FIP) / LEAGUE_AVG_FIP * 100)
  home_fip_d <- round((home_blended_fip - LEAGUE_AVG_FIP) / LEAGUE_AVG_FIP * 100)
  bp_frac_away <- 1 - away_sp_frac
  bp_frac_home <- 1 - home_sp_frac

  bp_label <- if (any(c("fg_xFIP") %in% names(bullpen))) "bullpen xFIP" else "bullpen ERA"
  factors <- c(factors,
    paste0(away_name, " ", away_fip_label, " = ", round(away_sp_fip, 2),
           " \u00b7 ", bp_label, " = ", round(away_bp_era, 2),
           " \u2192 blended = ", round(away_blended_fip, 2),
           " (", round(away_sp_frac * 100), "% SP / ",
           round(bp_frac_away * 100), "% BP innings)"),
    paste0(home_name, " ", home_fip_label, " = ", round(home_sp_fip, 2),
           " \u00b7 ", bp_label, " = ", round(home_bp_era, 2),
           " \u2192 blended = ", round(home_blended_fip, 2),
           " (", round(home_sp_frac * 100), "% SP / ",
           round(bp_frac_home * 100), "% BP innings)"),
    paste0(away_team, " lineup avg wRC+ = ", round(away_wrc),
           if (away_wrc_flag) " <em>(stabilized regression + age curve \u2014 Steamer not available)</em>" else
             if (steamer_ok) " <em>(Steamer · hand-adjusted · slot-weighted)</em>" else ""),
    paste0(home_team, " lineup avg wRC+ = ", round(home_wrc),
           if (home_wrc_flag) " <em>(stabilized regression + age curve \u2014 Steamer not available)</em>" else
             if (steamer_ok) " <em>(Steamer · hand-adjusted · slot-weighted)</em>" else "")
  )
  if (!is.null(pf_note))             factors <- c(factors, pf_note)
  if (!is.null(weather_note_factor)) factors <- c(factors, paste0("Temperature: ", weather_note_factor))
  if (!is.null(wind_note_factor))    factors <- c(factors, paste0("Wind: ", wind_note_factor))
  if (away_def_mult != 1.0) factors <- c(factors,
    paste0(home_team, " defense adj: \u00d7", round(away_def_mult, 3),
           " on ", away_team, " runs (OAA/DRS based, 30% signal weight)"))
  if (home_def_mult != 1.0) factors <- c(factors,
    paste0(away_team, " defense adj: \u00d7", round(home_def_mult, 3),
           " on ", home_team, " runs (OAA/DRS based, 30% signal weight)"))
  away_form_note <- if (away_form_mult != 1.0)
    paste0(away_team, " recent form adj: \u00d7", round(away_form_mult, 3),
           " (L7 team OPS data)")
  else NULL
  home_form_note <- if (home_form_mult != 1.0)
    paste0(home_team, " recent form adj: \u00d7", round(home_form_mult, 3),
           " (L7 team OPS data)")
  else NULL

  factors <- c(factors,
    paste0("Home field residual: +", HOME_FIELD_BONUS,
           " R/game (last-at-bat/crowd; empirical run diff \u22480 over 2022\u20132024)")
  )
  if (!is.null(away_form_note)) factors <- c(factors, away_form_note)
  if (!is.null(home_form_note)) factors <- c(factors, home_form_note)

  # --- HTML rows helper ---
  tr <- function(label, away_val, home_val, style = "") {
    paste0(
      '<tr style="', style, '">',
      '<td style="padding:6px 14px; border-bottom:1px solid #eee;">', label, '</td>',
      '<td style="padding:6px 16px; text-align:center; border-bottom:1px solid #eee;">', away_val, '</td>',
      '<td style="padding:6px 16px; text-align:center; border-bottom:1px solid #eee;">', home_val, '</td>',
      '</tr>'
    )
  }

  away_win_bg <- if (p_away > p_home) "background:#e8f5e9;" else ""
  home_win_bg <- if (p_home > p_away) "background:#e8f5e9;" else ""

  # Blended FIP label shows SP quality label / bullpen
  away_blend_label <- paste0("Blended (", away_fip_label, " + BP)")
  home_blend_label <- paste0("Blended (", home_fip_label, " + BP)")
  blend_label      <- paste0("Pitcher Quality (", away_fip_label, "+BP / ", home_fip_label, "+BP)")

  pf_display <- if (pf != 1.0) paste0("Park Factor (", round(pf, 3), "×)") else NULL

  table_html <- paste0(
    '<table style="border-collapse:collapse; font-size:13px; margin-bottom:0.8rem;">',
    '<thead><tr style="background:#f1f3f4;">',
    '<th style="padding:8px 14px; text-align:left; border-bottom:2px solid #ddd; min-width:200px;"></th>',
    '<th style="padding:8px 16px; text-align:center; border-bottom:2px solid #ddd; min-width:140px;">',
    away_team, ' (Away)</th>',
    '<th style="padding:8px 16px; text-align:center; border-bottom:2px solid #ddd; min-width:140px;">',
    home_team, ' (Home)</th>',
    '</tr></thead><tbody>',
    tr("Starting Pitcher", away_name, home_name),
    tr(paste0("SP Quality (", away_fip_label, " / ", home_fip_label, ")"),
       round(away_sp_fip, 2), round(home_sp_fip, 2)),
    tr("Bullpen ERA (avail/fresh arms)",
       round(away_bp_era, 2), round(home_bp_era, 2)),
    tr(blend_label,
       paste0("<strong>", round(away_blended_fip, 2), "</strong>"),
       paste0("<strong>", round(home_blended_fip, 2), "</strong>")),
    tr("Lineup avg wRC+", round(away_wrc_disp), round(home_wrc_disp)),
    if (!is.null(pf_display)) paste0(
      '<tr><td style="padding:6px 14px; border-bottom:1px solid #eee; color:#555;">',
      pf_display, '</td>',
      '<td colspan="2" style="padding:6px 14px; border-bottom:1px solid #eee; ',
      'text-align:center; color:#555; font-style:italic;">',
      round(pf, 3), ' \u2014 applies to both teams</td></tr>'
    ) else "",
    tr("<strong>Projected Runs</strong>",
       paste0("<strong>", round(away_exp_r, 1), "</strong>"),
       paste0("<strong>", round(home_exp_r, 1), "</strong>")),
    tr("<strong>Win Probability</strong>",
       paste0('<span style="', away_win_bg, 'padding:2px 10px; border-radius:4px; font-weight:bold;">',
              round(p_away * 100), "%</span>"),
       paste0('<span style="', home_win_bg, 'padding:2px 10px; border-radius:4px; font-weight:bold;">',
              round(p_home * 100), "%</span>")),
    '</tbody></table>',
    '<p style="font-size:12px; color:#444; margin:0 0 0.8rem 0;">',
    '<strong>Projected total: ', round(total_exp, 1), ' runs</strong>',
    ' \u00b7 Projected winner: <strong>', fav_team, '</strong> (',
    round(fav_pct * 100), '% win probability)</p>'
  )

  factors_html <- paste0(
    '<p style="font-size:12px; font-weight:bold; color:#444; margin:0.8rem 0 4px 0;">',
    'Model inputs:</p>',
    '<ul style="font-size:12px; color:#555; margin:0 0 0.8rem 0; ',
    'padding-left:18px; line-height:1.8;">',
    paste0("<li>", factors, "</li>", collapse = ""),
    "</ul>"
  )

  method_html <- paste0(
    '<p style="font-size:11px; color:#888; padding:8px 10px; background:#f8f9fa; ',
    'border-radius:4px; border-left:3px solid #dee2e6; margin:0;">',
    '<strong>Model:</strong> E[runs] = ', LEAGUE_AVG_RUNS,
    ' \u00d7 (wRC+/100) \u00d7 park_factor \u00d7 temp_adj \u00d7 wind_adj \u00d7 (blended_FIP/', LEAGUE_AVG_FIP, ')',
    ' \u00d7 recent_form_adj \u00d7 defense_adj. ',
    'Blended FIP = SP_frac \u00d7 SP_xFIP + BP_frac \u00d7 bullpen_ERA. ',
    'Wind adj: \u00b110% max (Out/In directions), \u00b12% crosswind, 0 under 8 mph. ',
    'Defense adj: OAA > DRS > fg_Defense, 30% signal weight, \u00b15% hard cap. ',
    'Recent form adj (\u00b18% max) from last-7-game team OPS; no adjustment when <5 batters have data. ',
    'Win probability via Poisson distribution. ',
    'Constants calibrated to 2022\u20132024 (n=7,300 games). ',
    'Accuracy improves as the season progresses and sample sizes grow.',
    '</p>'
  )

  paste0(table_html, factors_html, method_html)
}

# ============================================================
# make_beat_streak_html()
# Ranks all of today's lineup batters by P(get a hit tonight).
# Model: true-talent AVG × SP suppression × handedness split + recent form
# P(hit) = 1 - (1 - p_per_AB)^expected_AB
# ============================================================

LEAGUE_H9   <- 8.8   # MLB avg H/9 allowed (2022-2024 empirical)
H9_STAB_IP  <- 80L   # IP at which H/9 is 50/50 signal vs noise
AVG_STAB_AB <- 150L  # AB at which observed AVG is 50/50 signal vs noise

make_beat_streak_html <- function(top_n = 20) {
  tryCatch({
    if (!exists("lineup_context") || nrow(lineup_context) == 0) return("")

    # ── 1. Base batter table ─────────────────────────────────────────────────
    batters <- lineup_context %>%
      dplyr::filter(!is.na(batting_slot), batting_slot %in% 1:9,
                    !is.na(mlbam_id)) %>%
      dplyr::select(game_pk, side, team_abbr, batting_slot, mlbam_id,
                    player_name, mlb_pa, mlb_avg)

    if (nrow(batters) == 0) return("")

    # ── 2. xAVG from offense_master_season ──────────────────────────────────
    xavg_tbl <- if (exists("offense_master_season") &&
                    "fg_xAVG" %in% names(offense_master_season)) {
      offense_master_season %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, fg_xAVG)
    } else dplyr::tibble(mlbam_id = integer(), fg_xAVG = numeric())

    batters <- batters %>% dplyr::left_join(xavg_tbl, by = "mlbam_id")

    # ── 3. Opposing SP (flip side: away batter faces home SP, etc.) ──────────
    sp_tbl <- starter_matchup %>%
      dplyr::select(game_pk, sp_side = side, pitcher_name, pitch_hand,
                    fg_H_per_9, mlb_ip) %>%
      dplyr::mutate(side = dplyr::if_else(sp_side == "away", "home", "away")) %>%
      dplyr::select(-sp_side)

    batters <- batters %>%
      dplyr::left_join(sp_tbl, by = c("game_pk", "side"))

    # ── 4. Handedness splits ─────────────────────────────────────────────────
    if (exists("lineup_context_splits") && nrow(lineup_context_splits) > 0 &&
        "sp_avg" %in% names(lineup_context_splits)) {
      split_tbl <- lineup_context_splits %>%
        dplyr::select(game_pk, side, mlbam_id, sp_avg, sp_pa,
                      mlb_avg_split = mlb_avg, split_label)
      batters <- batters %>%
        dplyr::left_join(split_tbl, by = c("game_pk", "side", "mlbam_id"))
    } else {
      batters <- batters %>%
        dplyr::mutate(sp_avg = NA_real_, sp_pa = NA_integer_,
                      mlb_avg_split = NA_real_, split_label = NA_character_)
    }

    # ── 5. Recent streaks ────────────────────────────────────────────────────
    if (exists("recent_batter_streaks") && nrow(recent_batter_streaks) > 0) {
      streak_tbl <- recent_batter_streaks %>%
        dplyr::select(mlbam_id, hit_streak, last7_avg, is_hot, is_cold)
      batters <- batters %>% dplyr::left_join(streak_tbl, by = "mlbam_id")
    } else {
      batters <- batters %>%
        dplyr::mutate(hit_streak = NA_integer_, last7_avg = NA_real_,
                      is_hot = FALSE, is_cold = FALSE)
    }

    # ── 6. Compute P(hit tonight) ────────────────────────────────────────────
    batters <- batters %>%
      dplyr::mutate(
        pa_val  = dplyr::coalesce(suppressWarnings(as.numeric(mlb_pa)), 0),
        obs_avg = suppressWarnings(as.numeric(mlb_avg)),
        xa      = suppressWarnings(as.numeric(fg_xAVG)),

        # True-talent AVG: blend xAVG with observed weighted by PA
        base_avg = dplyr::case_when(
          !is.na(xa) & !is.na(obs_avg) & pa_val >= 50 ~
            xa * pmax(0.40, 1 - pa_val / (pa_val + AVG_STAB_AB)) +
            obs_avg * pmin(0.60, pa_val / (pa_val + AVG_STAB_AB)),
          !is.na(xa)      ~ xa,
          !is.na(obs_avg) ~
            (obs_avg * pa_val + .250 * AVG_STAB_AB) / (pa_val + AVG_STAB_AB),
          TRUE ~ .250
        ),

        # SP H/9 regressed toward league mean (small-sample dampening)
        sp_ip_val = dplyr::coalesce(suppressWarnings(as.numeric(mlb_ip)), 0),
        sp_h9_val = suppressWarnings(as.numeric(fg_H_per_9)),
        sp_h9_reg = dplyr::case_when(
          !is.na(sp_h9_val) & sp_ip_val > 0 ~
            (sp_h9_val * sp_ip_val + LEAGUE_H9 * H9_STAB_IP) /
            (sp_ip_val + H9_STAB_IP),
          TRUE ~ LEAGUE_H9
        ),
        # sp_factor < 1 = tough SP (allows fewer H), > 1 = hitter-friendly
        sp_factor = pmin(1.18, pmax(0.82, sp_h9_reg / LEAGUE_H9)),

        # Handedness split: sp_avg vs overall avg ratio
        sp_avg_v  = suppressWarnings(as.numeric(sp_avg)),
        mlb_avg_v = suppressWarnings(as.numeric(mlb_avg_split)),
        sp_pa_v   = suppressWarnings(as.integer(sp_pa)),
        split_mult = dplyr::case_when(
          !is.na(sp_avg_v) & !is.na(mlb_avg_v) &
            mlb_avg_v > .100 & !is.na(sp_pa_v) & sp_pa_v >= 25L ~
            pmin(1.20, pmax(0.80, sp_avg_v / mlb_avg_v)),
          TRUE ~ 1.0
        ),

        # Recent form additive
        form_add = dplyr::case_when(
          !is.na(hit_streak) & hit_streak >= 5 ~  0.015,
          isTRUE(is_hot)                        ~  0.010,
          isTRUE(is_cold)                       ~ -0.010,
          TRUE                                  ~  0.000
        ),

        # P(hit per AB)
        p_per_ab = pmin(0.450, pmax(0.100,
          base_avg * sp_factor * split_mult + form_add)),

        # Expected ABs by lineup slot
        exp_ab = dplyr::case_when(
          batting_slot == 1 ~ 3.70, batting_slot == 2 ~ 3.60,
          batting_slot == 3 ~ 3.50, batting_slot == 4 ~ 3.40,
          batting_slot == 5 ~ 3.30, batting_slot == 6 ~ 3.20,
          batting_slot == 7 ~ 3.10, batting_slot == 8 ~ 3.00,
          batting_slot == 9 ~ 2.90, TRUE ~ 3.30
        ),

        # P(at least 1 hit tonight)
        p_hit = 1 - (1 - p_per_ab)^exp_ab,

        # Note badges
        note_streak = dplyr::case_when(
          !is.na(hit_streak) & hit_streak >= 5 ~
            paste0(hit_streak, "-game streak"),
          isTRUE(is_hot) & !is.na(last7_avg) ~
            paste0("hot L7 (.", sprintf("%03d", round(last7_avg * 1000)), ")"),
          TRUE ~ ""
        ),
        note_split = dplyr::if_else(split_mult >= 1.10, "favorable split", ""),
        note_sp    = dplyr::if_else(sp_factor <= 0.88, "tough SP", "")
      )

    # ── 7. Top N ─────────────────────────────────────────────────────────────
    top <- batters %>%
      dplyr::filter(!is.na(p_hit)) %>%
      dplyr::arrange(dplyr::desc(p_hit)) %>%
      dplyr::slice_head(n = top_n)

    if (nrow(top) == 0) return("")

    # ── 8. Render HTML ───────────────────────────────────────────────────────
    rows_html <- vapply(seq_len(nrow(top)), function(i) {
      r   <- top[i, ]
      pct <- round(r$p_hit * 100, 1)
      pct_col <- if (pct >= 70) "#27ae60" else if (pct >= 63) "#1a73e8" else "#555"

      sp_lbl <- if (!is.na(r$pitcher_name) && nchar(r$pitcher_name) > 0) {
        hand <- if (!is.na(r$pitch_hand) && r$pitch_hand %in% c("L", "R"))
          paste0(" (", r$pitch_hand, "HP)") else ""
        paste0(r$pitcher_name, hand)
      } else "TBD"

      note_parts <- c(
        dplyr::coalesce(r$note_streak, ""),
        dplyr::coalesce(r$note_split, ""),
        dplyr::coalesce(r$note_sp, "")
      )
      note_parts <- note_parts[nchar(note_parts) > 0]
      notes_html <- if (length(note_parts) > 0) {
        paste(vapply(note_parts, function(n) {
          if (grepl("streak|hot", n)) {
            bg <- "#fff3cd"; fg <- "#856404"
          } else if (grepl("split", n)) {
            bg <- "#d4edda"; fg <- "#155724"
          } else {
            bg <- "#fce4e4"; fg <- "#721c24"
          }
          sprintf(
            '<span style="background:%s;color:%s;font-size:0.72rem;padding:1px 6px;border-radius:3px;white-space:nowrap;">%s</span>',
            bg, fg, n
          )
        }, character(1)), collapse = " ")
      } else ""

      sprintf(
        '<tr style="border-bottom:1px solid #f0f0f0;">
          <td style="padding:5px 8px;color:#aaa;font-size:0.82rem;text-align:right;">%d</td>
          <td style="padding:5px 10px;font-weight:600;">%s</td>
          <td style="padding:5px 8px;color:#666;font-size:0.85rem;">%s</td>
          <td style="padding:5px 8px;color:#555;font-size:0.82rem;">%s</td>
          <td style="padding:5px 8px;text-align:center;color:#888;font-size:0.85rem;">%s</td>
          <td style="padding:5px 10px;text-align:center;font-weight:700;color:%s;font-size:0.95rem;">%.1f%%</td>
          <td style="padding:5px 8px;">%s</td>
        </tr>',
        i,
        r$player_name,
        dplyr::coalesce(r$team_abbr, ""),
        sp_lbl,
        as.character(r$batting_slot),
        pct_col, pct,
        notes_html
      )
    }, character(1))

    paste0(
      '<div style="max-width:860px;">',
      '<table style="width:100%;border-collapse:collapse;font-size:14px;">',
      '<thead>',
      '<tr style="border-bottom:2px solid #dee2e6;color:#777;font-size:0.75rem;',
      'text-transform:uppercase;letter-spacing:0.04em;">',
      '<th style="padding:5px 8px;text-align:right;width:32px;">#</th>',
      '<th style="padding:5px 10px;text-align:left;">Player</th>',
      '<th style="padding:5px 8px;text-align:left;">Team</th>',
      '<th style="padding:5px 8px;text-align:left;">Opp SP</th>',
      '<th style="padding:5px 8px;text-align:center;">Slot</th>',
      '<th style="padding:5px 10px;text-align:center;">Hit%</th>',
      '<th style="padding:5px 8px;text-align:left;"></th>',
      '</tr>',
      '</thead>',
      '<tbody>',
      paste(rows_html, collapse = "\n"),
      '</tbody>',
      '</table>',
      '<p style="font-size:0.72rem;color:#aaa;margin-top:8px;line-height:1.5;">',
      'Model: true-talent AVG (Statcast xAVG blended with season observed) ',
      '\u00d7 SP suppression (regressed H/9) \u00d7 handedness split \u00b1 recent form. ',
      'P(1+ hit) = 1 \u2212 (1 \u2212 p<sub>AB</sub>)<sup>exp AB</sup>. ',
      'Early-season probabilities regressed toward mean until sample sizes stabilize.',
      '</p>',
      '</div>'
    )
  }, error = function(e) "")
}
