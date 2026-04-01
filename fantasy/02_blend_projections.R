# ============================================================
# FANTASY BASEBALL — Blend Steamer with Statcast Adjustments
# ============================================================
# PURPOSE:
#   Take Steamer as the base projection and apply targeted
#   adjustments using our Statcast + FanGraphs data:
#
#   BATTERS:
#     HR   — adjust via Brl% vs expected HR rate
#     AVG  — adjust via xBA vs projected AVG
#     SB   — adjust via sprint_speed percentile
#
#   PITCHERS:
#     ERA  — adjust via FIP/xFIP vs projected ERA
#     K    — adjust via K/9 trend vs Steamer
#
# OUTPUT:
#   proj_bat  — blended batter projections
#   proj_pit  — blended pitcher projections
# ============================================================

source("fantasy/00_fantasy_config.R")

# ------------------------------------------------------------
# Pull Statcast + FanGraphs actual data (prior season)
# Uses most recent season available in pipeline data
# ------------------------------------------------------------

# Most recent batter season data
sc_bat <- player_season_statcast_offense %>%
  dplyr::arrange(mlbam_id, dplyr::desc(season)) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id,
    dplyr::any_of(c(
      "sc_brl_percent",   # barrel % (whole number, e.g. 12.4)
      "sc_ev95percent",   # hard hit % (whole number)
      "sc_avg_hit_speed", # avg exit velo
      "sc_est_ba",        # xBA
      "sc_est_woba",      # xwOBA
      "sc_sprint_speed"   # ft/sec
    ))
  )

# Most recent pitcher season data
sc_pit_master <- pitching_master_season %>%
  dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(season, 0L))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::select(
    mlbam_id,
    dplyr::any_of(c(
      "fg_FIP", "fg_xFIP", "fg_SIERA",
      "fg_K_9", "fg_BB_9",
      "mlb_era", "mlb_ip", "mlb_k9"
    ))
  )

# ------------------------------------------------------------
# Batter blend
# ------------------------------------------------------------

# League-average barrel% → HR/PA relationship (MLB 2024/2025 baseline)
# ~1.0% brl = ~0.035 HR/PA at average; use a simple linear scaler
# brl_to_hr_adj: if player brl% is 2 pts above league avg (8%), nudge HR up ~7%
LEAGUE_AVG_BRL_PCT <- 8.0   # whole-number units
LEAGUE_AVG_XBA     <- 0.248

proj_bat <- steamer_bat %>%
  dplyr::left_join(sc_bat, by = "mlbam_id") %>%
  dplyr::mutate(

    # ── HR adjustment via barrel% ──────────────────────────
    # Coef tuned via 2022-2024 backtest (was 0.012)
    brl_adj_factor = dplyr::case_when(
      !is.na(sc_brl_percent) & !is.na(proj_hr) & proj_hr > 0 ~
        1 + (sc_brl_percent - LEAGUE_AVG_BRL_PCT) * 0.015,
      TRUE ~ 1.0
    ),
    brl_adj_factor = pmax(0.75, pmin(1.35, brl_adj_factor)),
    adj_hr         = round(proj_hr * brl_adj_factor),

    # ── AVG adjustment via xBA + xwOBA ────────────────────
    # xwOBA (corr 0.47) outperforms xBA (corr 0.35) from backtest.
    # Use xwOBA to nudge AVG when available; fall back to xBA.
    # xwOBA → implied AVG via simple linear scaling (xwOBA ≈ 0.82*AVG + 0.18)
    xwoba_implied_avg = dplyr::if_else(
      !is.na(sc_est_woba),
      (sc_est_woba - 0.18) / 0.82,
      NA_real_
    ),
    best_avg_signal  = dplyr::coalesce(xwoba_implied_avg, sc_est_ba),
    xba_diff         = dplyr::coalesce(best_avg_signal, proj_avg) - proj_avg,
    avg_adj          = proj_avg + xba_diff * 0.40,  # blend tuned from 0.35
    avg_adj          = pmax(0.150, pmin(0.380, dplyr::coalesce(avg_adj, proj_avg))),

    # Recalculate H from adjusted AVG (keeping AB fixed)
    h_adj          = round(dplyr::coalesce(as.numeric(proj_ab), 0) * avg_adj),

    # Adjust HR-linked counting: extra HR → extra R/RBI proportionally
    hr_delta       = dplyr::coalesce(adj_hr, 0L) - dplyr::coalesce(proj_hr, 0L),
    r_adj          = round(dplyr::coalesce(proj_r,   0L) + hr_delta * 0.85),
    rbi_adj        = round(dplyr::coalesce(proj_rbi, 0L) + hr_delta * 0.90),

    # ── SB adjustment via sprint speed ────────────────────
    # Coef tuned via backtest (was 0.06); corr sprint→SB = 0.547
    sb_adj_factor  = dplyr::case_when(
      !is.na(sc_sprint_speed) & !is.na(proj_sb) & proj_sb > 0 ~
        1 + (sc_sprint_speed - 27.0) * 0.07,
      TRUE ~ 1.0
    ),
    sb_adj_factor  = pmax(0.70, pmin(1.50, sb_adj_factor)),
    sb_adj         = round(dplyr::coalesce(proj_sb, 0L) * sb_adj_factor),

    # ── Blended final projections ──────────────────────────
    # Steamer weight vs Statcast-adjusted weight
    final_hr  = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_hr, 0L) +
                      BLEND_STATCAST_WT * dplyr::coalesce(adj_hr,  0L)),
    final_h   = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_h, 0L) +
                      BLEND_STATCAST_WT * dplyr::coalesce(h_adj,  0L)),
    final_avg = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_avg, 0) +
                      BLEND_STATCAST_WT * dplyr::coalesce(avg_adj, 0), 3),
    final_r   = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_r, 0L) +
                      BLEND_STATCAST_WT * dplyr::coalesce(r_adj,  0L)),
    final_rbi = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_rbi, 0L) +
                      BLEND_STATCAST_WT * dplyr::coalesce(rbi_adj, 0L)),
    final_sb  = round(BLEND_STEAMER_WT * dplyr::coalesce(proj_sb, 0L) +
                      BLEND_STATCAST_WT * dplyr::coalesce(sb_adj,  0L)),
    # Pass-through (no adjustment logic for these)
    final_2b  = dplyr::coalesce(proj_2b,  0L),
    final_3b  = dplyr::coalesce(proj_3b,  0L),
    final_cs  = dplyr::coalesce(proj_cs,  0L),
    final_bb  = dplyr::coalesce(proj_bb,  0L),
    final_hbp = dplyr::coalesce(proj_hbp, 0L),
    final_pa  = dplyr::coalesce(proj_pa,  0L)
  ) %>%
  dplyr::select(
    fg_id, mlbam_id, player_name, team_abbr, proj_pos_raw, proj_wrc_plus,
    final_pa, final_h, final_2b, final_3b, final_hr,
    final_r, final_rbi, final_sb, final_cs, final_bb, final_hbp, final_avg,
    # Keep original Steamer for reference
    steamer_hr = proj_hr, steamer_avg = proj_avg, steamer_sb = proj_sb,
    # Statcast raw for reference (sc_est_woba now used in avg adjustment)
    dplyr::any_of(c("sc_brl_percent", "sc_est_ba", "sc_est_woba",
                    "sc_sprint_speed", "sc_avg_hit_speed"))
  )

message("proj_bat blended: ", nrow(proj_bat), " batters")

# ------------------------------------------------------------
# Pitcher blend
# ------------------------------------------------------------

# League-average ERA (used for adjustment reference)
LEAGUE_AVG_ERA <- 4.20

proj_pit <- steamer_pit %>%
  dplyr::left_join(sc_pit_master, by = "mlbam_id") %>%
  # Add any FIP columns that may be missing after the join (any_of silently drops them)
  dplyr::mutate(
    fg_xFIP  = if ("fg_xFIP"  %in% names(.)) fg_xFIP  else NA_real_,
    fg_FIP   = if ("fg_FIP"   %in% names(.)) fg_FIP   else NA_real_,
    fg_SIERA = if ("fg_SIERA" %in% names(.)) fg_SIERA else NA_real_,
    fg_K_9   = if ("fg_K_9"   %in% names(.)) fg_K_9   else NA_real_,
    mlb_k9   = if ("mlb_k9"   %in% names(.)) mlb_k9   else NA_real_
  ) %>%
  dplyr::mutate(

    # ── ERA adjustment via FIP/xFIP ───────────────────────
    # Use blend of FIP and xFIP vs projected ERA
    best_era_estimator = dplyr::coalesce(fg_xFIP, fg_FIP, fg_SIERA),
    era_signal         = dplyr::if_else(
      !is.na(best_era_estimator),
      # Weight: 60% FIP/xFIP signal, 40% projected ERA
      0.60 * best_era_estimator + 0.40 * dplyr::coalesce(proj_era, LEAGUE_AVG_ERA),
      dplyr::coalesce(proj_era, LEAGUE_AVG_ERA)
    ),
    # Blend Steamer ERA vs FIP-adjusted ERA
    final_era = round(
      BLEND_STEAMER_WT  * dplyr::coalesce(proj_era, LEAGUE_AVG_ERA) +
      BLEND_STATCAST_WT * era_signal,
      2
    ),
    final_era = pmax(2.50, pmin(7.00, final_era)),  # sanity cap

    # Recalculate ER from adjusted ERA
    final_er  = round(final_era * dplyr::coalesce(proj_ip, 0) / 9, 1),

    # ── K adjustment via K/9 trend ─────────────────────────
    # k9_signal = K/9 rate * projected IP / 9 = total projected K
    actual_k9   = dplyr::coalesce(fg_K_9, mlb_k9),
    k9_signal   = dplyr::if_else(
      !is.na(actual_k9),
      actual_k9 / 9 * dplyr::coalesce(proj_ip, 0),
      dplyr::coalesce(as.numeric(proj_k), 0)
    ),
    final_k   = round(
      BLEND_STEAMER_WT  * dplyr::coalesce(as.numeric(proj_k), 0) +
      BLEND_STATCAST_WT * k9_signal
    ),

    # Pass-through
    final_ip  = dplyr::coalesce(proj_ip, 0),
    final_w   = dplyr::coalesce(proj_w,  0L),
    final_sv  = dplyr::coalesce(proj_sv, 0L),
    final_qs  = dplyr::coalesce(proj_qs, 0),
    final_gs  = dplyr::coalesce(proj_gs, 0L),
    final_g   = dplyr::coalesce(proj_g,  0L)
  ) %>%
  dplyr::select(
    fg_id, mlbam_id, player_name, team_abbr, proj_role,
    final_ip, final_gs, final_g, final_w, final_sv, final_k,
    final_er, final_era, final_qs,
    steamer_era = proj_era, steamer_k = proj_k,
    dplyr::any_of(c("fg_FIP", "fg_xFIP", "fg_SIERA",
                    "actual_k9", "best_era_estimator"))
  )

message("proj_pit blended: ", nrow(proj_pit), " pitchers")

# ============================================================
# PART 3 — Park Factors + Team Run Environment + Aging Curve
# ============================================================
# Projection systems (Steamer/ZiPS/ATC) already bake in park
# factors, so we apply a residual-only adjustment (25% weight)
# to capture what our Statcast overlay may over/underfit.
# Team run environment is derived from our own consensus
# projections so it stays self-consistent.
# Aging adjustments are intentionally light — systems already
# model decline well. We add only a small nudge at extremes.
# ============================================================

# ── Normalize team abbreviations (multiple sources use variants)
normalize_team_abbr <- function(x) {
  dplyr::case_when(
    x %in% c("WAS","WSN")      ~ "WSH",
    x %in% c("TBR","TAM")      ~ "TB",
    x %in% c("SDP","SDG","SAN")~ "SD",
    x %in% c("SFG","SFN")      ~ "SF",
    x %in% c("ATH")            ~ "OAK",
    x %in% c("CLG")            ~ "CLE",
    TRUE ~ x
  )
}

# ── Park factors (FanGraphs 5-yr HR-based, 2025 baseline) ───
# 1.00 = neutral. Effective multiplier = (PF + 1) / 2 because
# only ~half of games are at home; away parks average to ~1.0.
# Residual weight: 25% — assumes systems already captured 75%.
PARK_FACTORS <- dplyr::tibble(
  team_norm   = c("COL","CIN","BOS","TEX","ATL","BAL","PHI","CHC","NYY",
                  "KC","WSH","MIL","TOR","HOU","LAD","STL","DET","CLE",
                  "ARI","TB","MIA","LAA","MIN","OAK","CWS","PIT","SEA",
                  "NYM","SD","SF"),
  park_factor = c(1.39, 1.12, 1.10, 1.09, 1.08, 1.07, 1.06, 1.05, 1.05,
                  1.04, 1.03, 1.03, 1.02, 1.02, 1.01, 1.00, 0.99, 0.98,
                  0.98, 0.97, 0.97, 0.97, 0.96, 0.95, 0.94, 0.94, 0.93,
                  0.92, 0.91, 0.88)
) %>%
  dplyr::mutate(
    pf_eff  = (park_factor + 1.0) / 2.0,          # home-game-weighted effective PF
    pf_mult = 1 + (pf_eff - 1.0) * 0.25           # 25% residual on top of systems
  )

# ── Team run environment (self-consistent: derived from our own projections)
# Captures how much R/RBI opportunity each team's lineup generates.
# Applied at 20% weight — strong signal but mostly in projections already.
team_run_env <- steamer_bat %>%
  dplyr::mutate(team_norm = normalize_team_abbr(team_abbr)) %>%
  dplyr::group_by(team_norm) %>%
  dplyr::summarise(
    team_r  = sum(proj_r,  na.rm = TRUE),
    team_pa = sum(proj_pa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(team_norm), team_norm != "", team_pa >= 500) %>%
  dplyr::mutate(
    r_per_pa    = team_r / team_pa,
    lg_r_per_pa = mean(r_per_pa, na.rm = TRUE),
    team_r_adj  = 1 + (r_per_pa / lg_r_per_pa - 1) * 0.20,
    team_r_adj  = pmax(0.88, pmin(1.12, team_r_adj))
  ) %>%
  dplyr::select(team_norm, team_r_adj)

# ── Player ages (referenced to Opening Day of projection season)
opening_day <- as.Date(paste0(SEASON_PROJ, "-04-01"))
age_lookup <- player_master_ids %>%
  dplyr::filter(!is.na(mlbam_id), !is.na(birth_date)) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
  dplyr::transmute(
    mlbam_id,
    player_age = as.numeric(difftime(opening_day, birth_date, units = "days")) / 365.25
  )

# ── Apply to batters ──────────────────────────────────────────
proj_bat <- proj_bat %>%
  dplyr::mutate(team_norm = normalize_team_abbr(team_abbr)) %>%
  dplyr::left_join(PARK_FACTORS %>% dplyr::select(team_norm, pf_mult),
                   by = "team_norm") %>%
  dplyr::left_join(team_run_env, by = "team_norm") %>%
  dplyr::left_join(age_lookup,   by = "mlbam_id") %>%
  dplyr::mutate(
    pf_mult    = dplyr::coalesce(pf_mult,   1.00),
    team_r_adj = dplyr::coalesce(team_r_adj, 1.00),
    player_age = dplyr::coalesce(player_age, 28.0),   # assume peak if unknown

    # Aging multipliers — light residual only (systems handle the bulk)
    age_hr_mult = dplyr::case_when(
      player_age < 24  ~ 1.02,
      player_age < 27  ~ 1.01,
      player_age <= 30 ~ 1.00,
      player_age <= 32 ~ 0.99,
      player_age <= 34 ~ 0.97,
      TRUE             ~ 0.94
    ),
    age_sb_mult = dplyr::case_when(
      player_age < 24  ~ 1.04,
      player_age < 27  ~ 1.01,
      player_age <= 29 ~ 1.00,
      player_age <= 31 ~ 0.97,
      player_age <= 33 ~ 0.93,
      TRUE             ~ 0.87
    ),
    age_avg_mult = dplyr::case_when(
      player_age <= 30 ~ 1.000,
      player_age <= 33 ~ 0.995,
      TRUE             ~ 0.990
    ),

    # Apply adjustments
    final_hr  = round(final_hr  * pf_mult   * age_hr_mult),
    final_avg = round(final_avg * age_avg_mult, 3),
    final_r   = round(final_r   * team_r_adj),
    final_rbi = round(final_rbi * team_r_adj),
    final_sb  = round(final_sb  * age_sb_mult)
  ) %>%
  dplyr::select(-team_norm, -pf_mult, -team_r_adj, -player_age,
                -age_hr_mult, -age_sb_mult, -age_avg_mult)

message("Batters — park/team/aging applied")

# ── Apply to pitchers ─────────────────────────────────────────
# Park raises/lowers ERA for pitchers pitching in extreme environments.
# Aging: ERA creeps up, K-rate drops after ~32.
proj_pit <- proj_pit %>%
  dplyr::mutate(team_norm = normalize_team_abbr(team_abbr)) %>%
  dplyr::left_join(PARK_FACTORS %>% dplyr::select(team_norm, pf_mult),
                   by = "team_norm") %>%
  dplyr::left_join(age_lookup, by = "mlbam_id") %>%
  dplyr::mutate(
    pf_mult    = dplyr::coalesce(pf_mult,   1.00),
    player_age = dplyr::coalesce(player_age, 28.0),

    # For pitchers park factor hurts ERA (higher PF = more runs allowed)
    age_era_mult = dplyr::case_when(
      player_age < 27  ~ 0.99,
      player_age <= 30 ~ 1.00,
      player_age <= 32 ~ 1.01,
      player_age <= 34 ~ 1.03,
      TRUE             ~ 1.06
    ),
    age_k_mult = dplyr::case_when(
      player_age < 25  ~ 1.02,
      player_age <= 29 ~ 1.00,
      player_age <= 32 ~ 0.98,
      player_age <= 34 ~ 0.95,
      TRUE             ~ 0.91
    ),

    final_era = pmax(2.50, pmin(7.00, round(final_era * pf_mult * age_era_mult, 2))),
    final_er  = round(final_era * final_ip / 9, 1),
    final_k   = round(final_k   * age_k_mult)
  ) %>%
  dplyr::select(-team_norm, -pf_mult, -player_age, -age_era_mult, -age_k_mult)

message("Pitchers — park/aging applied")
message("02_blend_projections complete.")
