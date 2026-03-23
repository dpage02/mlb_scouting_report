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
    # Direction: if sc_brl_percent >> what Steamer implies, boost HR
    brl_adj_factor = dplyr::case_when(
      !is.na(sc_brl_percent) & !is.na(proj_hr) & proj_hr > 0 ~
        1 + (sc_brl_percent - LEAGUE_AVG_BRL_PCT) * 0.012,
      TRUE ~ 1.0
    ),
    brl_adj_factor = pmax(0.75, pmin(1.35, brl_adj_factor)),  # cap ±35%
    adj_hr         = round(proj_hr * brl_adj_factor),

    # ── AVG adjustment via xBA ─────────────────────────────
    # If xBA > projected AVG by meaningful margin, lift AVG
    xba_diff       = dplyr::coalesce(sc_est_ba, proj_avg) - proj_avg,
    avg_adj        = proj_avg + xba_diff * 0.35,  # blend 35% toward xBA signal
    avg_adj        = pmax(0.150, pmin(0.380, dplyr::coalesce(avg_adj, proj_avg))),

    # Recalculate H from adjusted AVG (keeping AB fixed)
    h_adj          = round(dplyr::coalesce(as.numeric(proj_ab), 0) * avg_adj),

    # Adjust HR-linked counting: extra HR → extra R/RBI proportionally
    hr_delta       = dplyr::coalesce(adj_hr, 0L) - dplyr::coalesce(proj_hr, 0L),
    r_adj          = round(dplyr::coalesce(proj_r,   0L) + hr_delta * 0.85),
    rbi_adj        = round(dplyr::coalesce(proj_rbi, 0L) + hr_delta * 0.90),

    # ── SB adjustment via sprint speed ────────────────────
    # sprint_speed MLB avg ~27.0 ft/sec; each 0.5 ft/sec above ~ +15% SB
    sb_adj_factor  = dplyr::case_when(
      !is.na(sc_sprint_speed) & !is.na(proj_sb) & proj_sb > 0 ~
        1 + (sc_sprint_speed - 27.0) * 0.06,
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
    # Statcast raw for reference
    dplyr::any_of(c("sc_brl_percent", "sc_est_ba", "sc_sprint_speed",
                    "sc_avg_hit_speed", "sc_est_woba"))
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
    actual_k9   = dplyr::coalesce(fg_K_9, mlb_k9),
    k9_signal   = dplyr::if_else(
      !is.na(actual_k9),
      (actual_k9 / dplyr::coalesce(proj_ip, 150) * dplyr::coalesce(proj_ip, 150) / 9),
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
message("02_blend_projections complete.")
