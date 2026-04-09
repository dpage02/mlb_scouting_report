# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 07_bullpen_context
# SCRIPT: 99_bullpen_context_join.R
# ============================================================
# PURPOSE:
#   Combine depth chart roles, availability flags, and season
#   performance stats into one bullpen context table.
#
# JOIN LOGIC:
#   depth_charts (spine — all active bullpen arms)
#   bullpen_availability → join on mlbam_id (recent usage)
#   value_master_season  → join on mlbam_id (WAR/ERA)
#   player_season_mlb_pitching → join on mlbam_id (season stats)
#
# GRAIN:
#   One row per pitcher per team
#   Pitchers who changed teams may appear twice
#
# OUTPUT:
#   bullpen_context
# ============================================================

required_objects <- c(
  "depth_charts",
  "bullpen_availability",
  "value_master_season",
  "player_season_mlb_pitching"
)

missing_objects <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Spine — active bullpen arms from depth chart
# Includes starters too (for opener/piggyback awareness)
# ------------------------------------------------------------

active_pitchers <- depth_charts %>%
  dplyr::filter(fg_position %in% c("SP", "RP", "SP/RP"),
                roster_type %in% c("mlb-sp", "mlb-bp", "il-sp", "il-rp")) %>%
  dplyr::select(mlbam_id, player_name, team_abbr, fg_team_abbr,
                fg_role, fg_position, roster_type)

# ------------------------------------------------------------
# Month-aware scaling factor for availability thresholds
# April: managers protect arms early (0.85×)
# Sept+: expanded rosters, playoff push (1.10×)
# ------------------------------------------------------------

month_num    <- as.integer(format(target_date, "%m"))
month_factor <- dplyr::case_when(
  month_num %in% c(3L, 4L)  ~ 0.85,
  month_num %in% c(9L, 10L) ~ 1.10,
  TRUE                       ~ 1.00
)

# Role-based default pitches per outing (used when season IP/G unavailable)
role_default_pitches <- c(
  "CL" = 16, "SU8" = 18, "SU7" = 18, "SU6" = 17,
  "SU" = 18, "MID" = 20, "LR" = 35
)

bullpen_context <- active_pitchers %>%

  dplyr::left_join(bullpen_availability, by = "mlbam_id") %>%

  # Fill NAs for pitchers with no recent log
  dplyr::mutate(
    days_rest           = dplyr::coalesce(days_rest, NA_integer_),
    pitches_yesterday   = dplyr::coalesce(pitches_yesterday, 0L),
    pitches_last_3_days = dplyr::coalesce(pitches_last_3_days, 0L),
    appearances_last_7d = dplyr::coalesce(appearances_last_7d, 0L),
    consecutive_days    = dplyr::coalesce(consecutive_days, 0L)
  ) %>%

  # Join season stats from MLB pitching module
  dplyr::left_join(
    player_season_mlb_pitching %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, mlb_g, mlb_gs, mlb_ip, mlb_era,
                    mlb_whip, mlb_so, mlb_bb, mlb_sv, mlb_hld),
    by = "mlbam_id"
  ) %>%

  # Join fWAR and dollar value
  dplyr::left_join(
    value_master_season %>%
      dplyr::filter(player_type == "pitcher") %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, dplyr::any_of(c(
        "fg_WAR", "fg_Dollars", "fg_RAR",
        "bbref_ERA", "bbref_WHIP", "bbref_SO"
      ))),
    by = "mlbam_id"
  ) %>%

  # Join xFIP for bullpen quality estimate (best FIP-based metric for relievers)
  dplyr::left_join(
    if (exists("player_season_fg_pitching") &&
        "fg_xFIP" %in% names(player_season_fg_pitching)) {
      player_season_fg_pitching %>%
        dplyr::arrange(mlbam_id, dplyr::desc(
          dplyr::coalesce(
            if ("fg_ip" %in% names(player_season_fg_pitching))
              player_season_fg_pitching$fg_ip else rep(0, nrow(player_season_fg_pitching)),
            0
          )
        )) %>%
        dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
        dplyr::select(mlbam_id, fg_xFIP)
    } else {
      dplyr::tibble(mlbam_id = integer(), fg_xFIP = numeric())
    },
    by = "mlbam_id"
  ) %>%

  # -------------------------------------------------------
  # Role-aware, month-aware availability classification
  # 5 tiers: fresh / available / limited / doubtful / unavailable
  # (injured overrides everything via roster_type check)
  #
  # typical_pitches: from season IP/G × 15 p/IP, else role default
  # Thresholds scaled by month_factor (0.85 Apr, 1.0 May-Aug, 1.1 Sep+)
  #
  #   unavailable — 3 consecutive days OR ≥ 1.5× typical yesterday
  #   doubtful    — ≥ 1.0× typical yesterday (full normal outing)
  #   limited     — any pitches yesterday but < typical, OR 2 consecutive days
  #   available   — pitched in last 7d but rested ≥ 1 day
  #   fresh       — nothing in last 3+ days
  # -------------------------------------------------------

  dplyr::mutate(
    typical_pitches = dplyr::case_when(
      !is.na(mlb_ip) & !is.na(mlb_g) & mlb_g > 0 ~
        (mlb_ip / mlb_g) * 15,
      fg_role %in% names(role_default_pitches) ~
        as.numeric(role_default_pitches[fg_role]),
      TRUE ~ 20
    ),

    availability = dplyr::case_when(
      roster_type %in% c("il-sp", "il-rp")                               ~ "injured",
      consecutive_days >= 3                                               ~ "unavailable",
      pitches_yesterday >= typical_pitches * month_factor * 1.5          ~ "unavailable",
      pitches_yesterday >= typical_pitches * month_factor * 1.0          ~ "doubtful",
      consecutive_days >= 2                                               ~ "limited",
      pitches_yesterday > 0                                               ~ "limited",
      !is.na(days_rest) & days_rest <= 3                                  ~ "available",
      TRUE                                                                ~ "fresh"
    )
  ) %>%

  dplyr::select(
    mlbam_id, player_name, team_abbr,
    dplyr::any_of(c("fg_role", "fg_position")), roster_type,
    dplyr::any_of(c(
      "availability", "typical_pitches", "days_rest", "pitches_yesterday",
      "pitches_last_3_days", "appearances_last_7d", "consecutive_days",
      "last_outing_date"
    )),
    dplyr::any_of(c("mlb_g", "mlb_ip", "mlb_era", "mlb_whip",
                    "mlb_so", "mlb_bb", "mlb_sv", "mlb_hld")),
    dplyr::any_of(c("fg_WAR", "fg_Dollars", "bbref_ERA", "bbref_WHIP")),
    dplyr::everything()
  ) %>%

  dplyr::arrange(team_abbr, dplyr::across(dplyr::any_of("fg_role")))

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

message("99_bullpen_context_join complete: ",
        nrow(bullpen_context), " pitchers across ",
        dplyr::n_distinct(bullpen_context$team_abbr), " teams | month_factor=", month_factor,
        " | unavailable=",  sum(bullpen_context$availability == "unavailable", na.rm = TRUE),
        " | doubtful=",     sum(bullpen_context$availability == "doubtful",    na.rm = TRUE),
        " | limited=",      sum(bullpen_context$availability == "limited",     na.rm = TRUE),
        " | available=",    sum(bullpen_context$availability == "available",   na.rm = TRUE),
        " | fresh=",        sum(bullpen_context$availability == "fresh",       na.rm = TRUE),
        " | injured=",      sum(bullpen_context$availability == "injured",     na.rm = TRUE))
