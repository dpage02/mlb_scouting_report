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
# Join availability
# ------------------------------------------------------------

bullpen_context <- active_pitchers %>%

  dplyr::left_join(bullpen_availability, by = "mlbam_id") %>%

  # Mark pitchers with no recent log as fresh
  dplyr::mutate(
    availability        = dplyr::coalesce(availability, "fresh"),
    days_rest           = dplyr::coalesce(days_rest, NA_integer_),
    pitches_yesterday   = dplyr::coalesce(pitches_yesterday, 0L),
    pitches_last_3_days = dplyr::coalesce(pitches_last_3_days, 0L),
    appearances_last_7d = dplyr::coalesce(appearances_last_7d, 0L),
    consecutive_days    = dplyr::coalesce(consecutive_days, 0L)
  ) %>%

  # Override: IL pitchers are always unavailable
  dplyr::mutate(
    availability = dplyr::if_else(
      roster_type %in% c("il-sp", "il-rp"),
      "injured",
      availability
    )
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

  dplyr::select(
    mlbam_id, player_name, team_abbr,
    dplyr::any_of(c("fg_role", "fg_position")), roster_type,
    dplyr::any_of(c(
      "availability", "days_rest", "pitches_yesterday",
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
        dplyr::n_distinct(bullpen_context$team_abbr), " teams | ",
        sum(bullpen_context$availability == "unavailable", na.rm = TRUE), " unavailable | ",
        sum(bullpen_context$availability == "injured",     na.rm = TRUE), " injured | ",
        sum(bullpen_context$availability == "fresh",       na.rm = TRUE), " fresh")
