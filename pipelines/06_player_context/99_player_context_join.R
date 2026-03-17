# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 06_player_context
# SCRIPT: 99_player_context_join.R
# ============================================================
# PURPOSE:
#   Combine player_season_scope with depth chart roles to build
#   the full player context table used by downstream modules.
#
# JOIN LOGIC:
#   player_season_scope (spine) — who is in the league universe
#   depth_charts → join on mlbam_id for role, team, position
#
# KEY OUTPUT COLUMNS:
#   mlbam_id       — canonical player ID
#   player_name    — display name
#   team_abbr      — current team abbreviation
#   fg_role        — SP1/SP2/CL/SU/MID/LR/batting slot etc.
#   fg_position    — SP/RP/C/1B/2B/SS/3B/OF/DH
#   roster_type    — mlb-sp/mlb-bp/mlb-sl/il-sp/il-rp/off-*
#   is_pitcher     — TRUE if SP or RP
#   is_starter     — TRUE if SP1-SP5
#   is_closer      — TRUE if CL
#   is_active_mlb  — TRUE if on active 25/26-man (mlb-sp/mlb-bp/mlb-sl)
#   on_40man       — from player_season_scope
#
# OUTPUT:
#   player_context
# ============================================================

required_objects <- c("player_season_scope", "depth_charts")
missing_objects  <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Active MLB depth chart only (drop org/minor league rows)
# ------------------------------------------------------------

depth_active <- depth_charts %>%
  dplyr::select(mlbam_id, team_abbr, fg_team_abbr,
                fg_role, fg_position, roster_type,
                dplyr::any_of(c(
                  "proj_pit_WAR", "proj_bat_WAR",
                  "proj_pit_ERA", "proj_pit_IP",
                  "proj_pit_GS",  "proj_pit_SV",
                  "proj_bat_OPS", "proj_bat_wRC+"
                ))) %>%
  dplyr::distinct(mlbam_id, .keep_all = TRUE)

# ------------------------------------------------------------
# Join
# ------------------------------------------------------------

player_context <- player_season_scope %>%
  dplyr::left_join(depth_active, by = "mlbam_id") %>%

  # Derive role flags
  dplyr::mutate(
    is_pitcher    = fg_position %in% c("SP", "RP", "SP/RP"),
    is_starter    = stringr::str_detect(fg_role, "^SP[1-5]$"),
    is_closer     = fg_role == "CL",
    is_setup      = stringr::str_detect(fg_role, "^SU"),
    is_active_mlb = roster_type %in% c("mlb-sp", "mlb-bp", "mlb-sl")
  ) %>%

  # Coalesce player_name from depth chart if missing from master IDs
  dplyr::mutate(
    player_name = dplyr::coalesce(
      player_name,
      depth_charts$player_name[match(mlbam_id, depth_charts$mlbam_id)]
    )
  ) %>%

  dplyr::select(
    mlbam_id, player_name, team_abbr, fg_team_abbr,
    fg_role, fg_position, roster_type,
    is_pitcher, is_starter, is_closer, is_setup, is_active_mlb,
    on_40man, appeared_in_games, active_this_season,
    dplyr::everything()
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

n_active_starters <- sum(player_context$is_starter,  na.rm = TRUE)
n_active_closers  <- sum(player_context$is_closer,   na.rm = TRUE)
n_active_mlb      <- sum(player_context$is_active_mlb, na.rm = TRUE)

if (n_active_closers < 28) {
  warning("Only ", n_active_closers, " closers identified — expected ~30")
}

message("99_player_context_join complete: ",
        nrow(player_context), " players | ",
        n_active_starters, " rotation starters | ",
        n_active_closers,  " closers | ",
        n_active_mlb,      " active MLB roster players")
