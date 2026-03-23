# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 08_game_model
# SCRIPT: 04_matchup_splits.R
# ============================================================
# PURPOSE:
#   Build the matchup-specific split context for each lineup slot.
#   Determines starter handedness, then attaches the applicable
#   batter split (vs RHP or vs LHP) to each batter in lineup_context.
#
#   Also attaches pitcher platoon splits (vs RHH / vs LHH) to
#   starter_matchup for use in the deep dive report.
#
# GRAIN:
#   lineup_context_splits:  one row per batter per side per game_pk
#   starter_splits:         one row per starter per game_pk
#
# INPUT:
#   starter_matchup                  — today's starters with mlbam_id
#   lineup_context                   — today's lineups
#   player_season_mlb_offense_splits — batter vr/vl splits
#   player_season_mlb_pitching_splits — pitcher vr/vl splits
#
# OUTPUT:
#   lineup_context_splits   — lineup enriched with matchup-applicable split
#   starter_splits          — starter platoon splits (vs RHH / vs LHH)
# ============================================================

required_objects <- c(
  "starter_matchup", "lineup_context",
  "player_season_mlb_offense_splits",
  "player_season_mlb_pitching_splits"
)
missing_objects <- required_objects[!required_objects %in% ls()]
if (length(missing_objects) > 0) {
  stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
}

# ------------------------------------------------------------
# Step 1: Get pitcher handedness for today's starters
# Uses mlb_people() to look up pitchHand for each starter
# ------------------------------------------------------------

starter_ids <- unique(starter_matchup$mlbam_id[!is.na(starter_matchup$mlbam_id)])

pitcher_hands <- tryCatch({
  info <- baseballr::mlb_people(
    person_ids = paste(starter_ids, collapse = ",")
  )
  info %>%
    dplyr::select(
      mlbam_id    = id,
      pitch_hand  = dplyr::any_of(c("pitchHand.code", "pitch_hand_code"))
    ) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id))
}, error = function(e) {
  message("Could not fetch pitcher handedness: ", e$message)
  dplyr::tibble(mlbam_id = integer(), pitch_hand = character())
})

# Normalize column name (mlb_people() may vary)
if (!"pitch_hand" %in% names(pitcher_hands)) {
  hand_col <- grep("pitch.hand|pitch_hand", names(pitcher_hands),
                   value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(hand_col)) {
    pitcher_hands <- pitcher_hands %>%
      dplyr::rename(pitch_hand = dplyr::all_of(hand_col))
  }
}

# ------------------------------------------------------------
# Step 2: Attach pitcher handedness to starter_matchup
# Then determine which batter split applies:
#   pitcher throws R → batter's vr split (batter vs RHP)
#   pitcher throws L → batter's vl split (batter vs LHP)
# ------------------------------------------------------------

starter_with_hand <- starter_matchup %>%
  dplyr::left_join(
    pitcher_hands %>% dplyr::select(mlbam_id, pitch_hand),
    by = "mlbam_id"
  ) %>%
  dplyr::mutate(
    # The split the opposing lineup should use
    opp_split_code = dplyr::case_when(
      pitch_hand == "R" ~ "vr",   # batters face RHP → use vr split
      pitch_hand == "L" ~ "vl",   # batters face LHP → use vl split
      TRUE              ~ NA_character_
    )
  )

# Map game_pk + side → opposing side's applicable split_code
# (away lineup faces home starter and vice versa)
game_split_map <- starter_with_hand %>%
  dplyr::select(game_pk, side, opp_split_code) %>%
  dplyr::mutate(
    opp_side = dplyr::if_else(side == "home", "away", "home")
  ) %>%
  dplyr::select(game_pk, side = opp_side, applicable_split = opp_split_code)

# ------------------------------------------------------------
# Step 3: Build lineup_context_splits
# Each batter gets the split that applies against today's starter
# ------------------------------------------------------------

batter_splits_clean <- player_season_mlb_offense_splits %>%
  dplyr::select(
    mlbam_id, split_code,
    sp_pa    = mlb_pa,
    sp_avg   = mlb_avg,
    sp_obp   = mlb_obp,
    sp_slg   = mlb_slg,
    sp_ops   = mlb_ops,
    sp_hr    = mlb_hr,
    sp_bb    = mlb_bb,
    sp_so    = mlb_so,
    sp_babip = mlb_babip
  )

lineup_context_splits <- lineup_context %>%
  dplyr::left_join(game_split_map, by = c("game_pk", "side")) %>%
  dplyr::left_join(
    batter_splits_clean,
    by = c("mlbam_id", "applicable_split" = "split_code")
  ) %>%
  dplyr::mutate(
    split_label = dplyr::case_when(
      applicable_split == "vr" ~ "vs RHP",
      applicable_split == "vl" ~ "vs LHP",
      TRUE                     ~ "Overall"
    )
  ) %>%
  dplyr::select(
    game_pk, game_date, side, team_name, team_abbr,
    batting_slot, mlbam_id, player_name, fg_position,
    split_label, applicable_split,
    sp_pa, sp_avg, sp_obp, sp_slg, sp_ops, sp_hr, sp_bb, sp_so,
    # Keep overall stats too
    mlb_avg, mlb_obp, mlb_slg, mlb_ops, mlb_hr, mlb_rbi,
    dplyr::any_of("fg_wRC_plus")
  ) %>%
  dplyr::arrange(game_pk, side, batting_slot)

# ------------------------------------------------------------
# Step 4: Attach pitcher platoon splits to starter_matchup
# ------------------------------------------------------------

pitcher_splits_clean <- player_season_mlb_pitching_splits %>%
  dplyr::select(
    mlbam_id, split_code,
    ps_ip    = mlb_ip,
    ps_era   = mlb_era,
    ps_whip  = mlb_whip,
    ps_avg   = mlb_avg,
    ps_obp   = mlb_obp,
    ps_slg   = mlb_slg,
    ps_ops   = mlb_ops,
    ps_hr    = mlb_hr,
    ps_bb    = mlb_bb,
    ps_so    = mlb_so
  )

# Add vs RHH and vs LHH as wide columns on starter_matchup
starter_splits <- starter_with_hand %>%
  dplyr::left_join(
    pitcher_splits_clean %>%
      dplyr::filter(split_code == "vr") %>%
      dplyr::select(-split_code) %>%
      dplyr::rename_with(~ paste0(.x, "_vr"), -mlbam_id),
    by = "mlbam_id"
  ) %>%
  dplyr::left_join(
    pitcher_splits_clean %>%
      dplyr::filter(split_code == "vl") %>%
      dplyr::select(-split_code) %>%
      dplyr::rename_with(~ paste0(.x, "_vl"), -mlbam_id),
    by = "mlbam_id"
  )

# ------------------------------------------------------------
# Validate
# ------------------------------------------------------------

# Write pitch_hand back to starter_matchup so the report helper can display it
starter_matchup <- starter_matchup %>%
  dplyr::left_join(
    pitcher_hands %>% dplyr::select(mlbam_id, pitch_hand),
    by = "mlbam_id"
  )

splits_filled <- sum(!is.na(lineup_context_splits$sp_avg))
splits_total  <- nrow(lineup_context_splits)
hand_filled   <- sum(!is.na(starter_with_hand$pitch_hand))

message("04_matchup_splits complete:")
message("  Lineup splits: ", splits_filled, "/", splits_total,
        " batters with applicable split (",
        round(splits_filled / splits_total * 100), "%)")
message("  Pitcher hands identified: ", hand_filled, "/",
        nrow(starter_with_hand), " starters")
message("  Split labels: ",
        paste(table(lineup_context_splits$split_label), collapse = " | "))
