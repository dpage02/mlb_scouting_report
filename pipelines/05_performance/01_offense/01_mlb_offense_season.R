# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_mlb_offense_season.R
# ============================================================
# PURPOSE:
#   Construct league-wide season-level MLB offense fact table.
#
# GRAIN:
#   One row per mlbam_id per season per team_abbr
#
# DATA SOURCE:
#   MLB Stats API (league leaderboard)
#
# DEFAULT BEHAVIOR:
#   - Pulls entire MLB season leaderboard
#   - No team filtering
#
# OUTPUT:
#   - player_season_mlb_offense
#
# DESIGN:
#   League-wide warehouse layer.
#   Neutral.
#   No team bias baked in.
# ============================================================


# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

source("pipelines/05_performance/00_schema/00_grain_definition.R")



# ------------------------------------------------------------
# Pull League-Wide Season Hitting Leaderboard
# ------------------------------------------------------------

season_to_pull <- target_season

# If preseason (no data yet), fall back one year for dev
mlb_league_raw <- baseballr::mlb_stats(
  stat_type = "season",
  stat_group = "hitting",
  player_pool = "all",
  season = season_to_pull,
  limit = 3000
)

if (!"player_id" %in% colnames(mlb_league_raw)) {
  message("No official stats found for ", season_to_pull,
          ". Falling back to ", season_to_pull - 1)
  
  season_to_pull <- season_to_pull - 1
  
  mlb_league_raw <- baseballr::mlb_stats(
    stat_type = "season",
    stat_group = "hitting",
    player_pool = "all",
    season = season_to_pull,
    limit = 3000
  )
}

# ------------------------------------------------------------
# Build League-Wide Fact Table
# ------------------------------------------------------------

player_season_mlb_offense <- mlb_league_raw %>%
  dplyr::transmute(
    
    # -------------------------
    # Canonical Keys
    # -------------------------
    mlbam_id = player_id,
    season = as.integer(season),
    team_abbr = team_name,   # optional: use team_name or derive abbr separately
    team_id = team_id,
    
    # -------------------------
    # Counting Stats
    # -------------------------
    mlb_pa   = plate_appearances,
    mlb_ab   = at_bats,
    mlb_h    = hits,
    mlb_2b   = doubles,
    mlb_3b   = triples,
    mlb_hr   = home_runs,
    mlb_bb   = base_on_balls,
    mlb_ibb  = intentional_walks,
    mlb_so   = strike_outs,
    mlb_hbp  = hit_by_pitch,
    mlb_sf   = sac_flies,
    mlb_sh   = sac_bunts,
    mlb_gidp = ground_into_double_play,
    mlb_r    = runs,
    mlb_rbi  = rbi,
    mlb_tb   = total_bases,
    
    # -------------------------
    # Rate Stats
    # -------------------------
    mlb_avg   = as.numeric(avg),
    mlb_obp   = as.numeric(obp),
    mlb_slg   = as.numeric(slg),
    mlb_ops   = as.numeric(ops),
    mlb_babip = as.numeric(babip),
    
    # -------------------------
    # Baserunning
    # -------------------------
    mlb_sb     = stolen_bases,
    mlb_cs     = caught_stealing,
    mlb_sb_pct = as.numeric(stolen_base_percentage)
    
  )


# ------------------------------------------------------------
# Validate Grain
# ------------------------------------------------------------

validate_performance_table(player_season_mlb_offense)


# ------------------------------------------------------------
# Completion Message
# ------------------------------------------------------------

message("01_mlb_offense_season complete: ",
        nrow(player_season_mlb_offense),
        " league-wide player-season rows created.")
