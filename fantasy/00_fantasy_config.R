# ============================================================
# FANTASY BASEBALL — League Configuration
# ============================================================
# Edit this file to match your league settings.
# All other fantasy scripts source this file.
# ============================================================

# ------------------------------------------------------------
# League Settings
# ------------------------------------------------------------
LEAGUE_TEAMS    <- 10
LEAGUE_FORMAT   <- "H2H_points"
DRAFT_TYPE      <- "snake"   # "snake" or "auction"
SEASON_PROJ     <- 2026

# ------------------------------------------------------------
# Roster Construction (per team)
# ------------------------------------------------------------
ROSTER_SLOTS <- list(
  C    = 1,
  `1B` = 1,
  `2B` = 1,
  `3B` = 1,
  SS   = 1,
  OF   = 3,
  Util = 4,   # any position
  SP   = 5,
  RP   = 3,
  IL   = 3    # injured list (not scored, just holds)
)

# Active scoring roster per team (excludes IL)
ACTIVE_HITTER_SLOTS <- ROSTER_SLOTS$C + ROSTER_SLOTS$`1B` + ROSTER_SLOTS$`2B` +
                       ROSTER_SLOTS$`3B` + ROSTER_SLOTS$SS + ROSTER_SLOTS$OF +
                       ROSTER_SLOTS$Util  # = 12

ACTIVE_PITCHER_SLOTS <- ROSTER_SLOTS$SP + ROSTER_SLOTS$RP  # = 8

# Total starters rostered across the league (sets replacement level depth)
TOTAL_C_ROSTERED   <- LEAGUE_TEAMS * ROSTER_SLOTS$C    # 10
TOTAL_1B_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$`1B` # 10
TOTAL_2B_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$`2B` # 10
TOTAL_3B_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$`3B` # 10
TOTAL_SS_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$SS   # 10
TOTAL_OF_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$OF   # 30
TOTAL_SP_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$SP   # 50
TOTAL_RP_ROSTERED  <- LEAGUE_TEAMS * ROSTER_SLOTS$RP   # 30

# Util adds extra hitter spots — used for replacement level adjustment
TOTAL_UTIL_ROSTERED <- LEAGUE_TEAMS * ROSTER_SLOTS$Util  # 40

# ------------------------------------------------------------
# Batter Scoring Weights
# ------------------------------------------------------------
BAT_WEIGHTS <- list(
  single  = 1,    # Singles (H - 2B - 3B - HR)
  double  = 2,    # Doubles
  triple  = 3,    # Triples
  hr      = 4,    # Home Runs
  r       = 1,    # Runs
  rbi     = 1,    # RBI
  sb      = 2,    # Stolen Bases
  cs      = -1,   # Caught Stealing
  bb      = 1,    # Walks
  hbp     = 1     # Hit By Pitch
  # CYC = 10 pts but too rare to project — ignored
)

# Derived: points per hit by type (for projection use)
# pts_per_hit = single*1 + double*2 + triple*3 + hr*4
# Simplified batter points formula:
#   pts = H + 2B + 2*(3B) + 3*(HR) + R + RBI + 2*SB - CS + BB + HBP
# Derivation: (H-2B-3B-HR)*1 + 2B*2 + 3B*3 + HR*4 + R + RBI + 2SB - CS + BB + HBP
#           = H + 2B + 2*3B + 3*HR + R + RBI + 2*SB - CS + BB + HBP

# ------------------------------------------------------------
# Pitcher Scoring Weights
# ------------------------------------------------------------
PITCH_WEIGHTS <- list(
  win  = 4,     # Wins
  sv   = 2,     # Saves
  out  = 0.33,  # Per OUT recorded (IP * 3 outs/IP * 0.33 = IP * 0.99 ≈ 1 pt/IP)
  er   = -1,    # Earned Runs
  k    = 1,     # Strikeouts
  qs   = 2      # Quality Starts
  # NH = 10, PG = 20 — too rare to project, ignored
)

# Pitcher points formula:
#   pts = IP*0.99 + K + 4*W + 2*SV - ER + 2*QS

# ------------------------------------------------------------
# Projection Blend Weights
# Steamer is the base; our Statcast adjustments modify it
# ------------------------------------------------------------
BLEND_STEAMER_WT  <- 0.65   # weight on Steamer projection  (tuned via backtest 2022-2024)
BLEND_STATCAST_WT <- 0.35   # weight on Statcast-adjusted projection

# ------------------------------------------------------------
# Replacement Level Buffer
# How many extra picks beyond starters (bench + IL churn)
# ------------------------------------------------------------
REPLACEMENT_BUFFER <- 5     # add 5 to each position depth for bench/IL

# ------------------------------------------------------------
# Big Board Size
# 10 teams x 22 roster spots = 220 total drafted players.
# Extra buffer for late-round depth hunting (backup C, streaming SP, etc.)
# ------------------------------------------------------------
BOARD_SIZE <- 450

# ------------------------------------------------------------
# Draft Tiers — VOR breakpoints
# Players are assigned a tier based on their VOR score.
# Tier 1 = elite (200+), Tier 6 = replacement-level (<20).
# Used by the draft app to keep positional targeting within
# the same talent band — never reach a tier down to fill a need.
# Tune these to match your board's natural VOR distribution.
# ------------------------------------------------------------
TIER_BREAKS <- c(200, 150, 100, 60, 20)
# Tier 1: VOR > 200   (~round 1, ~10 players)
# Tier 2: VOR 150–200 (~rounds 2–3, ~25 players)
# Tier 3: VOR 100–150 (~rounds 4–7, ~30 players)
# Tier 4: VOR 60–100  (~rounds 8–11, ~40 players)
# Tier 5: VOR 20–60   (late rounds)
# Tier 6: VOR < 20    (waiver/speculative)

# ------------------------------------------------------------
# Shared Utilities
# ------------------------------------------------------------

# Normalize player names for fuzzy matching across data sources
# Converts accented chars (José→Jose), lowercases, strips non-alpha
normalize_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[^a-z ]") %>%
    stringr::str_squish()
}
