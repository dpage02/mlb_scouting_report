# ============================================================
# 05 PERFORMANCE — GRAIN DEFINITION
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 01_column_dictionary.R
# ============================================================

# -----------------------------
# Source Prefix Rules
# -----------------------------
# All metrics must be prefixed by source:
#
# mlb_     = MLB Stats API
# fg_      = FanGraphs
# sc_      = Statcast
# bref_    = Baseball Reference
# lahman_  = Lahman Database
#
# Example:
# fg_wRC_plus
# sc_barrel_pct
# mlb_OBP
# bref_WAR
# lahman_HR

performance_source_prefixes <- c(
  "mlb_",
  "fg_",
  "sc_",
  "bref_",
  "lahman_"
)

# -----------------------------
# Naming Standards
# -----------------------------
# 1. All lowercase
# 2. Underscores only
# 3. No spaces
# 4. Percent stats use _pct
# 5. Per 9 stats use _per9
# 6. No duplicated metric names without source prefix

naming_rules <- list(
  lowercase = TRUE,
  use_underscores = TRUE,
  percent_suffix = "_pct",
  per9_suffix = "_per9"
)

# -----------------------------
# Non-Metric Columns
# -----------------------------
# These columns must NOT be prefixed:

performance_key_columns <- c(
  "mlbam_id",
  "season",
  "team_abbr",
  "player_name",
  "team_name"
)
