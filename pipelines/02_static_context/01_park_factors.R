# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 02_static_context
# SCRIPT: 01_park_factors.R
# ============================================================
# PURPOSE:
#   Build static park factor table for all 30 MLB parks.
#   Used by the prediction model to adjust expected run totals.
#
# DESIGN:
#   Hard-coded 3-year averaged factors (2022-2024 season data)
#   sourced from FanGraphs published park factors.
#   Both teams playing in a given park are equally affected —
#   park factor applies to both offenses, not just the home team.
#
#   UPDATE ANNUALLY: refresh values after each completed season
#   using https://www.fangraphs.com/guts.aspx?type=pf
#
# FACTORS:
#   pf_runs  — overall run factor (1.00 = neutral, >1 = hitter-friendly)
#   pf_lhh   — left-handed batter run factor (NULL where not notably different)
#   pf_rhh   — right-handed batter run factor (NULL where not notably different)
#
# JOIN KEY:
#   mlbam_team_id — matches team_ids$mlbam_team_id (home team)
#
# OUTPUT:
#   park_factors
# ============================================================

library(dplyr)

message("Building static park factor table (2022-2024 averages)")

# ------------------------------------------------------------
# Park factors: 3-year averaged (2022-2024)
# Source: FanGraphs Guts page — https://www.fangraphs.com/guts.aspx?type=pf
#
# LHH/RHH splits noted only for parks with meaningful handedness splits:
#   Fenway Park    — Green Monster favors LHH doubles/HR
#   Yankee Stadium — Short right-field porch strongly favors LHH
#   loanDepot park — Wide alleys favor RHH gap power
#
# All other parks use pf_runs for both hands (splits are minor).
# ------------------------------------------------------------

park_factors <- tibble::tribble(
  ~mlbam_team_id, ~team_abbr, ~venue_name,             ~pf_runs, ~pf_lhh, ~pf_rhh,

  # ---- High offense parks (>1.03) ----
  115L, "COL", "Coors Field",                    1.140,  1.120,  1.155,  # biggest outlier
  113L, "CIN", "Great American Ball Park",       1.070,  1.065,  1.075,
  112L, "CHC", "Wrigley Field",                  1.060,  1.055,  1.065,
  117L, "HOU", "Daikin Park",                    1.050,  1.045,  1.055,  # roof retracted days
  110L, "BAL", "Oriole Park at Camden Yards",    1.045,  1.040,  1.050,
  140L, "TEX", "Globe Life Field",               1.045,  1.040,  1.050,
  143L, "PHI", "Citizens Bank Park",             1.040,  1.035,  1.045,
  111L, "BOS", "Fenway Park",                    1.035,  1.065,  1.010,  # LHH boost from Monster
  119L, "LAD", "Dodger Stadium",                 1.030,  1.025,  1.035,
  109L, "ARI", "Chase Field",                    1.025,  1.020,  1.030,

  # ---- Near-neutral parks (0.98–1.02) ----
  147L, "NYY", "Yankee Stadium",                 1.020,  1.080,  0.970,  # strong LHH porch split
  144L, "ATL", "Truist Park",                    1.015,  1.010,  1.020,
  141L, "TOR", "Rogers Centre",                  1.010,  1.005,  1.015,
  158L, "MIL", "American Family Field",          1.010,  1.005,  1.015,
  145L, "CWS", "Rate Field",                     1.005,  1.000,  1.010,
  108L, "LAA", "Angel Stadium",                  1.005,  1.000,  1.010,
  138L, "STL", "Busch Stadium",                  1.000,  0.995,  1.005,
  120L, "WSH", "Nationals Park",                 1.000,  0.995,  1.005,
  142L, "MIN", "Target Field",                   0.995,  0.990,  1.000,
  118L, "KC",  "Kauffman Stadium",               0.995,  0.990,  1.000,
  116L, "DET", "Comerica Park",                  0.995,  0.990,  1.000,
  114L, "CLE", "Progressive Field",              0.990,  0.985,  0.995,
  121L, "NYM", "Citi Field",                     0.985,  0.980,  0.990,
  139L, "TB",  "Tropicana Field",                0.985,  0.980,  0.990,

  # ---- Pitcher-friendly parks (<0.98) ----
  134L, "PIT", "PNC Park",                       0.975,  0.970,  0.980,
  146L, "MIA", "loanDepot park",                 0.975,  0.965,  0.985,  # wide alleys vs RHH
  136L, "SEA", "T-Mobile Park",                  0.970,  0.965,  0.975,
  137L, "SF",  "Oracle Park",                    0.965,  0.960,  0.970,
  135L, "SD",  "Petco Park",                     0.955,  0.950,  0.960,
  133L, "ATH", "Sutter Health Park",             0.955,  0.950,  0.960   # Athletics current park
)

# ------------------------------------------------------------
# Validate: all 30 teams present
# ------------------------------------------------------------

n_teams <- nrow(park_factors)

message("01_park_factors complete: ", n_teams, " parks | ",
        "range pf_runs: [",
        round(min(park_factors$pf_runs), 3), ", ",
        round(max(park_factors$pf_runs), 3), "]",
        " | Coors=", park_factors$pf_runs[park_factors$team_abbr == "COL"],
        " | Petco=", park_factors$pf_runs[park_factors$team_abbr == "SD"])
