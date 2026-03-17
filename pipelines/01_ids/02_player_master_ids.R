# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 02_player_master_ids.R
# ============================================================
# PURPOSE:
#   Build the GLOBAL player identity spine (player_master_ids).
#
# THIS TABLE IS:
#   - Season agnostic
#   - One row per human
#   - Pure identity (no roster, no stats, no eligibility)
#
# SOURCES:
#   - Lahman::People (historical backbone)
#   - baseballr::chadwick_player_lu() (ID reconciliation)
#
# OUTPUT:
#   - player_master_ids
#
# DESIGN PRINCIPLES:
#   - Stable
#   - Deterministic
#   - No API season calls
#   - No Statcast dependency
# ============================================================

library(dplyr)
library(baseballr)
library(Lahman)
library(stringr)

message("Running 02_player_master_ids.R")

# ------------------------------------------------------------
# 1. Lahman backbone
# ------------------------------------------------------------

people <- Lahman::People %>%
  as_tibble() %>%
  select(
    lahman_id = playerID,
    bbref_id  = bbrefID,
    name_first = nameFirst,
    name_last  = nameLast,
    birth_year = birthYear,
    birth_month = birthMonth,
    birth_day   = birthDay
  ) %>%
  mutate(
    birth_date = suppressWarnings(
      as.Date(paste(birth_year, birth_month, birth_day, sep = "-"))
    ),
    player_name = str_trim(paste(name_first, name_last))
  ) %>%
  select(
    lahman_id,
    bbref_id,
    player_name,
    name_first,
    name_last,
    birth_date
  )

message("Lahman players loaded: ", nrow(people))

# ------------------------------------------------------------
# 2. Chadwick crosswalk (BBRef ↔ MLBAM)
# ------------------------------------------------------------

chadwick <- baseballr::chadwick_player_lu() %>%
  as_tibble() %>%
  select(
    bbref_id  = key_bbref,
    mlbam_id  = key_mlbam,
    fg_id     = key_fangraphs
  ) %>%
  distinct()

message("Chadwick lookup loaded: ", nrow(chadwick))

# ------------------------------------------------------------
# 3. Merge Lahman + Chadwick
# ------------------------------------------------------------

player_master_ids <- people %>%
  left_join(chadwick, by = "bbref_id") %>%
  mutate(
    player_master_id = row_number()
  ) %>%
  select(
    player_master_id,
    lahman_id,
    bbref_id,
    mlbam_id,
    fg_id,
    player_name,
    name_first,
    name_last,
    birth_date
  )

# removing NA's
player_master_ids <- player_master_ids %>%
  filter(!is.na(mlbam_id))

# enforcing uniqueness
player_master_ids <- player_master_ids %>%
  distinct(mlbam_id, .keep_all = TRUE)

# creating hard integirty rule
stopifnot(
  nrow(player_master_ids) ==
    n_distinct(player_master_ids$mlbam_id)
)

# ------------------------------------------------------------
# 4. Integrity checks
# ------------------------------------------------------------

stopifnot(!anyDuplicated(player_master_ids$player_master_id))

message("player_master_ids built: ", nrow(player_master_ids))

# ============================================================
# END 02_player_master_ids.R
# ============================================================
