# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 04_umpire_ids_retrosheet.R
# ============================================================
# PURPOSE:
#   Build a canonical, season-scoped umpire identity table
#   using Retrosheet UMPIRESYYYY.TXT files.
#
# DATASETS CREATED:
#   - umpire_ids_retrosheet   (Retrosheet-based identity)
#   - umpire_ids              (pipeline-canonical identity)
#
# DESIGN NOTES:
#   - Retrosheet umpire files are SEASON-SPECIFIC
#   - Availability is inconsistent across seasons
#   - This table represents "who is an MLB umpire THIS season"
#   - Game assignments and roles are handled later in game_context
#   - MLBAM umpire IDs are NOT included here by design
#
# SOURCE:
#   https://github.com/chadwickbureau/retrosheet
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(glue)
library(httr)

# ---- Parameters ----
# Retrosheet umpire files are NOT available for every season.
# Attempt to use DEFAULT_SEASON, but fall back if unavailable.
season_requested <- DEFAULT_SEASON

build_retrosheet_url <- function(season) {
  paste0(
    "https://raw.githubusercontent.com/chadwickbureau/retrosheet/master/seasons/",
    season,
    "/UMPIRES",
    season,
    ".TXT"
  )
}

retrosheet_url <- build_retrosheet_url(season_requested)

# ---- Validate availability ----
url_status <- try(httr::status_code(httr::HEAD(retrosheet_url)), silent = TRUE)

if (inherits(url_status, "try-error") || url_status != 200) {
  log_message(glue(
    "Retrosheet umpire file not found for season {season_requested}. Falling back to 2025."
  ))
  season <- 2025
  retrosheet_url <- build_retrosheet_url(season)
} else {
  season <- season_requested
}

# ============================================================
# READ RAW RETROSHEET FILE
# ============================================================
# Expected format:
#   ID,last,first

umpires_raw <- read_delim(
  retrosheet_url,
  delim = ",",
  show_col_types = FALSE
)

# ============================================================
# BUILD umpire_ids_retrosheet
# ============================================================

umpire_ids_retrosheet <- umpires_raw %>%
  transmute(
    retrosheet_umpire_id = ID,
    first_name           = str_squish(first),
    last_name            = str_squish(last),
    umpire_name          = str_squish(paste(first, last)),
    season               = season,
    is_active             = TRUE,
    source               = "retrosheet"
  ) %>%
  distinct() %>%
  arrange(retrosheet_umpire_id) %>%
  mutate(
    umpire_id = row_number()
  ) %>%
  relocate(umpire_id, .before = retrosheet_umpire_id)

# ============================================================
# BUILD pipeline-canonical umpire_ids
# ============================================================
# NOTE:
#   This is a thin canonical wrapper used for consistency with
#   other ID tables. It intentionally mirrors the retrosheet table
#   for now and can be extended later with MLBAM mappings.

umpire_ids <- umpire_ids_retrosheet %>%
  select(
    umpire_id,
    retrosheet_umpire_id,
    umpire_name,
    season,
    is_active,
    source
  )

# ---- Integrity checks ----
stopifnot(!anyDuplicated(umpire_ids_retrosheet$retrosheet_umpire_id))
stopifnot(!anyDuplicated(umpire_ids$retrosheet_umpire_id))

# ---- Logging ----
log_message(glue(
  "umpire_ids_retrosheet built for season {season}: {nrow(umpire_ids_retrosheet)} rows"
))

# ============================================================
# END 04_umpire_ids_retrosheet.R
# ============================================================
