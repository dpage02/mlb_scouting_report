# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 99_write_id_tables.R
# ============================================================
# PURPOSE:
#   Persist all Phase 01 ID tables to disk.
#   This script is the COMMIT POINT for identity data.
#
# DATASETS WRITTEN:
#   - team_ids
#   - league_ids
#   - park_ids
#   - umpire_ids_retrosheet
#
# NOTES:
#   - No transformations occur here
#   - Assumes ID tables already exist in memory
#   - Fails loudly if any expected table is missing
# ============================================================

# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 01_ids
# SCRIPT: 99_write_id_tables.R
# ============================================================

library(readr)
library(glue)
library(purrr)

# ---- Output directory ----
ids_dir <- file.path("data", "ids")

if (!dir.exists(ids_dir)) {
  dir.create(ids_dir, recursive = TRUE)
}

# ---- Expected ID tables (must already exist in memory) ----
expected_tables <- list(
  team_ids              = team_ids,
  league_ids            = league_ids,
  park_ids              = park_ids,
  umpire_ids_retrosheet = umpire_ids_retrosheet
)

# ---- Write each table ----
purrr::iwalk(expected_tables, function(tbl, name) {
  
  out_path <- file.path(ids_dir, paste0(name, ".csv"))
  
  write_csv(tbl, out_path)
  
  log_message(glue(
    "Wrote {name} to {out_path} ({nrow(tbl)} rows)"
  ))
})

# ---- Completion log ----
log_message("Phase 01_ids complete: all ID tables written")

# ============================================================
# END 99_write_id_tables.R
# ============================================================
