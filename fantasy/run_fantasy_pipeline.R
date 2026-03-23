# ============================================================
# FANTASY BASEBALL — Run Full Pipeline
# ============================================================
# Run this script to build the big board and launch the app.
#
# USAGE (from project root):
#   source("fantasy/run_fantasy_pipeline.R")
#   — or —
#   Rscript fantasy/run_fantasy_pipeline.R
#
# WHAT IT DOES:
#   1. Runs the main scouting pipeline (loads all player data)
#   2. Pulls Steamer 2026 projections from FanGraphs
#   3. Blends with our Statcast/FG historical data
#   4. Builds big board with VOR rankings
#   5. Launches the Shiny draft app
# ============================================================

# Ensure we are at project root
if (basename(getwd()) == "fantasy") setwd("..")

# ── Step 1: Load scouting pipeline data ─────────────────────
message("=== FANTASY PIPELINE ===")
message("[1/4] Loading player data...")

# Use cache if it exists and is fresh, otherwise run full pipeline
cache_path <- "data/pipeline_cache.rds"
cache_ok   <- file.exists(cache_path) &&
              difftime(Sys.time(), file.mtime(cache_path), units = "hours") < 24

if (cache_ok) {
  message("  Using cached pipeline data (< 24h old)")
  cache <- readRDS(cache_path)
  list2env(cache, envir = .GlobalEnv)
  rm(cache)
} else {
  message("  Running full pipeline (no fresh cache found)...")
  invisible(capture.output(source("run_pipeline_phase01.R")))
}

# Verify player_master_ids exists — required for FG ID joins
if (!exists("player_master_ids") || nrow(player_master_ids) == 0) {
  stop("player_master_ids not found. Run run_pipeline_phase01.R first.")
}

# ── Step 2: Pull Steamer projections ────────────────────────
message("[2/4] Pulling Steamer projections...")
source("fantasy/01_steamer_projections.R")

# ── Step 3: Blend with Statcast data ────────────────────────
message("[3/4] Blending projections with Statcast adjustments...")
source("fantasy/02_blend_projections.R")

# ── Step 4: Build big board ─────────────────────────────────
message("[4/4] Building big board and calculating VOR...")
source("fantasy/03_big_board.R")

message("")
message("=== PIPELINE COMPLETE ===")
message("Big board: ", nrow(big_board), " players")
message("")
message("Launch the draft app with:")
message('  shiny::runApp("fantasy/draft_app")')
message("")

# Auto-launch app
if (interactive()) {
  message("Launching draft app...")
  shiny::runApp("fantasy/draft_app")
}
