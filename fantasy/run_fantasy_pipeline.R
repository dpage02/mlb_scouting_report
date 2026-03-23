# ============================================================
# FANTASY BASEBALL — Run Full Pipeline + Launch Draft App
# ============================================================
#
# RUN THIS TONIGHT TO BUILD YOUR BIG BOARD:
#   source("fantasy/run_fantasy_pipeline.R")
#
# ON DRAFT DAY TO LAUNCH THE APP (if board already built):
#   shiny::runApp("fantasy/draft_app")
#
# WHAT IT DOES:
#   1. Loads player data (from cache or full pipeline)
#   2. Pulls consensus projections (5 FG systems averaged)
#   3. Blends with our Statcast adjustments
#   4. Pulls ADP from FantasyPros + Yahoo
#   5. Builds big board with VOR rankings + ADP value
#   6. Generates printable HTML cheat sheet → reports/
#   7. Saves board for the Shiny app
#   8. Launches the two-team draft app
# ============================================================

if (basename(getwd()) == "fantasy") setwd("..")

message(""); message("╔══════════════════════════════════════╗")
message("║   FANTASY BASEBALL PIPELINE          ║")
message("╚══════════════════════════════════════╝"); message("")

# ── Step 1: Load player data ─────────────────────────────────
message("[1/5] Loading player data...")
cache_path <- "data/pipeline_cache.rds"
cache_ok   <- file.exists(cache_path) &&
              difftime(Sys.time(), file.mtime(cache_path), units = "hours") < 24

if (cache_ok) {
  message("  Using cached pipeline data")
  cache <- readRDS(cache_path)
  list2env(cache, envir = .GlobalEnv)
  rm(cache)
} else {
  message("  Running full scouting pipeline (5-10 min)...")
  invisible(capture.output(source("run_pipeline_phase01.R")))
}

if (!exists("player_master_ids") || nrow(player_master_ids) == 0) {
  stop("player_master_ids not found. Run run_pipeline_phase01.R first.")
}
message("  Player data ready: ", nrow(player_master_ids), " players in ID map")

# ── Step 2: Consensus projections ────────────────────────────
message("[2/5] Pulling consensus projections (5 systems)...")
source("fantasy/01_consensus_projections.R")

# ── Step 3: Blend with Statcast ──────────────────────────────
message("[3/5] Blending with Statcast adjustments...")
source("fantasy/02_blend_projections.R")

# ── Step 4: Build big board ──────────────────────────────────
message("[4/5] Building big board + VOR rankings...")
source("fantasy/03_big_board.R")

# ── Step 4b: Pull ADP ────────────────────────────────────────
message("  Pulling ADP rankings...")
tryCatch(
  source("fantasy/01b_adp_rankings.R"),
  error = function(e) message("  ADP pull failed (", e$message, ") — continuing without ADP")
)

# Re-run big board if ADP was added after initial build
adp_available <- exists("adp_master") && is.data.frame(adp_master) &&
                 nrow(adp_master) > 0 && "adp" %in% names(adp_master)
board_has_adp  <- "adp" %in% names(big_board) && sum(!is.na(big_board$adp)) > 0

if (adp_available && !board_has_adp) {
  message("  Merging ADP into big board...")
  adp_clean <- adp_master %>%
    dplyr::filter(!is.na(fg_id)) %>%
    dplyr::arrange(adp) %>%
    dplyr::distinct(fg_id, .keep_all = TRUE) %>%   # prevent many-to-many
    dplyr::select(fg_id, adp, adp_fantasypros, adp_yahoo, fp_rank)

  big_board <- big_board %>%
    dplyr::left_join(adp_clean, by = "fg_id") %>%
    dplyr::mutate(
      value_vs_adp = dplyr::if_else(!is.na(adp), round(adp - overall_rank), NA_integer_)
    )
  saveRDS(big_board, "fantasy/draft_app/data/big_board.rds")
  message("  ADP merged: ", sum(!is.na(big_board$adp)), " players matched")
} else if (!adp_available) {
  message("  No ADP data available — board saved without ADP")
}

# ── Step 5: Printable cheat sheet ────────────────────────────
message("[5/5] Generating printable big board...")
tryCatch(
  source("fantasy/04_print_big_board.R"),
  error = function(e) message("  Print board failed: ", e$message)
)

# ── Summary ───────────────────────────────────────────────────
message("")
message("╔══════════════════════════════════════╗")
message("║   PIPELINE COMPLETE                  ║")
message("╚══════════════════════════════════════╝")
message("  Players ranked: ", nrow(big_board))
message("  With ADP data:  ", sum(!is.na(big_board$adp)))
message("")
print_path <- list.files("reports", pattern="^fantasy_big_board", full.names=TRUE)
if (length(print_path) > 0) {
  message("  Printable board: ", tail(print_path, 1))
  message("  Open in browser → Cmd+P to print")
}
message("")
message("══════════════════════════════════════════")
message("  TO LAUNCH DRAFT APP:")
message('  shiny::runApp("fantasy/draft_app")')
message("══════════════════════════════════════════")
message("")

if (interactive()) {
  message("Launching draft app...")
  shiny::runApp("fantasy/draft_app")
}
