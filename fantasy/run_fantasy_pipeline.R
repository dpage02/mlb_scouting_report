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

# ── Step 3.5: Pull ADP + Yahoo positions ─────────────────────
# Run BEFORE big board so Yahoo position eligibility is available
# during VOR calculation and position filtering
message("  Pulling ADP + Yahoo positions...")
tryCatch(
  source("fantasy/01b_adp_rankings.R"),
  error = function(e) message("  ADP pull failed (", e$message, ") — continuing without ADP")
)

# ── Step 4: Build big board ──────────────────────────────────
message("[4/5] Building big board + VOR rankings...")
source("fantasy/03_big_board.R")

# Re-merge ADP only if we got actual non-NA ADP values from scraping
adp_has_data  <- exists("adp_master") && is.data.frame(adp_master) &&
                 "adp" %in% names(adp_master) && sum(!is.na(adp_master$adp)) > 0
board_has_adp <- "adp" %in% names(big_board) && sum(!is.na(big_board$adp)) > 0

if (adp_has_data && !board_has_adp) {
  message("  Merging ADP into big board...")
  adp_clean <- adp_master %>%
    dplyr::filter(!is.na(fg_id), !is.na(adp)) %>%
    dplyr::arrange(adp) %>%
    dplyr::distinct(fg_id, .keep_all = TRUE) %>%
    dplyr::select(fg_id, adp, adp_fantasypros, adp_yahoo, fp_rank)

  adp_cols <- c("adp","adp_fantasypros","adp_yahoo","fp_rank","value_vs_adp")
  big_board <- big_board %>%
    dplyr::select(-dplyr::any_of(adp_cols)) %>%
    dplyr::left_join(adp_clean, by = "fg_id") %>%
    dplyr::mutate(
      value_vs_adp = dplyr::if_else(!is.na(adp), round(adp - overall_rank), NA_integer_)
    )
  saveRDS(big_board, "fantasy/draft_app/data/big_board.rds")
  message("  ADP merged: ", sum(!is.na(big_board$adp)), " players matched")
} else {
  message("  ADP: no new data to merge (scraping returned 0 matches — using VOR rankings)")
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

# ============================================================
# OPTIONAL: Run backtesting to validate/tune blend weights
# ============================================================
# Pulls 2022-2024 actual stats, grid-searches weight combos,
# and reports which weights minimize RMSE vs actual fpts.
# Run this separately — takes a few minutes.
#
#   source("fantasy/05_backtest.R")
#
# After reviewing results, update in 00_fantasy_config.R:
#   BLEND_STEAMER_WT  <- <optimal_steamer_wt>
#   BLEND_STATCAST_WT <- 1 - BLEND_STEAMER_WT
# And in 02_blend_projections.R update the signal coefficients:
#   brl_adj_factor coef (currently 0.012)
#   xba blend weight (currently 0.35)
#   sprint speed coef (currently 0.06)
# ============================================================
