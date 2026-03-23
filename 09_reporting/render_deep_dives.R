# ============================================================
# mlb_scouting_report
# SCRIPT: render_deep_dives.R
# ============================================================
# PURPOSE:
#   Render one game deep dive HTML per game in game_context.
#   Outputs to reports/ alongside the daily scouting report.
#
# USAGE:
#   source("09_reporting/render_deep_dives.R")
#   — run after run_pipeline_phase01.R and render_report.R
#
# OUTPUT FILES:
#   reports/deepdive_2026-03-18_HOU_STL.html  (one per game)
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Package 'quarto' is required.")
}

if (!dir.exists("reports")) dir.create("reports")

report_date <- tryCatch(
  as.character(target_date),
  error = function(e) as.character(Sys.Date())
)

message("Rendering deep dives for ", report_date,
        " — ", nrow(game_context), " games...")

# ------------------------------------------------------------
# Build filename for each game
# ------------------------------------------------------------

game_files <- game_context %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, away_abbr = team_abbr),
    by = c("away_team_id" = "mlbam_team_id")
  ) %>%
  dplyr::left_join(
    team_ids %>% dplyr::select(mlbam_team_id, home_abbr = team_abbr),
    by = c("home_team_id" = "mlbam_team_id")
  ) %>%
  dplyr::mutate(
    away_abbr    = dplyr::coalesce(away_abbr, as.character(away_team_id)),
    home_abbr    = dplyr::coalesce(home_abbr, as.character(home_team_id)),
    output_file  = paste0("deepdive_", report_date, "_",
                          away_abbr, "_", home_abbr, ".html"),
    final_path   = file.path("reports", output_file)
  )

# ------------------------------------------------------------
# Render each game
# ------------------------------------------------------------

rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row      <- game_files[i, ]
  gpk      <- row$game_pk
  matchup  <- sprintf("%s @ %s", row$away_team_name, row$home_team_name)
  out_file <- row$output_file

  message(sprintf("  [%d/%d] %s...", i, nrow(game_files), matchup))

  tryCatch({
    quarto::quarto_render(
      input          = "09_reporting/mlb_game_deepdive.qmd",
      execute_params = list(
        game_pk   = gpk,
        game_date = report_date
      ),
      output_file = out_file
    )

    # Move from project root to reports/
    # Quarto writes output_file relative to QMD dir, so "../name.html" lands at project root
    if (file.exists(out_file)) {
      file.rename(out_file, row$final_path)
      rendered <- c(rendered, row$final_path)
      message("    Saved: ", row$final_path)
    }

  }, error = function(e) {
    message("    ERROR rendering ", matchup, ": ", e$message)
  })
}

message("\nDeep dives complete: ", length(rendered), "/",
        nrow(game_files), " rendered.")
message("Files saved to reports/")
