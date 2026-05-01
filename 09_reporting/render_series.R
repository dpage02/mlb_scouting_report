# ============================================================
# mlb_scouting_report
# SCRIPT: render_series.R
# ============================================================
# PURPOSE:
#   When today is a Braves series finale, render:
#     1. Series recap  for the completed series
#     2. Series preview for the upcoming next series
#
#   Triggered automatically by braves_series_context$is_finale_today.
#   Safe to source on any day — skips silently when not a finale.
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE))
  stop("Package 'quarto' is required.")

if (!exists("braves_series_context")) {
  message("render_series: braves_series_context not found — skipping.")
} else if (!isTRUE(braves_series_context$is_finale_today)) {
  message("render_series: today is not a Braves series finale — skipping.")
} else {

  if (!dir.exists("reports")) dir.create("reports")

  project_root <- normalizePath(getwd(), winslash = "/")
  cs <- braves_series_context$current_series
  ns <- braves_series_context$next_series

  # Abbreviation helper
  .abbr <- function(tid) {
    tryCatch(
      team_ids %>% dplyr::filter(mlbam_team_id == tid) %>%
        dplyr::pull(team_abbr) %>% dplyr::first(),
      error = function(e) as.character(tid)
    )
  }

  # ----------------------------------------------------------
  # 1. Series Recap
  # ----------------------------------------------------------
  if (!is.null(cs)) {
    away_abbr  <- .abbr(cs$away_team_id)
    home_abbr  <- .abbr(cs$home_team_id)
    recap_file <- paste0("series_recap_",
                         format(cs$end_date, "%Y-%m-%d"), "_",
                         away_abbr, "_", home_abbr, ".html")

    message("Rendering series recap: ", recap_file)

    tryCatch({
      local({
        old_wd <- setwd(file.path(project_root, "09_reporting"))
        on.exit(setwd(old_wd))
        quarto::quarto_render(
          input       = file.path(project_root, "09_reporting", "mlb_series_recap.qmd"),
          output_file = recap_file
        )
      })
      rendered <- file.path("09_reporting", recap_file)
      final    <- file.path("reports", recap_file)
      if (file.exists(rendered)) {
        file.rename(rendered, final)
        message("Series recap saved: ", final)
      }
      files_dir <- file.path("09_reporting", "mlb_series_recap_files")
      if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
    }, error = function(e) message("ERROR rendering series recap: ", e$message))
  }

  # ----------------------------------------------------------
  # 2. Series Preview
  # ----------------------------------------------------------
  if (!is.null(ns)) {
    away_abbr    <- .abbr(ns$away_team_id)
    home_abbr    <- .abbr(ns$home_team_id)
    preview_file <- paste0("series_preview_",
                           format(ns$start_date, "%Y-%m-%d"), "_",
                           away_abbr, "_", home_abbr, ".html")

    message("Rendering series preview: ", preview_file)

    tryCatch({
      local({
        old_wd <- setwd(file.path(project_root, "09_reporting"))
        on.exit(setwd(old_wd))
        quarto::quarto_render(
          input       = file.path(project_root, "09_reporting", "mlb_series_preview.qmd"),
          output_file = preview_file
        )
      })
      rendered <- file.path("09_reporting", preview_file)
      final    <- file.path("reports", preview_file)
      if (file.exists(rendered)) {
        file.rename(rendered, final)
        message("Series preview saved: ", final)
      }
      files_dir <- file.path("09_reporting", "mlb_series_preview_files")
      if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
    }, error = function(e) message("ERROR rendering series preview: ", e$message))
  }

  message("render_series.R complete.")
}
