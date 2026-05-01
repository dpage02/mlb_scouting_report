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
# Helper: render a QMD using absolute paths so Quarto's
# self-contained bundler finds _files/ next to the QMD.
# Passing relative cross-directory paths causes either
# "No valid input files" or a bundler readfile error.
# Returns the absolute path of the rendered file.
# ------------------------------------------------------------
.qmd_dir      <- normalizePath(file.path(getwd(), "09_reporting"), winslash = "/")
.project_root <- normalizePath(getwd(), winslash = "/")

.qmd_render <- function(qmd_basename, params = NULL, output_file = NULL, ...) {
  qmd_abs <- file.path(.qmd_dir, qmd_basename)

  # cd into the QMD's directory so the bare output filename and the
  # _files/ bundler directory both resolve next to the QMD.
  old_wd <- setwd(.qmd_dir)
  on.exit(setwd(old_wd))

  quarto::quarto_render(
    input          = qmd_abs,
    execute_params = params,
    output_file    = output_file,   # bare filename or NULL
    ...
  )

  # Clean up intermediate _files/ dir (resources already bundled into HTML)
  stem      <- tools::file_path_sans_ext(qmd_basename)
  files_dir <- file.path(.qmd_dir, paste0(stem, "_files"))
  if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)

  # Return absolute path where Quarto wrote the file
  if (!is.null(output_file)) file.path(.qmd_dir, output_file) else NULL
}

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
    rendered_path <- .qmd_render(
      "mlb_game_deepdive.qmd",
      params      = list(game_pk = gpk, game_date = report_date),
      output_file = out_file
    )
    if (!is.null(rendered_path) && file.exists(rendered_path)) {
      file.rename(rendered_path, row$final_path)
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

# ------------------------------------------------------------
# Render team deep dives (one per side per game)
# ------------------------------------------------------------

message("\nRendering team deep dives...")

team_rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row <- game_files[i, ]
  gpk <- row$game_pk

  for (side in c("away", "home")) {
    abbr      <- if (side == "away") row$away_abbr else row$home_abbr
    team_name <- if (side == "away") row$away_team_name else row$home_team_name
    out_file  <- paste0("team_", report_date, "_", abbr, ".html")
    final_path <- file.path("reports", out_file)

    message(sprintf("  %s — %s...", abbr, team_name))

    tryCatch({
      rendered_path <- .qmd_render(
        "mlb_team_deepdive.qmd",
        params      = list(game_pk = gpk, side = side, game_date = report_date),
        output_file = out_file
      )
      if (!is.null(rendered_path) && file.exists(rendered_path)) {
        file.rename(rendered_path, final_path)
        team_rendered <- c(team_rendered, final_path)
        message("    Saved: ", final_path)
      }
    }, error = function(e) {
      message("    ERROR rendering ", abbr, " team page: ", e$message)
    })
  }
}

message("\nTeam deep dives complete: ", length(team_rendered), "/",
        nrow(game_files) * 2, " rendered.")

# ------------------------------------------------------------
# Render pitcher matchup pages (one per game)
# ------------------------------------------------------------

message("\nRendering pitcher matchup pages...")
matchup_rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row      <- game_files[i, ]
  gpk      <- row$game_pk
  matchup  <- sprintf("%s @ %s", row$away_team_name, row$home_team_name)
  out_file <- paste0("matchup_", report_date, "_", row$away_abbr, "_", row$home_abbr, ".html")
  final_path <- file.path("reports", out_file)

  message(sprintf("  [%d/%d] %s...", i, nrow(game_files), matchup))

  tryCatch({
    rendered_path <- .qmd_render(
      "mlb_pitcher_matchup.qmd",
      params      = list(game_pk = gpk, game_date = report_date),
      output_file = out_file
    )
    if (!is.null(rendered_path) && file.exists(rendered_path)) {
      file.rename(rendered_path, final_path)
      matchup_rendered <- c(matchup_rendered, final_path)
      message("    Saved: ", final_path)
    }
  }, error = function(e) {
    message("    ERROR rendering matchup ", matchup, ": ", e$message)
  })
}

message("\nMatchup pages complete: ", length(matchup_rendered), "/",
        nrow(game_files), " rendered.")

# ------------------------------------------------------------
# Render prediction pages (one per game)
# ------------------------------------------------------------

message("\nRendering prediction pages...")
pred_rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row      <- game_files[i, ]
  gpk      <- row$game_pk
  matchup  <- sprintf("%s @ %s", row$away_team_name, row$home_team_name)
  out_file <- paste0("prediction_", report_date, "_", row$away_abbr, "_", row$home_abbr, ".html")
  final_path <- file.path("reports", out_file)

  message(sprintf("  [%d/%d] %s...", i, nrow(game_files), matchup))

  tryCatch({
    rendered_path <- .qmd_render(
      "mlb_prediction.qmd",
      params      = list(game_pk = gpk, game_date = report_date),
      output_file = out_file
    )
    if (!is.null(rendered_path) && file.exists(rendered_path)) {
      file.rename(rendered_path, final_path)
      pred_rendered <- c(pred_rendered, final_path)
      message("    Saved: ", final_path)
    }
  }, error = function(e) {
    message("    ERROR rendering prediction ", matchup, ": ", e$message)
  })
}

message("\nPrediction pages complete: ", length(pred_rendered), "/",
        nrow(game_files), " rendered.")

# ------------------------------------------------------------
# Render hitting pages (one per game)
# ------------------------------------------------------------

message("\nRendering hitting pages...")
hitting_rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row      <- game_files[i, ]
  gpk      <- row$game_pk
  matchup  <- sprintf("%s @ %s", row$away_team_name, row$home_team_name)
  out_file <- paste0("hitting_", report_date, "_", row$away_abbr, "_", row$home_abbr, ".html")
  final_path <- file.path("reports", out_file)

  message(sprintf("  [%d/%d] %s...", i, nrow(game_files), matchup))

  tryCatch({
    rendered_path <- .qmd_render(
      "mlb_hitting.qmd",
      params      = list(game_pk = gpk, game_date = report_date),
      output_file = out_file
    )
    if (!is.null(rendered_path) && file.exists(rendered_path)) {
      file.rename(rendered_path, final_path)
      hitting_rendered <- c(hitting_rendered, final_path)
      message("    Saved: ", final_path)
    }
  }, error = function(e) {
    message("    ERROR rendering hitting ", matchup, ": ", e$message)
  })
}

message("\nHitting pages complete: ", length(hitting_rendered), "/",
        nrow(game_files), " rendered.")

# ------------------------------------------------------------
# Render print pages (one per game)
# ------------------------------------------------------------

message("\nRendering print pages...")
print_rendered <- character()

for (i in seq_len(nrow(game_files))) {
  row      <- game_files[i, ]
  gpk      <- row$game_pk
  matchup  <- sprintf("%s @ %s", row$away_team_name, row$home_team_name)
  out_file <- paste0("print_", report_date, "_", row$away_abbr, "_", row$home_abbr, ".html")
  final_path <- file.path("reports", out_file)

  message(sprintf("  [%d/%d] %s...", i, nrow(game_files), matchup))

  tryCatch({
    rendered_path <- .qmd_render(
      "mlb_print.qmd",
      params      = list(game_pk = gpk, game_date = report_date),
      output_file = out_file
    )
    if (!is.null(rendered_path) && file.exists(rendered_path)) {
      file.rename(rendered_path, final_path)
      print_rendered <- c(print_rendered, final_path)
      message("    Saved: ", final_path)
    }
  }, error = function(e) {
    message("    ERROR rendering print page ", matchup, ": ", e$message)
  })
}

message("\nPrint pages complete: ", length(print_rendered), "/",
        nrow(game_files), " rendered.")

# ------------------------------------------------------------
# Render stat reference page (once, not per game)
# ------------------------------------------------------------
message("\nRendering stat reference page...")
tryCatch({
  rendered_path <- .qmd_render("mlb_stat_reference.qmd", output_file = "stat_reference.html")
  if (!is.null(rendered_path) && file.exists(rendered_path)) {
    file.rename(rendered_path, "reports/stat_reference.html")
    message("Saved: reports/stat_reference.html")
  }
}, error = function(e) message("ERROR rendering stat reference: ", e$message))
