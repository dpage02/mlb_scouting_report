# ============================================================
# mlb_scouting_report
# SCRIPT: render_game_results.R
# ============================================================
# PURPOSE:
#   Render one post-game result page per completed game.
#   Only renders games with actual scores in predictions_log.csv.
#   Safe to re-run daily — overwrites existing result files.
#
# OUTPUT FILES:
#   reports/result_YYYY-MM-DD_AWAY_HOME.html  (one per completed game)
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE))
  stop("Package 'quarto' is required.")

if (!dir.exists("reports")) dir.create("reports")

log_path <- "data/predictions_log.csv"
log_df <- NULL

if (!file.exists(log_path)) {
  message("No predictions log found at ", log_path, " — skipping game results render.")
} else {
  log_df <- tryCatch(
    readr::read_csv(log_path, show_col_types = FALSE) %>%
      dplyr::filter(!is.na(actual_away_runs), !is.na(actual_home_runs)),
    error = function(e) {
      message("Could not read predictions log: ", e$message)
      NULL
    }
  )
  # Rows logged before file_suffix existed have no such column.
  if (!is.null(log_df) && !"file_suffix" %in% names(log_df)) {
    log_df$file_suffix <- NA_character_
  }
  if (!is.null(log_df)) log_df$file_suffix <- dplyr::coalesce(log_df$file_suffix, "")
  if (is.null(log_df) || nrow(log_df) == 0) {
    message("No completed games in predictions log yet — skipping game results render.")
    log_df <- NULL
  }
}

if (is.null(log_df)) {
  invisible(NULL)
} else {

# Load team_ids for abbreviation lookup (may already be in env from pipeline)
if (!exists("team_ids") || nrow(team_ids) == 0) {
  team_ids_path <- "data/ids/team_ids.csv"
  if (file.exists(team_ids_path)) {
    team_ids <- readr::read_csv(team_ids_path, show_col_types = FALSE)
  } else {
    team_ids <- dplyr::tibble(mlbam_team_id = integer(), team_abbr = character())
  }
}

abbr_for <- function(team_name) {
  m <- team_ids %>%
    dplyr::filter(stringr::str_detect(tolower(team_name_full),
                                       tolower(team_name))) %>%
    dplyr::pull(team_abbr) %>% dplyr::first()
  if (is.na(m) || length(m) == 0) {
    # Fallback: use first 3 chars of last word
    toupper(substr(rev(strsplit(trimws(team_name), " ")[[1]])[1], 1, 3))
  } else m
}

# Build abbreviation lookup from log
away_abbrs <- vapply(log_df$away_team, abbr_for, character(1))
home_abbrs <- vapply(log_df$home_team, abbr_for, character(1))

message("Rendering game results: ", nrow(log_df), " completed games...")

project_root <- normalizePath(getwd(), winslash = "/")
qmd_abs      <- file.path(project_root, "09_reporting", "mlb_game_result.qmd")

rendered <- character()

for (i in seq_len(nrow(log_df))) {
  row       <- log_df[i, ]
  gpk       <- row$game_pk
  gdate     <- as.character(row$game_date)
  away_abbr <- away_abbrs[i]
  home_abbr <- home_abbrs[i]
  suffix    <- dplyr::coalesce(row$file_suffix, "")
  matchup   <- sprintf("%s @ %s", row$away_team, row$home_team)
  out_file  <- paste0("result_", gdate, "_", away_abbr, "_", home_abbr, suffix, ".html")
  final_path <- file.path("reports", out_file)

  message(sprintf("  [%d/%d] %s (%s)...", i, nrow(log_df), matchup, gdate))

  tryCatch({
    # cd into 09_reporting/ so the bare output filename and the _files/
    # bundler directory both resolve next to the QMD.
    local({
      old_wd <- setwd(file.path(project_root, "09_reporting"))
      on.exit(setwd(old_wd))
      quarto::quarto_render(
        input          = qmd_abs,
        execute_params = list(
          game_pk     = gpk,
          game_date   = gdate,
          away_abbr   = away_abbr,
          home_abbr   = home_abbr,
          file_suffix = suffix
        ),
        output_file = out_file
      )
    })
    rendered_path <- file.path("09_reporting", out_file)
    if (file.exists(rendered_path)) {
      file.rename(rendered_path, final_path)
      rendered <- c(rendered, final_path)
      message("    Saved: ", final_path)
    }
    # Clean up _files/
    stem      <- tools::file_path_sans_ext("mlb_game_result.qmd")
    files_dir <- file.path("09_reporting", paste0(stem, "_files"))
    if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
  }, error = function(e) {
    message("    ERROR rendering ", matchup, ": ", e$message)
  })
}

message("\nGame results complete: ", length(rendered), "/", nrow(log_df), " rendered.")
}
