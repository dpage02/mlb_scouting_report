# ============================================================
# mlb_scouting_report
# SCRIPT: render_report.R
# ============================================================
# PURPOSE:
#   Entry point for rendering the daily scouting report.
#   The Quarto document sources the pipeline internally,
#   so this script just triggers the render and handles
#   output file naming.
#
# USAGE:
#   Rscript 09_reporting/render_report.R
#   — or —
#   source("09_reporting/render_report.R")  # from RStudio
#
# OPTIONAL OVERRIDES (set before sourcing):
#   TARGET_DATE      <- "2026-04-01"
#   TARGET_TEAM_ABBR <- "ATL"  # single team only
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Package 'quarto' is required. Install with: install.packages('quarto')")
}

# Create reports directory if it doesn't exist
if (!dir.exists("reports")) dir.create("reports")

# Determine output filename
# TARGET_DATE may not be set yet — use today as fallback for naming
report_date <- tryCatch(
  as.character(TARGET_DATE),
  error = function(e) as.character(Sys.Date())
)

output_filename <- paste0("scouting_", report_date, ".html")

message("Rendering scouting report for ", report_date, "...")

# Use absolute paths for both input and output so Quarto's self-contained
# bundler finds _files/ next to the QMD (09_reporting/) rather than at CWD.
# Passing relative cross-directory paths causes "No valid input files" or
# a bundler readfile error depending on the quarto R package version.
project_root <- normalizePath(getwd(), winslash = "/")
qmd_abs      <- file.path(project_root, "09_reporting", "mlb_scouting_report.qmd")

# Temporarily cd into 09_reporting/ so the bare output filename and the
# _files/ bundler directory both resolve next to the QMD.  Without this,
# the output goes to the project root while _files/ stays in 09_reporting/,
# and Quarto's self-contained bundler can't find the JS/CSS assets.
local({
  old_wd <- setwd(file.path(project_root, "09_reporting"))
  on.exit(setwd(old_wd))
  quarto::quarto_render(input = qmd_abs, output_file = output_filename)
})

# Move self-contained HTML from 09_reporting/ to reports/
rendered_path <- file.path("09_reporting", output_filename)
final_path    <- file.path("reports", output_filename)
if (!dir.exists("reports")) dir.create("reports")
if (file.exists(rendered_path)) file.rename(rendered_path, final_path)

# Clean up intermediate _files/ dir (resources already embedded in HTML)
files_dir <- file.path("09_reporting", "mlb_scouting_report_files")
if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)

message("Report saved: ", final_path)
