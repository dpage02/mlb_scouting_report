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

quarto::quarto_render(
  input       = "09_reporting/mlb_scouting_report.qmd",
  output_file = output_filename
)

# Move rendered file to reports/
# Quarto renders relative to QMD dir, so output lands one level up (project root)
rendered_path <- output_filename
final_path    <- file.path("reports", output_filename)
if (!dir.exists("reports")) dir.create("reports")
file.rename(rendered_path, final_path)

message("Report saved: ", final_path)
