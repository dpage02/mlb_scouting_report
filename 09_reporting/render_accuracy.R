# ============================================================
# mlb_scouting_report
# SCRIPT: render_accuracy.R
# ============================================================
# PURPOSE:
#   Render the season prediction accuracy tracker page.
#   Output: reports/accuracy.html  (no date stamp — overwrites daily)
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE))
  stop("Package 'quarto' is required.")

if (!dir.exists("reports")) dir.create("reports")

message("Rendering prediction accuracy tracker...")

project_root <- normalizePath(getwd(), winslash = "/")
qmd_abs      <- file.path(project_root, "09_reporting", "mlb_accuracy.qmd")

# cd into 09_reporting/ so the bare output filename and the _files/
# bundler directory both resolve next to the QMD.
local({
  old_wd <- setwd(file.path(project_root, "09_reporting"))
  on.exit(setwd(old_wd))
  quarto::quarto_render(input = qmd_abs, output_file = "accuracy.html")
})

rendered_path <- file.path("09_reporting", "accuracy.html")
final_path    <- file.path("reports", "accuracy.html")

if (file.exists(rendered_path)) {
  file.rename(rendered_path, final_path)
}

# Clean up intermediate _files/ dir
files_dir <- file.path("09_reporting", "mlb_accuracy_files")
if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)

message("Accuracy tracker saved: ", final_path)
