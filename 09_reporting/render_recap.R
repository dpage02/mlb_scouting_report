# ============================================================
# mlb_scouting_report
# SCRIPT: render_recap.R
# ============================================================
# PURPOSE:
#   Render yesterday's MLB results recap page.
#   Pulls live data at render time — no pipeline dependency.
#
# OUTPUT:
#   reports/recap_YYYY-MM-DD.html  (yesterday's date)
# ============================================================

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("Package 'quarto' is required.")
}

if (!dir.exists("reports")) dir.create("reports")

# Yesterday's date
recap_date <- tryCatch(
  as.Date(target_date) - 1L,
  error = function(e) Sys.Date() - 1L
)

recap_filename <- paste0("recap_", format(recap_date, "%Y-%m-%d"), ".html")

message("Rendering yesterday's recap for ", format(recap_date, "%Y-%m-%d"), "...")

# Absolute paths: keep input + output in the same dir (09_reporting/) so
# Quarto's self-contained bundler resolves _files/ correctly.
project_root  <- normalizePath(getwd(), winslash = "/")
qmd_abs       <- file.path(project_root, "09_reporting", "mlb_recap.qmd")

# Temporarily cd into 09_reporting/ so the bare output filename and
# the _files/ bundler directory both resolve next to the QMD.
local({
  old_wd <- setwd(file.path(project_root, "09_reporting"))
  on.exit(setwd(old_wd))
  quarto::quarto_render(
    input          = qmd_abs,
    output_file    = recap_filename,
    execute_params = list(recap_date = format(recap_date, "%Y-%m-%d"))
  )
})

# Clean up intermediate _files/ dir
files_dir <- file.path("09_reporting", "mlb_recap_files")
if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)

rendered_path <- file.path("09_reporting", recap_filename)
final_path    <- file.path("reports", recap_filename)
if (file.exists(rendered_path)) {
  file.rename(rendered_path, final_path)
  message("Recap saved: ", final_path)
}
