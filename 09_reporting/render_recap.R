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

quarto::quarto_render(
  input          = "09_reporting/mlb_recap.qmd",
  output_file    = recap_filename,
  execute_params = list(recap_date = format(recap_date, "%Y-%m-%d"))
)

rendered_path <- recap_filename
final_path    <- file.path("reports", recap_filename)
if (file.exists(rendered_path)) {
  file.rename(rendered_path, final_path)
  message("Recap saved: ", final_path)
} else {
  message("Recap render may have written to: ", file.path("09_reporting", recap_filename))
  alt_path <- file.path("09_reporting", recap_filename)
  if (file.exists(alt_path)) file.rename(alt_path, final_path)
}
