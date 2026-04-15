# ============================================================
# mlb_scouting_report
# SCRIPT: push_to_web.R
# ============================================================
# PURPOSE:
#   Push today's rendered reports to GitHub Pages.
#   Run this after run_all.R completes.
#
# USAGE:
#   source("push_to_web.R")
# ============================================================

if (!dir.exists("reports")) stop("No reports/ directory found. Run run_all.R first.")

# Generate index page
message("Generating index.html...")
source("generate_index.R")

# Get remote URL from current repo
remote_url <- trimws(system("git remote get-url origin", intern = TRUE))
if (length(remote_url) == 0 || nchar(remote_url) == 0) {
  stop("Could not determine git remote URL.")
}

message("Pushing reports/ to gh-pages branch at: ", remote_url)

# Use a throw-away git repo in /tmp — avoids touching the working tree
# and sidesteps gitignore on reports/
tmpdir <- file.path(tempdir(), paste0("gh-pages-", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(tmpdir, recursive = TRUE)

on.exit({
  unlink(tmpdir, recursive = TRUE)
}, add = TRUE)

# Copy all reports into tmp dir
file.copy(
  list.files("reports", full.names = TRUE),
  tmpdir,
  recursive = TRUE
)

script <- paste0('
set -e
cd ', shQuote(tmpdir), '
git init -q
git checkout -q -b gh-pages
git add -A
git commit -q -m "Deploy reports ', Sys.Date(), '"
git remote add origin ', shQuote(remote_url), '

# Pull existing gh-pages history if it exists (keeps archive intact)
git fetch origin gh-pages 2>/dev/null && \
  git reset -q --soft FETCH_HEAD || true

git add -A
git commit -q -m "Deploy reports ', Sys.Date(), '" --allow-empty
git push origin gh-pages --force-with-lease 2>/dev/null || \
  git push origin gh-pages --force
')

result <- system(paste("bash -c", shQuote(script)), intern = FALSE)

if (result == 0) {
  message("\nDone! Site live at: https://dpage02.github.io/mlb_scouting_report/")
} else {
  message("\nPush failed — check output above.")
}
