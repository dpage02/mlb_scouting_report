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

source("generate_index.R")

remote_url <- trimws(system("git remote get-url origin", intern = TRUE)[1])
if (is.na(remote_url) || nchar(remote_url) == 0)
  stop("Could not read git remote URL.")

message("Deploying to: ", remote_url)

tmpdir <- paste0("/tmp/ghpages_", format(Sys.time(), "%Y%m%d%H%M%S"))

script <- sprintf('
set -e

# 1. Create temp dir and copy all reports into it
mkdir -p %s
cp -r %s/. %s/

# 2. Init a fresh git repo there
cd %s
git init -q
git checkout -q -b gh-pages

# 3. Pull existing gh-pages history so archive is preserved
git remote add origin %s
git fetch origin gh-pages --depth=1 2>/dev/null && \
  git reset -q --soft FETCH_HEAD || true

# 4. Commit and push
git add -A
git commit -q -m "Deploy reports %s" --allow-empty
git push origin gh-pages --force

# 5. Clean up
rm -rf %s
',
  tmpdir,
  shQuote(normalizePath("reports")),
  tmpdir,
  tmpdir,
  shQuote(remote_url),
  Sys.Date(),
  tmpdir
)

result <- system(paste("bash -c", shQuote(script)), intern = FALSE)

if (result == 0) {
  message("\nDone! Site live at: https://dpage02.github.io/mlb_scouting_report/")
} else {
  message("\nPush failed — check output above.")
}
