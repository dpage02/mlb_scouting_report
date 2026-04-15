# ============================================================
# mlb_scouting_report
# SCRIPT: push_to_web.R
# ============================================================
# PURPOSE:
#   Push today's rendered reports to GitHub Pages.
#   Only keeps last KEEP_DAYS days of reports to stay under
#   GitHub Pages' 1GB limit.
#
# USAGE:
#   source("push_to_web.R")
# ============================================================

KEEP_DAYS <- 14   # how many days of reports to keep live

if (!dir.exists("reports")) stop("No reports/ directory found. Run run_all.R first.")

source("generate_index.R")

# Get auth token from GitHub CLI
gh_token <- trimws(system("gh auth token 2>/dev/null", intern = TRUE)[1])
if (is.na(gh_token) || nchar(gh_token) < 10) {
  stop("Could not get GitHub token. Make sure `gh auth login` has been run.")
}

# Get remote URL and inject token
remote_url <- trimws(system("git remote get-url origin", intern = TRUE)[1])
# Convert https://github.com/... → https://x-access-token:TOKEN@github.com/...
auth_url <- sub("https://", paste0("https://x-access-token:", gh_token, "@"), remote_url)

message("Preparing reports (last ", KEEP_DAYS, " days)...")

# Determine which report files to include:
# - Always: index.html, stat_reference.html
# - Date-stamped files: only if within KEEP_DAYS
cutoff_date <- Sys.Date() - KEEP_DAYS

all_files  <- list.files("reports", full.names = TRUE, recursive = FALSE)
keep_files <- character(0)

for (f in all_files) {
  bn <- basename(f)
  # Extract date from filename (pattern: something_YYYY-MM-DD_...)
  dm <- regmatches(bn, regexpr("\\d{4}-\\d{2}-\\d{2}", bn))
  if (length(dm) == 0) {
    # No date → always include (index.html, stat_reference.html, etc.)
    keep_files <- c(keep_files, f)
  } else {
    if (as.Date(dm) >= cutoff_date) keep_files <- c(keep_files, f)
  }
}

message("  Keeping ", length(keep_files), " of ", length(all_files), " files")

tmpdir <- paste0("/tmp/ghpages_", format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(tmpdir, recursive = TRUE)

# Copy selected files to temp dir
invisible(file.copy(keep_files, tmpdir))

# Also copy any subdirectories needed (e.g. libs/)
subdirs <- list.dirs("reports", recursive = FALSE, full.names = TRUE)
for (d in subdirs) {
  file.copy(d, tmpdir, recursive = TRUE)
}

script <- sprintf('
set -e
cd %s

git init -q
git checkout -q -b gh-pages
git remote add origin %s

# Fetch existing gh-pages to preserve older files not in this push
git fetch origin gh-pages --depth=1 2>/dev/null && \
  git reset -q --soft FETCH_HEAD || true

git add -A
git commit -q -m "Deploy reports %s" --allow-empty
git push origin gh-pages --force

cd /tmp
rm -rf %s
',
  shQuote(tmpdir),
  shQuote(auth_url),
  Sys.Date(),
  shQuote(tmpdir)
)

message("Pushing to GitHub Pages...")
result <- system(paste("bash -c", shQuote(script)), intern = FALSE)

if (result == 0) {
  message("\nDone! Site live at: https://dpage02.github.io/mlb_scouting_report/")
} else {
  message("\nPush failed — check output above.")
}
