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

# Verify gh CLI is authenticated
gh_check <- system("gh auth status 2>/dev/null", intern = FALSE)
if (gh_check != 0) {
  stop("Not authenticated. Run `gh auth login` first.")
}

# Get remote URL (plain, no token injection — gh credential helper handles auth)
remote_url <- trimws(system("git remote get-url origin", intern = TRUE)[1])

# Find gh CLI path for credential helper
gh_path <- trimws(system("which gh", intern = TRUE)[1])

message("Preparing today's reports for push...")

# Only push today's files + permanent pages (index, stat_reference, etc.)
# The gh-pages branch accumulates history — older dates stay from prior pushes.
# KEEP_DAYS controls a periodic cleanup of stale files already on the branch.
all_files  <- list.files("reports", full.names = TRUE, recursive = FALSE)
keep_files <- character(0)

for (f in all_files) {
  bn <- basename(f)
  dm <- regmatches(bn, regexpr("\\d{4}-\\d{2}-\\d{2}", bn))
  if (length(dm) == 0) {
    # No date → always include (index.html, stat_reference.html, etc.)
    keep_files <- c(keep_files, f)
  } else if (as.Date(dm) == Sys.Date()) {
    # Only today's date-stamped reports
    keep_files <- c(keep_files, f)
  }
}

message("  Pushing ", length(keep_files), " files (today + permanent pages)")

# Stage new files separately so they survive the gh-pages checkout below
stagedir <- paste0("/tmp/ghpages_stage_", format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(stagedir, recursive = TRUE)
invisible(file.copy(keep_files, stagedir))
subdirs <- list.dirs("reports", recursive = FALSE, full.names = TRUE)
for (d in subdirs) file.copy(d, stagedir, recursive = TRUE)

tmpdir <- paste0("/tmp/ghpages_", format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(tmpdir, recursive = TRUE)

script <- sprintf('
set -e
cd %s

git init -q
git checkout -q -b gh-pages
git remote add origin %s

# Use gh CLI as credential helper (handles OAuth tokens correctly)
git config credential.helper "!%s auth git-credential"
git config user.email "deploy@mlb-scouting-report"
git config user.name "MLB Scouting Report Deploy"
# Increase HTTP buffer to handle large HTML file pushes (default 1MB causes HTTP 400)
git config http.postBuffer 524288000

# Fetch existing gh-pages so prior dates are preserved
git fetch origin gh-pages --depth=1 2>/dev/null && \
  git checkout -q FETCH_HEAD -- . 2>/dev/null || true

# Remove files older than KEEP_DAYS from the branch
find . -maxdepth 1 -name "*[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*" | while read f; do
  fdate=$(echo "$f" | grep -oE "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  if [ -n "$fdate" ] && [ "$fdate" \\< "%s" ]; then
    rm -f "$f"
  fi
done

# Copy new files AFTER checkout so they overwrite old versions
cp -r %s/. .

git add -A
git commit -q -m "Deploy reports %s" --allow-empty
git push origin gh-pages --force

cd /tmp
rm -rf %s %s
',
  shQuote(tmpdir),
  shQuote(remote_url),
  gh_path,
  format(Sys.Date() - KEEP_DAYS),  # cutoff for stale file removal
  shQuote(stagedir),                 # copy new files over old checkout
  Sys.Date(),                        # commit message date
  shQuote(tmpdir),
  shQuote(stagedir)
)

message("Pushing to GitHub Pages...")
result <- system(paste("bash -c", shQuote(script)), intern = FALSE)

if (result == 0) {
  message("\nDone! Site live at: https://dpage02.github.io/mlb_scouting_report/")
} else {
  message("\nPush failed — check output above.")
}
