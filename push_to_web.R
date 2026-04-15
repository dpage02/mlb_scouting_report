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
#   — or —
#   Rscript push_to_web.R
# ============================================================

if (!dir.exists("reports")) stop("No reports/ directory found. Run run_all.R first.")

# Generate index page
message("Generating index.html...")
source("generate_index.R")

# Push reports/ to gh-pages branch via shell
message("Pushing to gh-pages...")

result <- system(paste(
  "cd", shQuote(getwd()), "&&",
  "git add reports/ &&",
  "git stash -- reports/ &&",
  "git fetch origin gh-pages 2>/dev/null || true &&",
  "(git worktree add /tmp/gh-pages-deploy gh-pages 2>/dev/null ||",
  " git worktree add /tmp/gh-pages-deploy --orphan gh-pages) &&",
  "cp -r reports/. /tmp/gh-pages-deploy/ &&",
  "cd /tmp/gh-pages-deploy &&",
  "git add -A &&",
  paste0("git commit -m 'Deploy reports ", Sys.Date(), "' --allow-empty &&"),
  "git push origin gh-pages &&",
  "cd", shQuote(getwd()), "&&",
  "git worktree remove /tmp/gh-pages-deploy --force &&",
  "git stash pop 2>/dev/null || true"
), intern = FALSE)

if (result == 0) {
  message("\nDone! Site updated at: https://dpage02.github.io/mlb_scouting_report/")
} else {
  message("\nPush failed. Check git output above.")
}
