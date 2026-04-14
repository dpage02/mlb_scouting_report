# ============================================================
# 02_pipeline_config.R
# PURPOSE:
# - Define pipeline-level configuration values
# - These are operational defaults, not baseball logic
# INPUTS:
# - None
# OUTPUTS:
# - Config objects available to downstream scripts
# ============================================================

# ---- Pipeline identity ----
PIPELINE_NAME <- "mlb_scouting_report"
PIPELINE_PHASE <- "pipeline"
PIPELINE_VERSION <- "v1.0"

# ---- Runtime defaults ----
DEFAULT_SEASON  <- as.integer(format(Sys.Date(), "%Y"))
target_season   <- DEFAULT_SEASON        # alias used throughout pipeline
DEFAULT_TIMEZONE <- "America/New_York"

# ---- API safety ----
API_SLEEP_SECONDS <- 0.5
API_MAX_RETRIES <- 3

# ---- Date handling ----
RUN_DATE <- Sys.Date()
RUN_TIMESTAMP <- Sys.time()

# ---- Logging ----
LOG_FILE <- file.path("logs", paste0("pipeline_", RUN_DATE, ".log"))

log_message <- function(msg) {
  timestamped <- paste0("[", Sys.time(), "] ", msg)
  cat(timestamped, "\n", file = LOG_FILE, append = TRUE)
  cat(timestamped, "\n")
}

log_message("Pipeline configuration loaded")