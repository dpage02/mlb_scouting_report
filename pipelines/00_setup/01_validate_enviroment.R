# ============================================================
# 01_validate_environment.R
# PURPOSE:
# - Validate that the runtime environment is sane
# - Fail fast if critical assumptions are violated
# INPUTS:
# - None
# OUTPUTS:
# - Stops execution if validation fails
# ============================================================

# Check R version
min_r_version <- "4.2.0"
if (getRversion() < min_r_version) {
  stop("R version ", min_r_version, " or higher is required")
}

# Required directories
required_dirs <- c(
  "pipeline",
  "config",
  "data",
  "data/ids",
  "data/static",
  "data/rosters",
  "data/context",
  "data/performance",
  "data/derived",
  "logs"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Created missing directory: ", dir)
  }
}

# Check internet (required for MLB APIs)
connection_test <- try(httr::GET("https://statsapi.mlb.com"), silent = TRUE)
if (inherits(connection_test, "try-error")) {
  stop("No internet connection or MLB Stats API unreachable")
}

message("[00_setup] Environment validation passed")