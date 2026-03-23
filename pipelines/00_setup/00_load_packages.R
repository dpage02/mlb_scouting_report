# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 00_setup
# ============================================================
# This document contains the initial setup scripts for the pipeline phase.
# In practice, each section should live in its own file under:
# pipeline/00_setup/
#
# 00_load_packages.R
# 01_validate_environment.R
# 02_pipeline_config.R
# ============================================================

# ============================================================
# 00_load_packages.R
# PURPOSE:
# - Load all required packages for the pipeline
# - Install missing packages (optionally)
# - Centralize library loading so downstream scripts assume availability
# INPUTS:
# - None
# OUTPUTS:
# - Loaded libraries in session
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "baseballr",
  "jsonlite",
  "httr",
  "readr",
  "stringr",
  "glue",
  "here",
  "gt",
  "quarto",
  "R.utils",
  "rvest",
  "shiny",
  "DT"
)

installed <- rownames(installed.packages())
missing_pkgs <- setdiff(required_packages, installed)

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

message("[00_setup] Packages loaded successfully")