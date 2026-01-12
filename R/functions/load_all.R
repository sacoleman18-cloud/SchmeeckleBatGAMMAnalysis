# ==============================================================================
# load_all.R — Source All Project Functions
# ==============================================================================
# PURPOSE: Centralized function loading for all workflows
#
# This script sources all custom functions from R/functions/ subdirectories.
# Call this at the start of every workflow script to load the complete
# function library.
#
# DESIGN GOALS (CODING_STANDARDS-ALIGNED)
# --------------------------------------
# - Deterministic: fixed module load order
# - Defensive: validates required packages and module existence
# - Auditable: prints what was loaded (and what failed)
# - Workflow-agnostic: safe to call from Backbone / Production / Exploratory
#
# USAGE:
#   source(here::here("R", "functions", "load_all.R"))
# ==============================================================================

# -------------------------
# Package Requirements
# -------------------------
required_packages <- c(
  "here",
  "dplyr",
  "readr",
  "tidyr",
  "lubridate",
  "yaml",
  "mgcv"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Required package '%s' not installed.\n  Install with: install.packages('%s')",
      pkg, pkg
    ))
  }
}

# Suppress package startup messages
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(yaml)
  library(mgcv)
})

# -------------------------
# Source Function Files
# -------------------------
message("Loading project functions...")

functions_dir <- here::here("R", "functions")

if (!dir.exists(functions_dir)) {
  stop(sprintf(
    "Functions directory not found: %s\n  Please ensure R/functions/ exists in project root",
    functions_dir
  ))
}

# Define modules in loading order:
# - core first
# - validation second
# - modeling last
#
# NOTE:
# Keep this list explicit (not auto-discovered) to preserve deterministic
# ordering and avoid accidental sourcing of experimental files.
modules <- c(
  "core/utilities.R",
  "core/config.R",
  "validation/validation.R",
  "modeling/fit_nb_gamm.R",
  "modeling/extract_habitat_effects.R"
)

n_loaded <- 0L
errors <- character(0)

for (module in modules) {
  module_path <- file.path(functions_dir, module)

  if (!file.exists(module_path)) {
    errors <- c(errors, sprintf("Module not found: %s", module))
    next
  }

  tryCatch({
    source(module_path, local = FALSE)
    n_loaded <- n_loaded + 1L
    message(sprintf("  ✓ Loaded: %s", module))
  }, error = function(e) {
    errors <- c(errors, sprintf("Failed to load %s: %s", module, e$message))
  })
}

if (length(errors) > 0) {
  warning(sprintf(
    "Function loading completed with errors:\n%s",
    paste(paste0("  - ", errors), collapse = "\n")
  ))
}

message(sprintf("✓ Loaded %d function module(s)", n_loaded))

# -------------------------
# Clean up
# -------------------------
rm(required_packages, pkg, functions_dir, modules, module, module_path, n_loaded, errors)
