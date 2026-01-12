# ==============================================================================
# core/read_kpro_release_calls_per_night.R — Read Calls-Per-Night from KPro Zip
# ==============================================================================
# PURPOSE
# -------
# Read the calls-per-night grid from a standardized KPro "study release" zip
# dropped into data/kpro_pipeline/. This supports portable transfer between
# projects without requiring the user to rename CSVs or edit YAML paths.
#
# CONTRACT
# --------
# - Reads a single zip file at a known project-relative path
# - Extracts ONLY the required CSV to a temporary directory
# - Returns the raw calls-per-night data frame (no transformations)
# - Never modifies the zip or writes into data/kpro_pipeline/
#
# DOES NOT
# --------
# - Does not validate scientific correctness (Backbone validation does that)
# - Does not reconstruct calls-per-night from detections
# - Does not write any outputs to outputs/data_backbone/
# ==============================================================================

#' Read calls-per-night raw grid from a KPro release zip
#'
#' @description
#' Loads `data/calls_per_night_raw.csv` from a KPro release zip stored at a
#' project-local path (e.g., `data/kpro_pipeline/kpro_release.zip`).
#' Intended for Backbone ingestion only.
#'
#' @param zip_path Character. Path to the KPro release zip.
#' @param inner_csv_path Character. Path inside the zip to the raw CSV.
#'   Default = "data/calls_per_night_raw.csv".
#' @param required_cols Character vector. Required columns to assert at import time.
#'   Default matches the Backbone raw schema expectation.
#' @param log_path Character. Optional pipeline log path for audit messages.
#' @param quiet Logical. Suppress console output? Default = FALSE.
#'
#' @return Tibble containing the raw calls-per-night grid (as read from CSV).
#'
#' @section CONTRACT:
#' - Fails fast if zip is missing
#' - Fails fast if the CSV is not found inside the zip
#' - Extracts to a temp directory (does not leave files behind)
#' - Does not modify source zip
#'
#' @section DOES NOT:
#' - Does not parse dates into final types (Backbone Stage 1.3 does that)
#' - Does not validate ranges or grid completeness (Backbone Stage 1.5 does that)
#' - Does not write outputs
#'
#' @examples
#' \dontrun{
#' df_raw <- read_kpro_release_calls_per_night(
#'   zip_path = here::here("data", "kpro_pipeline", "kpro_release.zip"),
#'   log_path = here::here("logs", "pipeline_log.txt")
#' )
#' }
#'
#' @export
read_kpro_release_calls_per_night <- function(zip_path,
                                              inner_csv_path = "data/calls_per_night_raw.csv",
                                              required_cols = c(
                                                "Detector", "Night", "CallsPerNight", "RecordingHours",
                                                "StartDateTime", "EndDateTime"
                                              ),
                                              log_path = NULL,
                                              quiet = FALSE) {
  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(zip_path) || length(zip_path) != 1) {
    stop("zip_path must be a single character string")
  }
  if (!file.exists(zip_path)) {
    stop(sprintf(
      "KPro release zip not found: %s\n  Fix: Place the KPro release zip at data/kpro_pipeline/kpro_release.zip",
      zip_path
    ))
  }
  if (!is.character(inner_csv_path) || length(inner_csv_path) != 1) {
    stop("inner_csv_path must be a single character string")
  }

  # -------------------------
  # List zip contents + check required file exists
  # -------------------------
  zip_listing <- tryCatch({
    utils::unzip(zipfile = zip_path, list = TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to list zip contents: %s\n  Error: %s", zip_path, e$message))
  })

  if (!"Name" %in% names(zip_listing)) {
    stop(sprintf("Unexpected zip listing format for: %s", zip_path))
  }

  if (!inner_csv_path %in% zip_listing$Name) {
    stop(sprintf(
      "Required file not found inside KPro zip:\n  Expected: %s\n  Zip: %s\n  Found examples: %s",
      inner_csv_path,
      basename(zip_path),
      paste(utils::head(zip_listing$Name, 10), collapse = ", ")
    ))
  }

  # -------------------------
  # Extract required file to temp dir (safe, non-destructive)
  # -------------------------
  tmp_dir <- tempfile("kpro_release_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  extracted_path <- tryCatch({
    utils::unzip(
      zipfile = zip_path,
      files = inner_csv_path,
      exdir = tmp_dir
    )
  }, error = function(e) {
    stop(sprintf(
      "Failed to extract %s from zip: %s\n  Error: %s",
      inner_csv_path, zip_path, e$message
    ))
  })

  # unzip() returns the extracted file path(s)
  if (length(extracted_path) != 1 || !file.exists(extracted_path)) {
    stop(sprintf(
      "Extraction failed: expected one extracted file for %s, got %d",
      inner_csv_path, length(extracted_path)
    ))
  }

  if (!quiet) {
    message(sprintf("Reading calls-per-night raw CSV from KPro zip: %s", basename(zip_path)))
  }
  if (!is.null(log_path)) {
    log_message(
      sprintf("Reading calls-per-night raw CSV from KPro zip: %s (%s)", basename(zip_path), inner_csv_path),
      log_path = log_path,
      quiet = TRUE
    )
  }

  # -------------------------
  # Read CSV (schema check only)
  # -------------------------
  df_raw <- safe_read_csv(
    file_path = extracted_path,
    required_cols = required_cols,
    log_path = log_path,
    quiet = TRUE
  )

  df_raw
}
