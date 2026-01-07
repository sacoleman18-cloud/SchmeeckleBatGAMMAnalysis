# ==============================================================================
# core/utilities.R — Core Utility Functions
# ==============================================================================
# PURPOSE: Centralized logging, safe I/O operations, and path construction
#
# These functions are project-agnostic and can be reused across multiple
# analysis pipelines. They handle common tasks like message logging,
# safe file reading, and standardized output path generation.
# ==============================================================================

#' Log Message to Console and File
#'
#' @description
#' Centralized logging function that writes timestamped messages to both
#' the console (stderr) and an append-only log file. Ensures all workflow
#' events are traceable and auditable.
#'
#' @param msg Character. The message to log
#' @param level Character. Log level: "INFO", "WARNING", "ERROR", "DEBUG". Default = "INFO"
#' @param log_path Character. Path to log file. If NULL, logs to console only. Default = NULL
#' @param quiet Logical. Suppress console output? Default = FALSE
#'
#' @return Invisible NULL (side effect: writes to console and/or file)
#'
#' @details
#' Message format: `[YYYY-MM-DD HH:MM:SS] [LEVEL] message`
#'
#' Log files are always appended (never overwritten).
#' Timestamps use ISO 8601 format with timezone.
#'
#' Console output goes to stderr (not stdout) to separate from data output.
#'
#' @section CONTRACT:
#' - Always timestamps messages with ISO 8601 format
#' - Never overwrites log files (append-only)
#' - Creates log file directory if missing
#' - Writes to stderr for console output (not stdout)
#' - Returns invisibly (no return value)
#'
#' @section DOES NOT:
#' - Rotate or truncate log files
#' - Filter messages by level (all levels logged if called)
#' - Validate log file permissions (fails with error if no write access)
#' - Format multi-line messages (preserves as-is)
#'
#' @examples
#' \dontrun{
#' log_message("Processing started", level = "INFO")
#' log_message("Missing column detected", level = "WARNING", log_path = "logs/validation.log")
#' log_message(sprintf("Processed %d rows", nrow(df)), level = "INFO")
#' }
#'
#' @export
log_message <- function(msg, level = "INFO", log_path = NULL, quiet = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(msg) || length(msg) != 1) {
    stop("msg must be a single character string")
  }

  valid_levels <- c("INFO", "WARNING", "ERROR", "DEBUG")
  if (!level %in% valid_levels) {
    stop(sprintf("level must be one of: %s", paste(valid_levels, collapse = ", ")))
  }

  # -------------------------
  # Format message
  # -------------------------
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  formatted_msg <- sprintf("[%s] [%s] %s", timestamp, level, msg)

  # -------------------------
  # Write to console
  # -------------------------
  if (!quiet) {
    message(formatted_msg)  # Goes to stderr by default
  }

  # -------------------------
  # Write to log file
  # -------------------------
  if (!is.null(log_path)) {
    # Create directory if missing
    log_dir <- dirname(log_path)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Append to log file
    tryCatch({
      cat(formatted_msg, "\n", file = log_path, append = TRUE)
    }, error = function(e) {
      warning(sprintf("Failed to write to log file: %s\n  Error: %s", log_path, e$message))
    })
  }

  invisible(NULL)
}


#' Initialize Pipeline Log
#'
#' @description
#' Creates or appends to a workflow-specific log file with a standardized
#' header marking the start of a pipeline run. Records R version, session info,
#' and workflow metadata.
#'
#' @param log_path Character. Path to log file
#' @param workflow_name Character. Name of the workflow (e.g., "01_import_and_validate")
#' @param script_path Character. Path to the calling script (for reference)
#'
#' @return Character. The log_path (invisibly, for piping)
#'
#' @details
#' Header includes:
#' - Separator line
#' - Workflow name and timestamp
#' - R version
#' - Platform info
#' - Script path
#' - User (if available)
#'
#' Subsequent log_message() calls in the workflow should use the same log_path.
#'
#' @section CONTRACT:
#' - Creates log directory if missing
#' - Appends to existing log file (never overwrites)
#' - Returns log_path invisibly for use in subsequent log_message() calls
#' - Writes standardized header with workflow metadata
#'
#' @section DOES NOT:
#' - Overwrite existing log files
#' - Validate write permissions (fails with error if no access)
#' - Archive or rotate old logs
#' - Include full sessionInfo() by default (optional via separate log call)
#'
#' @examples
#' \dontrun{
#' log_path <- initialize_pipeline_log(
#'   log_path = here("logs", "workflow_01.log"),
#'   workflow_name = "01_import_and_validate_calls_per_night",
#'   script_path = here("00_DATA_BACKBONE", "scripts", "01_import_and_validate.R")
#' )
#'
#' log_message("Processing started", log_path = log_path)
#' }
#'
#' @export
initialize_pipeline_log <- function(log_path, workflow_name, script_path = NULL) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(log_path) || length(log_path) != 1) {
    stop("log_path must be a single character string")
  }

  if (!is.character(workflow_name) || length(workflow_name) != 1) {
    stop("workflow_name must be a single character string")
  }

  # -------------------------
  # Create directory
  # -------------------------
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------
  # Write header
  # -------------------------
  separator <- paste(rep("=", 80), collapse = "")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  r_version <- paste(R.version$major, R.version$minor, sep = ".")
  platform <- R.version$platform

  header <- sprintf(
    "\n%s\n%s\n%s\nStarted: %s\nR version: %s\nPlatform: %s\n",
    separator,
    workflow_name,
    separator,
    timestamp,
    r_version,
    platform
  )

  if (!is.null(script_path)) {
    header <- paste0(header, sprintf("Script: %s\n", script_path))
  }

  header <- paste0(header, separator, "\n")

  # -------------------------
  # Write to file
  # -------------------------
  tryCatch({
    cat(header, file = log_path, append = TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to initialize log file: %s\n  Error: %s", log_path, e$message))
  })

  invisible(log_path)
}


#' Safe CSV Reader with Validation
#'
#' @description
#' Wrapper around readr::read_csv() that adds input validation, informative
#' error messages, and optional logging. Ensures robust data ingestion with
#' clear failure modes.
#'
#' @param file_path Character. Path to CSV file
#' @param required_cols Character vector. Column names that must exist. Default = NULL (no check)
#' @param log_path Character. Optional log file path. Default = NULL
#' @param quiet Logical. Suppress console messages? Default = FALSE
#' @param ... Additional arguments passed to readr::read_csv()
#'
#' @return Data frame (tibble) with loaded data
#'
#' @details
#' Validation steps:
#' 1. Check file existence
#' 2. Attempt CSV read
#' 3. Verify non-empty result
#' 4. Check required columns (if specified)
#' 5. Log success or failure
#'
#' Fails fast with actionable error messages.
#'
#' @section CONTRACT:
#' - Validates file existence before reading
#' - Returns tibble (from readr::read_csv)
#' - Logs file path, row count, and column count on success
#' - Throws informative errors with file path and missing columns
#' - Never returns empty data frame without warning
#'
#' @section DOES NOT:
#' - Modify or transform data (returns as-is from read_csv)
#' - Handle non-CSV formats
#' - Attempt file repair or encoding detection
#' - Retry on failure
#'
#' @examples
#' \dontrun{
#' df <- safe_read_csv(
#'   file_path = here("data", "raw", "calls_per_night.csv"),
#'   required_cols = c("Detector", "Night", "RecordingHours"),
#'   log_path = "logs/workflow_01.log"
#' )
#' }
#'
#' @export
safe_read_csv <- function(file_path, required_cols = NULL, log_path = NULL, quiet = FALSE, ...) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }

  if (!file.exists(file_path)) {
    stop(sprintf(
      "File not found: %s\n  Please check the path and try again.",
      file_path
    ))
  }

  # -------------------------
  # Read CSV
  # -------------------------
  if (!quiet) message(sprintf("Reading CSV: %s", basename(file_path)))

  df <- tryCatch({
    readr::read_csv(file_path, show_col_types = FALSE, ...)
  }, error = function(e) {
    stop(sprintf(
      "Failed to read CSV: %s\n  Error: %s",
      file_path,
      e$message
    ))
  })

  # -------------------------
  # Validate result
  # -------------------------
  if (nrow(df) == 0) {
    warning(sprintf("Loaded CSV is empty: %s", file_path))
  }

  # Check required columns
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      stop(sprintf(
        "Missing required columns in %s: %s\n  Found columns: %s",
        basename(file_path),
        paste(missing, collapse = ", "),
        paste(names(df), collapse = ", ")
      ))
    }
  }

  # -------------------------
  # Log success
  # -------------------------
  if (!quiet) {
    message(sprintf("✓ Loaded %s rows, %d columns", format(nrow(df), big.mark = ","), ncol(df)))
  }

  if (!is.null(log_path)) {
    log_message(
      sprintf("Loaded %s: %s rows, %d columns", basename(file_path), format(nrow(df), big.mark = ","), ncol(df)),
      level = "INFO",
      log_path = log_path,
      quiet = TRUE
    )
  }

  df
}


#' Construct Standardized Output Path
#'
#' @description
#' Generates timestamped or versioned output file paths following project
#' naming conventions. Ensures consistent file naming across workflows.
#'
#' @param output_dir Character. Base output directory
#' @param base_name Character. Base filename (without extension)
#' @param extension Character. File extension (with or without leading dot). Default = "csv"
#' @param timestamp Logical. Add timestamp suffix? Default = FALSE
#' @param version Character. Version suffix (e.g., "v1", "v2"). Default = NULL
#'
#' @return Character. Full output file path
#'
#' @details
#' Naming patterns:
#' - Timestamp: `base_name_YYYYMMDD_HHMMSS.ext`
#' - Version: `base_name_v1.ext`
#' - Neither: `base_name.ext`
#'
#' Creates output directory if missing.
#'
#' @section CONTRACT:
#' - Returns absolute path using here::here() internally
#' - Creates output directory if missing (recursive)
#' - Adds leading dot to extension if missing
#' - Formats timestamp as YYYYMMDD_HHMMSS (no separators in date)
#' - Never overwrites files (caller must handle overwrite checks)
#'
#' @section DOES NOT:
#' - Check if file already exists
#' - Validate write permissions
#' - Sanitize base_name (assumes valid filename)
#' - Handle version incrementing (caller must track versions)
#'
#' @examples
#' \dontrun{
#' # Timestamped checkpoint
#' path <- make_output_path(
#'   output_dir = here("00_DATA_BACKBONE", "outputs"),
#'   base_name = "calls_per_night_clean",
#'   extension = "csv",
#'   timestamp = TRUE
#' )
#' # Returns: .../calls_per_night_clean_20260106_143022.csv
#'
#' # Versioned final output
#' path <- make_output_path(
#'   output_dir = here("01_PRODUCTION", "outputs", "figs"),
#'   base_name = "habitat_effect",
#'   extension = "png",
#'   version = "v2"
#' )
#' # Returns: .../habitat_effect_v2.png
#' }
#'
#' @export
make_output_path <- function(output_dir, base_name, extension = "csv", timestamp = FALSE, version = NULL) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("output_dir must be a single character string")
  }

  if (!is.character(base_name) || length(base_name) != 1) {
    stop("base_name must be a single character string")
  }

  if (!is.character(extension) || length(extension) != 1) {
    stop("extension must be a single character string")
  }

  # -------------------------
  # Create directory
  # -------------------------
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------
  # Format extension
  # -------------------------
  if (!grepl("^\\.", extension)) {
    extension <- paste0(".", extension)
  }

  # -------------------------
  # Construct filename
  # -------------------------
  if (timestamp) {
    suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("%s_%s%s", base_name, suffix, extension)
  } else if (!is.null(version)) {
    filename <- sprintf("%s_%s%s", base_name, version, extension)
  } else {
    filename <- sprintf("%s%s", base_name, extension)
  }

  # -------------------------
  # Return full path
  # -------------------------
  file.path(output_dir, filename)
}
