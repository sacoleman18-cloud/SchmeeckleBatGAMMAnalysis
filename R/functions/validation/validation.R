# ==============================================================================
# VALIDATION EVENT LOGGING
# ==============================================================================

#' Create Validation Event Entry
#'
#' @description
#' Creates a single-row tibble representing a validation event for logging
#' to the backbone validation_log. csv.  Uses local machine time with timezone.
#'
#' @param stage Character.  Processing stage identifier (e.g., "1. 5.1", "1.5.2")
#' @param event_type Character. Event classification:  "PASS", "WARNING", "FAIL", "INFO"
#' @param message Character. Human-readable description of the validation event
#' @param rows_affected Integer. Number of rows involved in this event.  Default = 0L
#' @param detector Character. Detector ID affected, or "ALL" for global events.  Default = "ALL"
#' @param night Date. Night affected, or NA for global events. Default = NA_Date_
#' @param script Character. Name of the calling script. Default = "unknown"
#'
#' @return Tibble with one row containing validation event data:
#'   - timestamp: Character (local time with timezone, e.g., "2026-01-11 14:32:15 CST")
#'   - script: Character
#'   - stage: Character
#'   - detector: Character
#'   - night: Date
#'   - event_type: Character
#'   - message: Character
#'   - rows_affected: Integer
#'
#' @details
#' Timestamp format uses local machine time with timezone abbreviation (%Z).
#' This ensures timestamps are human-readable and reflect the actual time
#' when validation occurred on the machine running the script.
#'
#' All columns are character or integer to ensure type-safe CSV append operations.
#' When reading existing validation logs, force timestamp to character to avoid
#' readr auto-parsing to datetime<UTC>.
#'
#' @section CONTRACT:
#' - Returns a single-row tibble with consistent column types
#' - Timestamp uses local time with timezone (never UTC)
#' - All string columns are character class
#' - rows_affected is always integer
#' - Does NOT write to disk (caller handles file I/O)
#'
#' @section DOES NOT:
#' - Write to validation_log.csv (caller's responsibility)
#' - Validate that stage/event_type are from a controlled vocabulary
#' - Check if detector exists in study configuration
#' - Perform any file I/O operations
#'
#' @examples
#' \dontrun{
#' # Create a PASS event for column validation
#' event <- create_validation_event(
#'   stage = "1.5.1",
#'   event_type = "PASS",
#'   message = "All required columns present",
#'   rows_affected = 10L,
#'   script = "01_import_and_validate_calls_per_night. R"
#' )
#'
#' # Create a WARNING event for duplicates
#' event <- create_validation_event(
#'   stage = "1.5.2",
#'   event_type = "WARNING",
#'   message = "Duplicate detector×night combinations found",
#'   rows_affected = 3L,
#'   detector = "MULTIPLE",
#'   script = "01_import_and_validate_calls_per_night.R"
#' )
#'
#' # Append to validation log
#' validation_entries <- bind_rows(validation_entries, event)
#' }
#'
#' @export
create_validation_event <- function(stage,
                                    event_type,
                                    message,
                                    rows_affected = 0L,
                                    detector = "ALL",
                                    night = NA_Date_,
                                    script = "unknown") {


  # -------------------------
  # Input validation
  # -------------------------
  if (!is.character(stage) || length(stage) != 1) {
    stop("stage must be a single character string")
  }

  if (!is.character(event_type) || length(event_type) != 1) {
    stop("event_type must be a single character string")
  }

  valid_event_types <- c("PASS", "WARNING", "FAIL", "INFO", "EXCLUDE")
  if (!event_type %in% valid_event_types) {
    warning(sprintf(
      "event_type '%s' is not in standard vocabulary: %s",
      event_type,
      paste(valid_event_types, collapse = ", ")
    ))
  }

  if (!is.character(message) || length(message) != 1) {
    stop("message must be a single character string")
  }

  # -------------------------
  # Create tibble with local timestamp
  # -------------------------
  tibble::tibble(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    script = as.character(script),
    stage = as.character(stage),
    detector = as.character(detector),
    night = as.Date(night),
    event_type = as.character(event_type),
    message = as.character(message),
    rows_affected = as.integer(rows_affected)
  )
}


#' Append Validation Events to Log File
#'
#' @description
#' Type-safe append of validation events to validation_log.csv.
#' Handles the timestamp type mismatch issue by forcing character type
#' when reading existing log files.
#'
#' @param validation_entries Tibble.  Validation events to append (from create_validation_event)
#' @param log_path Character. Path to validation_log.csv
#' @param quiet Logical.  Suppress console messages?  Default = FALSE
#'
#' @return Invisible NULL.  Side effect: writes to log file.
#'
#' @details
#' This function handles a common issue where readr:: read_csv() auto-parses

#' timestamp columns as datetime<UTC>, causing bind_rows() to fail when
#' combining with new entries that have character timestamps.
#'
#' Solution: Force timestamp to character when reading existing log.
#'
#' @section CONTRACT:
#' - Creates log file if it doesn't exist
#' - Appends to existing log (never overwrites)
#' - Forces timestamp to character to avoid type mismatch
#' - Creates parent directory if missing
#'
#' @section DOES NOT:
#' - Validate the structure of validation_entries
#' - Remove or modify existing log entries
#' - Handle log rotation or archiving
#'
#' @examples
#' \dontrun
#' events <- bind_rows(
#'   create_validation_event("1.5.1", "PASS", "Columns OK", script = "01_import... R"),
#'   create_validation_event("1.5.2", "PASS", "No duplicates", script = "01_import...R")
#' )
#'
#' append_validation_log(events, here("outputs", "data_backbone", "validation_log. csv"))
#' }
#'
#' @export
append_validation_log <- function(validation_entries, log_path, quiet = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!inherits(validation_entries, "data.frame")) {
    stop("validation_entries must be a data frame or tibble")
  }

  if (! is.character(log_path) || length(log_path) != 1) {
    stop("log_path must be a single character string")
  }

  # -------------------------
  # Create directory if needed
  # -------------------------
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------
  # Write or append
  # -------------------------
  if (file.exists(log_path)) {
    # Force timestamp to character to avoid type mismatch with datetime<UTC>
    existing_log <- readr::read_csv(
      log_path,
      show_col_types = FALSE,
      col_types = readr::cols(timestamp = readr::col_character())
    )

    # Ensure new entries also have character timestamp
    validation_entries <- validation_entries %>%
      dplyr::mutate(timestamp = as.character(timestamp))

    combined_log <- dplyr::bind_rows(existing_log, validation_entries)
    readr::write_csv(combined_log, log_path)

    if (!quiet) {
      message(sprintf("✓ Appended %d validation event(s) to %s",
                      nrow(validation_entries), basename(log_path)))
    }
  } else {
    readr::write_csv(validation_entries, log_path)

    if (!quiet) {
      message(sprintf("✓ Created %s with %d event(s)",
                      basename(log_path), nrow(validation_entries)))
    }
  }

  invisible(NULL)
}
