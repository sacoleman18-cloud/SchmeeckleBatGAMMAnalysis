# ==============================================================================
# validation/validation.R — Data Validation Functions
# ==============================================================================
# PURPOSE: Input validation, assertion helpers, and data quality checks
#
# These functions provide defensive programming patterns for data validation.
# They fail fast with clear error messages when data doesn't meet requirements.
# All assert_* functions throw errors on failure; validate_* functions return
# logical or diagnostic information.
# ==============================================================================

#' Assert Columns Exist in Data Frame
#'
#' @description
#' Checks that all required columns are present in a data frame.
#' Throws informative error if any are missing.
#'
#' @param df Data frame to check
#' @param required_cols Character vector of required column names
#' @param context Character. Context for error message (e.g., "for modeling"). Default = NULL
#'
#' @return Invisible NULL (side effect: stops on missing columns)
#'
#' @details
#' Column matching is case-sensitive.
#' Error message shows missing columns and available columns for debugging.
#'
#' @section CONTRACT:
#' - Throws error if any required columns missing
#' - Case-sensitive column matching
#' - Lists both missing and available columns in error
#' - Returns invisibly if all columns present
#'
#' @section DOES NOT:
#' - Check column types or values
#' - Modify data frame
#' - Suggest similar column names
#' - Handle case-insensitive matching
#'
#' @examples
#' \dontrun{
#' assert_columns_exist(df, c("detector_id", "Night", "calls_per_night"))
#' assert_columns_exist(df, c("habitat", "site"), context = "for GAMM random effects")
#' }
#'
#' @export
assert_columns_exist <- function(df, required_cols, context = NULL) {
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    context_msg <- if (!is.null(context)) paste0(" ", context) else ""

    stop(sprintf(
      "Missing required columns%s: %s\n  Available columns: %s",
      context_msg,
      paste(missing, collapse = ", "),
      paste(names(df), collapse = ", ")
    ))
  }

  invisible(NULL)
}


#' Assert Data Frame is Non-Empty
#'
#' @description
#' Checks that a data frame has at least one row.
#' Throws error if empty.
#'
#' @param df Data frame to check
#' @param context Character. Context for error message. Default = NULL
#'
#' @return Invisible NULL (side effect: stops on empty data)
#'
#' @details
#' Useful as early validation in pipeline stages to prevent processing
#' empty datasets that would cause cryptic downstream errors.
#'
#' @section CONTRACT:
#' - Throws error if nrow(df) == 0
#' - Returns invisibly if data has rows
#' - Includes context in error message if provided
#'
#' @section DOES NOT:
#' - Check for NA values
#' - Validate column existence
#' - Check minimum row count beyond zero
#'
#' @examples
#' \dontrun{
#' assert_not_empty(df)
#' assert_not_empty(df, context = "after filtering short nights")
#' }
#'
#' @export
assert_not_empty <- function(df, context = NULL) {
  if (nrow(df) == 0) {
    context_msg <- if (!is.null(context)) paste0(" ", context) else ""
    stop(sprintf("Data frame is empty%s", context_msg))
  }

  invisible(NULL)
}


#' Assert Column Has Valid Type
#'
#' @description
#' Checks that a column in a data frame has the expected type/class.
#' Throws error if type mismatch.
#'
#' @param df Data frame containing column
#' @param col_name Character. Name of column to check
#' @param expected_type Character. Expected type/class (e.g., "Date", "numeric", "character")
#'
#' @return Invisible NULL (side effect: stops on type mismatch)
#'
#' @details
#' Uses inherits() for class checking, works with S3/S4 classes.
#' For numeric, checks is.numeric() instead of class.
#'
#' @section CONTRACT:
#' - Throws error if column doesn't exist (calls assert_columns_exist)
#' - Throws error if type doesn't match
#' - Shows actual vs expected type in error
#' - Returns invisibly if type matches
#'
#' @section DOES NOT:
#' - Coerce types
#' - Check for NA values
#' - Handle multiple allowed types (caller must call multiple times)
#'
#' @examples
#' \dontrun{
#' assert_column_type(df, "Night", "Date")
#' assert_column_type(df, "recording_hours", "numeric")
#' assert_column_type(df, "detector_id", "character")
#' }
#'
#' @export
assert_column_type <- function(df, col_name, expected_type) {
  # First check column exists
  assert_columns_exist(df, col_name)

  actual_type <- class(df[[col_name]])[1]

  # Special handling for numeric (includes integer)
  if (expected_type == "numeric") {
    if (!is.numeric(df[[col_name]])) {
      stop(sprintf(
        "Column '%s' must be numeric\n  Actual type: %s",
        col_name,
        actual_type
      ))
    }
  } else {
    if (!inherits(df[[col_name]], expected_type)) {
      stop(sprintf(
        "Column '%s' must be of type '%s'\n  Actual type: %s",
        col_name,
        expected_type,
        actual_type
      ))
    }
  }

  invisible(NULL)
}


#' Assert Column Values in Valid Range
#'
#' @description
#' Checks that all values in a numeric column fall within expected range.
#' Throws error if any values are out of bounds.
#'
#' @param df Data frame containing column
#' @param col_name Character. Name of column to check
#' @param min_value Numeric. Minimum allowed value (inclusive). Default = -Inf
#' @param max_value Numeric. Maximum allowed value (inclusive). Default = Inf
#' @param allow_na Logical. Allow NA values? Default = FALSE
#'
#' @return Invisible NULL (side effect: stops on out-of-range values)
#'
#' @details
#' Range check is inclusive: min_value <= x <= max_value.
#' NA values trigger error unless allow_na = TRUE.
#'
#' @section CONTRACT:
#' - Throws error if column doesn't exist
#' - Throws error if column is not numeric
#' - Throws error if any values outside [min_value, max_value]
#' - Throws error if NA values present and allow_na = FALSE
#' - Shows count of invalid values in error
#' - Returns invisibly if all values valid
#'
#' @section DOES NOT:
#' - Remove or modify out-of-range values
#' - Suggest corrected values
#' - Check for infinite values (unless explicitly excluded via min/max)
#'
#' @examples
#' \dontrun{
#' # Recording hours must be 0-24
#' assert_column_range(df, "recording_hours", min_value = 0, max_value = 24)
#'
#' # Calls must be non-negative
#' assert_column_range(df, "calls_per_night", min_value = 0)
#'
#' # Allow NA in optional column
#' assert_column_range(df, "temperature", min_value = -40, max_value = 50, allow_na = TRUE)
#' }
#'
#' @export
assert_column_range <- function(df, col_name, min_value = -Inf, max_value = Inf, allow_na = FALSE) {
  # Check column exists and is numeric
  assert_columns_exist(df, col_name)
  assert_column_type(df, col_name, "numeric")

  values <- df[[col_name]]

  # Check NA values
  if (!allow_na && any(is.na(values))) {
    n_na <- sum(is.na(values))
    stop(sprintf(
      "Column '%s' contains %d NA value(s)\n  Set allow_na = TRUE if NA values are acceptable",
      col_name,
      n_na
    ))
  }

  # Check range (excluding NAs)
  out_of_range <- !is.na(values) & (values < min_value | values > max_value)

  if (any(out_of_range)) {
    n_invalid <- sum(out_of_range)
    invalid_vals <- values[out_of_range]

    # Show first few invalid values
    sample_vals <- if (n_invalid > 5) {
      c(head(invalid_vals, 5), "...")
    } else {
      invalid_vals
    }

    stop(sprintf(
      "Column '%s' has %d value(s) outside valid range [%.2f, %.2f]\n  Invalid values: %s",
      col_name,
      n_invalid,
      min_value,
      max_value,
      paste(sample_vals, collapse = ", ")
    ))
  }

  invisible(NULL)
}


#' Assert No NA Values in Column
#'
#' @description
#' Checks that a column contains no NA values.
#' Throws error if any NAs found.
#'
#' @param df Data frame containing column
#' @param col_name Character. Name of column to check
#' @param context Character. Context for error message. Default = NULL
#'
#' @return Invisible NULL (side effect: stops if NAs present)
#'
#' @details
#' Useful for required fields in modeling (e.g., predictors, random effects).
#'
#' @section CONTRACT:
#' - Throws error if column doesn't exist
#' - Throws error if any NA values present
#' - Shows count of NA values in error
#' - Returns invisibly if no NAs
#'
#' @section DOES NOT:
#' - Remove NA values
#' - Suggest imputation strategies
#' - Check for empty strings or other missing indicators
#'
#' @examples
#' \dontrun{
#' assert_no_na(df, "detector_id")
#' assert_no_na(df, "habitat", context = "for habitat effect modeling")
#' }
#'
#' @export
assert_no_na <- function(df, col_name, context = NULL) {
  assert_columns_exist(df, col_name)

  if (any(is.na(df[[col_name]]))) {
    n_na <- sum(is.na(df[[col_name]]))
    context_msg <- if (!is.null(context)) paste0(" ", context) else ""

    stop(sprintf(
      "Column '%s' contains %d NA value(s)%s\n  NAs must be removed or imputed before proceeding",
      col_name,
      n_na,
      context_msg
    ))
  }

  invisible(NULL)
}


#' Validate Data Frame Structure and Contents
#'
#' @description
#' Comprehensive validation function that checks multiple data quality aspects:
#' column existence, types, value ranges, and NA presence. Returns diagnostic
#' information rather than stopping on first error.
#'
#' @param df Data frame to validate
#' @param required_cols Character vector. Columns that must exist. Default = NULL
#' @param col_types Named list. Column name → expected type. Default = NULL
#' @param col_ranges Named list. Column name → list(min, max, allow_na). Default = NULL
#' @param no_na_cols Character vector. Columns that must have no NA. Default = NULL
#' @param stop_on_error Logical. Throw error on first validation failure? Default = TRUE
#'
#' @return List with validation results:
#'   - valid: Logical (TRUE if all checks passed)
#'   - errors: Character vector of error messages
#'   - warnings: Character vector of warning messages
#'
#' @details
#' Performs checks in order:
#' 1. Non-empty data frame
#' 2. Required columns exist
#' 3. Column types match
#' 4. Value ranges valid
#' 5. No NA in specified columns
#'
#' If stop_on_error = FALSE, collects all errors and returns diagnostic list.
#'
#' @section CONTRACT:
#' - Returns list with validation results
#' - Stops on first error if stop_on_error = TRUE
#' - Collects all errors if stop_on_error = FALSE
#' - Checks performed in fixed order (as listed above)
#'
#' @section DOES NOT:
#' - Modify data frame
#' - Fix validation errors
#' - Check for duplicates (use check_duplicates)
#' - Validate study window dates (use separate function)
#'
#' @examples
#' \dontrun{
#' # Basic validation with stopping on error
#' validate_data_frame(
#'   df,
#'   required_cols = c("detector_id", "Night", "calls_per_night"),
#'   col_types = list(Night = "Date", calls_per_night = "numeric"),
#'   no_na_cols = c("detector_id", "Night")
#' )
#'
#' # Validation with diagnostic return
#' result <- validate_data_frame(
#'   df,
#'   required_cols = c("detector_id", "recording_hours"),
#'   col_ranges = list(recording_hours = list(min = 0, max = 24, allow_na = FALSE)),
#'   stop_on_error = FALSE
#' )
#'
#' if (!result$valid) {
#'   cat("Validation errors:\n", paste(result$errors, collapse = "\n"))
#' }
#' }
#'
#' @export
validate_data_frame <- function(df,
                                required_cols = NULL,
                                col_types = NULL,
                                col_ranges = NULL,
                                no_na_cols = NULL,
                                stop_on_error = TRUE) {

  errors <- character(0)
  warnings <- character(0)

  # -------------------------
  # Check: Non-empty
  # -------------------------
  if (nrow(df) == 0) {
    errors <- c(errors, "Data frame is empty (0 rows)")
  }

  # -------------------------
  # Check: Required columns
  # -------------------------
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      errors <- c(errors, sprintf(
        "Missing required columns: %s",
        paste(missing, collapse = ", ")
      ))
    }
  }

  # -------------------------
  # Check: Column types
  # -------------------------
  if (!is.null(col_types)) {
    for (col in names(col_types)) {
      if (!col %in% names(df)) {
        warnings <- c(warnings, sprintf("Column '%s' not found (skipping type check)", col))
        next
      }

      expected <- col_types[[col]]
      actual <- class(df[[col]])[1]

      # Special handling for numeric
      if (expected == "numeric") {
        if (!is.numeric(df[[col]])) {
          errors <- c(errors, sprintf(
            "Column '%s' must be numeric (actual: %s)",
            col, actual
          ))
        }
      } else {
        if (!inherits(df[[col]], expected)) {
          errors <- c(errors, sprintf(
            "Column '%s' must be type '%s' (actual: %s)",
            col, expected, actual
          ))
        }
      }
    }
  }

  # -------------------------
  # Check: Value ranges
  # -------------------------
  if (!is.null(col_ranges)) {
    for (col in names(col_ranges)) {
      if (!col %in% names(df)) {
        warnings <- c(warnings, sprintf("Column '%s' not found (skipping range check)", col))
        next
      }

      if (!is.numeric(df[[col]])) {
        warnings <- c(warnings, sprintf("Column '%s' is not numeric (skipping range check)", col))
        next
      }

      range_spec <- col_ranges[[col]]
      min_val <- if ("min" %in% names(range_spec)) range_spec$min else -Inf
      max_val <- if ("max" %in% names(range_spec)) range_spec$max else Inf
      allow_na <- if ("allow_na" %in% names(range_spec)) range_spec$allow_na else FALSE

      values <- df[[col]]

      # Check NA
      if (!allow_na && any(is.na(values))) {
        errors <- c(errors, sprintf(
          "Column '%s' contains %d NA value(s)",
          col, sum(is.na(values))
        ))
      }

      # Check range
      out_of_range <- !is.na(values) & (values < min_val | values > max_val)
      if (any(out_of_range)) {
        errors <- c(errors, sprintf(
          "Column '%s' has %d value(s) outside range [%.2f, %.2f]",
          col, sum(out_of_range), min_val, max_val
        ))
      }
    }
  }

  # -------------------------
  # Check: No NA in specified columns
  # -------------------------
  if (!is.null(no_na_cols)) {
    for (col in no_na_cols) {
      if (!col %in% names(df)) {
        warnings <- c(warnings, sprintf("Column '%s' not found (skipping NA check)", col))
        next
      }

      if (any(is.na(df[[col]]))) {
        errors <- c(errors, sprintf(
          "Column '%s' contains %d NA value(s) (not allowed)",
          col, sum(is.na(df[[col]]))
        ))
      }
    }
  }

  # -------------------------
  # Return or stop
  # -------------------------
  valid <- length(errors) == 0

  result <- list(
    valid = valid,
    errors = errors,
    warnings = warnings
  )

  if (stop_on_error && !valid) {
    stop(sprintf(
      "Data validation failed:\n%s",
      paste(paste0("  - ", errors), collapse = "\n")
    ))
  }

  result
}


#' Check for Duplicate Rows
#'
#' @description
#' Identifies duplicate rows based on specified key columns.
#' Returns diagnostic information about duplicates found.
#'
#' @param df Data frame to check
#' @param key_cols Character vector. Columns that define uniqueness (e.g., c("detector_id", "Night"))
#' @param action Character. Action to take: "report", "stop", "warn". Default = "report"
#' @param return_duplicates Logical. Return duplicate rows? Default = FALSE
#'
#' @return List with:
#'   - has_duplicates: Logical
#'   - n_duplicates: Integer (number of duplicate rows)
#'   - duplicate_rows: Data frame (if return_duplicates = TRUE, else NULL)
#'
#' @details
#' Actions:
#' - "report": Return diagnostic info only
#' - "warn": Issue warning if duplicates found
#' - "stop": Throw error if duplicates found
#'
#' Duplicate rows are identified where all key_cols values match.
#'
#' @section CONTRACT:
#' - Checks key_cols exist before proceeding
#' - Returns list with diagnostic information
#' - Warns or stops based on action parameter
#' - Can return actual duplicate rows for inspection
#'
#' @section DOES NOT:
#' - Remove duplicates
#' - Modify data frame
#' - Determine which duplicate to keep
#' - Check for near-duplicates (fuzzy matching)
#'
#' @examples
#' \dontrun{
#' # Check for duplicates, throw error if found
#' check_duplicates(
#'   df,
#'   key_cols = c("detector_id", "Night"),
#'   action = "stop"
#' )
#'
#' # Get duplicate rows for inspection
#' result <- check_duplicates(
#'   df,
#'   key_cols = c("detector_id", "Night"),
#'   action = "warn",
#'   return_duplicates = TRUE
#' )
#'
#' if (result$has_duplicates) {
#'   View(result$duplicate_rows)
#' }
#' }
#'
#' @export
check_duplicates <- function(df, key_cols, action = "report", return_duplicates = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  assert_columns_exist(df, key_cols)

  valid_actions <- c("report", "stop", "warn")
  if (!action %in% valid_actions) {
    stop(sprintf(
      "action must be one of: %s",
      paste(valid_actions, collapse = ", ")
    ))
  }

  # -------------------------
  # Identify duplicates
  # -------------------------
  dup_mask <- duplicated(df[key_cols]) | duplicated(df[key_cols], fromLast = TRUE)
  n_duplicates <- sum(dup_mask)
  has_duplicates <- n_duplicates > 0

  duplicate_rows <- if (return_duplicates && has_duplicates) {
    df[dup_mask, ]
  } else {
    NULL
  }

  # -------------------------
  # Take action
  # -------------------------
  if (has_duplicates) {
    msg <- sprintf(
      "Found %d duplicate row(s) based on key columns: %s",
      n_duplicates,
      paste(key_cols, collapse = ", ")
    )

    if (action == "stop") {
      stop(msg)
    } else if (action == "warn") {
      warning(msg)
    }
  }

  # -------------------------
  # Return diagnostic info
  # -------------------------
  list(
    has_duplicates = has_duplicates,
    n_duplicates = n_duplicates,
    duplicate_rows = duplicate_rows
  )
}
