# ==============================================================================
# core/config.R — Configuration Management Functions
# ==============================================================================
# PURPOSE: Load and validate YAML configuration files for study parameters
#
# These functions handle reading study design parameters, detector mappings,
# and analysis settings from YAML files. Ensures configuration consistency
# across all workflows.
# ==============================================================================

#' Load Study Parameters from YAML
#'
#' @description
#' Reads and validates the study_parameters.yaml configuration file.
#' Parses dates, extracts detector mappings, and validates required fields.
#' This is the central configuration loader for all workflows.
#'
#' @param config_path Character. Path to YAML config file.
#'   Default = here::here("inst", "config", "study_parameters.yaml")
#' @param validate Logical. Perform validation checks? Default = TRUE
#' @param quiet Logical. Suppress console messages? Default = FALSE
#'
#' @return List with named elements:
#'   - study_name: Character
#'   - timezone: Character
#'   - start_date: Date
#'   - end_date: Date
#'   - detector_mapping: Data frame with columns: detector, site, habitat
#'   - night_classification: List with threshold values
#'   - raw_input_path: Character
#'
#' @details
#' Required YAML structure:
#' ```yaml
#' study_parameters:
#'   study_name: "Study Name"
#'   timezone: "America/Chicago"
#'   start_date: "2023-10-04"
#'   end_date: "2023-11-01"
#'   detector_mapping:
#'     LPE: {site: "LP", habitat: "Edge"}
#'     ...
#' night_classification:
#'   pass_threshold: 12.5
#'   partial_threshold: 11.5
#'   short_threshold: 1.0
#' data_paths:
#'   raw_input: "data/raw/file.csv"
#' ```
#'
#' Validation checks:
#' - Config file exists
#' - Required top-level keys present
#' - Dates parseable and in order (start < end)
#' - Timezone valid
#' - Detector mapping non-empty
#' - Thresholds are numeric and properly ordered
#'
#' @section CONTRACT:
#' - Returns nested list matching YAML structure
#' - Converts date strings to Date objects
#' - Flattens detector_mapping to data frame for easy joining
#' - Validates required fields if validate = TRUE
#' - Returns NULL if file missing and validate = FALSE
#'
#' @section DOES NOT:
#' - Modify or write config files
#' - Check if detector codes match actual data
#' - Validate study window against data date range
#' - Handle multiple config versions (uses single config_version field)
#'
#' @examples
#' \dontrun{
#' params <- load_study_parameters()
#'
#' # Access elements:
#' study_name <- params$study_name
#' detector_map <- params$detector_mapping  # Data frame
#' start_date <- params$start_date  # Date object
#' }
#'
#' @export
load_study_parameters <- function(config_path = here::here("inst", "config", "study_parameters.yaml"),
                                  validate = TRUE,
                                  quiet = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!file.exists(config_path)) {
    if (validate) {
      stop(sprintf(
        "Configuration file not found: %s\n  Please create inst/config/study_parameters.yaml",
        config_path
      ))
    } else {
      if (!quiet) warning(sprintf("Config file not found: %s", config_path))
      return(NULL)
    }
  }

  # -------------------------
  # Load YAML
  # -------------------------
  if (!quiet) message(sprintf("Loading configuration: %s", basename(config_path)))

  config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e) {
    stop(sprintf(
      "Failed to parse YAML config: %s\n  Error: %s",
      config_path,
      e$message
    ))
  })

  # -------------------------
  # Extract and validate study parameters
  # -------------------------
  if (validate) {
    required_keys <- c("study_parameters", "night_classification", "data_paths")
    missing <- setdiff(required_keys, names(config))
    if (length(missing) > 0) {
      stop(sprintf(
        "Missing required config sections: %s",
        paste(missing, collapse = ", ")
      ))
    }

    required_study_params <- c("study_name", "timezone", "start_date", "end_date", "detector_mapping")
    missing_study <- setdiff(required_study_params, names(config$study_parameters))
    if (length(missing_study) > 0) {
      stop(sprintf(
        "Missing required study_parameters fields: %s",
        paste(missing_study, collapse = ", ")
      ))
    }
  }

  # -------------------------
  # Parse dates
  # -------------------------
  start_date <- tryCatch({
    as.Date(config$study_parameters$start_date)
  }, error = function(e) {
    stop(sprintf("Invalid start_date format: %s", config$study_parameters$start_date))
  })

  end_date <- tryCatch({
    as.Date(config$study_parameters$end_date)
  }, error = function(e) {
    stop(sprintf("Invalid end_date format: %s", config$study_parameters$end_date))
  })

  if (validate && start_date >= end_date) {
    stop(sprintf(
      "start_date must be before end_date\n  start_date: %s\n  end_date: %s",
      start_date,
      end_date
    ))
  }

  # -------------------------
  # Validate timezone
  # -------------------------
  tz <- config$study_parameters$timezone
  if (validate && !tz %in% OlsonNames()) {
    warning(sprintf(
      "Timezone '%s' may not be valid\n  See OlsonNames() for valid options",
      tz
    ))
  }

  # -------------------------
  # Flatten detector mapping to data frame
  # -------------------------
  detector_map_list <- config$study_parameters$detector_mapping

  if (validate && length(detector_map_list) == 0) {
    stop("detector_mapping cannot be empty")
  }

  detector_map_df <- data.frame(
    detector = names(detector_map_list),
    site = sapply(detector_map_list, function(x) x$site),
    habitat = sapply(detector_map_list, function(x) x$habitat),
    stringsAsFactors = FALSE
  )

  # -------------------------
  # Validate night classification thresholds
  # -------------------------
  if (validate) {
    required_thresholds <- c("pass_threshold", "partial_threshold", "short_threshold")
    missing_thresh <- setdiff(required_thresholds, names(config$night_classification))
    if (length(missing_thresh) > 0) {
      stop(sprintf(
        "Missing night_classification thresholds: %s",
        paste(missing_thresh, collapse = ", ")
      ))
    }

    # Check threshold ordering
    pass_t <- config$night_classification$pass_threshold
    partial_t <- config$night_classification$partial_threshold
    short_t <- config$night_classification$short_threshold

    if (!is.numeric(c(pass_t, partial_t, short_t))) {
      stop("All night_classification thresholds must be numeric")
    }

    if (!(pass_t > partial_t && partial_t > short_t && short_t > 0)) {
      stop(sprintf(
        "Thresholds must satisfy: pass > partial > short > 0\n  Got: pass=%.1f, partial=%.1f, short=%.1f",
        pass_t, partial_t, short_t
      ))
    }
  }

  # -------------------------
  # Construct return object
  # -------------------------
  result <- list(
    study_name = config$study_parameters$study_name,
    timezone = tz,
    start_date = start_date,
    end_date = end_date,
    detector_mapping = detector_map_df,
    night_classification = config$night_classification,
    raw_input_path = config$data_paths$raw_input
  )

  if (!quiet) {
    message("✓ Configuration loaded successfully")
    message(sprintf("  Study: %s", result$study_name))
    message(sprintf("  Period: %s to %s", result$start_date, result$end_date))
    message(sprintf("  Detectors: %d", nrow(result$detector_mapping)))
    message(sprintf("  Timezone: %s", result$timezone))
  }

  result
}


#' Save Study Parameters to YAML
#'
#' @description
#' Writes a study parameters list to YAML format. Useful for programmatically
#' generating or updating configuration files. Converts R objects (Date, data frame)
#' back to YAML-compatible formats.
#'
#' @param params List. Study parameters object (typically from load_study_parameters())
#' @param output_path Character. Path where YAML will be written
#' @param overwrite Logical. Overwrite existing file? Default = FALSE
#' @param quiet Logical. Suppress console messages? Default = FALSE
#'
#' @return Character. Path to written file (invisibly)
#'
#' @details
#' Converts R-specific types to YAML:
#' - Date → character (YYYY-MM-DD)
#' - Data frame (detector_mapping) → nested list
#'
#' Creates directory if missing.
#' Never overwrites without explicit permission.
#'
#' @section CONTRACT:
#' - Creates output directory if missing
#' - Fails if file exists and overwrite = FALSE
#' - Converts Date objects to ISO 8601 strings
#' - Converts detector_mapping data frame to nested YAML structure
#' - Returns output path invisibly
#'
#' @section DOES NOT:
#' - Validate parameter structure before writing
#' - Backup existing config file
#' - Handle version migration
#' - Preserve YAML comments from original file
#'
#' @examples
#' \dontrun{
#' params <- load_study_parameters()
#'
#' # Modify parameters
#' params$study_name <- "Updated Study Name"
#'
#' # Save to new location
#' save_study_parameters(
#'   params = params,
#'   output_path = here("inst", "config", "study_parameters_v2.yaml"),
#'   overwrite = FALSE
#' )
#' }
#'
#' @export
save_study_parameters <- function(params, output_path, overwrite = FALSE, quiet = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.list(params)) {
    stop("params must be a list")
  }

  if (!is.character(output_path) || length(output_path) != 1) {
    stop("output_path must be a single character string")
  }

  # Check overwrite
  if (file.exists(output_path) && !overwrite) {
    stop(sprintf(
      "File already exists: %s\n  Set overwrite = TRUE to replace",
      output_path
    ))
  }

  # -------------------------
  # Create directory
  # -------------------------
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # -------------------------
  # Convert R objects to YAML-compatible formats
  # -------------------------

  # Convert Date objects to strings
  if (inherits(params$start_date, "Date")) {
    start_date_str <- as.character(params$start_date)
  } else {
    start_date_str <- params$start_date
  }

  if (inherits(params$end_date, "Date")) {
    end_date_str <- as.character(params$end_date)
  } else {
    end_date_str <- params$end_date
  }

  # Convert detector_mapping data frame to nested list
  if (is.data.frame(params$detector_mapping)) {
    detector_map_list <- list()
    for (i in seq_len(nrow(params$detector_mapping))) {
      detector_code <- params$detector_mapping$detector[i]
      detector_map_list[[detector_code]] <- list(
        site = params$detector_mapping$site[i],
        habitat = params$detector_mapping$habitat[i]
      )
    }
  } else {
    detector_map_list <- params$detector_mapping
  }

  # -------------------------
  # Construct YAML structure
  # -------------------------
  yaml_structure <- list(
    config_version = 1,
    study_parameters = list(
      study_name = params$study_name,
      timezone = params$timezone,
      start_date = start_date_str,
      end_date = end_date_str,
      detector_mapping = detector_map_list
    ),
    night_classification = params$night_classification,
    data_paths = list(
      raw_input = params$raw_input_path
    )
  )

  # -------------------------
  # Write YAML
  # -------------------------
  if (!quiet) message(sprintf("Writing configuration to: %s", basename(output_path)))

  tryCatch({
    yaml::write_yaml(yaml_structure, file = output_path)
  }, error = function(e) {
    stop(sprintf(
      "Failed to write YAML config: %s\n  Error: %s",
      output_path,
      e$message
    ))
  })

  if (!quiet) message("✓ Configuration saved successfully")

  invisible(output_path)
}
