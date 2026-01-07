# ==============================================================================
# 01_import_and_validate_calls_per_night.R — DATA BACKBONE (LOCKED)
# ==============================================================================
# LAYER: BACKBONE
#
# PURPOSE
# -------
# Ingests an already-finalized detector × night calls-per-night grid and
# performs transparent validation. This script does NOT reconstruct data —
# it certifies an existing dataset for downstream GAMM analysis.
#
# This is the SINGLE SOURCE OF TRUTH for the entire project. All production
# and exploratory scripts consume outputs from this backbone.
#
# WORKFLOW POSITION
# -----------------
#   Previous: External KPro pipeline (generated calls_per_night_raw.csv)
#   Next:     01_PRODUCTION/10_model_nb_gamm.R
#
# INPUTS
# ------
# Files:
#   - data/raw/calls_per_night_raw.csv (user-provided, finalized grid)
#   - inst/config/study_parameters.yaml (study configuration)
#
# Configuration (from YAML):
#   - Study date range (start_date, end_date)
#   - Detector → Site/Habitat mapping
#   - Night classification thresholds
#
# PROCESSING STAGES
# -----------------
# Stage 1.1: Setup
#   - Load packages, source functions, load YAML config, initialize log
#
# Stage 1.2: Import Raw Data
#   - Read CSV from configured path
#   - Log initial row/column counts
#
# Stage 1.3: Schema Transformation
#   - Rename columns to snake_case
#   - Parse dates and datetimes
#   - Derive site and habitat from detector codes
#   - Drop pre-calculated calls_per_hour (we use offset instead)
#
# Stage 1.4: Night Classification
#   - Re-derive night_class based on recording_hours thresholds
#   - Pass / Partial / Short / Dead classification
#
# Stage 1.5: Validation
#   - Required columns, no duplicates, value ranges
#   - Study window bounds, grid completeness
#   - Log all validation outcomes
#
# Stage 1.6: Write Outputs
#   - validation_log.csv (append)
#   - calls_per_night_clean.csv
#   - calls_per_night_clean.rds
#
# OUTPUTS
# -------
# Files Created:
#   - 00_DATA_BACKBONE/outputs/calls_per_night_clean.csv
#   - 00_DATA_BACKBONE/outputs/calls_per_night_clean.rds
#   - 00_DATA_BACKBONE/outputs/validation_log.csv
#
# EXPECTED INPUT
# --------------
# A complete detector × night grid with columns:
#   Detector, Night, CallsPerNight, RecordingHours, StartDateTime, EndDateTime, Status
#
# Grid dimensions are determined dynamically from:
#   - Number of unique detectors in data
#   - Study date range from YAML config
#
# DEPENDENCIES
# ------------
# R Packages:
#   - tidyverse (dplyr, readr, tidyr, stringr)
#   - lubridate (date/time parsing)
#   - here (path management
#   - yaml (configuration)
#
# Custom Functions (via R/load_all.R):
#   - log_message(), initialize_pipeline_log()
#   - safe_read_csv()
#   - assert_*, validate_data_frame(), check_duplicates()
#   - load_study_parameters()
#
# USAGE
# -----
# source(here::here("00_DATA_BACKBONE/scripts/01_import_and_validate_calls_per_night.R"))
#
# CHANGELOG
# ---------
# 2026-01-06: Initial version for GAMM analysis pipeline
#
# ==============================================================================


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                         STAGE 1.1: SETUP                                   │
# └────────────────────────────────────────────────────────────────────────────┘

message("\n=== WORKFLOW 01: Import and Validate Calls Per Night ===\n")

# -------------------------
# Load packages
# -------------------------

library(tidyverse)
library(lubridate)
library(here)
library(yaml)

# -------------------------
# Source custom functions
# -------------------------

source(here("R", "load_all.R"))

# -------------------------
# Initialize logging
# -------------------------

log_path <- here("logs", "pipeline_log.txt")
initialize_pipeline_log(log_path)
log_message("=== WORKFLOW 01 START ===", log_path = log_path)

# -------------------------
# Load YAML configuration
# -------------------------

config_path <- here("inst", "config", "study_parameters.yaml")

if (!file.exists(config_path)) {
  stop(sprintf(

    "Configuration file not found: %s\n  Please create study_parameters.yaml with study settings.",
    config_path
  ))
}

config <- yaml::read_yaml(config_path)

# Extract configuration values
study_name     <- config$study_parameters$study_name
timezone       <- config$study_parameters$timezone
study_start    <- as.Date(config$study_parameters$start_date)
study_end      <- as.Date(config$study_parameters$end_date)
detector_map   <- config$study_parameters$detector_mapping
raw_input_path <- here(config$data_paths$raw_input)

# Night classification thresholds
pass_threshold    <- config$night_classification$pass_threshold
partial_threshold <- config$night_classification$partial_threshold
short_threshold   <- config$night_classification$short_threshold

message("✓ Configuration loaded")
message(sprintf("  Study: %s", study_name))
message(sprintf("  Date range: %s to %s", study_start, study_end))
message(sprintf("  Timezone: %s", timezone))
message(sprintf("  Detectors configured: %d", length(detector_map)))

log_message(sprintf("[Stage 1.1] Configuration loaded: %s", study_name), log_path = log_path)


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                       STAGE 1.2: IMPORT RAW DATA                           │
# └────────────────────────────────────────────────────────────────────────────┘

message("\nImporting raw calls-per-night data...")

# -------------------------
# Validate input file exists
# -------------------------

if (!file.exists(raw_input_path)) {
  stop(sprintf(
    "Raw input file not found: %s\n  Please place your calls_per_night CSV in the configured location.",
    raw_input_path
  ))
}

# -------------------------
# Read CSV
# -------------------------

df_raw <- readr::read_csv(
  raw_input_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# -------------------------
# Initial diagnostics
# -------------------------

n_rows_raw <- nrow(df_raw)
n_cols_raw <- ncol(df_raw)

message(sprintf("✓ Imported: %s rows, %s columns",
                format(n_rows_raw, big.mark = ","),
                n_cols_raw))
message(sprintf("  Columns: %s", paste(names(df_raw), collapse = ", ")))

log_message(sprintf("[Stage 1.2] Imported %d rows, %d columns from %s",
                    n_rows_raw, n_cols_raw, basename(raw_input_path)),
            log_path = log_path)


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                    STAGE 1.3: SCHEMA TRANSFORMATION                        │
# └────────────────────────────────────────────────────────────────────────────┘

message("\nTransforming schema...")

# -------------------------
# 1.3.1: Rename columns to snake_case
# -------------------------

df <- df_raw %>%
  rename(
    detector        = Detector,
    night           = Night,
    calls_per_night = CallsPerNight,
    recording_hours = RecordingHours,
    start_datetime  = StartDateTime,
    end_datetime    = EndDateTime
    # Note: Status and CallsPerHour will be handled below
  )

# Remove calls_per_hour if present (we use offset, not pre-calculated rate)
if ("CallsPerHour" %in% names(df_raw)) {
  df <- df %>% select(-any_of("CallsPerHour"))
  message("  Dropped CallsPerHour column (using offset instead)")
}

# Remove original Status if present (will be re-derived)
if ("Status" %in% names(df_raw)) {
  df <- df %>% select(-any_of("Status"))
  message("  Dropped Status column (will re-derive as night_class)")
}

message("  ✓ Columns renamed to snake_case")

# -------------------------
# 1.3.2: Parse dates
# -------------------------

df <- df %>%
  mutate(
    night = lubridate::mdy(night)
  )

# Validate parsing
n_na_night <- sum(is.na(df$night))
if (n_na_night > 0) {
  warning(sprintf("%d night values failed to parse", n_na_night))
}

message("  ✓ Parsed night column as Date")

# -------------------------
# 1.3.3: Parse datetimes
# -------------------------

df <- df %>%
  mutate(
    start_datetime = lubridate::mdy_hm(start_datetime, quiet = TRUE),
    end_datetime   = lubridate::mdy_hm(end_datetime, quiet = TRUE)
  )

message("  ✓ Parsed start_datetime and end_datetime")

# -------------------------
# 1.3.4: Convert numeric columns
# -------------------------

df <- df %>%
  mutate(
    calls_per_night = as.integer(calls_per_night),
    recording_hours = as.numeric(recording_hours)
  )

message("  ✓ Converted calls_per_night (integer) and recording_hours (numeric)")

# -------------------------
# 1.3.5: Derive site and habitat from detector codes
# -------------------------

# Build lookup tibble from YAML config
detector_lookup <- tibble(
  detector = names(detector_map),
  site     = map_chr(detector_map, ~ .x$site),
  habitat  = map_chr(detector_map, ~ .x$habitat)
)

# Join to main data
df <- df %>%
  left_join(detector_lookup, by = "detector")

# Check for unmapped detectors
unmapped <- df %>%
  filter(is.na(site) | is.na(habitat)) %>%
  distinct(detector) %>%
  pull(detector)

if (length(unmapped) > 0) {
  warning(sprintf(
    "Detectors not found in YAML mapping: %s\n  These will have NA for site/habitat.",
    paste(unmapped, collapse = ", ")
  ))
}

message(sprintf("  ✓ Derived site and habitat from detector mapping (%d detectors)",
                nrow(detector_lookup)))

# -------------------------
# 1.3.6: Create detector_id for random effects
# -------------------------

# detector_id is the unique identifier for the random effect in the GAMM
# Format: site_habitat (e.g., "LP_Edge", "MC_Interior")
df <- df %>%
  mutate(
    detector_id = paste(site, habitat, sep = "_")
  )

message("  ✓ Created detector_id for random effects")

log_message("[Stage 1.3] Schema transformation complete", log_path = log_path)


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                    STAGE 1.4: NIGHT CLASSIFICATION                         │
# └────────────────────────────────────────────────────────────────────────────┘

message("\nClassifying nights by recording effort...")

# -------------------------
# Apply classification thresholds from YAML
# -------------------------

df <- df %>%
  mutate(
    night_class = case_when(
      is.na(recording_hours)        ~ "Dead",
      recording_hours == 0          ~ "Dead",
      recording_hours >= pass_threshold    ~ "Pass",
      recording_hours >= partial_threshold ~ "Partial",
      recording_hours >= short_threshold   ~ "Short",
      recording_hours > 0                  ~ "Short",
      TRUE                                 ~ "Dead"
    )
  )

# -------------------------
# Summarize classification
# -------------------------

class_summary <- df %>%
  count(night_class) %>%
  mutate(pct = round(100 * n / sum(n), 1))

message("  Night classification summary:")
for (i in seq_len(nrow(class_summary))) {
  message(sprintf("    %s: %d nights (%.1f%%)",
                  class_summary$night_class[i],
                  class_summary$n[i],
                  class_summary$pct[i]))
}

log_message(sprintf("[Stage 1.4] Night classification: Pass=%d, Partial=%d, Short=%d, Dead=%d",
                    sum(df$night_class == "Pass"),
                    sum(df$night_class == "Partial"),
                    sum(df$night_class == "Short"),
                    sum(df$night_class == "Dead")),
            log_path = log_path)


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                       STAGE 1.5: VALIDATION                                │
# └────────────────────────────────────────────────────────────────────────────┘

message("\nValidating data integrity...")

# Initialize validation log entries
validation_entries <- tibble(
  timestamp    = character(),
  script       = character(),
  stage        = character(),
  detector     = character(),
  night        = as.Date(character()),
  event_type   = character(),
  message      = character(),
  rows_affected = integer()
)

# -------------------------
# 1.5.1: Required columns
# -------------------------

required_cols <- c("detector", "night", "calls_per_night", "recording_hours",
                   "site", "habitat", "detector_id", "night_class")

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns after transformation: %s",
               paste(missing_cols, collapse = ", ")))
}

message("  ✓ All required columns present")

validation_entries <- validation_entries %>%
  add_row(
    timestamp = as.character(Sys.time()),
    script = "01_import_and_validate_calls_per_night.R",
    stage = "1.5.1",
    detector = "ALL",
    night = NA_Date_,
    event_type = "PASS",
    message = "All required columns present",
    rows_affected = ncol(df)
  )

# -------------------------
# 1.5.2: No duplicate detector × night combinations
# -------------------------

dup_check <- df %>%
  group_by(detector, night) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(dup_check) > 0) {
  warning(sprintf("%d duplicate detector×night combinations found", nrow(dup_check)))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.2",
      detector = "MULTIPLE",
      night = NA_Date_,
      event_type = "WARNING",
      message = "Duplicate detector×night combinations found",
      rows_affected = nrow(dup_check)
    )
} else {
  message("  ✓ No duplicate detector×night combinations")

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.2",
      detector = "ALL",
      night = NA_Date_,
      event_type = "PASS",
      message = "No duplicate detector×night combinations",
      rows_affected = 0
    )
}

# -------------------------
# 1.5.3: Value ranges
# -------------------------

# Calls must be non-negative
negative_calls <- df %>% filter(calls_per_night < 0)
if (nrow(negative_calls) > 0) {
  stop(sprintf("%d rows have negative calls_per_night", nrow(negative_calls)))
}

# Recording hours must be non-negative
negative_hours <- df %>% filter(recording_hours < 0)
if (nrow(negative_hours) > 0) {
  stop(sprintf("%d rows have negative recording_hours", nrow(negative_hours)))
}

# Recording hours should not exceed 24
excessive_hours <- df %>% filter(recording_hours > 24)
if (nrow(excessive_hours) > 0) {
  warning(sprintf("%d rows have recording_hours > 24", nrow(excessive_hours)))
}

message("  ✓ Value ranges valid (calls≥0, hours≥0)")

validation_entries <- validation_entries %>%
  add_row(
    timestamp = as.character(Sys.time()),
    script = "01_import_and_validate_calls_per_night.R",
    stage = "1.5.3",
    detector = "ALL",
    night = NA_Date_,
    event_type = "PASS",
    message = "Value ranges valid (calls≥0, hours≥0)",
    rows_affected = nrow(df)
  )

# -------------------------
# 1.5.4: Study window bounds
# -------------------------

nights_before <- df %>% filter(night < study_start)
nights_after  <- df %>% filter(night > study_end)

if (nrow(nights_before) > 0) {
  warning(sprintf("%d nights before study start (%s)", nrow(nights_before), study_start))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.4",
      detector = "MULTIPLE",
      night = NA_Date_,
      event_type = "WARNING",
      message = sprintf("Nights before study start (%s)", study_start),
      rows_affected = nrow(nights_before)
    )
}

if (nrow(nights_after) > 0) {
  warning(sprintf("%d nights after study end (%s)", nrow(nights_after), study_end))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.4",
      detector = "MULTIPLE",
      night = NA_Date_,
      event_type = "WARNING",
      message = sprintf("Nights after study end (%s)", study_end),
      rows_affected = nrow(nights_after)
    )
}

if (nrow(nights_before) == 0 && nrow(nights_after) == 0) {
  message(sprintf("  ✓ All nights within study window (%s to %s)", study_start, study_end))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.4",
      detector = "ALL",
      night = NA_Date_,
      event_type = "PASS",
      message = sprintf("All nights within study window (%s to %s)", study_start, study_end),
      rows_affected = nrow(df)
    )
}

# -------------------------
# 1.5.5: Grid completeness
# -------------------------

# Calculate expected grid size from data and config
n_detectors <- n_distinct(df$detector)
n_nights    <- as.integer(study_end - study_start) + 1L
expected_rows <- n_detectors * n_nights
actual_rows   <- nrow(df)

completeness_pct <- round(100 * actual_rows / expected_rows, 1)

if (actual_rows < expected_rows) {
  message(sprintf("  ⚠ Grid incomplete: %d rows (expected %d for %d detectors × %d nights = %.1f%%)",
                  actual_rows, expected_rows, n_detectors, n_nights, completeness_pct))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.5",
      detector = "ALL",
      night = NA_Date_,
      event_type = "WARNING",
      message = sprintf("Grid incomplete: %d/%d rows (%.1f%%)",
                        actual_rows, expected_rows, completeness_pct),
      rows_affected = expected_rows - actual_rows
    )
} else if (actual_rows == expected_rows) {
  message(sprintf("  ✓ Grid complete: %d rows (%d detectors × %d nights)",
                  actual_rows, n_detectors, n_nights))

  validation_entries <- validation_entries %>%
    add_row(
      timestamp = as.character(Sys.time()),
      script = "01_import_and_validate_calls_per_night.R",
      stage = "1.5.5",
      detector = "ALL",
      night = NA_Date_,
      event_type = "PASS",
      message = sprintf("Grid complete: %d detectors × %d nights", n_detectors, n_nights),
      rows_affected = actual_rows
    )
} else {
  warning(sprintf("More rows than expected: %d (expected %d)", actual_rows, expected_rows))
}

log_message(sprintf("[Stage 1.5] Validation complete: %d rows validated", nrow(df)),
            log_path = log_path)


# ┌────────────────────────────────────────────────────────────────────────────┐
# │                       STAGE 1.6: WRITE OUTPUTS                             │
# └────────────────────────────────────────────────────────────────────────────┘

message("\nWriting outputs...")

# -------------------------
# Ensure output directory exists
# -------------------------

output_dir <- here("00_DATA_BACKBONE", "outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message(sprintf("  Created directory: %s", output_dir))
}

# -------------------------
# 1.6.1: Write validation log
# -------------------------

validation_log_path <- here(output_dir, "validation_log.csv")

if (file.exists(validation_log_path)) {
  # Append to existing log
  existing_log <- readr::read_csv(validation_log_path, show_col_types = FALSE)
  combined_log <- bind_rows(existing_log, validation_entries)
  readr::write_csv(combined_log, validation_log_path)
  message(sprintf("  ✓ Appended %d entries to validation_log.csv", nrow(validation_entries)))
} else {
  readr::write_csv(validation_entries, validation_log_path)
  message(sprintf("  ✓ Created validation_log.csv with %d entries", nrow(validation_entries)))
}

# -------------------------
# 1.6.2: Write clean CSV
# -------------------------

# Select and order final columns
df_clean <- df %>%
  select(
    detector,
    detector_id,
    site,
    habitat,
    night,
    calls_per_night,
    recording_hours,
    night_class,
    start_datetime,
    end_datetime
  ) %>%
  arrange(detector, night)

csv_path <- here(output_dir, "calls_per_night_clean.csv")
readr::write_csv(df_clean, csv_path)
message(sprintf("  ✓ Wrote calls_per_night_clean.csv (%d rows)", nrow(df_clean)))

# -------------------------
# 1.6.3: Write clean RDS
# -------------------------

rds_path <- here(output_dir, "calls_per_night_clean.rds")
saveRDS(df_clean, rds_path)
message(sprintf("  ✓ Wrote calls_per_night_clean.rds (%d rows)", nrow(df_clean)))

log_message(sprintf("[Stage 1.6] Outputs written: %d rows to %s",
                    nrow(df_clean), output_dir),
            log_path = log_path)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                         WORKFLOW 01 COMPLETE                               ║
# ╚════════════════════════════════════════════════════════════════════════════╝

message("\n")
message("========================================")
message("✓ WORKFLOW 01 COMPLETE")
message("========================================")
message(sprintf("  Study: %s", study_name))
message(sprintf("  Rows: %s", format(nrow(df_clean), big.mark = ",")))
message(sprintf("  Detectors: %d", n_distinct(df_clean$detector)))
message(sprintf("  Sites: %d", n_distinct(df_clean$site)))
message(sprintf("  Nights: %d", n_distinct(df_clean$night)))
message(sprintf("  Date range: %s to %s", min(df_clean$night), max(df_clean$night)))
message("")
message("  Night classification:")
for (i in seq_len(nrow(class_summary))) {
  message(sprintf("    %s: %d (%.1f%%)",
                  class_summary$night_class[i],
                  class_summary$n[i],
                  class_summary$pct[i]))
}
message("")
message("  Outputs:")
message(sprintf("    %s", csv_path))
message(sprintf("    %s", rds_path))
message(sprintf("    %s", validation_log_path))
message("========================================\n")

log_message("=== WORKFLOW 01 COMPLETE ===", log_path = log_path)

# -------------------------
# Store in environment for downstream use
# -------------------------

calls_per_night_clean <- df_clean

message("Data available as: calls_per_night_clean")
