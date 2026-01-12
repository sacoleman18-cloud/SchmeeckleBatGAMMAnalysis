# ==============================================================================
# 01_import_and_validate_calls_per_night.R — DATA BACKBONE (LOCKED)
# ==============================================================================
# LAYER:  BACKBONE
#
# PURPOSE
# -------
# Ingest an already-finalized detector × night calls-per-night grid and
# perform transparent validation.  This workflow does NOT reconstruct data —
# it certifies an existing dataset for downstream GAMM analysis.
#
# This is the SINGLE SOURCE OF TRUTH for the entire project.  All production
# and exploratory scripts consume outputs from this backbone.
#
# WORKFLOW POSITION
# -----------------
#   Previous:  External KPro pipeline (generated calls_per_night_raw.csv)
#   Next:      R/workflows/production/02_model_nb_gamm. R
#
# INPUTS
# ------
# Files:
#   - inst/config/study_parameters.yaml (study configuration; required)
#   - data_paths.raw_input from YAML (calls-per-night grid; required)
#
# PROCESSING STAGES
# -----------------
# Stage 1.1: Setup
#   - Anchor project root, source functions, load config, initialize log
#
# Stage 1.2: Import Raw Data
#   - Read raw CSV specified in YAML
#   - Validate required raw columns
#
# Stage 1.3: Schema Transformation
#   - Rename to canonical snake_case
#   - Parse night (Date) + datetimes (tz-aware)
#   - Join detector mapping (site/habitat)
#   - Drop non-authoritative columns
#   - Enforce canonical schema
#
# Stage 1.4: Night Classification
#   - Re-classify using YAML thresholds (Pass/Partial/Short/Dead)
#
# Stage 1.5: Validation (Global Checks)
#   - Required columns present
#   - Duplicate detector×night check
#   - Value ranges check
#   - Study window bounds check
#   - Grid completeness check
#
# Stage 1.6: Write Outputs
#   - Append validation events to validation_log.csv
#   - Write calls_per_night_clean.csv and calls_per_night_clean.rds
#
# OUTPUTS
# -------
# Files Created / Updated:
#   - outputs/data_backbone/validation_log.csv (append-only)
#   - outputs/data_backbone/calls_per_night_clean. csv
#   - outputs/data_backbone/calls_per_night_clean.rds
#
# PERFORMANCE EXPECTATIONS
# ------------------------
# Approximate dataset sizes and runtimes:
#   - Small (< 500 rows): < 5 seconds
#   - Medium (500-5000 rows): 5-15 seconds
#   - Large (> 5000 rows): 15-60 seconds
#
# DEPENDENCIES
# ------------
# R Packages:
#   - tidyverse, lubridate, here, yaml
#
# Custom Functions (via R/functions/load_all. R):
#   - core/utilities.R: log_message(), initialize_pipeline_log(), safe_read_csv()
#   - core/config.R: load_study_parameters()
#   - validation/validation.R:  assert_*, validate_data_frame(), check_duplicates(),
#                              create_validation_event(), append_validation_log()
#
# TROUBLESHOOTING
# ---------------
# Issue: "Configuration file not found" error
# Fix:    Ensure inst/config/study_parameters.yaml exists with correct structure
#
# Issue: "Missing required columns" error
# Fix:   Verify raw CSV has columns:  Detector, Night, CallsPerNight, RecordingHours,
#        StartDateTime, EndDateTime
#
# Issue:  "Can't combine timestamp <datetime> and <character>" error
# Fix:    Delete outputs/data_backbone/validation_log.csv and re-run workflow
#
# USAGE
# -----
# source(here:: here("R", "workflows", "data_backbone", "01_import_and_validate_calls_per_night.R"))
#
# MAINTAINER NOTES
# ----------------
# - Outputs are written to outputs/data_backbone/ (root-level) to keep code and outputs separate.
# - Dead nights are retained for grid completeness but must be excluded from modeling
#   because offset(log(recording_hours)) cannot accept NA or <= 0.
# - Timestamps use local machine time with timezone abbreviation (e.g., "2026-01-11 14:32:15 CST")
# - Validation event creation uses create_validation_event() from validation. R
#
# CHANGELOG
# ---------
# 2026-01-11: Moved append_validation_event() to validation.R as create_validation_event()
#             Fixed timestamp type mismatch in validation_log.csv append logic
#             Added PERFORMANCE EXPECTATIONS and TROUBLESHOOTING sections
#             Fixed here:: i_am() path typo (0data_backbone -> data_backbone)
#             Added [BACKBONE] prefix to all log messages
#             Changed output_dir from 00_data_backbone to data_backbone
# 2026-01-10: Move backbone outputs to root-level outputs/data_backbone/
#
# ==============================================================================

# LAYER: BACKBONE

# -------------------------
# Anchor project root (portable, deterministic)
# -------------------------
here::i_am("R/workflows/data_backbone/01_import_and_validate_calls_per_night.R")

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║             WORKFLOW 01: IMPORT AND VALIDATE CALLS PER NIGHT                ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║             WORKFLOW 01: IMPORT AND VALIDATE CALLS PER NIGHT                ║")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")


# ------------------------------------------------------------------------------
# STAGE 1.1: SETUP
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                     STAGE 1.1: Setup                           │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Setting up workflow environment...")

# -------------------------
# Load packages
# -------------------------
library(tidyverse)
library(lubridate)
library(here)
library(yaml)

# -------------------------
# Source custom functions (portable)
# -------------------------
source(here::here("R", "functions", "load_all.R"))

# -------------------------
# Initialize logging
# -------------------------
log_path <- here:: here("logs", "pipeline_log.txt")

initialize_pipeline_log(
  log_path = log_path,
  workflow_name = "01_import_and_validate_calls_per_night",
  script_path = here::here("R", "workflows", "data_backbone", "01_import_and_validate_calls_per_night. R")
)

log_message("[BACKBONE] === WORKFLOW 01 START ===", log_path = log_path)

# -------------------------
# Load configuration (YAML-driven via centralized loader)
# -------------------------
params <- load_study_parameters(
  config_path = here:: here("inst", "config", "study_parameters.yaml"),
  validate = TRUE,
  quiet = FALSE
)

study_name <- params$study_name
timezone <- params$timezone
study_start <- params$start_date
study_end <- params$end_date
detector_map_df <- params$detector_mapping

pass_threshold <- params$night_classification$pass_threshold
partial_threshold <- params$night_classification$partial_threshold
short_threshold <- params$night_classification$short_threshold

raw_input_path <- here::here(params$raw_input_path)

# Store script name for validation events
script_name <- "01_import_and_validate_calls_per_night. R"

message("✓ Setup complete")
message(sprintf("  Study:  %s", study_name))
message(sprintf("  Date range (YAML): %s to %s", study_start, study_end))
message(sprintf("  Timezone: %s", timezone))
message(sprintf("  Detectors configured: %d", nrow(detector_map_df)))
message(sprintf("  Raw input: %s", raw_input_path))

log_message(sprintf("[BACKBONE] [Stage 1.1] Setup complete:  %s (%s to %s)",
                    study_name, study_start, study_end),
            log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 1.2: IMPORT RAW DATA
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                 STAGE 1.2: Import Raw Data                      │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Importing raw calls-per-night data...")

if (! file.exists(raw_input_path)) {
  stop(sprintf(
    "Raw input file not found: %s\n  Fix: Update inst/config/study_parameters.yaml -> data_paths.raw_input",
    raw_input_path
  ))
}

df_raw <- safe_read_csv(
  file_path = raw_input_path,
  required_cols = c(
    "Detector", "Night", "CallsPerNight", "RecordingHours",
    "StartDateTime", "EndDateTime"
  ),
  log_path = log_path,
  quiet = FALSE
)

message(sprintf("✓ Raw import complete: %s rows, %d columns",
                format(nrow(df_raw), big.mark = ","),
                ncol(df_raw)))

log_message(sprintf("[BACKBONE] [Stage 1.2] Imported %d rows, %d cols from %s",
                    nrow(df_raw), ncol(df_raw), basename(raw_input_path)),
            log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 1.3: SCHEMA TRANSFORMATION
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│              STAGE 1.3: Schema Transformation                   │")
message("└──────────────────────��─────────────────────────────────────────┘\n")

message("Transforming schema to canonical backbone format...")

# -------------------------
# Define canonical columns (keep only these)
# -------------------------
canonical_raw_cols <- c("Detector", "Night", "CallsPerNight", "RecordingHours",
                        "StartDateTime", "EndDateTime")

df <- df_raw %>%
  # Keep only canonical columns (drop everything else)
  select(all_of(canonical_raw_cols)) %>%
  # Rename to snake_case
  rename(
    detector = Detector,
    night = Night,
    calls_per_night = CallsPerNight,
    recording_hours = RecordingHours,
    start_datetime = StartDateTime,
    end_datetime = EndDateTime
  ) %>%
  # Parse types
  mutate(
    night = lubridate::ymd(night),
    start_datetime = lubridate::mdy_hm(start_datetime, tz = timezone, quiet = TRUE),
    end_datetime = lubridate::mdy_hm(end_datetime, tz = timezone, quiet = TRUE),
    calls_per_night = as.integer(calls_per_night),
    recording_hours = as.numeric(recording_hours)
  ) %>%
  # Join detector mapping (site/habitat)
  left_join(detector_map_df, by = "detector") %>%
  # Create detector_id
  mutate(detector_id = paste(site, habitat, sep = "_")) %>%
  # Enforce canonical column order
  select(
    detector,
    detector_id,
    site,
    habitat,
    night,
    calls_per_night,
    recording_hours,
    start_datetime,
    end_datetime
  )

n_na_night <- sum(is.na(df$night))
if (n_na_night > 0) {
  warning(sprintf(
    "%d night value(s) failed to parse to Date using ymd(). Check the raw CSV Night column.",
    n_na_night
  ))
}

unmapped <- df %>%
  filter(is.na(site) | is.na(habitat)) %>%
  distinct(detector) %>%
  pull(detector)

if (length(unmapped) > 0) {
  warning(sprintf(
    "Detector(s) not found in YAML mapping: %s\n  These rows will retain NA site/habitat.",
    paste(unmapped, collapse = ", ")
  ))
}

message("✓ Schema transformation complete")
message(sprintf("  Rows:  %s", format(nrow(df), big.mark = ",")))
message(sprintf("  Columns: %s", paste(names(df), collapse = ", ")))

log_message("[BACKBONE] [Stage 1.3] Schema transformation complete", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 1.4: NIGHT CLASSIFICATION
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│             STAGE 1.4: Night Classification                     │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Classifying nights by recording effort...")

df <- df %>%
  mutate(
    night_class = case_when(
      is.na(recording_hours) ~ "Dead",
      recording_hours == 0 ~ "Dead",
      recording_hours >= pass_threshold ~ "Pass",
      recording_hours >= partial_threshold ~ "Partial",
      recording_hours >= short_threshold ~ "Short",
      recording_hours > 0 ~ "Short",
      TRUE ~ "Dead"
    )
  )

class_summary <- df %>%
  count(night_class) %>%
  mutate(pct = round(100 * n / sum(n), 1))

message("✓ Night classification complete")
for (i in seq_len(nrow(class_summary))) {
  message(sprintf("  %s: %d (%.1f%%)",
                  class_summary$night_class[i],
                  class_summary$n[i],
                  class_summary$pct[i]))
}

log_message(sprintf("[BACKBONE] [Stage 1.4] Night classification:  Pass=%d, Partial=%d, Short=%d, Dead=%d",
                    sum(df$night_class == "Pass"),
                    sum(df$night_class == "Partial"),
                    sum(df$night_class == "Short"),
                    sum(df$night_class == "Dead")),
            log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 1.5: VALIDATION (GLOBAL CHECKS)
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│              STAGE 1.5: Validation (Global Checks)              │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Validating data integrity...")

# Initialize empty validation entries tibble
validation_entries <- tibble(
  timestamp = character(),
  script = character(),
  stage = character(),
  detector = character(),
  night = as.Date(character()),
  event_type = character(),
  message = character(),
  rows_affected = integer()
)

# -------------------------
# Check 1.5.1: Required columns
# -------------------------
required_cols <- c(
  "detector", "night", "calls_per_night", "recording_hours",
  "site", "habitat", "detector_id", "night_class"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(sprintf(
    "Missing required columns after transformation: %s",
    paste(missing_cols, collapse = ", ")
  ))
}

message("✓ Required columns present")
validation_entries <- bind_rows(
  validation_entries,
  create_validation_event(
    stage = "1.5.1",
    event_type = "PASS",
    message = "All required columns present",
    rows_affected = length(required_cols),
    script = script_name
  )
)

# -------------------------
# Check 1.5.2: Duplicate detector×night
# -------------------------
dup_check <- df %>%
  count(detector, night) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning(sprintf("%d duplicate detector×night combination(s) found", nrow(dup_check)))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.2",
      event_type = "WARNING",
      message = "Duplicate detector×night combinations found",
      rows_affected = nrow(dup_check),
      detector = "MULTIPLE",
      script = script_name
    )
  )
} else {
  message("✓ No duplicate detector×night combinations")
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.2",
      event_type = "PASS",
      message = "No duplicate detector×night combinations",
      rows_affected = 0L,
      script = script_name
    )
  )
}

# -------------------------
# Check 1.5.3: Value ranges
# -------------------------
n_negative_calls <- sum(df$calls_per_night < 0, na.rm = TRUE)
if (n_negative_calls > 0) stop(sprintf("%d row(s) have negative calls_per_night", n_negative_calls))

n_negative_hours <- sum(df$recording_hours < 0, na.rm = TRUE)
if (n_negative_hours > 0) stop(sprintf("%d row(s) have negative recording_hours", n_negative_hours))

n_excessive_hours <- sum(df$recording_hours > 24, na.rm = TRUE)
if (n_excessive_hours > 0) warning(sprintf("%d row(s) have recording_hours > 24", n_excessive_hours))

message("✓ Value ranges valid (calls≥0, hours≥0)")
validation_entries <- bind_rows(
  validation_entries,
  create_validation_event(
    stage = "1.5.3",
    event_type = "PASS",
    message = "Value ranges valid (calls≥0, hours≥0)",
    rows_affected = nrow(df),
    script = script_name
  )
)

# -------------------------
# Check 1.5.4: Study window bounds
# -------------------------
n_before <- sum(df$night < study_start, na.rm = TRUE)
n_after <- sum(df$night > study_end, na.rm = TRUE)

if (n_before > 0) {
  warning(sprintf("%d night(s) before study start (%s)", n_before, study_start))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.4",
      event_type = "WARNING",
      message = sprintf("Nights before study start (%s)", study_start),
      rows_affected = n_before,
      detector = "MULTIPLE",
      script = script_name
    )
  )
}

if (n_after > 0) {
  warning(sprintf("%d night(s) after study end (%s)", n_after, study_end))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.4",
      event_type = "WARNING",
      message = sprintf("Nights after study end (%s)", study_end),
      rows_affected = n_after,
      detector = "MULTIPLE",
      script = script_name
    )
  )
}

if (n_before == 0 && n_after == 0) {
  message(sprintf("✓ All nights within study window (%s to %s)", study_start, study_end))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.4",
      event_type = "PASS",
      message = sprintf("All nights within study window (%s to %s)", study_start, study_end),
      rows_affected = nrow(df),
      script = script_name
    )
  )
}

# -------------------------
# Check 1.5.5: Grid completeness
# -------------------------
n_detectors <- n_distinct(df$detector)
n_nights <- as.integer(study_end - study_start) + 1L
expected_rows <- n_detectors * n_nights
actual_rows <- nrow(df)
completeness_pct <- round(100 * actual_rows / expected_rows, 1)

if (actual_rows < expected_rows) {
  warning(sprintf("Grid incomplete: %d/%d rows (%.1f%%)", actual_rows, expected_rows, completeness_pct))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.5",
      event_type = "WARNING",
      message = sprintf("Grid incomplete: %d/%d rows (%.1f%%)", actual_rows, expected_rows, completeness_pct),
      rows_affected = expected_rows - actual_rows,
      script = script_name
    )
  )
} else if (actual_rows == expected_rows) {
  message(sprintf("✓ Grid complete: %d detectors × %d nights (%d rows)", n_detectors, n_nights, actual_rows))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.5",
      event_type = "PASS",
      message = sprintf("Grid complete: %d detectors × %d nights", n_detectors, n_nights),
      rows_affected = actual_rows,
      script = script_name
    )
  )
} else {
  warning(sprintf("More rows than expected: %d (expected %d)", actual_rows, expected_rows))
  validation_entries <- bind_rows(
    validation_entries,
    create_validation_event(
      stage = "1.5.5",
      event_type = "WARNING",
      message = sprintf("More rows than expected: %d (expected %d)", actual_rows, expected_rows),
      rows_affected = actual_rows - expected_rows,
      script = script_name
    )
  )
}

log_message(sprintf("[BACKBONE] [Stage 1.5] Validation complete: %d rows validated", nrow(df)), log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 1.6: WRITE OUTPUTS
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                 STAGE 1.6: Write Outputs                        │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Writing outputs...")

output_dir <- here::here("outputs", "data_backbone")
if (! dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

validation_log_path <- file.path(output_dir, "validation_log.csv")
csv_path <- file.path(output_dir, "calls_per_night_clean.csv")
rds_path <- file.path(output_dir, "calls_per_night_clean.rds")

# -------------------------
# Write validation log using centralized function (type-safe append)
# -------------------------
append_validation_log(
  validation_entries = validation_entries,
  log_path = validation_log_path,
  quiet = FALSE
)

# -------------------------
# Write clean data outputs
# -------------------------
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

readr::write_csv(df_clean, csv_path)
saveRDS(df_clean, rds_path)

message("✓ Outputs written")
message(sprintf("  calls_per_night_clean.csv: %s rows", format(nrow(df_clean), big.mark = ",")))
message(sprintf("  calls_per_night_clean.rds: %s rows", format(nrow(df_clean), big.mark = ",")))

log_message(sprintf("[BACKBONE] [Stage 1.6] Outputs written: %d rows to %s", nrow(df_clean), output_dir),
            log_path = log_path)


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║                         WORKFLOW 01 COMPLETE                                ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║                         WORKFLOW 01 COMPLETE                                ║")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")

message("--- Workflow Summary ---")
message(sprintf("  Study: %s", study_name))
message(sprintf("  Rows: %s", format(nrow(df_clean), big.mark = ",")))
message(sprintf("  Detectors: %d", n_distinct(df_clean$detector)))
message(sprintf("  Sites: %d", n_distinct(df_clean$site)))
message(sprintf("  Nights: %d", n_distinct(df_clean$night)))
message(sprintf("  Date range: %s to %s", min(df_clean$night), max(df_clean$night)))

message("\n  Night classification:")
for (i in seq_len(nrow(class_summary))) {
  message(sprintf("    %s: %d (%. 1f%%)",
                  class_summary$night_class[i],
                  class_summary$n[i],
                  class_summary$pct[i]))
}

message("\n  Outputs:")
message(sprintf("    %s", csv_path))
message(sprintf("    %s", rds_path))
message(sprintf("    %s", validation_log_path))
message("")

log_message("[BACKBONE] === WORKFLOW 01 COMPLETE ===", log_path = log_path)

# -------------------------
# Store in environment for downstream use (interactive convenience)
# -------------------------
calls_per_night_clean <- df_clean
message("Data available as:  calls_per_night_clean")
