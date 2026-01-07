# ==============================================================================
# MAINLINE WORKFLOW: 01_import_and_validate.R
# ==============================================================================
# PURPOSE
# -------
# Validates calls_per_night data and assigns Status based on recording hours.
# Generates a validation log and summary table.
#
# WORKFLOW POSITION
# -----------------
# Successor: 02_???
#
# INPUTS
# ------
# - calls_per_night CSV (user-selected via file.choose())
#
# PROCESSING STAGES
# -----------------
# Stage 1.0: Load libraries
# Stage 1.1: Load CSV
# Stage 1.2: Apply validation rules
# Stage 1.3: Build validation log
# Stage 1.4: Save outputs
# Stage 1.5: Print summary table
#
# OUTPUTS
# -------
# - calls_per_night_validated.csv
# - validation_log.csv
# - summary_table printed to console
# ==============================================================================

# ============================
# STAGE 1.0: Load Libraries
# ============================
message("Loading libraries...")
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(here)
})
message("✓ Libraries loaded\n")

# ============================
# STAGE 1.1: Load Calls Per Night CSV Robustly
# ============================
message("Please select the calls_per_night CSV to validate...")
calls_file <- file.choose()   # user picks file interactively

if (!file.exists(calls_file)) {
  stop(sprintf("File not found: %s", calls_file))
}

# Step 1: read all date/time columns as character
calls <- read_csv(calls_file,
                  col_types = cols(
                    Detector = col_character(),
                    Night = col_character(),
                    CallsPerNight = col_double(),
                    RecordingHours = col_double(),
                    StartDateTime = col_character(),
                    EndDateTime   = col_character(),
                    Status = col_character(),
                    CallsPerHour = col_double()
                  ))

# Step 2: convert date/time columns robustly
calls <- calls %>%
  mutate(
    StartDateTime = mdy_hm(StartDateTime, tz = Sys.timezone(), quiet = TRUE),
    EndDateTime   = mdy_hm(EndDateTime, tz = Sys.timezone(), quiet = TRUE),
    Night         = ymd(Night, quiet = TRUE)
  )


# Step 3: Check for rows that failed parsing
failed_dt <- calls %>%
  filter(is.na(StartDateTime) | is.na(EndDateTime) | is.na(Night))

if(nrow(failed_dt) > 0){
  warning(sprintf("⚠ %d rows have invalid date/time formatting. Please check!", nrow(failed_dt)))
  print(failed_dt)
}

# ============================
# STAGE 1.2: Validation Log
# ============================

# Initialize validation log
validation_log <- tibble(
  Detector = character(),
  Night = as.Date(character()),
  RecordingHours = numeric(),
  Status = character(),
  Action = character(),
  Note = character()
)

# Apply validation rules
calls_validated <- calls %>%
  rowwise() %>%
  mutate(
    Status = case_when(
      RecordingHours == 0 ~ "Fail",
      RecordingHours < 13 & RecordingHours > 0 ~ "Partial",
      RecordingHours >= 13 ~ "Pass",
      TRUE ~ "Check"
    )
  ) %>%
  ungroup()

# ============================
# STAGE 1.3: Build Validation Log
# ============================
validation_log <- calls_validated %>%
  mutate(
    Action = case_when(
      Status == "Fail" ~ "Remove",
      Status == "Partial" ~ "Keep (Partial)",
      Status == "Pass" ~ "Keep",
      TRUE ~ "Check"
    ),
    Note = case_when(
      Status == "Fail" ~ "Zero recording hours",
      Status == "Partial" ~ "Short or partial night",
      Status == "Pass" ~ "Full night",
      TRUE ~ "Manual check needed"
    )
  ) %>%
  select(Detector, Night, RecordingHours, Status, Action, Note)

message("✅ Calls loaded and validated successfully.")

# ============================
# STAGE 1.4: Save Outputs
# ============================
output_dir <- here("00_DATA_BACKBONE", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

validated_file <- file.path(output_dir, "calls_per_night_validated.csv")
log_file <- file.path(output_dir, "validation_log.csv")

write_csv(calls_validated, validated_file)
write_csv(validation_log, log_file)

message(sprintf("✓ Saved validated calls: %s", basename(validated_file)))
message(sprintf("✓ Saved validation log: %s", basename(log_file)))

# ============================
# STAGE 1.5: Summary Output
# ============================
summary_table <- calls_validated %>%
  group_by(Status) %>%
  summarise(Nights = n(), .groups = "drop")

message("\nValidation Summary:")
print(summary_table)

message("\n=== STAGE 01 COMPLETE: calls_per_night validation finished ===")
