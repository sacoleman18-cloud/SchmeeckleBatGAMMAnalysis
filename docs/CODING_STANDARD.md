# ==============================================================================
# BAT ACOUSTIC ANALYSIS PIPELINE: CODING STANDARDS (UPDATED FOR DUAL-LAYER PROJECT)
# ==============================================================================
# VERSION: 1.0
# LAST UPDATED: 2026-01-01
# PURPOSE: Single source of truth for all code development in this project
# ==============================================================================

## CORE PHILOSOPHY

This pipeline is designed to be:

- **Safe** – Never corrupts data, always validates inputs
    
- **Defensive** – Assumes things will go wrong and handles gracefully
    
- **Reproducible** – Same inputs → same outputs
    
- **Replicable** – Works on any computer/OS
    
- **Portable** – No hardcoded paths, no environment dependencies
    
- **User-Friendly** – Designed for researchers with minimal R experience
    
- **Audit-Compliant** – Every transformation logged/tracked
    
- **Publication-Ready** – Supports Quarto reports and poster outputs
    
- **Future-Proof** – Supports Production/Exploratory separation
    
- **Maintainable** – Clear, documented, modular code
---

## 1. PROJECT ARCHITECTURE STANDARDS

### 1.1 Directory Structure (LOCKED)

```text
project_root/
├── 00_DATA_BACKBONE/         # Single source of cleaned, shared data
│   └── outputs/              # Standardized objects for both layers
├── 01_PRODUCTION/            # Poster / publication-ready workflow
│   ├── scripts/
│   │   ├── 10_model_nb_gamm.R
│   │   ├── 11_prediction_and_plots.R
│   │   ├── 12_summary_tables.R
│   │   └── ...
│   └── outputs/              # Figures, tables, RDS for poster
├── 02_EXPLORATORY/           # Pedagogical, exploratory analyses
│   ├── scripts/
│   └── outputs/              # Figures, temporary plots, diagnostics
├── R/
│   ├── functions/            # Core, standardization, validation, analysis, output
├── inst/config/              # YAML configs for params & study design
├── logs/
├── docs/
└── tests/
```

**Rules:**
✅ All paths relative to project root  
✅ Use `here::here()`  
✅ Only Production layer writes final outputs; Exploratory can generate temporary outputs  
❌ Never duplicate core data in Exploratory scripts  
❌ Never hardcode paths

### 1.2 File Naming Conventions

**Workflow scripts:**
```
01_ingest_raw_data.R
02_standardize.R
03_calls_per_night.R
```
Format: `##_verb_noun.R` (numbered, descriptive, snake_case)

**Function files:**
```
schema_detection.R
datetime_conversion.R
recording_hours.R
```
Format: `noun_noun.R` or `verb_noun.R` (descriptive, snake_case)

**Output files:**
```
01_intro_standardized_20241226_143022.csv
02_kpro_master_20241226_151530.csv
03_CallsPerNight_Template_ORIGINAL_20241226_160045.csv
CallsPerNight_final_v1.csv
```
Format: `##_description_YYYYMMDD_HHMMSS.csv` or `description_vN.csv`

**RULES:**
- ✅ Use snake_case for all files
- ✅ Include timestamps for checkpoints
- ✅ Version numbers for final outputs
- ✅ Descriptive names (no abbreviations like `tmp`, `data1`, `final_FINAL_v2`)
- ❌ NEVER use spaces in filenames
- ❌ NEVER use special characters except `_` and `-`

---

## 2. DOCUMENTATION STANDARDS

### 2.1 Function Documentation (Roxygen2 Format)
- CONTRACT & DOES NOT sections mandatory
    
- Examples realistic and runnable
    
- Functions should clearly separate **computation vs display** (Quarto-ready)

**Additional for Dual-Layer Projects:**

- Functions should be agnostic to workflow layer
    
- Exploratory scripts can call Production functions but **must not write backbone outputs**

**REQUIRED sections:**
```r
#' Function Title (One Line Summary)
#'
#' @description
#' Detailed explanation of what this function does, why it exists,
#' and how it fits into the larger workflow.
#'
#' @param param_name Type. Description of parameter, valid values, defaults.
#'
#' @return Description of return value, type, structure.
#'
#' @details
#' Additional context, algorithms, edge cases, performance notes.
#'
#' @section CONTRACT:
#' - Guarantee 1: What this function ALWAYS does
#' - Guarantee 2: What this function NEVER does
#' - Guarantee 3: Error handling behavior
#'
#' @section DOES NOT:
#' - Explicitly state what this function does NOT do
#' - Prevents scope creep and clarifies boundaries
#'
#' @examples
#' \dontrun{
#' # Realistic example with actual data
#' result <- my_function(data, param = "value")
#' }
#'
#' @export
```

**EXAMPLE (Good):**
```r
#' Convert 4-Letter Species Codes to 6-Letter
#'
#' @description
#' Applies species code mapping to auto_id, alternate_1, alternate_2, 
#' and alternate_3 columns using the SPECIES_CODE_MAP_4_TO_6 lookup table.
#'
#' @param df Data frame containing species code columns
#'
#' @return Data frame with updated species codes (4-letter → 6-letter)
#'
#' @details
#' This function processes four columns:
#' - auto_id (primary species identification)
#' - alternate_1 (first alternative species)
#' - alternate_2 (second alternative species)
#' - alternate_3 (third alternative species)
#'
#' **Conversion logic:**
#' - Codes in SPECIES_CODE_MAP_4_TO_6 → converted to 6-letter
#' - Codes NOT in map → preserved as-is (no error)
#' - NA values → remain NA
#' - Case-insensitive matching
#'
#' @section CONTRACT:
#' - Only species code columns are modified
#' - Unknown codes preserved (not errored or removed)
#' - Column names case-insensitive (auto_id, Auto_ID, AUTO_ID all work)
#' - Does not add/remove columns
#' - Logs conversion completion
#'
#' @section DOES NOT:
#' - Modify non-species columns
#' - Validate species code correctness
#' - Remove rows with unknown codes
#' - Add new columns
#' - Write files to disk
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   auto_id = c("MYLU", "EPFU", "NOID"),
#'   alternate_1 = c("LABO", NA, "LACI")
#' )
#' 
#' df_converted <- convert_4letter_to_6letter(df)
#' # auto_id: MYOLUC, EPTFUS, UNKNOWN
#' # alternate_1: LASBOR, NA, LASCIN
#' }
#'
#' @export
```

**RULES:**
- ✅ All exported functions MUST have complete Roxygen2 headers
- ✅ CONTRACT and DOES NOT sections are MANDATORY
- ✅ Examples must be realistic and runnable (even if wrapped in `\dontrun{}`)
- ✅ Parameter descriptions include type, valid values, defaults
- ❌ NEVER use vague descriptions like "processes data" or "does stuff"

### 2.2 Workflow Script Headers
- Include **Layer designation**: `# LAYER: PRODUCTION` or `# LAYER: EXPLORATORY`
    
- Document predecessor/successor scripts
    
- Include YAML config file references for parameterization

**REQUIRED sections:**
```r
# ==============================================================================
# MAINLINE WORKFLOW: ##_script_name.R
# ==============================================================================
# PURPOSE
# -------
# One-paragraph summary of what this workflow accomplishes
#
# WORKFLOW POSITION
# -----------------
# Where this fits in the pipeline (show predecessor/successor scripts)
#
# INPUTS
# ------
# - List all expected inputs (files, in-memory objects, user prompts)
#
# PROCESSING STAGES
# -----------------
# Stage X.Y: Name
#   - Bullet points explaining what happens
#
# OUTPUTS
# -------
# - List all files created, in-memory objects, logs
#
# [OPTIONAL SECTIONS]
# - Data transformations applied
# - Performance expectations
# - Dependencies
# - Troubleshooting
# - Usage examples
# - Maintainer notes
#
# ==============================================================================
```

**RULES:**
- ✅ Headers must be comprehensive (see 02_standardize.R as gold standard)
- ✅ Include workflow position diagram
- ✅ List all stages with descriptions
- ✅ Document performance expectations for different dataset sizes
- ✅ Include troubleshooting section
- ❌ NEVER skip header documentation

### 2.3 Inline Comments

- Explain **why**, not just what
    
- For exploratory layer, clearly mark pedagogical experiments:

**GOOD examples:**
```r
# Remove NoID calls that were NOT manually identified
# This preserves NoID calls where user confirmed species via manual_id
kpro_master <- kpro_master %>%
  filter(!(auto_id %in% c("NoID", "UNKNOWN") & is.na(manual_id)))

# Calculate study night (calls before noon = previous date)
# Example: 2024-10-26 03:15:00 → Night: 2024-10-25
Night = if_else(hour(DateTime) < 12, as.Date(DateTime) - 1, as.Date(DateTime))
```

**BAD examples:**
```r
# Filter data
kpro_master <- kpro_master %>% filter(...)

# Calculate night
Night = if_else(...)
```

**RULES:**
- ✅ Explain WHY, not just WHAT
- ✅ Include examples for complex logic
- ✅ Comment non-obvious code
- ❌ NEVER state the obvious ("add 1 to x")
- ❌ NEVER leave commented-out code (remove or explain why it's there)

---

## 3. CODE DESIGN STANDARDS

### 3.1 Function Design Principles

- Always validate inputs
    
- Return new objects, **never modify in place**
    
- Logging optional in quiet mode (Quarto-ready)
    
- One function = one purpose

**EXAMPLE (Good):**
```r
calculate_recording_hours <- function(start_time, end_time) {
  
  # -------------------------
  # Input validation
  # -------------------------
  if (is.na(start_time) || is.na(end_time)) return(NA_real_)
  
  if (!is.character(start_time) || !is.character(end_time)) {
    stop("start_time and end_time must be character or NA")
  }
  
  # -------------------------
  # Computation
  # -------------------------
  start_h <- as.numeric(hms::as_hms(start_time)) / 3600
  end_h   <- as.numeric(hms::as_hms(end_time)) / 3600
  
  # Handle overnight recordings
  if (end_h < start_h) {
    (24 - start_h) + end_h
  } else {
    end_h - start_h
  }
}
```

**EXAMPLE (Bad):**
```r
calc_hours <- function(s, e) {
  # No validation
  # Cryptic variable names
  # No handling of edge cases
  as.numeric(hms::as_hms(e)) / 3600 - as.numeric(hms::as_hms(s)) / 3600
}
```

**RULES:**
- ✅ Validate ALL inputs before processing
- ✅ Use descriptive variable names
- ✅ Use sections (`# ------`) to organize code
- ✅ Return early for edge cases (NA, empty, etc.)
- ✅ Functions should do ONE thing well
- ❌ NEVER assume inputs are valid
- ❌ NEVER modify global state
- ❌ NEVER use single-letter variable names (except loop counters)

### 3.2 Error Handling Standards

- STOP: Critical failures (missing Production data)
    
- WARNING: Optional issues (Exploratory missing optional files)
    
- MESSAGE: Progress updates

**Error types:**

**STOP (critical failure):**
```r
# Use when:
# - Required file doesn't exist
# - Required columns missing
# - Invalid configuration
# - Data corruption detected

if (!file.exists(config_file)) {
  stop(sprintf(
    "Configuration file not found: %s\n  Please run 01_ingest_raw_data.R first.",
    config_file
  ))
}
```

**WARNING (non-fatal issue):**
```r
# Use when:
# - Optional feature unavailable
# - Data quality concerns
# - Deprecated features used

if (any(is.na(df$detector_id))) {
  warning(sprintf(
    "%s row(s) have NA detector_id - these detections cannot be assigned to a location",
    sum(is.na(df$detector_id))
  ))
}
```

**MESSAGE (informational):**
```r
# Use when:
# - Reporting progress
# - Confirming completion
# - Providing guidance

message("✓ Schema transformation complete")
message(sprintf("  Transformed %s rows", format(nrow(df), big.mark = ",")))
```

**RULES:**
- ✅ Error messages must be actionable (tell user what to do)
- ✅ Include context (filename, column name, row count)
- ✅ Use `sprintf()` for formatted messages
- ✅ Include suggestions for fixing the error
- ❌ NEVER use generic errors ("Error occurred")
- ❌ NEVER crash silently

### 3.3 Non-Destructive Operations
- Backbone data (`00_DATA_BACKBONE/outputs`) **never modified** by Exploratory scripts
    
- Exploratory layers can produce derived objects, figures, temporary RDS

**ALWAYS return new objects:**
```r
# GOOD:
df_clean <- remove_duplicates(df_raw)

# BAD:
remove_duplicates(df_raw)  # Modifies df_raw in place
```

**ALWAYS preserve original data:**
```r
# GOOD:
df <- df %>%
  mutate(
    DateTime_orig = DateTime,  # Preserve original
    DateTime = with_tz(DateTime, "America/Chicago")  # Transform
  )

# BAD:
df$DateTime <- with_tz(df$DateTime, "America/Chicago")  # Original lost
```

**RULES:**
- ✅ Functions return new objects
- ✅ Preserve original values when transforming
- ✅ Make backups before destructive operations
- ❌ NEVER modify function arguments in place
- ❌ NEVER overwrite source files

---

## 4. PATH MANAGEMENT STANDARDS

### 4.1 Absolute Path Prohibition
- **Always** use `here::here()`
    
- Exploratory scripts may reference Production outputs, but only for reading
    
- User input files must be validated

**NEVER do this:**
```r
# ❌ BAD:
data <- read.csv("C:/Users/JohnSmith/Documents/BatProject/data/master.csv")
output_dir <- "C:\\Users\\JohnSmith\\Desktop\\outputs"
source("C:/My Projects/Bats/R/functions/utilities.R")
```

**ALWAYS do this:**
```r
# ✅ GOOD:
data <- read.csv(here::here("data", "master.csv"))
output_dir <- here::here("outputs")
source(here::here("R", "functions", "utilities.R"))
```

### 4.2 Project Root as Anchor

**Using here::here():**
```r
library(here)

# Set project root explicitly (optional, but recommended)
here::i_am("R/workflows/02_standardize.R")

# All paths relative to project root
config_file <- here::here("inst", "config", "study_parameters.yaml")
output_file <- here::here("outputs", sprintf("master_%s.csv", timestamp))
```

**RULES:**
- ✅ ALWAYS use `here::here()` for file paths
- ✅ Project root = where .Rproj file lives
- ✅ Cross-platform compatibility (Windows/Mac/Linux)
- ❌ NEVER use `setwd()` in functions or scripts
- ❌ NEVER use `~/` or `%USERPROFILE%`
- ❌ NEVER hardcode drive letters (C:, D:, etc.)

### 4.3 User File Selection

**For user-provided files:**
```r
# ✅ GOOD (interactive):
manual_id_file <- file.choose()

# ✅ GOOD (programmatic with validation):
manual_id_file <- readline("Enter file path: ")
if (!file.exists(manual_id_file)) {
  stop(sprintf("File not found: %s", manual_id_file))
}

# ❌ BAD:
manual_id_file <- "C:/Users/JohnSmith/Desktop/my_data.csv"
```

**RULES:**
- ✅ Use `file.choose()` for interactive selection
- ✅ Validate file existence before processing
- ✅ Provide clear prompts for file paths
- ❌ NEVER assume file locations

---

## 5. DATA QUALITY STANDARDS

### 5.1 Validation Checkpoints
- Validation checkpoints required after every transformation
    
- Backbone writes logs daily
    
- Exploratory logs optional, but include stage info

**Required validation points:**
```r
# After loading data
if (nrow(df) == 0) {
  stop("Loaded data is empty - check source file")
}

# After transformations
required_cols <- c("Detector", "DateTime", "auto_id")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
}

# Before saving
if (any(duplicated(df[, c("Detector", "DateTime", "auto_id")]))) {
  warning("Duplicate rows detected - consider deduplication")
}
```

**RULES:**
- ✅ Validate after every major transformation
- ✅ Check for empty data frames
- ✅ Verify column existence before accessing
- ✅ Check data types match expectations
- ✅ Validate ranges (dates, counts, etc.)
- ❌ NEVER assume data is valid

### 5.2 Data Provenance Tracking

**ALWAYS log:**
```r
# What was done
log_message("[Stage 2.3] Transformed v1/v2/v3 schemas to unified format")

# How many rows affected
log_message(sprintf("[Stage 2.6] Removed %d duplicates", n_removed))

# What files were created
log_message(sprintf("[Stage 2.7] Saved: %s", basename(output_file)))

# User decisions
log_message("[Stage 3.0] User chose manual ID workflow")
```

**RULES:**
- ✅ Log all data transformations
- ✅ Include row counts in logs
- ✅ Log file operations (read/write)
- ✅ Log user choices (for reproducibility)
- ✅ Use consistent timestamp format
- ❌ NEVER suppress logging in production code

---

## 6. WORKFLOW SCRIPT STANDARDS

### 6.1 Stage Structure

**ASCII Box Formatting:**
```r
# ==============================================================================
# WORKFLOW START (double-line box)
# ==============================================================================

# ┌────────────────────────────────────────────────────────────────┐
# │          STAGE 3.X: Stage Name (single-line box)               │
# └────────────────────────────────────────────────────────────────┘

# ╔════════════════════════════════════════════════════════════════╗
# ║          WORKFLOW COMPLETE (double-line box)                   ║
# ╚════════════════════════════════════════════════════════════════╝
```

**RULES:**
- ✅ Double-line boxes ONLY for workflow start/complete
- ✅ Single-line boxes for all intermediate stages
- ✅ Consistent width (66 characters inside box)
- ✅ Centered text with proper spacing
- ❌ NEVER mix box styles within same workflow

### 6.2 Progress Messaging

**Good progress messages:**
```r
message("Applying detector mapping...")
# ... processing ...
message("✓ Detector mapping applied")
message(sprintf("  Mapped %d detectors", n_detectors))
```

**Informative summaries:**
```r
message("\nTemplate summary:")
message(sprintf("  Detectors: %d", detectors_count))
message(sprintf("  Nights: %d", nights_count))
message(sprintf("  Total rows: %s", format(nrow(template), big.mark = ",")))
```

**RULES:**
- ✅ Use checkmarks (✓) for completed steps
- ✅ Use indentation for sub-items
- ✅ Format large numbers with commas
- ✅ Show progress for long operations
- ❌ NEVER be silent during processing
- ❌ NEVER use print() in production code (use message())

### 6.3 User Interaction

**Interactive prompts:**
```r
# Clear question
message("Have you manually reviewed calls in Kaleidoscope Pro?")
message("(This step is optional but improves species identification accuracy)\n")

# Simple y/n response
response <- tolower(trimws(readline("Manually ID'd calls? (y/n): ")))

# Validate input
if (response == "y") {
  # ... handle yes case
} else if (response == "n") {
  # ... handle no case
} else {
  stop("Invalid response. Please enter 'y' or 'n'")
}
```

**RULES:**
- ✅ Clear, specific questions
- ✅ Explain why you're asking
- ✅ Validate all user input
- ✅ Provide examples in prompts
- ✅ Use `trimws()` to clean input
- ❌ NEVER assume user input is valid
- ❌ NEVER use complex multi-choice prompts

---

## 7. QUARTO INTEGRATION STANDARDS

### 7.1 Quarto-Ready Code Structure
- Functions must **return objects**, not print
    
- Quiet mode supported
    
- Production scripts generate Quarto-ready outputs
    
- Exploratory scripts may include plots/figures for learning or diagnostics

**Functions must support:**
```r
# Quiet mode (suppress messages)
my_function <- function(data, quiet = FALSE) {
  if (!quiet) {
    message("Processing data...")
  }
  # ... processing ...
  if (!quiet) {
    message("✓ Complete")
  }
  result
}

# Return results instead of printing
my_function <- function(data) {
  result <- list(
    summary = summary_stats,
    plot = ggplot_object,
    table = formatted_table
  )
  invisible(result)
}
```

**RULES:**
- ✅ Functions can run in quiet mode
- ✅ Return structured results (lists, data frames)
- ✅ Separate computation from display
- ✅ Generate ggplot objects (not print them)
- ❌ NEVER hard-code output formats (PDF vs HTML)
- ❌ NEVER use device-specific plots (png(), pdf())

### 7.2 Parameterization

**Config-driven workflows:**
```r
# All parameters in YAML
params <- yaml::read_yaml(here::here("inst", "config", "analysis_params.yaml"))

# Use params throughout
start_date <- params$recording_period$start
end_date <- params$recording_period$end
uniform_start <- params$recording_schedule$uniform_start
```

**RULES:**
- ✅ Move hardcoded values to config files
- ✅ Document all parameters in YAML
- ✅ Validate params after loading
- ❌ NEVER hardcode analysis parameters in scripts

---

## 8. SHINY INTEGRATION STANDARDS

### 8.1 Reactive-Ready Functions

**Separate data from UI:**
```r
# ✅ GOOD (pure function):
calculate_metrics <- function(data, start_date, end_date) {
  data %>%
    filter(Night >= start_date & Night <= end_date) %>%
    summarise(total_calls = sum(CallsPerNight))
}

# ❌ BAD (mixed with UI):
calculate_metrics <- function() {
  start_date <- input$start_date  # Shiny-specific
  # ... processing ...
}
```

**RULES:**
- ✅ Functions take data as arguments (not reactive values)
- ✅ No Shiny-specific code in core functions
- ✅ Return data frames or lists (not rendered UI)
- ✅ Functions are testable without Shiny
- ❌ NEVER access `input$` or `output$` in core functions


---

## 9. VERSION CONTROL STANDARDS

### 9.1 Git Commit Messages
- Use `renv`
    
- Record versions in `README`
    
- Exploratory may use additional learning packages, but Production only uses pinned core deps

**Format:**
```
<type>: <short summary> (<max 50 chars>)

<detailed description if needed>

<footer: issue references, breaking changes>
```

**Types:**
- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation only
- `refactor:` Code restructuring (no behavior change)
- `test:` Adding tests
- `chore:` Maintenance (dependencies, etc.)

**Examples:**
```
feat: Add manual ID import workflow to Script 03

- Add Stage 3.0 for manual ID file selection
- Implement NoID filtering with Option B logic
- Update documentation for manual ID workflow

Closes #42
```

```
fix: Correct Excel formula column references in template

Template was referencing columns F/E instead of E/D.
Updated row-aware formula generation to use correct refs.
```

**RULES:**
- ✅ Use conventional commit format
- ✅ First line ≤ 50 characters
- ✅ Use imperative mood ("Add" not "Added")
- ✅ Reference issues when applicable
- ❌ NEVER commit with message "updates" or "changes"

### 9.2 .gitignore Requirements

**ALWAYS ignore:**
```gitignore
# User data (never commit sensitive or large data)
data/
*.csv
*.wav
*.txt

# Outputs (reproducible, shouldn't be in repo)
outputs/
logs/

# R/RStudio files
.Rproj.user/
.Rhistory
.RData
.Ruserdata

# Operating system
.DS_Store
Thumbs.db

# Credentials
*.yaml  # If contains sensitive info
.env
```

**RULES:**
- ✅ Never commit user data
- ✅ Never commit outputs (use releases for sharing)
- ✅ Never commit credentials
- ✅ Commit .gitignore to repo
- ❌ NEVER commit large files (> 10 MB)

---

## 10. TESTING STANDARDS

### 10.1 Testing Requirements

**Test coverage expectations:**
- Production: full coverage
    
- Exploratory: optional tests

**Example test:**
```r
test_that("calculate_recording_hours handles overnight correctly", {
  # Test overnight recording
  hours <- calculate_recording_hours("20:00:00", "08:00:00")
  expect_equal(hours, 12)
  
  # Test same-day recording
  hours <- calculate_recording_hours("06:00:00", "18:00:00")
  expect_equal(hours, 12)
  
  # Test NA handling
  hours <- calculate_recording_hours(NA, "08:00:00")
  expect_true(is.na(hours))
})
```

**RULES:**
- ✅ Test edge cases (NA, empty, zero)
- ✅ Test expected behavior
- ✅ Test error conditions
- ✅ Use descriptive test names
- ❌ NEVER skip testing validation functions

---

## 11. PERFORMANCE STANDARDS

### 11.1 Memory Management
- Production layer scripts audited strictly
    
- Exploratory layer audited for pedagogical clarity
    
- Any changes to backbone objects require version bump & review

**For large datasets:**
```r
# ✅ GOOD (clean up intermediates):
df_clean <- process_step1(df_raw)
rm(df_raw)  # Remove original if no longer needed
gc()  # Force garbage collection

df_final <- process_step2(df_clean)
rm(df_clean)
gc()

# ❌ BAD (accumulates memory):
df_clean <- process_step1(df_raw)
df_final <- process_step2(df_clean)
# df_raw and df_clean still in memory
```

**RULES:**
- ✅ Remove large intermediates when done
- ✅ Use `gc()` after major operations
- ✅ Report memory usage for large datasets
- ✅ Use data.table for > 1M rows
- ❌ NEVER load entire dataset if you only need subset

### 11.2 Progress Indicators

**For long operations:**
```r
message("Processing large dataset...")
message("  This may take several minutes...")

# Use progress bars for loops
pb <- txtProgressBar(min = 0, max = n_files, style = 3)
for (i in seq_along(files)) {
  # ... process file ...
  setTxtProgressBar(pb, i)
}
close(pb)
```

**RULES:**
- ✅ Warn users about long operations
- ✅ Use progress bars for >30 second operations
- ✅ Report time taken for benchmarking
- ❌ NEVER leave users wondering if code is frozen

---

### 11.3 Avoid External System Commands

**NEVER use system2() or system() for core functionality:**

❌ WRONG:
```r
system2("grep", ...)   # Breaks on Windows
system2("findstr", ...) # Breaks on Unix
system2("awk", ...)    # Not portable
```

✅ CORRECT:
```r
# Use built-in R functions that work everywhere
readLines()  # Cross-platform
grepl()      # Cross-platform
list.files() # Cross-platform
```

**Exceptions where system2() is acceptable:**
- Calling user-installed tools (git, pandoc, etc.) where you check if tool exists first
- Optional features that fail gracefully if tool is missing
- Platform-specific optimizations with fallback to pure R

**RULE:**
- Core functionality MUST use only base R and CRAN packages
- Never require platform-specific tools for basic operations

## 12. SECURITY & PRIVACY STANDARDS

### 12.1 Data Privacy

**NEVER commit:**
- Personal information (names, locations)
- Detector serial numbers (if sensitive)
- GPS coordinates (without permission)
- Unpublished data
- Credentials (API keys, passwords)

**RULES:**
- ✅ Use synthetic data for examples
- ✅ Anonymize data in documentation
- ✅ Store credentials in .env (gitignored)
- ❌ NEVER hardcode passwords/keys

### 12.2 File Permissions

**Check before writing:**
```r
output_dir <- here::here("outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!file.access(output_dir, mode = 2) == 0) {
  stop(sprintf("Cannot write to output directory: %s", output_dir))
}
```

**RULES:**
- ✅ Check write permissions before saving
- ✅ Create directories if missing
- ✅ Handle permission errors gracefully
- ❌ NEVER assume write access

---

## 13. SCIENTIFIC REPRODUCIBILITY STANDARDS

### 13.1 Deterministic Operations

**ALWAYS:**
```r
# Set seed for random operations
set.seed(123)

# Sort data before operations that depend on order
df <- df %>% arrange(Detector, Night)

# Use explicit timezone
DateTime <- as.POSIXct("2024-10-25 22:30:00", tz = "America/Chicago")
```

**RULES:**
- ✅ Set random seed when using randomness
- ✅ Sort data explicitly (don't rely on input order)
- ✅ Always specify timezones
- ✅ Document any non-deterministic steps
- ❌ NEVER rely on system defaults (locale, timezone, etc.)

### 13.2 Audit Trail

**Log everything:**
```r
log_message("=== WORKFLOW 02 START ===")
log_message(sprintf("[Input] Loaded %d files", n_files))
log_message(sprintf("[Transform] Converted %d rows from v1 to unified", n_v1))
log_message(sprintf("[Filter] Removed %d duplicates", n_removed))
log_message(sprintf("[Output] Saved: %s", output_file))
log_message("=== WORKFLOW 02 COMPLETE ===")
```

**RULES:**
- ✅ Log all inputs (files loaded, user choices)
- ✅ Log all transformations (with row counts)
- ✅ Log all outputs (files saved, versions)
- ✅ Include timestamps in logs
- ✅ One log file per day (append mode)
- ❌ NEVER delete logs

---

## 14. COLLABORATION STANDARDS

### 14.1 Code Review Process

**Before merging code:**
1. **Self-review checklist:**
   - [ ] Code follows all standards in this document
   - [ ] All functions have Roxygen2 headers
   - [ ] No hardcoded paths
   - [ ] Error handling in place
   - [ ] Tested with realistic data
   - [ ] Documentation updated

2. **Peer review (if applicable):**
   - Reviewer checks standards compliance
   - Reviewer tests code with sample data
   - Both parties sign off on changes

3. **Testing:**
   - Run all workflows end-to-end
   - Check outputs match expected
   - Verify logs are complete

**RULES:**
- ✅ Self-review before requesting peer review
- ✅ Use checklist (don't skip steps)
- ✅ Test with real data (not toy examples)
- ❌ NEVER merge untested code

### 14.2 Issue Reporting

**Good issue format:**
```markdown
## Description
Clear, one-sentence summary of the issue

## Steps to Reproduce
1. Load data from...
2. Run workflow 02
3. Error occurs at Stage 2.4

## Expected Behavior
DateTime column should be created in CST

## Actual Behavior
Error: 'DateTime_UTC' not found

## Environment
- R version: 4.3.2
- OS: Windows 11
- Date: 2024-12-26

## Additional Context
- Error started after updating lubridate package
- Attaching reprex code and sample data
```

**RULES:**
- ✅ Include reproducible example
- ✅ Specify R version and OS
- ✅ Attach error messages and logs
- ✅ Search existing issues first
- ❌ NEVER report issues without context

### 14.3 Proposing Changes

**Feature proposal template:**
```markdown
## Feature Request: [Short Title]

### Problem
What problem does this solve? Who is affected?

### Proposed Solution
How would you implement this?

### Alternatives Considered
What other approaches were rejected? Why?

### Implementation Plan
1. Step 1
2. Step 2
3. Testing strategy

### Breaking Changes
Will this break existing code? Migration plan?

### Documentation Needs
What docs need updating?
```

**RULES:**
- ✅ Justify why feature is needed
- ✅ Consider backward compatibility
- ✅ Include implementation plan
- ✅ Discuss with maintainer before coding
- ❌ NEVER submit large changes without discussion

### 14.4 Branch Naming

**Format:**
```
<type>/<short-description>

Examples:
feat/manual-id-workflow
fix/excel-formula-columns
docs/update-standards
refactor/split-validation-functions
```

**RULES:**
- ✅ Use descriptive branch names
- ✅ Use type prefix (feat, fix, docs, etc.)
- ✅ Use kebab-case
- ✅ Delete branches after merging
- ❌ NEVER work directly on main/master

---

## 15. PACKAGE DEPENDENCY STANDARDS

### 15.1 Core Dependencies (Required)

```r
# Data manipulation
library(tidyverse)  # dplyr, tidyr, readr, purrr, ggplot2

# Date/time
library(lubridate)
library(hms)

# Configuration
library(yaml)

# Path management
library(here)
```

### 15.2 Version Pinning

**For reproducibility:**
```r
# Use renv for package management
renv::init()
renv::snapshot()  # Save package versions

# Document versions in README
# R version: 4.3.2
# tidyverse: 2.0.0
# lubridate: 1.9.3
```

**RULES:**
- ✅ Use renv for reproducible environments
- ✅ Document package versions
- ✅ Test with minimum required versions
- ❌ NEVER rely on bleeding-edge packages
- ❌ NEVER use GitHub-only packages (unless necessary)

---

## 16. NAMING CONVENTIONS

### 16.1 Variables

**Use snake_case:**
```r
# ✅ GOOD:
detector_id
calls_per_night
recording_start_time

# ❌ BAD:
detectorID  # camelCase
calls.per.night  # dots
RecordingStartTime  # PascalCase
```

### 16.2 Functions

**Use verb_noun pattern:**
```r
# ✅ GOOD:
calculate_recording_hours()
generate_template()
validate_schema()
remove_duplicates()

# ❌ BAD:
hours()  # too vague
make_template()  # vague verb
checker()  # not a verb
```

### 16.3 Constants

**Use SCREAMING_SNAKE_CASE:**
```r
# ✅ GOOD:
SPECIES_CODE_MAP_4_TO_6
MAX_RECORDING_HOURS <- 24
DEFAULT_TIMEZONE <- "America/Chicago"
```

**RULES:**
- ✅ All caps for constants
- ✅ Use descriptive names
- ✅ Document units if applicable
- ❌ NEVER use magic numbers (define as named constant)

---

## 17. CHECKLISTS

### 17.1 Before Committing Code

- [ ] Code follows all standards in this document
- [ ] No hardcoded paths (checked with grep)
- [ ] All functions have Roxygen2 headers
- [ ] Error handling in place
- [ ] Tested with realistic data
- [ ] No commented-out code (or justified)
- [ ] Logs are informative
- [ ] Documentation updated
- [ ] .gitignore updated if needed
- [ ] No large files added

### 17.2 Before Publishing/Sharing

- [ ] All workflows run end-to-end
- [ ] README includes installation instructions
- [ ] Example data provided (anonymized)
- [ ] LICENSE file included
- [ ] Citation information provided
- [ ] Dependencies documented
- [ ] Known limitations documented
- [ ] No sensitive data in repo
- [ ] Release notes written
- [ ] Version number incremented

### 17.3 Code Review Checklist

- [ ] Follows coding standards
- [ ] Functions are well-documented
- [ ] Error handling is appropriate
- [ ] No hardcoded values
- [ ] Performance is acceptable
- [ ] Tests pass (if applicable)
- [ ] Documentation is clear
- [ ] No security issues
- [ ] No backward-incompatible changes (or justified)
- [ ] Logs are helpful for debugging

---

## 18. ENFORCEMENT

### 18.1 How to Use This Document

**For new code:**
1. Reference this document before writing
2. Use templates and examples provided
3. Self-review against checklists
4. Get peer review if collaborating

**For existing code:**
1. Audit against standards
2. Prioritize critical violations (hardcoded paths, missing docs)
3. Refactor incrementally
4. Update this document if standards evolve

### 18.2 Updating This Document

**When to update:**
- New patterns emerge that should be standardized
- Common mistakes identified
- Best practices evolve
- New tools/packages adopted

**How to update:**
- Propose changes via issue/PR
- Discuss with team (or self-document reasoning)
- Update version number
- Communicate changes
- Archive old versions

---

## APPENDIX A: QUICK REFERENCE

### Good vs Bad Examples

**Paths:**
```r
# ❌ BAD:
read.csv("C:/Users/John/data/file.csv")

# ✅ GOOD:
read.csv(here::here("data", "file.csv"))
```

**Error Messages:**
```r
# ❌ BAD:
stop("Error")

# ✅ GOOD:
stop(sprintf("Required column '%s' not found. Did you run workflow 02 first?", col_name))
```

**Function Design:**
```r
# ❌ BAD:
f <- function(x) { x * 2 }

# ✅ GOOD:
double_value <- function(x) {
  if (!is.numeric(x)) stop("x must be numeric")
  x * 2
}
```

**Documentation:**
```r
# ❌ BAD:
# Function to process data
process <- function(d) { ... }

# ✅ GOOD:
#' Remove Duplicate Detections
#'
#' @description
#' Identifies and removes duplicate detection events based on
#' Detector, DateTime, and auto_id combination.
#'
#' @param df Data frame with detection events
#'
#' @return Data frame with duplicates removed
#'
#' @section CONTRACT:
#' - Keeps first occurrence of each unique detection
#' - Logs number of duplicates removed
#' - Does not modify input data frame
#'
#' @export
remove_duplicates <- function(df) { ... }
```

---

## APPENDIX B: TEMPLATES

### Function Template

```r
#' [Function Title]
#'
#' @description
#' [Detailed description]
#'
#' @param param_name [Type]. [Description]
#'
#' @return [Description of return value]
#'
#' @details
#' [Additional context]
#'
#' @section CONTRACT:
#' - [Guarantee 1]
#' - [Guarantee 2]
#'
#' @section DOES NOT:
#' - [Exclusion 1]
#' - [Exclusion 2]
#'
#' @examples
#' \dontrun{
#' # [Example code]
#' }
#'
#' @export
function_name <- function(param1, param2 = default) {
  
  # -------------------------
  # Input validation
  # -------------------------
  if (!is.data.frame(param1)) {
    stop("param1 must be a data frame")
  }
  
  # -------------------------
  # Processing
  # -------------------------
  result <- param1 %>%
    mutate(...)
  
  # -------------------------
  # Return
  # -------------------------
  result
}
```

### Workflow Stage Template

```r
# ==============================================================================
# STAGE X.Y: [STAGE NAME]
# ==============================================================================

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│          STAGE X.Y: [Stage Name]                               │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("[Description of what this stage does]...")

# -------------------------
# Sub-step 1
# -------------------------

# Code here

message("✓ [Sub-step 1 complete]")

# -------------------------
# Sub-step 2
# -------------------------

# Code here

message("✓ [Sub-step 2 complete]")

log_message("[Stage X.Y] [Summary of what happened]")
```

---

## VERSION HISTORY

**v1.0 (2024-12-26)**
- Initial comprehensive standards document
- Covers all aspects of bat acoustic analysis pipeline
- Includes collaboration standards
- Ready for Claude Project Knowledge integration

---

## ACKNOWLEDGMENTS

This standards document synthesizes best practices from:
- Tidyverse style guide
- Google R style guide
- rOpenSci development guide
- Scientific reproducibility literature
- Bat acoustic analysis domain expertise

---

**END OF CODING STANDARDS v1.0**


