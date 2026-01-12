# ==============================================================================
# 02_model_nb_gamm.R — PRODUCTION INFERENCE (LOCKED)
# ==============================================================================
# LAYER: PRODUCTION
#
# PURPOSE
# -------
# Fit the canonical Negative Binomial GAMM for bat activity using certified
# backbone data (detector × night). Produces poster-ready inference using
# Option B (within-model parametric habitat contrasts):
#   - habitat IRR (rate ratios) + 95% CI + p-values
#   - minimal model metadata (formula, family, AIC, deviance explained, n)
#
# NOTES ON INFERENCE CHOICES
# --------------------------
# This workflow is intentionally "Production-pure":
# - Poster inference uses Option B:  Wald tests on habitat coefficients within
#   the canonical model fit (REML).
# - Option A (null-vs-habitat model comparison via ML/LRT or ΔAIC) is explicitly
#   reserved for EXPLORATORY workflows and is NOT performed here.
#
# OUTPUTS VS RESULTS (PROJECT POLICY)
# -----------------------------------
# - outputs/  : continuity artifacts required for pipeline re-runs (e.g., . rds models)
# - results/  : human-facing deliverables for poster/report (figures, tables)
#
# INPUTS
# ------
# - outputs/data_backbone/calls_per_night_clean.rds
#
# OUTPUTS (CONTINUITY)
# --------------------
# - outputs/production/models/nb_gamm_habitat_reml_v1.rds
#
# RESULTS (POSTER)
# ----------------
# - results/production/tables/habitat_effects_v1.csv
# - results/production/tables/model_metadata_v1.csv
#
# PERFORMANCE EXPECTATIONS
# ------------------------
# Approximate dataset sizes and runtimes:
#   - Small (< 200 rows): < 10 seconds
#   - Medium (200-1000 rows): 10-30 seconds
#   - Large (> 1000 rows): 30-120 seconds
#
# DEPENDENCIES
# ------------
# R Packages:
#   - tidyverse, here, mgcv
#
# Custom Functions (via R/functions/load_all. R):
#   - core/utilities. R:  log_message(), initialize_pipeline_log()
#   - validation/validation.R: validate_data_frame(), assert_*, check_duplicates()
#   - modeling/fit_nb_gamm.R: fit_nb_gamm()
#   - modeling/extract_habitat_effects.R:  extract_habitat_effects()
#
# TROUBLESHOOTING
# ---------------
# Issue: "Backbone file not found" error
# Fix:    Run Workflow 01 first to generate outputs/data_backbone/calls_per_night_clean.rds
#
# Issue: Model fails to converge
# Fix:   Check for sufficient variation in predictors; consider reducing smooth_k
#
# USAGE
# -----
# source(here:: here("R", "workflows", "production", "02_model_nb_gamm.R"))
#
# MAINTAINER NOTES
# ----------------
# - night_scaled is computed inside fit_nb_gamm() using scale(as.numeric(night))
# - Habitat is releveled with Interior as reference
# - Use file. path() when base path is already from here:: here()
#
# CHANGELOG
# ---------
# 2026-01-11: Full CODING_STANDARDS v1.3 compliance audit and corrections
# 2026-01-10: Initial production workflow
#
# ==============================================================================

# LAYER: PRODUCTION

# -------------------------
# Anchor project root (portable, deterministic)
# -------------------------
here::i_am("R/workflows/production/02_model_nb_gamm.R")

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║                     WORKFLOW 02: FIT NB GAMM (PRODUCTION)                   ║
# ╚═════════════════════════════════════════════════════════════��════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║                     WORKFLOW 02: FIT NB GAMM (PRODUCTION)                   ║")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")


# ------------------------------------------------------------------------------
# STAGE 2.1: SETUP
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                     STAGE 2.1: Setup                           │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Setting up workflow environment...")

library(tidyverse)
library(here)
library(mgcv)

source(here::here("R", "functions", "load_all.R"))

# Model-run log (separate from pipeline_log. txt)
model_log_path <- here::here("logs", "model_runs. log")
initialize_pipeline_log(
  log_path = model_log_path,
  workflow_name = "02_model_nb_gamm",
  script_path = here::here("R", "workflows", "production", "02_model_nb_gamm.R")
)

log_message("[PRODUCTION] === WORKFLOW 02 START ===", log_path = model_log_path)

# -------------------------
# Output directories (continuity artifacts)
# -------------------------
out_models <- here::here("outputs", "production", "models")
dir.create(out_models, recursive = TRUE, showWarnings = FALSE)

# -------------------------
# Results directories (poster deliverables)
# -------------------------
res_tables <- here::here("results", "production", "tables")
res_figs <- here::here("results", "production", "figs")
dir.create(res_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(res_figs, recursive = TRUE, showWarnings = FALSE)

message("✓ Setup complete")
message(sprintf("  Model outputs: %s", out_models))
message(sprintf("  Result tables: %s", res_tables))

log_message("[PRODUCTION] [Stage 2.1] Setup complete", log_path = model_log_path)


# ------------------------------------------------------------------------------
# STAGE 2.2: LOAD CERTIFIED BACKBONE DATA
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 2.2: Load Certified Backbone Data               │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Loading backbone data...")

backbone_path <- here::here("outputs", "data_backbone", "calls_per_night_clean.rds")

if (! file.exists(backbone_path)) {
  stop(sprintf(
    "Backbone file not found: %s\n  Fix:  Run Workflow 01 to generate outputs/data_backbone/calls_per_night_clean.rds",
    backbone_path
  ))
}

df <- readRDS(backbone_path)

validate_data_frame(
  df,
  required_cols = c(
    "detector", "detector_id", "site", "habitat", "night",
    "calls_per_night", "recording_hours", "night_class"
  ),
  col_types = list(night = "Date", recording_hours = "numeric"),
  stop_on_error = TRUE
)

message("✓ Backbone data loaded")
message(sprintf("  Rows: %s", format(nrow(df), big.mark = ",")))
message(sprintf("  Detectors: %d", n_distinct(df$detector)))
message(sprintf("  Sites:  %d", n_distinct(df$site)))
message(sprintf("  Nights:  %d", n_distinct(df$night)))

log_message(
  sprintf("[PRODUCTION] [Stage 2.2] Loaded backbone:  %d rows from %s", nrow(df), basename(backbone_path)),
  log_path = model_log_path
)


# ------------------------------------------------------------------------------
# STAGE 2.3: PREPARE MODEL DATA (OFFSET-SAFE) + HABITAT CODING
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                STAGE 2.3: Prepare Model Data                    │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Preparing model data (offset-safe filtering)...")

df_model <- df %>%
  filter(! is.na(recording_hours) & recording_hours > 0) %>%
  filter(! is.na(habitat) & ! is.na(site) & !is.na(detector_id)) %>%
  filter(night_class != "Dead") %>%
  mutate(
    habitat = factor(habitat, levels = c("Interior", "Edge", "Open"))
  ) %>%
  arrange(detector_id, night)

assert_not_empty(df_model, context = "after filtering to offset-safe modeling rows")

check_duplicates(
  df_model,
  key_cols = c("detector_id", "night"),
  action = "stop",
  return_duplicates = FALSE
)

n_excluded <- nrow(df) - nrow(df_model)

message("✓ Model data prepared")
message(sprintf("  Rows retained: %s", format(nrow(df_model), big.mark = ",")))
message(sprintf("  Rows excluded (offset-unsafe or missing predictors): %s", format(n_excluded, big.mark = ",")))
message(sprintf("  Habitat levels: %s", paste(levels(df_model$habitat), collapse = ", ")))

log_message(
  sprintf("[PRODUCTION] [Stage 2.3] Prepared model data:  %d rows retained, %d excluded", nrow(df_model), n_excluded),
  log_path = model_log_path
)


# ------------------------------------------------------------------------------
# STAGE 2.4: FIT CANONICAL MODEL (OPTION B INFERENCE)
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 2.4: Fit Canonical Model (REML)                 │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Fitting canonical NB GAMM...")
message("Note: night_scaled is computed inside fit_nb_gamm() using scale(as. numeric(night)).")

model_habitat <- fit_nb_gamm(
  df = df_model,
  smooth_k = 7,
  method = "REML",
  quiet = FALSE
)

message("✓ Canonical model fit complete")
message(sprintf("  AIC: %.2f", AIC(model_habitat)))
message(sprintf("  Deviance explained: %.1f%%", summary(model_habitat)$dev.expl * 100))

log_message(
  sprintf("[PRODUCTION] [Stage 2.4] Model fit complete: AIC=%.2f, dev.expl=%. 3f",
          AIC(model_habitat), summary(model_habitat)$dev.expl),
  log_path = model_log_path
)


# ------------------------------------------------------------------------------
# STAGE 2.5: EXTRACT POSTER-READY RESULTS (OPTION B)
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 2.5: Extract Poster-Ready Results               │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Extracting habitat effects and model metadata...")

habitat_effects <- extract_habitat_effects(model_habitat, reference = "Interior")

model_metadata <- tibble(
  model_name = "nb_gamm_habitat_reml",
  family = model_habitat$family$family,
  link = model_habitat$family$link,
  method = model_habitat$method,
  smooth_k = 7L,
  n = nobs(model_habitat),
  n_detectors = n_distinct(df_model$detector_id),
  n_sites = n_distinct(df_model$site),
  date_min = min(df_model$night),
  date_max = max(df_model$night),
  aic = AIC(model_habitat),
  deviance_explained = summary(model_habitat)$dev.expl,
  formula = paste(deparse(formula(model_habitat)), collapse = " ")
)

message("✓ Results extracted")
message("  Habitat effects (IRR):")
print(habitat_effects)

log_message("[PRODUCTION] [Stage 2.5] Extracted habitat effects + model metadata",
            log_path = model_log_path)


# ------------------------------------------------------------------------------
# STAGE 2.6: WRITE OUTPUTS (CONTINUITY) + RESULTS (POSTER)
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│        STAGE 2.6: Write Outputs (Continuity) + Results          │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Writing outputs and results...")

# Use file.path() since out_models and res_tables are already full paths
model_path <- file.path(out_models, "nb_gamm_habitat_reml_v1.rds")
habitat_path <- file.path(res_tables, "habitat_effects_v1.csv")
metadata_path <- file.path(res_tables, "model_metadata_v1.csv")

saveRDS(model_habitat, model_path)
readr::write_csv(habitat_effects, habitat_path)
readr::write_csv(model_metadata, metadata_path)

message("✓ Outputs written")
message("  Continuity:")
message(sprintf("    %s", model_path))
message("  Poster results:")
message(sprintf("    %s", habitat_path))
message(sprintf("    %s", metadata_path))

log_message(
  sprintf("[PRODUCTION] [Stage 2.6] Wrote continuity model to %s and poster tables to %s",
          basename(model_path), res_tables),
  log_path = model_log_path
)


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║                         WORKFLOW 02 COMPLETE                                ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║                         WORKFLOW 02 COMPLETE                                ║")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")

message("--- Workflow Summary ---")
message(sprintf("  Model: %s", "nb_gamm_habitat_reml_v1"))
message(sprintf("  Observations: %d", nobs(model_habitat)))
message(sprintf("  Detectors: %d", n_distinct(df_model$detector_id)))
message(sprintf("  Sites:  %d", n_distinct(df_model$site)))
message(sprintf("  Date range: %s to %s", min(df_model$night), max(df_model$night)))
message(sprintf("  AIC: %.2f", AIC(model_habitat)))
message(sprintf("  Deviance explained: %. 1f%%", summary(model_habitat)$dev.expl * 100))
message("")
message("  Outputs:")
message(sprintf("    Model: %s", model_path))
message(sprintf("    Habitat effects: %s", habitat_path))
message(sprintf("    Model metadata: %s", metadata_path))
message("")

log_message("[PRODUCTION] === WORKFLOW 02 COMPLETE ===", log_path = model_log_path)

# -------------------------
# Convenience object for interactive use
# -------------------------
nb_gamm_model <- model_habitat
message("Model available as: nb_gamm_model")
