# Quick script to get abstract-ready numbers
# Run this after Workflow 02 completes

library(tidyverse)
library(here)
library(mgcv)

# Load your outputs
habitat_effects <- read_csv(here("results", "production", "tables", "habitat_effects_v1.csv"))
model_metadata <- read_csv(here("results", "production", "tables", "model_metadata_v1.csv"))
model <- readRDS(here("outputs", "production", "models", "nb_gamm_habitat_reml_v1.rds"))

# ═══════════════════════════════════════════════════════════════════
# ABSTRACT-READY NUMBERS
# ═══════════════════════════════════════════════════════════════════

message("\n═══════════════════════════════════════════════════════════════════")
message("ABSTRACT-READY RESULTS")
message("═══════════════════════════════════════════════════════════════════\n")

# Sample size
message("SAMPLE SIZE:")
message(sprintf("  n = %d detector-nights", model_metadata$n))
message(sprintf("  %d detectors across %d sites", model_metadata$n_detectors, model_metadata$n_sites))
message(sprintf("  Study period: %s to %s", model_metadata$date_min, model_metadata$date_max))

# Model fit
message("\nMODEL FIT:")
message(sprintf("  Deviance explained: %.1f%%", model_metadata$deviance_explained * 100))
message(sprintf("  AIC: %.2f", model_metadata$aic))

# Habitat effects (the key results!)
message("\nHABITAT EFFECTS (vs Interior reference):")
for (i in seq_len(nrow(habitat_effects))) {
  row <- habitat_effects[i, ]
  sig <- if (row$p_value < 0.001) "***" else if (row$p_value < 0.01) "**" else if (row$p_value < 0.05) "*" else ""
  message(sprintf(
    "  %s:  IRR = %.2f [95%% CI: %.2f–%.2f], p = %. 3f %s",
    gsub("habitat", "", row$term),
    row$irr,
    row$irr_low,
    row$irr_high,
    row$p_value,
    sig
  ))
}

# Temporal smooth significance
message("\nTEMPORAL SMOOTH:")
smooth_summary <- summary(model)$s.table
temporal_row <- smooth_summary[grep("night_scaled", rownames(smooth_summary)), ]
message(sprintf("  s(night_scaled): edf = %. 2f, F = %.2f, p = %. 4f",
                temporal_row["edf"], temporal_row["F"], temporal_row["p-value"]))

# Random effects variance
message("\nRANDOM EFFECTS (variance components):")
re_summary <- summary(model)$s.table
site_row <- re_summary[grep("site", rownames(re_summary)), ]
detector_row <- re_summary[grep("detector_id", rownames(re_summary)), ]
message(sprintf("  Site: edf = %.2f", site_row["edf"]))
message(sprintf("  Detector: edf = %.2f", detector_row["edf"]))

message("\n════════════��══════════════════════════════════════════════════════\n")
