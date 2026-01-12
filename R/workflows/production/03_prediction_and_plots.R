# ==============================================================================
# 03_prediction_and_plots. R — PRODUCTION VISUALIZATION (LOCKED)
# ==============================================================================
# LAYER:  PRODUCTION
#
# PURPOSE
# -------
# Generate poster-ready predictions and figures from the canonical NB GAMM.
# Produces habitat effect plots, temporal smooth visualization, and model
# diagnostics suitable for scientific presentation.
#
# WORKFLOW POSITION
# -----------------
#   Previous: R/workflows/production/02_model_nb_gamm.R
#   Next:      Poster compilation (poster. qmd)
#
# INPUTS
# ------
# Files:
#   - outputs/production/models/nb_gamm_habitat_reml_v1.rds (fitted model)
#   - outputs/data_backbone/calls_per_night_clean.rds (for raw data overlay)
#   - results/production/tables/habitat_effects_v1.csv (IRR table)
#
# OUTPUTS (RESULTS — POSTER DELIVERABLES)
# ----------------------------------------
# Figures:
#   - results/production/figs/habitat_predicted_v1.png
#   - results/production/figs/temporal_smooth_v1.png
#   - results/production/figs/diagnostic_panel_v1.png
#   - results/production/figs/raw_activity_by_habitat_v1.png
#
# Tables:
#   - results/production/tables/predictions_by_habitat_v1.csv
#
# PROCESSING STAGES
# -----------------
# Stage 3. 1: Setup
# Stage 3.2: Load Model and Data
# Stage 3.3: Generate Habitat Predictions
# Stage 3.4: Create Habitat Effect Figure
# Stage 3.5: Create Temporal Smooth Figure
# Stage 3.6: Create Raw Activity Figure
# Stage 3.7: Create Diagnostic Panel
# Stage 3.8: Write Outputs
#
# FIGURE SPECIFICATIONS
# ---------------------
# - Theme: theme_minimal()
# - Dimensions: 8x6 inches (default), 300 DPI
# - Colors: Interior=#2E7D32, Edge=#1565C0, Open=#F9A825
#
# DEPENDENCIES
# ------------
# R Packages:
#   - tidyverse, here, mgcv, ggplot2
#
# Custom Functions:
#   - R/functions/plotting/ (to be created if needed)
#
# USAGE
# -----
# source(here:: here("R", "workflows", "production", "03_prediction_and_plots.R"))
#
# MAINTAINER NOTES
# ----------------
# - Predictions use mean recording hours (~13 hrs) for interpretable scale
# - Temporal smooth plotted on original date scale for poster clarity
# - Diagnostic panel uses mgcv::gam. check() internals
#
# CHANGELOG
# ---------
# 2026-01-11: Initial version compliant with CODING_STANDARDS v1.3
#
# ==============================================================================

# LAYER:  PRODUCTION

here::i_am("R/workflows/production/03_prediction_and_plots.R")

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║              WORKFLOW 03: PREDICTION AND VISUALIZATION                      ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║              WORKFLOW 03: PREDICTION AND VISUALIZATION                      ��")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")


# ------------------------------------------------------------------------------
# STAGE 3.1: SETUP
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                     STAGE 3.1: Setup                           │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Setting up visualization environment...")

library(tidyverse)
library(here)
library(mgcv)

source(here:: here("R", "functions", "load_all. R"))

# Initialize logging
log_path <- here::here("logs", "visualization_runs. log")
initialize_pipeline_log(
  log_path = log_path,
  workflow_name = "03_prediction_and_plots",
  script_path = here::here("R", "workflows", "production", "03_prediction_and_plots. R")
)

log_message("[PRODUCTION] === WORKFLOW 03 START ===", log_path = log_path)

# -------------------------
# Output directories
# -------------------------
res_figs <- here::here("results", "production", "figs")
res_tables <- here::here("results", "production", "tables")
dir.create(res_figs, recursive = TRUE, showWarnings = FALSE)
dir.create(res_tables, recursive = TRUE, showWarnings = FALSE)

# -------------------------
# Define color palette (locked for poster)
# -------------------------
HABITAT_COLORS <- c(
  "Interior" = "#2E7D32",
  "Edge" = "#1565C0",
  "Open" = "#F9A825"
)

# -------------------------
# Figure specifications
# -------------------------
FIG_WIDTH <- 8
FIG_HEIGHT <- 6
FIG_DPI <- 300

message("Setup complete")
message(sprintf("  Output directory: %s", res_figs))

log_message("[PRODUCTION] [Stage 3.1] Setup complete", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.2: LOAD MODEL AND DATA
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│              STAGE 3.2: Load Model and Data                    │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Loading fitted model and backbone data...")

# -------------------------
# Load fitted model
# -------------------------
model_path <- here:: here("outputs", "production", "models", "nb_gamm_habitat_reml_v1.rds")

if (!file.exists(model_path)) {
  stop(sprintf(
    "Model file not found: %s\n  Fix:  Run Workflow 02 first to fit the canonical model",
    model_path
  ))
}

model <- readRDS(model_path)

# -------------------------
# Load backbone data
# -------------------------
backbone_path <- here::here("outputs", "data_backbone", "calls_per_night_clean.rds")

if (!file.exists(backbone_path)) {
  stop(sprintf(
    "Backbone file not found: %s\n  Fix:  Run Workflow 01 first",
    backbone_path
  ))
}

df_backbone <- readRDS(backbone_path)

# -------------------------
# Load habitat effects table
# -------------------------
effects_path <- here:: here("results", "production", "tables", "habitat_effects_v1.csv")

if (!file.exists(effects_path)) {
  stop(sprintf(
    "Habitat effects table not found:  %s\n  Fix: Run Workflow 02 first",
    effects_path
  ))
}

habitat_effects <- readr::read_csv(effects_path, show_col_types = FALSE)

# -------------------------
# Prepare modeling data (same filter as Workflow 02)
# -------------------------
df_model <- df_backbone %>%
  filter(! is.na(recording_hours) & recording_hours > 0) %>%
  filter(! is.na(habitat) & ! is.na(site) & !is.na(detector_id)) %>%
  filter(night_class != "Dead") %>%
  mutate(
    habitat = factor(habitat, levels = c("Interior", "Edge", "Open")),
    site = as.factor(site),
    detector_id = as.factor(detector_id),
    night_scaled = as.numeric(scale(as.numeric(night)))
  )

# Store scaling parameters for back-transformation
night_center <- mean(as.numeric(df_model$night))
night_scale <- sd(as.numeric(df_model$night))

message("Model and data loaded")
message(sprintf("  Model AIC: %. 2f", AIC(model)))
message(sprintf("  Backbone rows: %d", nrow(df_backbone)))
message(sprintf("  Modeling rows: %d", nrow(df_model)))

log_message(
  sprintf("[PRODUCTION] [Stage 3.2] Loaded model (AIC=%.2f) and data (%d rows)",
          AIC(model), nrow(df_model)),
  log_path = log_path
)


# ------------------------------------------------------------------------------
# STAGE 3.3: GENERATE HABITAT PREDICTIONS
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│            STAGE 3.3: Generate Habitat Predictions             │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Generating predictions for each habitat level...")

# -------------------------
# Create prediction grid
# Use mean recording hours and mean night_scaled for interpretable predictions
# -------------------------
mean_hours <- mean(df_model$recording_hours)
mean_night_scaled <- 0  # Center of study period

# Get representative site and detector for predictions
# (random effects will be marginalized out, but we need valid factor levels)
ref_site <- levels(df_model$site)[1]
ref_detector <- levels(df_model$detector_id)[1]

newdata_habitat <- tibble(
  habitat = factor(c("Interior", "Edge", "Open"), levels = c("Interior", "Edge", "Open")),
  night_scaled = mean_night_scaled,
  recording_hours = mean_hours,
  site = factor(ref_site, levels = levels(df_model$site)),
  detector_id = factor(ref_detector, levels = levels(df_model$detector_id))
)

# -------------------------
# Generate predictions with SE
# exclude. terms excludes random effects for population-level predictions
# -------------------------
preds <- predict(
  model,
  newdata = newdata_habitat,
  type = "link",
  se. fit = TRUE,
  exclude = c("s(site)", "s(detector_id)")
)

# -------------------------
# Convert to response scale with CI
# -------------------------
predictions_habitat <- newdata_habitat %>%
  mutate(
    fit_link = preds$fit,
    se_link = preds$se.fit,
    # Back-transform to response scale (expected calls per night at mean hours)
    predicted_calls = exp(fit_link),
    ci_low = exp(fit_link - 1.96 * se_link),
    ci_high = exp(fit_link + 1.96 * se_link),
    # Also compute rate per hour
    predicted_rate = predicted_calls / recording_hours,
    rate_ci_low = ci_low / recording_hours,
    rate_ci_high = ci_high / recording_hours
  )

message("Predictions generated")
print(predictions_habitat %>% select(habitat, predicted_calls, ci_low, ci_high, predicted_rate))

log_message("[PRODUCTION] [Stage 3.3] Generated habitat predictions", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.4: CREATE HABITAT EFFECT FIGURE
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.4: Create Habitat Effect Figure              │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating habitat effect figure...")

# -------------------------
# Predicted activity +/- CI by habitat
# -------------------------
fig_habitat <- ggplot(predictions_habitat, aes(x = habitat, y = predicted_calls, fill = habitat)) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.2,
    linewidth = 0.8,
    color = "black"
  ) +
  scale_fill_manual(values = HABITAT_COLORS, guide = "none") +
  labs(
    title = "Predicted Bat Activity by Habitat Type",
    subtitle = sprintf("Model predictions at mean effort (%.1f hrs/night)", mean_hours),
    x = "Habitat",
    y = "Predicted Calls per Night",
    caption = "Error bars show 95% confidence intervals\nRandom effects excluded for population-level inference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot. title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid. major. x = element_blank()
  )

message("Habitat effect figure created")

log_message("[PRODUCTION] [Stage 3.4] Created habitat effect figure", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.5: CREATE TEMPORAL SMOOTH FIGURE
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.5: Create Temporal Smooth Figure             │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating temporal smooth figure...")
# -------------------------
# Generate temporal predictions across study period
# -------------------------
night_seq_scaled <- seq(
  min(df_model$night_scaled),
  max(df_model$night_scaled),
  length.out = 100
)

# Back-transform to actual dates for plotting
night_seq_dates <- as.Date(night_seq_scaled * night_scale + night_center, origin = "1970-01-01")

newdata_temporal <- tibble(
  night_scaled = night_seq_scaled,
  night_date = night_seq_dates,
  habitat = factor("Interior", levels = c("Interior", "Edge", "Open")),
  recording_hours = mean_hours,
  site = factor(ref_site, levels = levels(df_model$site)),
  detector_id = factor(ref_detector, levels = levels(df_model$detector_id))
)

# Predict temporal trend (excluding habitat and random effects to show pure time trend)
preds_temporal <- predict(
  model,
  newdata = newdata_temporal,
  type = "link",
  se. fit = TRUE,
  exclude = c("s(site)", "s(detector_id)")
)

temporal_predictions <- newdata_temporal %>%
  mutate(
    fit_link = preds_temporal$fit,
    se_link = preds_temporal$se.fit,
    predicted_calls = exp(fit_link),
    ci_low = exp(fit_link - 1.96 * se_link),
    ci_high = exp(fit_link + 1.96 * se_link)
  )

# -------------------------
# Create temporal smooth figure
# -------------------------
fig_temporal <- ggplot(temporal_predictions, aes(x = night_date, y = predicted_calls)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.3, fill = "#1565C0") +
  geom_line(linewidth = 1.2, color = "#1565C0") +
  labs(
    title = "Seasonal Trend in Bat Activity",
    subtitle = "Penalized smooth s(night_scaled, k = 7)",
    x = "Date",
    y = "Predicted Calls per Night",
    caption = sprintf("Predictions at Interior habitat, %. 1f hrs effort\n95%% confidence band shown", mean_hours)
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text. x = element_text(angle = 45, hjust = 1)
  )

message("Temporal smooth figure created")

log_message("[PRODUCTION] [Stage 3.5] Created temporal smooth figure", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.6: CREATE RAW ACTIVITY FIGURE
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.6: Create Raw Activity Figure                │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating raw activity by habitat figure...")

# -------------------------
# Raw calls per night by habitat (faceted)
# -------------------------
fig_raw_activity <- df_model %>%
  ggplot(aes(x = night, y = calls_per_night, color = habitat)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1, span = 0.5) +
  facet_wrap(~habitat, ncol = 1, scales = "free_y") +
  scale_color_manual(values = HABITAT_COLORS, guide = "none") +
  labs(
    title = "Raw Bat Activity by Habitat Type",
    subtitle = "Calls per night with LOESS smoother",
    x = "Date",
    y = "Calls per Night"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal(base_size = 12) +
  theme(
    plot. title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )

message("Raw activity figure created")

log_message("[PRODUCTION] [Stage 3.6] Created raw activity figure", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.7: CREATE DIAGNOSTIC PANEL
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.7: Create Diagnostic Panel                   │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating diagnostic panel...")

# -------------------------
# Extract residuals and fitted values
# -------------------------
resids <- residuals(model, type = "deviance")
fitted_vals <- fitted(model)

diag_df <- tibble(
  fitted = fitted_vals,
  residuals = resids,
  observed = df_model$calls_per_night
)

# -------------------------
# Q-Q plot
# -------------------------
qq_plot <- ggplot(diag_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.5, color = "#1565C0") +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot",
    subtitle = "Deviance residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot. title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# -------------------------
# Residuals vs Fitted
# -------------------------
resid_fitted_plot <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5, color = "#1565C0") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 1) +
  labs(
    title = "Residuals vs Fitted",
    subtitle = "Deviance residuals",
    x = "Fitted Values",
    y = "Deviance Residuals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# -------------------------
# Histogram of residuals
# -------------------------
resid_hist <- ggplot(diag_df, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "#1565C0", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residual Distribution",
    subtitle = "Deviance residuals",
    x = "Residuals",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# -------------------------
# Observed vs Fitted
# -------------------------
obs_fitted_plot <- ggplot(diag_df, aes(x = fitted, y = observed)) +
  geom_point(alpha = 0.5, color = "#1565C0") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs Fitted",
    subtitle = "1:1 line shown",
    x = "Fitted Values",
    y = "Observed Calls"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# -------------------------
# Combine into panel using patchwork
# -------------------------
if (! requireNamespace("patchwork", quietly = TRUE)) {
  message("  Note: Install 'patchwork' package for combined diagnostic panel")
  message("  Saving individual diagnostic plots instead...")
  fig_diagnostic <- NULL
} else {
  library(patchwork)
  fig_diagnostic <- (qq_plot | resid_fitted_plot) / (resid_hist | obs_fitted_plot) +
    plot_annotation(
      title = "Model Diagnostics",
      subtitle = sprintf("NB GAMM: AIC = %.2f, Deviance explained = %.1f%%",
                         AIC(model), summary(model)$dev.expl * 100),
      theme = theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40")
      )
    )
}

message("Diagnostic panel created")

log_message("[PRODUCTION] [Stage 3.7] Created diagnostic panel", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.8: WRITE OUTPUTS
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│                 STAGE 3.8: Write Outputs                        │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Saving figures and tables...")

# -------------------------
# Save figures
# -------------------------
ggsave(
  filename = here::here(res_figs, "habitat_predicted_v1.png"),
  plot = fig_habitat,
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  dpi = FIG_DPI
)
message("  Saved: habitat_predicted_v1.png")

ggsave(
  filename = here::here(res_figs, "temporal_smooth_v1.png"),
  plot = fig_temporal,
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  dpi = FIG_DPI
)
message("  Saved: temporal_smooth_v1.png")

ggsave(
  filename = here::here(res_figs, "raw_activity_by_habitat_v1.png"),
  plot = fig_raw_activity,
  width = FIG_WIDTH,
  height = 10,
  dpi = FIG_DPI
)
message("  Saved:  raw_activity_by_habitat_v1.png")

if (! is.null(fig_diagnostic)) {
  ggsave(
    filename = here::here(res_figs, "diagnostic_panel_v1.png"),
    plot = fig_diagnostic,
    width = 10,
    height = 8,
    dpi = FIG_DPI
  )
  message("  Saved: diagnostic_panel_v1.png")
} else {
  # Save individual diagnostic plots
  ggsave(here::here(res_figs, "diag_qq_v1.png"), qq_plot, width = 6, height = 5, dpi = FIG_DPI)
  ggsave(here:: here(res_figs, "diag_resid_fitted_v1.png"), resid_fitted_plot, width = 6, height = 5, dpi = FIG_DPI)
  ggsave(here:: here(res_figs, "diag_resid_hist_v1.png"), resid_hist, width = 6, height = 5, dpi = FIG_DPI)
  ggsave(here::here(res_figs, "diag_obs_fitted_v1.png"), obs_fitted_plot, width = 6, height = 5, dpi = FIG_DPI)
  message("  Saved:  individual diagnostic plots (diag_*. png)")
}

# -------------------------
# Save prediction table
# -------------------------
predictions_export <- predictions_habitat %>%
  select(
    habitat,
    predicted_calls,
    ci_low,
    ci_high,
    predicted_rate,
    rate_ci_low,
    rate_ci_high,
    recording_hours
  )

readr::write_csv(
  predictions_export,
  here::here(res_tables, "predictions_by_habitat_v1.csv")
)
message("  Saved: predictions_by_habitat_v1.csv")

message("\nOutputs written successfully")

log_message(
  sprintf("[PRODUCTION] [Stage 3.8] Wrote %d figures and 1 table to %s",
          ifelse(is.null(fig_diagnostic), 7, 4), res_figs),
  log_path = log_path
)


# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║                         WORKFLOW 03 COMPLETE                                ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║                         WORKFLOW 03 COMPLETE                                ║")
message("╚══════════════════════════════════════════════════════════════════════════════╝\n")

message("--- Workflow Summary ---")
message(sprintf("  Model: NB GAMM (AIC = %.2f)", AIC(model)))
message(sprintf("  Deviance explained: %.1f%%", summary(model)$dev.expl * 100))
message(sprintf("  Predictions at:  %. 1f hrs effort (mean)", mean_hours))

message("\n  Habitat Predictions (calls/night):")
for (i in seq_len(nrow(predictions_habitat))) {
  message(sprintf("    %s: %. 1f [%. 1f, %.1f]",
                  predictions_habitat$habitat[i],
                  predictions_habitat$predicted_calls[i],
                  predictions_habitat$ci_low[i],
                  predictions_habitat$ci_high[i]))
}

message("\n  Figures saved:")
message(sprintf("    %s/habitat_predicted_v1.png", res_figs))
message(sprintf("    %s/temporal_smooth_v1.png", res_figs))
message(sprintf("    %s/raw_activity_by_habitat_v1.png", res_figs))
message(sprintf("    %s/diagnostic_panel_v1.png", res_figs))

message("\n  Tables saved:")
message(sprintf("    %s/predictions_by_habitat_v1.csv", res_tables))
message("")

log_message("[PRODUCTION] === WORKFLOW 03 COMPLETE ===", log_path = log_path)

# -------------------------
# Store objects for interactive use
# -------------------------
habitat_predictions <- predictions_habitat
temporal_predictions_df <- temporal_predictions
message("Objects available:  habitat_predictions, temporal_predictions_df")
