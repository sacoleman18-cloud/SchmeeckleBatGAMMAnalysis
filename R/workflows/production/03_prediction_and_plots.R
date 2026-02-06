# ==============================================================================
# 03_prediction_and_plots.R — PRODUCTION VISUALIZATION (LOCKED)
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
#   Next:      Poster compilation (poster.qmd)
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
# Stage 3.1: Setup
# Stage 3.2: Load Model and Data
#   → CENTRALIZED PREDICTIONS: Generate all model predictions (habitat + temporal)
#   → CENTRALIZED COMPARISONS: Compute all pairwise habitat comparisons
# Stage 3.3: Prepare Raw Data for Plotting
# Stage 3.4: Create Habitat Effect Figure
# Stage 3.5: Create Temporal Smooth Figure
# Stage 3.6: Create Raw Activity Figure
# Stage 3.7: Create Diagnostic Panel
# Stage 3.8: Write Outputs
#
# REFACTORING NOTES (2026-02-06)
# ------------------------------
# All model predictions are now generated in a centralized block after Stage 3.2.
# All pairwise habitat comparisons are computed in a centralized block after Stage 3.2.
# Each figure applies its own inline theme (POSTER_THEME abstraction removed).
# No confidence interval capping is applied anywhere in this script.
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
# source(here::here("R", "workflows", "production", "03_prediction_and_plots.R"))
#
# MAINTAINER NOTES
# ----------------
# - Predictions use mean recording hours (~13 hrs) for interpretable scale
# - Temporal smooth plotted on original date scale for poster clarity
# - Diagnostic panel uses mgcv::gam.check() internals
#
# CHANGELOG
# ---------
# 2026-02-06: Major refactoring for clean variable flow and standardization
#   - Centralized all model predictions (habitat + temporal) in one location
#   - Centralized pairwise habitat comparisons (all 3 comparisons + brackets)
#   - Removed POSTER_THEME abstraction - each figure has inline theme
#   - Removed all CI capping logic (was lines 648-668)
#   - Standardized variable naming: predicted_rate, ci_low, ci_high
#   - Eliminated redundant prediction blocks in Stages 3.5 and 3.6
#   - Preserved bracket positioning algorithm and statistical behavior
# 2026-01-11: Initial version compliant with CODING_STANDARDS v1.3
#
# ==============================================================================

# LAYER:  PRODUCTION

here::i_am("R/workflows/production/03_prediction_and_plots.R")

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║              WORKFLOW 03: PREDICTION AND VISUALIZATION                      ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

message("\n╔══════════════════════════════════════════════════════════════════════════════╗")
message("║              WORKFLOW 03: PREDICTION AND VISUALIZATION                      ║")
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

source(here::here("R", "functions", "load_all.R"))

# Initialize logging
log_path <- here::here("logs", "visualization_runs.log")
initialize_pipeline_log(
  log_path = log_path,
  workflow_name = "03_prediction_and_plots",
  script_path = here::here("R", "workflows", "production", "03_prediction_and_plots.R")
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
  "Interior" = "#ffab0b",
  "Edge" = "#52e900",
  "Open" = "#af04e8"
)

# -------------------------
# Display labels for poster figures (canonical naming)
# -------------------------
HABITAT_DISPLAY <- c(
  "Interior" = "Interior Forest",
  "Edge" = "Edge Habitat",
  "Open" = "Open Habitat"
)

# -------------------------
# Figure specifications
# -------------------------
FIG_WIDTH <- 8
FIG_HEIGHT <- 6
FIG_DPI <- 300

# -------------------------
# NOTE: POSTER_THEME removed - each figure now has inline theme
# -------------------------

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
model_path <- here::here("outputs", "production", "models", "nb_gamm_habitat_reml_v1.rds")

if (!file.exists(model_path)) {
  stop(sprintf(
    "Model file not found: %s\n  Fix: Run Workflow 02 first to fit the canonical model",
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
    "Backbone file not found: %s\n  Fix: Run Workflow 01 first",
    backbone_path
  ))
}

df_backbone <- readRDS(backbone_path)

# -------------------------
# Prepare modeling data (same filter as Workflow 02)
# -------------------------
df_model <- df_backbone %>%
  filter(!is.na(recording_hours) & recording_hours > 0) %>%
  filter(!is.na(habitat) & !is.na(site) & !is.na(detector_id)) %>%
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
message(sprintf("  Model AIC: %.2f", AIC(model)))
message(sprintf("  Backbone rows: %d", nrow(df_backbone)))
message(sprintf("  Modeling rows: %d", nrow(df_model)))

log_message(
  sprintf("[PRODUCTION] [Stage 3.2] Loaded model (AIC=%.2f) and data (%d rows)",
          AIC(model), nrow(df_model)),
  log_path = log_path
)


# ==============================================================================
# CENTRALIZED PREDICTIONS (ALL HABITATS & TEMPORAL)
# ==============================================================================
# NOTE: This block consolidates all prediction generation in one location to
# ensure consistency and eliminate redundancy across plotting stages.
# All downstream plotting stages reference these centralized prediction objects.
# ==============================================================================

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│     CENTRALIZED: Generate All Model Predictions               │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Generating centralized predictions for habitats and temporal trends...")

# -------------------------
# Prediction grid parameters (shared across all predictions)
# -------------------------
mean_hours <- mean(df_model$recording_hours)
mean_night_scaled <- 0  # Center of study period
ref_site <- levels(df_model$site)[1]
ref_detector <- levels(df_model$detector_id)[1]

# -------------------------
# HABITAT PREDICTIONS (at mean time and effort)
# -------------------------
newdata_habitat <- tibble(
  habitat = factor(c("Interior", "Edge", "Open"), levels = c("Interior", "Edge", "Open")),
  night_scaled = mean_night_scaled,
  recording_hours = mean_hours,
  site = factor(ref_site, levels = levels(df_model$site)),
  detector_id = factor(ref_detector, levels = levels(df_model$detector_id))
)

preds_habitat <- predict(
  model,
  newdata = newdata_habitat,
  type = "link",
  se.fit = TRUE,
  exclude = c("s(site)", "s(detector_id)")
)

predictions_habitat <- newdata_habitat %>%
  mutate(
    fit_link = preds_habitat$fit,
    se_link = preds_habitat$se.fit,
    predicted_calls = exp(fit_link),
    ci_low = exp(fit_link - 1.96 * se_link),
    ci_high = exp(fit_link + 1.96 * se_link),
    predicted_rate = predicted_calls / recording_hours,
    rate_ci_low = ci_low / recording_hours,
    rate_ci_high = ci_high / recording_hours
  )

message("✓ Habitat predictions generated")
print(predictions_habitat %>% select(habitat, predicted_rate, rate_ci_low, rate_ci_high))

# -------------------------
# TEMPORAL PREDICTIONS (across study period for all habitats)
# -------------------------
night_seq_scaled <- seq(
  min(df_model$night_scaled),
  max(df_model$night_scaled),
  length.out = 150
)

newdata_temporal <- expand_grid(
  night_scaled = night_seq_scaled,
  habitat = factor(c("Interior", "Edge", "Open"), levels = c("Interior", "Edge", "Open"))
) %>%
  mutate(
    night = as.Date(night_scaled * night_scale + night_center, origin = "1970-01-01"),
    recording_hours = mean_hours,
    site = factor(ref_site, levels = levels(df_model$site)),
    detector_id = factor(ref_detector, levels = levels(df_model$detector_id))
  )

preds_temporal <- predict(
  model,
  newdata = newdata_temporal,
  type = "link",
  se.fit = TRUE,
  exclude = c("s(site)", "s(detector_id)")
)

temporal_predictions <- newdata_temporal %>%
  mutate(
    fit_link = preds_temporal$fit,
    se_link = preds_temporal$se.fit,
    predicted_calls = exp(fit_link),
    ci_low = exp(fit_link - 1.96 * se_link),
    ci_high = exp(fit_link + 1.96 * se_link),
    predicted_rate = predicted_calls / recording_hours,
    rate_ci_low = ci_low / recording_hours,
    rate_ci_high = ci_high / recording_hours
  )

message("✓ Temporal predictions generated")
message(sprintf("  Points per habitat: %d", nrow(temporal_predictions) / 3))
message(sprintf("  Date range: %s to %s",
                min(temporal_predictions$night),
                max(temporal_predictions$night)))

log_message("[PRODUCTION] Generated centralized predictions (habitat + temporal)",
            log_path = log_path)


# ==============================================================================
# CENTRALIZED PAIRWISE HABITAT COMPARISONS
# ==============================================================================
# NOTE: This block consolidates all pairwise habitat comparison logic in one
# location. It includes:
#   1. Interior-reference comparisons (from script 02 output)
#   2. Edge-reference refit for Edge vs Open comparison
#   3. Combined comparison table with bracket positioning
# All downstream plotting stages reference this centralized comparison object.
# ==============================================================================

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│     CENTRALIZED: Pairwise Habitat Comparisons                 │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Computing all pairwise habitat comparisons...")

# -------------------------
# Load Interior-reference comparisons from script 02 output
# -------------------------
habitat_effects_interior <- read_csv(
  here::here("results", "production", "tables", "habitat_effects_v1.csv"),
  show_col_types = FALSE
)

message("✓ Loaded Interior-reference comparisons")
print(habitat_effects_interior)

# -------------------------
# Refit model with Edge as reference for Edge vs Open comparison
# -------------------------
message("\n  Refitting model with Edge reference for Edge vs Open comparison...")

df_model_edge_ref <- df_model %>%
  mutate(habitat = factor(habitat, levels = c("Edge", "Interior", "Open")))

model_edge_ref <- fit_nb_gamm(df_model_edge_ref, smooth_k = 7, method = "REML", quiet = TRUE)
habitat_effects_edge <- extract_habitat_effects(model_edge_ref, reference = "Edge")

message("✓ Edge-reference comparisons generated")
print(habitat_effects_edge)

# -------------------------
# Combine all pairwise comparisons into single canonical object
# -------------------------
pairwise_comparisons <- bind_rows(
  # Interior vs Edge, Interior vs Open
  habitat_effects_interior %>%
    mutate(
      comparison_name = case_when(
        grepl("Edge", term) ~ "Interior vs Edge",
        grepl("Open", term) ~ "Interior vs Open",
        TRUE ~ NA_character_
      ),
      x_start = 1,  # Interior position
      x_end = case_when(
        grepl("Edge", term) ~ 2,  # Edge position
        grepl("Open", term) ~ 3,  # Open position
        TRUE ~ NA_real_
      )
    ),
  # Edge vs Open
  habitat_effects_edge %>%
    filter(grepl("Open", term)) %>%
    mutate(
      comparison_name = "Edge vs Open",
      x_start = 2,  # Edge position
      x_end = 3     # Open position
    )
) %>%
  filter(!is.na(comparison_name)) %>%
  mutate(
    p_display = case_when(
      p_value < 0.001 ~ sprintf("p < 0.001 ***"),
      p_value < 0.01 ~ sprintf("p = %.3f **", p_value),
      p_value < 0.05 ~ sprintf("p = %.3f *", p_value),
      TRUE ~ sprintf("p = %.3f", p_value)
    ),
    sig_label = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

message("\n✓ All pairwise comparisons combined")
print(pairwise_comparisons %>% select(comparison_name, irr, p_value, sig_label, x_start, x_end))

# -------------------------
# Calculate bracket positions for significance annotations
# Bracket positioning algorithm preserved from original implementation
# -------------------------
max_prediction_rate <- max(predictions_habitat$rate_ci_high)
raw_data_cap <- 8  # Visual display cap for raw data points
display_ceiling <- max(raw_data_cap, max_prediction_rate * 1.5)

bracket_base <- display_ceiling * 0.75
bracket_spacing <- display_ceiling * 0.08

pairwise_comparisons <- pairwise_comparisons %>%
  arrange(x_end, x_start) %>%
  mutate(
    y_pos = bracket_base + (row_number() - 1) * bracket_spacing
  )

y_axis_max <- max(pairwise_comparisons$y_pos) + bracket_spacing * 1.2

message("\n=== BRACKET POSITIONS ===")
print(pairwise_comparisons %>% select(comparison_name, y_pos, p_display))

message(sprintf("\n  Y-axis range: 0 to %.2f calls/hour", y_axis_max))
message(sprintf("  Bracket display ceiling: %.2f calls/hour", display_ceiling))

log_message("[PRODUCTION] Generated centralized pairwise comparisons with bracket positions",
            log_path = log_path)



# ------------------------------------------------------------------------------
# STAGE 3.3: PREPARE RAW DATA FOR PLOTTING
# ------------------------------------------------------------------------------
# NOTE: Habitat predictions are now generated in the centralized predictions
# block above. This stage prepares raw data overlays for plotting.
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│            STAGE 3.3: Prepare Raw Data for Plotting           │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Preparing raw data for plotting overlays...")

# -------------------------
# Calculate sample sizes per habitat for annotations
# -------------------------
sample_sizes <- df_model %>%
  group_by(habitat) %>%
  summarise(
    n_nights = n(),
    n_detectors = n_distinct(detector_id),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("n=%d nights\n%d detectors", n_nights, n_detectors))

# -------------------------
# Calculate rates for raw data points
# -------------------------
df_model_rates <- df_model %>%
  mutate(rate_per_hour = calls_per_night / recording_hours)

# -------------------------
# Detect outliers for visual clarity annotation
# -------------------------
outlier_count <- df_model_rates %>%
  filter(rate_per_hour > raw_data_cap) %>%
  nrow()
outlier_max <- max(df_model_rates$rate_per_hour)

message(sprintf("✓ Raw data prepared for plotting"))
message(sprintf("  Raw data display capped at %.1f calls/hour for visual clarity", raw_data_cap))
if (outlier_count > 0) {
  message(sprintf("  %d outlier(s) above display cap (max: %.1f calls/hour)",
                  outlier_count, outlier_max))
}

log_message("[PRODUCTION] [Stage 3.3] Prepared raw data for plotting", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.4: CREATE HABITAT EFFECT FIGURE
# ------------------------------------------------------------------------------
# NOTE: This stage uses centralized predictions and pairwise comparisons
# generated in the consolidated blocks above.
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.4: Create Habitat Effect Figure              │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating habitat effect figure...")

# -------------------------
# Predicted activity +/- CI by habitat (RATES)
# Uses: predictions_habitat, pairwise_comparisons, df_model_rates (all centralized)
# -------------------------
fig_habitat <- ggplot(predictions_habitat,
                      aes(x = habitat, y = predicted_rate, color = habitat)) +
  # Raw data overlay - cap outliers for visual clarity
  geom_jitter(data = df_model_rates %>%
                mutate(rate_capped = pmin(rate_per_hour, raw_data_cap)),
              aes(y = rate_capped),
              width = 0.2, alpha = 0.55, size = 1.5, shape = 16,
              show.legend = FALSE) +
  # Point estimates for predictions
  geom_point(size = 5, show.legend = FALSE) +
  # 95% CI error bars (rate scale)
  geom_errorbar(aes(ymin = rate_ci_low, ymax = rate_ci_high),
                width = 0.15, linewidth = 1.2) +
  # P-value significance brackets (using centralized pairwise_comparisons)
  geom_segment(data = pairwise_comparisons,
               aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
               inherit.aes = FALSE, linewidth = 0.8, color = "black") +
  geom_segment(data = pairwise_comparisons,
               aes(x = x_start, xend = x_start,
                   y = y_pos, yend = y_pos - bracket_spacing * 0.15),
               inherit.aes = FALSE, linewidth = 0.8, color = "black") +
  geom_segment(data = pairwise_comparisons,
               aes(x = x_end, xend = x_end,
                   y = y_pos, yend = y_pos - bracket_spacing * 0.15),
               inherit.aes = FALSE, linewidth = 0.8, color = "black") +
  geom_text(data = pairwise_comparisons,
            aes(x = (x_start + x_end) / 2,
                y = y_pos + bracket_spacing * 0.45,
                label = p_display),
            inherit.aes = FALSE, size = 4.5, color = "black", fontface = "bold") +
  # Display labels and colors
  scale_x_discrete(labels = HABITAT_DISPLAY) +
  scale_color_manual(values = HABITAT_COLORS, guide = "none") +
  # Y-axis
  scale_y_continuous(limits = c(0, y_axis_max),
                     expand = expansion(mult = c(0, 0.05)),
                     breaks = scales::pretty_breaks(n = 6)) +
  # Axis labels
  labs(
    x = "Habitat Type",
    y = "Predicted Calls per Hour"
  ) +
  # Figure-specific theme (replaces POSTER_THEME)
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 11, color = "gray40", hjust = 0.5,
                                margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

res88 <- fig_habitat
res88
message("\n✓ Habitat effect figure created")

log_message("[PRODUCTION] [Stage 3.4] Created habitat effect figure", log_path = log_path)


# -------------------------
# Create simplified IRR figure (log scale)
# Uses: pairwise_comparisons (centralized)
# -------------------------

fig_habitat_irr <- ggplot(
  pairwise_comparisons %>%
    mutate(
      comparison = factor(
        comparison_name,
        levels = c(
          "Interior vs Edge",
          "Interior vs Open",
          "Edge vs Open"
        )
      )
    ),
  aes(x = comparison, y = irr)
) +
  # Null effect line (IRR = 1)
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.8
  ) +
  # IRR + 95% CI
  geom_pointrange(
    aes(ymin = ci_low, ymax = ci_high),
    size = 1.2
  ) +
  # P-value labels
  geom_text(
    aes(
      y = ci_high * 1.15,
      label = p_display
    ),
    size = 4.5,
    fontface = "bold"
  ) +
  # Log scale for symmetry
  scale_y_log10() +
  labs(
    x = "Habitat Comparison",
    y = "Incidence Rate Ratio (log scale)"
  ) +
  # Figure-specific theme (replaces POSTER_THEME)
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
    plot.caption = element_text(size = 11, color = "gray40", hjust = 0.5,
                                margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

res88 <- fig_habitat_irr
res88

message("\n✓ Habitat effect figure created (log-scale IRR)")

log_message(
  "[PRODUCTION] [Stage 3.4] Created simplified habitat effect figure",
  log_path = log_path
)

# ------------------------------------------------------------------------------
# STAGE 3.5: CREATE TEMPORAL SMOOTH FIGURE
# ------------------------------------------------------------------------------
# NOTE: This stage uses centralized temporal_predictions generated above.
# No CI capping is applied - predictions are shown as generated.
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.5: Create Temporal Smooth Figure             │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating temporal smooth figure...")

# Print summary of temporal predictions by habitat
for (hab in c("Interior", "Edge", "Open")) {
  hab_data <- temporal_predictions %>% filter(habitat == hab)
  message(sprintf("  %s rate range: %.2f to %.2f calls/hour",
                  hab,
                  min(hab_data$predicted_rate),
                  max(hab_data$predicted_rate)))
}

# -------------------------
# Create temporal smooth figure with all habitats
# Uses: temporal_predictions (centralized)
# -------------------------
fig_temporal <- ggplot(temporal_predictions, aes(x = night, y = predicted_rate,
                                                 color = habitat, fill = habitat)) +
  # 95% CI ribbons by habitat
  geom_ribbon(aes(ymin = rate_ci_low, ymax = rate_ci_high),
              alpha = 0.15, linewidth = 0.3) +
  # Temporal smooth lines by habitat
  geom_line(linewidth = 1.3) +
  # Use habitat display labels in legend
  scale_color_manual(values = HABITAT_COLORS, labels = HABITAT_DISPLAY, name = "Habitat") +
  scale_fill_manual(values = HABITAT_COLORS, labels = HABITAT_DISPLAY, name = "Habitat") +
  # X-axis: dates with weekly breaks
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  # Y-axis: start at 0, data-driven upper limit
  scale_y_continuous(limits = c(0, 10),
                     expand = expansion(mult = c(0, 0.05)),
                     breaks = scales::pretty_breaks(n = 6)) +
  # Axis labels
  labs(
    x = "Date",
    y = "Predicted Calls per Hour"
  ) +
  # Figure-specific theme (replaces POSTER_THEME)
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 11, color = "gray40", hjust = 0.5,
                                margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5, "lines")
  )

res1 <- fig_temporal
res1
message("✓ Temporal smooth figure created")

log_message("[PRODUCTION] [Stage 3.5] Created temporal smooth figure", log_path = log_path)


# ------------------------------------------------------------------------------
# STAGE 3.6: CREATE RAW ACTIVITY FIGURE
# ------------------------------------------------------------------------------
# NOTE: This stage uses centralized temporal_predictions generated above.
# Raw data is overlaid with model predictions on log scale.
# ------------------------------------------------------------------------------

message("\n┌────────────────────────────────────────────────────────────────┐")
message("│           STAGE 3.6: Create Raw Activity Figure                │")
message("└────────────────────────────────────────────────────────────────┘\n")

message("Creating raw activity by habitat figure with GAMM predictions...")

# -------------------------
# Create figure with log10 y-axis
# Uses: temporal_predictions (centralized), df_model_rates (prepared in Stage 3.3)
# -------------------------
fig_raw_activity <- ggplot() +
  geom_ribbon(
    data = temporal_predictions,
    aes(x = night, ymin = rate_ci_low, ymax = rate_ci_high, fill = habitat),
    alpha = 0.15, linewidth = 0.3
  ) +
  geom_point(
    data = df_model_rates,
    aes(x = night, y = rate_per_hour, color = habitat),
    alpha = 0.35, size = 1.5
  ) +
  geom_line(
    data = temporal_predictions,
    aes(x = night, y = predicted_rate, color = habitat),
    linewidth = 1.3
  ) +
  facet_wrap(
    ~habitat,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(habitat = HABITAT_DISPLAY)
  ) +
  scale_color_manual(values = HABITAT_COLORS, guide = "none") +
  scale_fill_manual(values = HABITAT_COLORS, guide = "none") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  scale_y_log10(
    labels = scales::comma_format(),
    breaks = scales::log_breaks(n = 10)
  ) +
  labs(
    x = "Date",
    y = "Calls per Hour (log scale)"
  ) +
  # Figure-specific theme (replaces POSTER_THEME)
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 11, color = "gray40", hjust = 0.5,
                                margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10),
    strip.text = element_text(face = "bold", size = 16)
  )

res3 <- fig_raw_activity
res3
message("✓ Raw activity figure (log10) created")

log_message("[PRODUCTION] [Stage 3.6] Created raw activity figure (log10)", log_path = log_path)

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
    plot.title = element_text(face = "bold"),
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
if (!requireNamespace("patchwork", quietly = TRUE)) {
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
message("│                 STAGE 3.8: Write Outputs                       │")
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
message("  Saved: raw_activity_by_habitat_v1.png")

if (!is.null(fig_diagnostic)) {
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
  ggsave(here::here(res_figs, "diag_resid_fitted_v1.png"), resid_fitted_plot, width = 6, height = 5, dpi = FIG_DPI)
  ggsave(here::here(res_figs, "diag_resid_hist_v1.png"), resid_hist, width = 6, height = 5, dpi = FIG_DPI)
  ggsave(here::here(res_figs, "diag_obs_fitted_v1.png"), obs_fitted_plot, width = 6, height = 5, dpi = FIG_DPI)
  message("  Saved: individual diagnostic plots (diag_*.png)")
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
message(sprintf("  Predictions at: %.1f hrs effort (mean)", mean_hours))

message("\n  Habitat Predictions (calls/night):")
for (i in seq_len(nrow(predictions_habitat))) {
  message(sprintf("    %s: %.1f [%.1f, %.1f]",
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
habitat_comparisons <- pairwise_comparisons
message("Objects available: habitat_predictions, temporal_predictions_df, habitat_comparisons")
