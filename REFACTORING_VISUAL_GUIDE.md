# Visual Guide: Script Refactoring

## Before vs After Structure

### BEFORE: Scattered Logic

```
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.1: Setup                                                │
│  ├─ Libraries, logging                                          │
│  ├─ Color palette                                               │
│  └─ POSTER_THEME (global - later modified per figure) ❌        │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.2: Load Model and Data                                  │
│  ├─ Load model                                                  │
│  ├─ Load backbone data                                          │
│  └─ Prepare modeling data                                       │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.3: Generate Habitat Predictions ⚠️                      │
│  ├─ Create prediction grid (habitat)                            │
│  ├─ predict() call                                              │
│  ├─ Calculate predicted_rate, ci_low, ci_high                   │
│  └─ Store in predictions_habitat                                │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.4: Create Habitat Effect Figure                         │
│  ├─ Calculate sample sizes ⚠️                                   │
│  ├─ Load habitat_effects_interior from CSV ⚠️                   │
│  ├─ Refit model with Edge reference ⚠️                          │
│  ├─ Combine into all_comparisons ⚠️                             │
│  ├─ Calculate bracket positions ⚠️                              │
│  ├─ Create figure                                               │
│  └─ Uses POSTER_THEME + modifications ❌                        │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.5: Create Temporal Smooth Figure                        │
│  ├─ Create prediction grid (temporal) ⚠️ DUPLICATE              │
│  ├─ predict() call ⚠️ DUPLICATE                                 │
│  ├─ Calculate temporal_predictions ⚠️ DUPLICATE                 │
│  ├─ Apply CI capping (lines 648-668) ❌ REMOVED                 │
│  ├─ Create figure                                               │
│  └─ Uses POSTER_THEME + different modifications ❌              │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.6: Create Raw Activity Figure                           │
│  ├─ Create prediction grid (temporal) ⚠️ DUPLICATE              │
│  ├─ predict() call ⚠️ DUPLICATE                                 │
│  ├─ Calculate model_predictions ⚠️ DUPLICATE                    │
│  ├─ Create figure                                               │
│  └─ Uses POSTER_THEME + different modifications ❌              │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.7: Create Diagnostic Panel                              │
│  └─ Uses theme_minimal() directly ✓                             │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.8: Write Outputs                                        │
└─────────────────────────────────────────────────────────────────┘

Legend:
  ❌ = Removed/Changed
  ⚠️ = Duplicated/Scattered
  ✓ = Good
```

---

### AFTER: Centralized Logic

```
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.1: Setup                                                │
│  ├─ Libraries, logging                                          │
│  ├─ Color palette                                               │
│  └─ POSTER_THEME removed ✓                                      │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.2: Load Model and Data                                  │
│  ├─ Load model                                                  │
│  ├─ Load backbone data                                          │
│  └─ Prepare modeling data                                       │
└─────────────────────────────────────────────────────────────────┘
                           ↓
╔═════════════════════════════════════════════════════════════════╗
║ ★ CENTRALIZED PREDICTIONS (NEW) ★                              ║
║                                                                 ║
║ Habitat Predictions:                                            ║
║  ├─ Create newdata_habitat                                      ║
║  ├─ predict() for habitat levels                                ║
║  └─ predictions_habitat (standardized names) ✓                  ║
║                                                                 ║
║ Temporal Predictions:                                           ║
║  ├─ Create newdata_temporal (150 points × 3 habitats)           ║
║  ├─ predict() for temporal trend                                ║
║  └─ temporal_predictions (standardized names) ✓                 ║
║                                                                 ║
║ ✓ Single source of truth                                       ║
║ ✓ No CI capping                                                 ║
║ ✓ Consistent variable naming                                   ║
╚═════════════════════════════════════════════════════════════════╝
                           ↓
╔═════════════════════════════════════════════════════════════════╗
║ ★ CENTRALIZED COMPARISONS (NEW) ★                              ║
║                                                                 ║
║ Load Interior Reference:                                        ║
║  └─ habitat_effects_interior from CSV                           ║
║                                                                 ║
║ Edge vs Open Comparison:                                        ║
║  ├─ Refit model with Edge reference                             ║
║  └─ habitat_effects_edge                                        ║
║                                                                 ║
║ Combine All Comparisons:                                        ║
║  ├─ bind_rows() all three comparisons                           ║
║  ├─ Add p_display, sig_label                                    ║
║  └─ pairwise_comparisons ✓                                      ║
║                                                                 ║
║ Calculate Bracket Positions:                                    ║
║  ├─ bracket_base, bracket_spacing                               ║
║  └─ y_pos for each comparison                                   ║
║                                                                 ║
║ ✓ All comparisons in one object                                ║
║ ✓ Bracket algorithm preserved                                  ║
╚═════════════════════════════════════════════════════════════════╝
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.3: Prepare Raw Data for Plotting ✓                     │
│  ├─ Calculate sample sizes                                      │
│  ├─ Calculate df_model_rates                                    │
│  └─ Detect outliers (visual annotation only)                    │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.4: Create Habitat Effect Figure ✓                      │
│  ├─ Uses: predictions_habitat (centralized)                     │
│  ├─ Uses: pairwise_comparisons (centralized)                    │
│  ├─ Uses: df_model_rates (from Stage 3.3)                       │
│  ├─ Create habitat effect figure                                │
│  ├─ Create IRR log-scale figure                                 │
│  └─ Each with inline theme ✓                                    │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.5: Create Temporal Smooth Figure ✓                     │
│  ├─ Uses: temporal_predictions (centralized)                    │
│  ├─ No duplicate prediction logic ✓                             │
│  ├─ No CI capping ✓                                             │
│  ├─ Create temporal smooth figure                               │
│  └─ Inline theme ✓                                              │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.6: Create Raw Activity Figure ✓                        │
│  ├─ Uses: temporal_predictions (centralized)                    │
│  ├─ Uses: df_model_rates (from Stage 3.3)                       │
│  ├─ No duplicate prediction logic ✓                             │
│  ├─ Create raw activity figure (log scale)                      │
│  └─ Inline theme ✓                                              │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.7: Create Diagnostic Panel ✓                           │
│  └─ Unchanged (already good)                                    │
└─────────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│ Stage 3.8: Write Outputs ✓                                     │
│  └─ Uses: predictions_habitat, temporal_predictions             │
└─────────────────────────────────────────────────────────────────┘

Legend:
  ✓ = Improved/Good
  ★ = New centralized block
```

---

## Data Flow Diagram

### BEFORE: Spaghetti Flow

```
┌──────────────┐
│   Model      │
│   Loaded     │
└──────┬───────┘
       │
       ├─────────────────────┐
       │                     │
       ↓                     ↓
  Stage 3.3            Stage 3.4
  predictions_habitat  ├─ Load CSV
       │               ├─ Refit model
       │               ├─ all_comparisons
       │               └─ pval_annotations
       │                     │
       ├─────────────────────┘
       │
       ├─────────────────────┐
       │                     │
       ↓                     ↓
  Stage 3.5            Stage 3.6
  temporal_predictions model_predictions
  (with CI capping)    (duplicate logic)
       │                     │
       └──────────┬──────────┘
                  ↓
             Plotting Stages
          (scattered references)
```

### AFTER: Clean Flow

```
┌──────────────┐
│   Model      │
│   Loaded     │
└──────┬───────┘
       │
       ↓
  ╔═══════════════════════╗
  ║ CENTRALIZED           ║
  ║ PREDICTIONS           ║
  ║                       ║
  ║ ├─ predictions_habitat║
  ║ └─ temporal_predictions
  ╚═══════════┬═══════════╝
              │
              ↓
  ╔═══════════════════════╗
  ║ CENTRALIZED           ║
  ║ COMPARISONS           ║
  ║                       ║
  ║ └─ pairwise_comparisons
  ╚═══════════┬═══════════╝
              │
              ├─────┬─────┬─────┐
              ↓     ↓     ↓     ↓
         Stage  Stage  Stage  Stage
          3.4    3.5    3.6    3.8
          
    All reference the same
    centralized objects ✓
```

---

## Variable Name Standardization

### BEFORE: Inconsistent Naming

```
# predictions_habitat (Stage 3.3)
├─ predicted_calls
├─ ci_low
├─ ci_high
├─ predicted_rate
├─ rate_ci_low
└─ rate_ci_high

# temporal_predictions (Stage 3.5)
├─ fit_link
├─ se_link
├─ predicted_rate
├─ rate_ci_low
├─ rate_ci_high
└─ rate_ci_high_capped ❌

# model_predictions (Stage 3.6)
├─ fit_link
├─ se_link
├─ predicted_calls
├─ ci_low_calls ⚠️ different name
├─ ci_high_calls ⚠️ different name
├─ predicted_rate
├─ ci_low_rate ⚠️ different name
└─ ci_high_rate ⚠️ different name

# df_model_rates (various locations)
└─ rate_per_hour ⚠️ yet another name
```

### AFTER: Consistent Naming

```
# predictions_habitat
├─ predicted_calls
├─ ci_low              ✓ standardized
├─ ci_high             ✓ standardized
├─ predicted_rate      ✓ standardized
├─ rate_ci_low         ✓ standardized
└─ rate_ci_high        ✓ standardized

# temporal_predictions
├─ predicted_calls
├─ ci_low              ✓ same names
├─ ci_high             ✓ same names
├─ predicted_rate      ✓ same names
├─ rate_ci_low         ✓ same names
└─ rate_ci_high        ✓ same names

# df_model_rates (raw data)
└─ rate_per_hour       ✓ distinct name for raw data
```

---

## Comparison Objects

### BEFORE: Multiple Objects

```
habitat_effects_interior (from CSV)
  ├─ term
  ├─ irr
  ├─ p_value
  └─ ...

habitat_effects_edge (refitted)
  ├─ term
  ├─ irr
  ├─ p_value
  └─ ...

all_comparisons (combined)
  ├─ comparison_name
  ├─ irr
  ├─ p_value
  ├─ x_start
  └─ x_end

pval_annotations (with positions) ⚠️
  ├─ ...all_comparisons columns
  └─ y_pos
```

### AFTER: Single Canonical Object

```
pairwise_comparisons (complete)
  ├─ term
  ├─ comparison_name
  ├─ irr
  ├─ ci_low
  ├─ ci_high
  ├─ p_value
  ├─ p_display
  ├─ sig_label
  ├─ x_start
  ├─ x_end
  └─ y_pos ✓

✓ Everything in one place
✓ Used by all plotting stages
✓ Easy to modify
```

---

## Theme Application

### BEFORE: Global with Modifications

```r
# Global definition
POSTER_THEME <- theme_minimal(base_size = 16) + theme(...)

# Figure 1
fig_habitat <- ggplot(...) +
  POSTER_THEME  # Uses global

# Figure 2  
fig_temporal <- ggplot(...) +
  POSTER_THEME +
  theme(legend.position = "top", ...)  # Modifies global ⚠️

# Figure 3
fig_raw <- ggplot(...) +
  POSTER_THEME +
  theme(strip.text = element_text(...))  # Different modification ⚠️

❌ Global theme defeated by per-figure modifications
❌ Hard to see what each figure actually uses
❌ Implicit dependency on global object
```

### AFTER: Inline Themes

```r
# No global theme ✓

# Figure 1
fig_habitat <- ggplot(...) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    ...
    # All settings explicit ✓
  )

# Figure 2
fig_temporal <- ggplot(...) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    ...
    legend.position = "top",  # Figure-specific ✓
    ...
  )

# Figure 3
fig_raw <- ggplot(...) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    ...
    strip.text = element_text(...),  # Figure-specific ✓
    ...
  )

✓ Each figure is self-contained
✓ Easy to see exact theme for each figure
✓ No hidden dependencies
✓ Visual consistency maintained
```

---

## Benefits Summary

### Code Quality Improvements

| Aspect | Before | After |
|--------|--------|-------|
| **Prediction blocks** | 3 duplicate blocks | 1 centralized block ✓ |
| **Lines of code** | 1069 lines | 1065 lines (-0.4%) |
| **Variable naming** | Inconsistent | Standardized ✓ |
| **Comparison objects** | 4 different objects | 1 canonical object ✓ |
| **Theme definition** | Global + per-figure mods | Explicit inline ✓ |
| **CI capping** | Applied in Stage 3.5 ❌ | None (removed) ✓ |

### Maintainability Improvements

| Task | Before | After |
|------|--------|-------|
| **Change predictions** | Update 3 places | Update 1 place ✓ |
| **Change comparisons** | Update scattered logic | Update 1 block ✓ |
| **Change figure theme** | Update POSTER_THEME + mods | Update inline theme ✓ |
| **Understand data flow** | Follow scattered refs | Follow centralized blocks ✓ |
| **Verify no CI capping** | Check multiple places | Check centralized block ✓ |

### Statistical Integrity

| Aspect | Before | After |
|--------|--------|-------|
| **CI capping** | Applied in temporal ❌ | None applied ✓ |
| **Prediction values** | Consistent but scattered | Consistent and centralized ✓ |
| **Pairwise comparisons** | Correct but scattered | Correct and centralized ✓ |
| **Statistical behavior** | Correct | Preserved exactly ✓ |

---

## Quick Reference: Where Things Live Now

### Predictions
```
Location: After Stage 3.2, in CENTRALIZED PREDICTIONS block
Objects created:
  - predictions_habitat (3 rows: Interior, Edge, Open)
  - temporal_predictions (450 rows: 150 points × 3 habitats)
```

### Comparisons
```
Location: After predictions, in CENTRALIZED COMPARISONS block
Objects created:
  - pairwise_comparisons (3 rows: Int-Edge, Int-Open, Edge-Open)
Includes: IRR, CI, p-values, bracket positions
```

### Raw Data Prep
```
Location: Stage 3.3
Objects created:
  - sample_sizes
  - df_model_rates
  - outlier_count, outlier_max
```

### Plotting
```
Stage 3.4: Uses predictions_habitat + pairwise_comparisons
Stage 3.5: Uses temporal_predictions
Stage 3.6: Uses temporal_predictions + df_model_rates
Stage 3.7: Unchanged
Stage 3.8: Uses predictions_habitat + temporal_predictions
```
