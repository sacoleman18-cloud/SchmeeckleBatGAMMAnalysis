# Visual Guide: Bracket Visibility Fix

## The Problem: Brackets Cut Off

### Before Fix - Hardcoded Y-Axis Limit

```
Y-axis (0 to 4)
│
4 ─────────────────────────────────────────────── ← Y-axis limit (hardcoded)
│                                                    BRACKETS HIDDEN ABOVE! ❌
│
│                                                 ╔═══════════════════════════╗
│                                                 ║  Edge vs Open (y=7.28)    ║
│                                                 ║  p = 0.156                ║
│                                                 ╚═══════════════════════════╝
│                                            ╔═══════════════════════════════╗
│                                            ║  Interior vs Open (y=6.64)    ║
│                                            ║  p < 0.001 ***                ║
│                                            ╚═══════════════════════════════╝
│                                  ╔═══════════════════════════════════╗
3 │                                 ║  Interior vs Edge (y=6.00)        ║
│                                   ║  p = 0.023 *                      ║
│                                   ╚═══════════════════════════════════╝
│
│                                   
2 │          ●                ●                 ●
│          │                │                 │
│         ───              ───               ───
│                                   
1 │
│
│
0 ─────────────────────────────────────────────────────────────────
     Interior          Edge              Open
```

**Problem**: All brackets positioned at y=6+ but y-axis stops at 4!

---

### After Fix - Calculated Y-Axis Limit

```
Y-axis (0 to 8.05)
│
8 ─────────────────────────────────────────────── ← Y-axis limit (calculated)
│                                                    y_axis_max = 8.05 ✓
│                              p = 0.156
│                        ┌──────────────────┐
7 │                       │                  │      ← Edge vs Open bracket
│                        ●                  ●
│                   p < 0.001 ***
│              ┌─────────────────────────────┐
6 │             │                             │      ← Interior vs Open bracket
│             ●                             ●
│     p = 0.023 *
│    ┌──────────────┐
5 │   │              │                                ← Interior vs Edge bracket
│   ●              ●
│
│
4 │                                                   ← Old hardcoded limit
│
│
3 │
│
│
2 │          ●                ●                 ●    ← Predictions with CI
│          │                │                 │
│         ───              ───               ───
│                                   
1 │          ○                ○                 ○    ← Raw data points (jittered)
│         ○○               ○○                ○○
│        ○ ○              ○ ○               ○ ○
│
0 ─────────────────────────────────────────────────────────────────
     Interior          Edge              Open
```

**Solution**: Y-axis extends to 8.05, all brackets visible! ✓

---

## Code Change

### The Fix (2 Lines)

```r
# Before
scale_y_continuous(limits = c(0, 4), ...)  # ❌ Hardcoded

# After  
scale_y_continuous(limits = c(0, y_axis_max), ...)  # ✓ Dynamic
```

### Where y_axis_max Comes From

```r
# Centralized comparisons block (after Stage 3.2)

# Step 1: Calculate bracket positions
display_ceiling <- max(raw_data_cap, max_prediction_rate * 1.5)
bracket_base <- display_ceiling * 0.75
bracket_spacing <- display_ceiling * 0.08

# Step 2: Assign positions to each bracket
pairwise_comparisons <- pairwise_comparisons %>%
  mutate(y_pos = bracket_base + (row_number() - 1) * bracket_spacing)

# Step 3: Calculate y-axis limit (with headroom above top bracket)
y_axis_max <- max(pairwise_comparisons$y_pos) + bracket_spacing * 1.2
```

---

## Bracket Anatomy

Each bracket consists of 4 ggplot layers:

```
        p = 0.023 *              ← geom_text (label = p_display)
        y = 6.29                    at y_pos + spacing * 0.45

  ┌──────────────┐              ← geom_segment (horizontal line)
  │              │                 at y = y_pos
  ●              ●
  │              │              ← geom_segment (left tick)
  │              └───              geom_segment (right tick)
  │                                 at y_pos - spacing * 0.15
  └─────
```

### Layer Breakdown

```r
# Layer 1: Horizontal line
geom_segment(
  data = pairwise_comparisons,
  aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
  linewidth = 0.8, color = "black"
)

# Layer 2: Left vertical tick
geom_segment(
  data = pairwise_comparisons,
  aes(x = x_start, xend = x_start,
      y = y_pos, yend = y_pos - bracket_spacing * 0.15),
  linewidth = 0.8, color = "black"
)

# Layer 3: Right vertical tick  
geom_segment(
  data = pairwise_comparisons,
  aes(x = x_end, xend = x_end,
      y = y_pos, yend = y_pos - bracket_spacing * 0.15),
  linewidth = 0.8, color = "black"
)

# Layer 4: P-value text
geom_text(
  data = pairwise_comparisons,
  aes(x = (x_start + x_end) / 2,
      y = y_pos + bracket_spacing * 0.45,
      label = p_display),
  size = 4.5, color = "black", fontface = "bold"
)
```

---

## Bracket Positioning Logic

### Why Brackets Stack

```r
pairwise_comparisons %>%
  arrange(x_end, x_start) %>%  # Sort order controls stacking
  mutate(y_pos = bracket_base + (row_number() - 1) * bracket_spacing)
```

**Sorting logic**:
1. `x_end` (primary): Brackets ending farther right go higher
2. `x_start` (secondary): Among brackets with same end, shorter spans go lower

**Result**:
```
Row 1: Interior vs Edge   (x_start=1, x_end=2) → y_pos = 6.00 (lowest)
Row 2: Interior vs Open   (x_start=1, x_end=3) → y_pos = 6.64
Row 3: Edge vs Open       (x_start=2, x_end=3) → y_pos = 7.28 (highest)
```

### Spacing Calculation

```
bracket_spacing = display_ceiling * 0.08
```

**Purpose**: 8% of display ceiling ensures:
- Brackets don't overlap
- Visually balanced spacing
- Scales with data range

**Example**:
- If `display_ceiling = 8`: spacing = 0.64
- If `display_ceiling = 12`: spacing = 0.96 (larger for bigger plots)

---

## Data Flow Diagram

```
┌─────────────────────────────────────┐
│  Stage 3.2: Load Model and Data     │
└─────────────┬───────────────────────┘
              │
              ↓
╔═════════════════════════════════════╗
║  CENTRALIZED PREDICTIONS            ║
║  ├─ predictions_habitat             ║
║  └─ temporal_predictions            ║
╚═════════════┬═══════════════════════╝
              │
              ↓
╔═════════════════════════════════════╗
║  CENTRALIZED COMPARISONS            ║
║  ├─ pairwise_comparisons            ║
║  │   ├─ comparison_name             ║
║  │   ├─ irr, ci_low, ci_high        ║
║  │   ├─ p_value, p_display          ║
║  │   ├─ x_start, x_end              ║
║  │   └─ y_pos ← calculated here     ║
║  │                                   ║
║  ├─ bracket_spacing ← used in plot  ║
║  └─ y_axis_max ← CRITICAL FOR FIX   ║
╚═════════════┬═══════════════════════╝
              │
              ↓
┌─────────────────────────────────────┐
│  Stage 3.4: Create Habitat Figure   │
│                                     │
│  fig_habitat <- ggplot(...) +       │
│    geom_segment(...) +  ← brackets  │
│    scale_y_continuous(              │
│      limits = c(0, y_axis_max) ← FIX│
│    )                                │
└─────────────────────────────────────┘
```

---

## Console Output (When Script Runs)

### Before Fix
```
✓ Habitat predictions generated
  habitat predicted_rate rate_ci_low rate_ci_high
  Interior          1.85        1.42         2.41
  Edge              1.92        1.46         2.52
  Open              0.89        0.65         1.22

✓ All pairwise comparisons combined
  comparison_name   irr p_value sig_label x_start x_end
  Interior vs Edge 1.04   0.023 *               1     2
  Interior vs Open 2.08   0.000 ***             1     3
  Edge vs Open     2.16   0.156                 2     3

  Bracket display ceiling: 8.00 calls/hour
  Y-axis max: 8.05 calls/hour
  
✓ Habitat effect figure created
```

**Problem**: Brackets at y=6-7, but no warning that y-axis stops at 4!

### After Fix
```
✓ Habitat predictions generated
  habitat predicted_rate rate_ci_low rate_ci_high
  Interior          1.85        1.42         2.41
  Edge              1.92        1.46         2.52
  Open              0.89        0.65         1.22

✓ All pairwise comparisons combined
  comparison_name   irr p_value sig_label x_start x_end
  Interior vs Edge 1.04   0.023 *               1     2
  Interior vs Open 2.08   0.000 ***             1     3
  Edge vs Open     2.16   0.156                 2     3

=== BRACKET POSITIONS ===
  comparison_name   y_pos p_display        
  Interior vs Edge  6.00 p = 0.023 *      
  Interior vs Open  6.64 p < 0.001 ***    
  Edge vs Open      7.28 p = 0.156        

  Y-axis range: 0 to 8.05 calls/hour
  Bracket display ceiling: 8.00 calls/hour

✓ Habitat effect figure created
```

**Solution**: Clear output shows bracket positions and y-axis range! ✓

---

## Testing Checklist

When verifying the fix, check:

### Console Output
- [ ] "=== BRACKET POSITIONS ===" section appears
- [ ] Three rows with comparison names and y_pos values
- [ ] Y-axis range shown (should be > 4, typically 7-9)
- [ ] All y_pos values fit within y-axis range

### Plot Visual
- [ ] Three horizontal brackets visible above data points
- [ ] Each bracket has vertical ticks on both ends
- [ ] P-value text legible and positioned above each bracket
- [ ] No brackets cut off at top of plot
- [ ] Y-axis extends with headroom above highest bracket

### Statistical Content
- [ ] Interior vs Edge comparison present
- [ ] Interior vs Open comparison present
- [ ] Edge vs Open comparison present
- [ ] P-values match those in console output
- [ ] Significance markers correct (*, **, ***)

---

## Summary

### The Bug
```diff
- Y-axis hardcoded to 4
- Brackets positioned at 6-7
= Brackets invisible (above plot area)
```

### The Fix
```diff
+ Y-axis calculated as max(bracket_y_pos) + headroom
+ Brackets positioned at 6-7
= Brackets visible (within plot area)
```

### Lines Changed
- **1 line** modified: `limits = c(0, 4)` → `limits = c(0, y_axis_max)`
- **3 lines** added: Diagnostic print statements
- **Total**: 4 lines changed

### Impact
✓ Brackets now visible  
✓ P-values now readable  
✓ Statistical comparisons now clear  
✓ All refactoring benefits maintained  
✓ No other behavior changed  
