# inst/config/study_parameters.yaml
# Schmeeckle Bat Habitat Edge GAMM Analysis
# =============================================================================

config_version: 1

study_parameters:
  study_name: "Schmeeckle Bat Habitat Edge Study"
timezone: "America/Chicago"
start_date: "2025-10-04"
end_date: "2025-10-31"

# Detector → Site/Habitat mapping (nested structure)
# Site codes: LP, MC, SM (3 independent sites, >400m apart)
# Habitat codes: Edge, Interior, Open
detector_mapping:
  LPE: {site: "LP", habitat: "Edge"}
LPI: {site: "LP", habitat: "Interior"}
LPO: {site: "LP", habitat: "Open"}
MCE: {site: "MC", habitat: "Edge"}
MCI: {site: "MC", habitat: "Interior"}
MCO: {site: "MC", habitat: "Open"}
SME: {site: "SM", habitat: "Edge"}
SMI: {site: "SM", habitat: "Interior"}
SMO: {site: "SM", habitat: "Open"}

night_classification:
  # Thresholds for classifying recording effort quality
  # Based on expected ~13 hr recording duration
  pass_threshold: 12.5       # ≥12.5 hr = full night

partial_threshold: 11.5    # 11.5–12.49 hr = nearly full
short_threshold: 1.0       # 1–11.49 hr = usable but low weight via offset
# 0 hr = dead night (excluded from model, retained in grid)

data_paths:
  raw_input: "data/raw/calls_per_night_raw.csv"
